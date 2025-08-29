/***************************************************************************
  reoshydraulicstructure2d.cpp - ReosHydraulicStructure2D

 ---------------------
 begin                : 9.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshydraulicstructure2d.h"
#include "reosmeshgenerator.h"
#include "reospolygonstructure.h"
#ifdef WITH_GMSH
#include "gmsh/reosgmshgenerator.h"
#endif //WITH_GMSH
#include "reostopographycollection.h"
#include "reoshydraulicsimulation.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoshydraulicsimulation.h"
#include "reoscalculationcontext.h"
#include "reoshydraulicsimulationresults.h"
#include "reoshydraulicscheme.h"
#include "reosgisengine.h"
#include "reostimewindowsettings.h"
#include "reosstructureimporter.h"

#include <QTimer>
#include <QProcess>
#include <QDir>
#include <QEventLoop>

ReosHydraulicStructure2D::ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( context.network() )
  , mCapabilities( GeometryEditable | MultiSimulation )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( domain, crs ) )
  , mMesh( ReosMesh::createMeshFrame( crs ) )
#ifdef WITH_GMSH
  , mMeshGenerator( new ReosGmshGenerator( this ) )
#endif //WITH_GMSH
  , mMeshResolutionController( new ReosMeshResolutionController( this, crs ) )
  , mTopographyCollection( ReosTopographyCollection::createTopographyCollection( context.network()->gisEngine(), this ) )
  , mRoughnessStructure( new ReosRoughnessStructure( crs ) )
  , mProfilesCollection( new ReosHydraulicStructureProfilesCollection( this ) )
  , mTimeWindowSettings( new ReosTimeWindowSettings( this ) )
{
  initConnection();
}

ReosHydraulicStructure2D::ReosHydraulicStructure2D(
  const ReosEncodedElement &encodedElement,
  const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( encodedElement, context.network() )
  , mProfilesCollection( new ReosHydraulicStructureProfilesCollection( this ) )
  , mTimeWindowSettings( new ReosTimeWindowSettings( this ) )
  , m3dMapSettings( encodedElement.getEncodedData( QStringLiteral( "3d-map-settings" ) ) )
  , m3dTerrainSettings( encodedElement.getEncodedData( QStringLiteral( "3d-terrain-settings" ) ) )
{
  // Before all, load cababilities
  if ( !encodedElement.getData( QStringLiteral( "capabilities" ), mCapabilities ) )
    mCapabilities = Structure2DCapabilities( GeometryEditable | MultiSimulation ) ;

  std::unique_ptr<ReosStructureImporter> importer;
  // Load base geometry (structure line  + mesh)
  if ( hasCapability( DefinedExternally ) )
  {
    if ( encodedElement.hasEncodedData( QStringLiteral( "structure-importer" ) ) )
    {
      mStructureImporterSource.reset(
        ReosSimulationEngineRegistery::instance()->createStructureImporterSource(
          encodedElement.getEncodedData( QStringLiteral( "structure-importer" ) ), mNetwork->context() ) );

      importer.reset( mStructureImporterSource->createImporter() );
    }

    if ( importer && importer->isValid() )
    {
      mPolylinesStructures = ReosPolylinesStructure::createPolylineStructure( importer->domain(), importer->crs() );
      mMesh.reset( importer->mesh( context.network()->gisEngine()->crs() ) );
    }
    else
    {
      mPolylinesStructures = ReosPolylinesStructure::createPolylineStructure( QString() );
      mMesh.reset( ReosMesh::createMeshFrame() );
    }
  }
  else
  {
    if ( encodedElement.hasEncodedData( QStringLiteral( "structure" ) ) )
      mPolylinesStructures = ReosPolylinesStructure::createPolylineStructure( encodedElement.getEncodedData( QStringLiteral( "structure" ) ) );
    ReosModule::Message message;
    mMesh.reset( ReosMesh::createMeshFrameFromFile( structureDirectory().path() + QStringLiteral( "/meshFrame.nc" ), context.crs(), message ) );
    if ( message.type != ReosModule::Simple )
      mNetwork->message( message, true );
  }

  // Load geometry editing helper if exists
  if ( encodedElement.hasEncodedData( QStringLiteral( "mesh-generator" ) ) )
    mMeshGenerator = ReosMeshGenerator::createMeshGenerator( encodedElement.getEncodedData( QStringLiteral( "mesh-generator" ) ), this );
  else if ( hasCapability( GeometryEditable ) )
#ifdef WITH_GMSH
    mMeshGenerator = new ReosGmshGenerator( this );
#else
    mMeshGenerator = nullptr;
#endif // WITH_GMSH

  if ( encodedElement.hasEncodedData( QStringLiteral( "topography-collection" ) ) )
    mTopographyCollection = ReosTopographyCollection::createTopographyCollection( encodedElement.getEncodedData( QStringLiteral( "topography-collection" ) ),
                            context.network()->gisEngine(), this );
  else if ( hasCapability( GeometryEditable ) )
    mTopographyCollection = ReosTopographyCollection::createTopographyCollection( context.network()->gisEngine(), this );

  if ( encodedElement.hasEncodedData( QStringLiteral( "roughness-structure" ) ) )
    mRoughnessStructure.reset( new ReosRoughnessStructure( encodedElement.getEncodedData( QStringLiteral( "roughness-structure" ) ) ) );

  if ( encodedElement.hasEncodedData( QStringLiteral( "mesh-resolution-controller" ) ) )
    mMeshResolutionController = new ReosMeshResolutionController( encodedElement.getEncodedData( QStringLiteral( "mesh-resolution-controller" ) ), this );
  else if ( hasCapability( GeometryEditable ) )
    mMeshResolutionController = new ReosMeshResolutionController( this );

  initConnection();

  mMesh->setQualityMeshParameter( encodedElement.getEncodedData( QStringLiteral( "mesh-quality-parameters" ) ) );

  encodedElement.getData( QStringLiteral( "boundaries-vertices" ), mBoundaryVertices );
  encodedElement.getData( QStringLiteral( "hole-vertices" ), mHolesVertices );
  mMesh->setBoundariesVertices( mBoundaryVertices );
  mMesh->setHolesVertices( mHolesVertices );

  // if the boundary conditions have associated elements in the network, attache it, if not creates them
  if ( encodedElement.hasEncodedData( QStringLiteral( "boundary-condition-ids" ) ) )
  {
    encodedElement.getData( QStringLiteral( "boundary-condition-ids" ), mBoundaryConditions );
    if ( hasCapability( DefinedExternally ) && importer )
    {
      // If the structure is defined externally (importation), some boundary could not be here anymore,
      // and some new could be appeared, we need to remove the ones not here anymore and add the new ones
      importer->updateBoundaryConditions( mBoundaryConditions, this, mNetwork->context() );
    }
  }
  else
  {
    const QStringList boundaryIdList = mPolylinesStructures->classes();
    mBoundaryConditions = QSet<QString>( boundaryIdList.constBegin(), boundaryIdList.constEnd() );
  }

  for ( const QString &bcId : std::as_const( mBoundaryConditions ) )
  {
    ReosHydraulicStructureBoundaryCondition *bc = boundaryConditionNetWorkElement( bcId );
    if ( bc )
    {
      if ( !bc->structure() )
        bc->attachStructure( this );
    }
    else
      mNetwork->addElement( new ReosHydraulicStructureBoundaryCondition( this, bcId, network()->context() ) );
  }

  const QList<ReosEncodedElement> encodedSimulations = encodedElement.getListEncodedData( QStringLiteral( "simulations" ) );
  for ( const ReosEncodedElement &elem : encodedSimulations )
  {
    ReosHydraulicSimulation *sim = ReosSimulationEngineRegistery::instance()->createSimulation( elem, this );
    if ( sim )
      mSimulations.append( sim );
  }

  QMap<QString, QByteArray> scalarSymbologies;
  encodedElement.getData( QStringLiteral( "scalar-results-symbologies" ), scalarSymbologies );
  mMesh->setDatasetScalarSymbologies( scalarSymbologies );
  QMap<QString, QByteArray> vectorSymbologies;
  encodedElement.getData( QStringLiteral( "vector-results-symbologies" ), vectorSymbologies );
  mMesh->setDatasetVectorSymbologies( vectorSymbologies );

  ReosMesh::WireFrameSettings wireframeSettings;
  encodedElement.getData( QStringLiteral( "wire-frame-enabled" ), wireframeSettings.enabled );
  encodedElement.getData( QStringLiteral( "wire-frame-color" ), wireframeSettings.color );
  encodedElement.getData( QStringLiteral( "wire-frame-width" ), wireframeSettings.width );
  mMesh->setWireFrameSettings( wireframeSettings, false );

  QString currentActivatedMeshDataset;
  encodedElement.getData( QStringLiteral( "current-mesh-dataset-id" ), currentActivatedMeshDataset );
  mMesh->activateDataset( currentActivatedMeshDataset, false );

  QString currentActivatedVectorMeshDataset;
  encodedElement.getData( QStringLiteral( "current-mesh-vector-dataset-id" ), currentActivatedVectorMeshDataset );
  mMesh->activateVectorDataset( currentActivatedVectorMeshDataset, true );

  if ( !encodedElement.getData( QStringLiteral( "mesh-need-to-be-generated" ), mMeshNeedToBeGenerated ) )
    mMeshNeedToBeGenerated = hasCapability( ReosHydraulicStructure2D::GeometryEditable );;

  mProfilesCollection->decode( encodedElement.getEncodedData( "profiles-collection" ), this );
}

ReosHydraulicStructure2D::ReosHydraulicStructure2D( ReosStructureImporter *importer, const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( context.network() )
  , mCapabilities( importer->capabilities() )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( importer->domain(), importer->crs() ) )
  , mMesh( importer->mesh( context.network()->gisEngine()->crs() ) )
  , mStructureImporterSource( importer->source()->clone() )
  , mProfilesCollection( new ReosHydraulicStructureProfilesCollection( this ) )
  , mTimeWindowSettings( new ReosTimeWindowSettings( this ) )
{
  initConnection();

  const QList<ReosHydraulicStructureBoundaryCondition *> bcs = importer->createBoundaryConditions( this, mNetwork->context() );
  for ( ReosHydraulicStructureBoundaryCondition *bc : bcs )
    mBoundaryConditions.insert( bc->boundaryConditionId() );

  mSimulations.append( importer->createSimulations( this ) );
  mCurrentSimulationIndex = mSimulations.isEmpty() ? -1 : 0;

  mMeshNeedToBeGenerated = hasCapability( ReosHydraulicStructure2D::GeometryEditable );

  updateResults( context.currentSchemeId() );

  saveConfiguration( context.network()->currentScheme() );
}

ReosTimeWindowSettings *ReosHydraulicStructure2D::timeWindowSettings() const
{
  return mTimeWindowSettings;
}

ReosHydraulicStructureProfilesCollection *ReosHydraulicStructure2D::profilesCollection() const
{
  return mProfilesCollection;
}

int ReosHydraulicStructure2D::createProfile( const QString &name, const QPolygonF &linesInPlan, const QString &linesCrs )
{
  QPolygonF lp = ReosGisEngine::transformToCoordinates( linesCrs, linesInPlan, mMesh->crs() );
  std::unique_ptr<ReosHydraulicStructureProfile> profile = std::make_unique<ReosHydraulicStructureProfile>( name, lp, this );

  mProfilesCollection->addProfile( profile.release() );

  return mProfilesCollection->rowCount( QModelIndex() ) - 1;
}

void ReosHydraulicStructure2D::removeProfile( int index )
{
  mProfilesCollection->removeProfile( index );
}

void ReosHydraulicStructure2D::renameProfile( int index, const QString &name )
{
  mProfilesCollection->renameProfile( index, name );
}

int ReosHydraulicStructure2D::profilesCount() const
{
  return mProfilesCollection->rowCount( QModelIndex() );
}

ReosHydraulicStructureProfile *ReosHydraulicStructure2D::profile( int profileIndex ) const
{
  return mProfilesCollection->profile( profileIndex );
}

int ReosHydraulicStructure2D::profileIndex( ReosHydraulicStructureProfile *profile )
{
  return mProfilesCollection->profileIndex( profile );
}

bool ReosHydraulicStructure2D::hasCapability( Structure2DCapability capability ) const
{
  return mCapabilities.testFlag( capability );
}

ReosStructureImporterSource *ReosHydraulicStructure2D::structureImporterSource() const
{
  return mStructureImporterSource.get();
}

void ReosHydraulicStructure2D::exportResultAsMesh( const QString &fileName, const ReosTimeWindow &timeWindow ) const
{
  ReosModule::Message message;
  mMesh->exportAsMesh( fileName, message );
  if ( network() && network()->currentScheme() )
    mMesh->exportSimulationResults( mSimulationResults.value( network()->currentScheme()->id() ), fileName, timeWindow );
}

ReosModule::Message ReosHydraulicStructure2D::exportResultAsMeshInGisProject( ReosHydraulicScheme *scheme, const QString &fileName, bool keepLayers )
{
  ReosModule::Message message;
  const QFileInfo fileInfo( fileName );
  const QString meshName = elementNameParameter()->value().replace( ':', '-' ) + '-' + scheme->schemeName()->value();
  QString meshFileName = fileInfo.dir().filePath( meshName );

  ReosMesh *meshFrameToExport = nullptr;
  std::unique_ptr<ReosMesh> externalMesh;
  if ( hasCapability( ReosHydraulicStructure2D::DefinedExternally ) )
  {
    std::unique_ptr<ReosStructureImporter> importer( mStructureImporterSource->createImporter() );
    if ( importer && importer->isValid() )
    {
      externalMesh.reset( importer->mesh( this, scheme, importer->crs() ) );
      meshFrameToExport = externalMesh.get();
    }
  }
  else
  {
    meshFrameToExport = mMesh.get();
  }

  if ( !meshFrameToExport )
  {
    message.type = ReosModule::Error;
    message.addText( tr( "Unable to export the mesh frame." ) );
    mNetwork->message( message );
  }

  meshFileName = meshFrameToExport->exportAsMesh( meshFileName, message );
  if ( message.type == ReosModule::Error )
  {
    mNetwork->message( message, false );
    return message;
  }

  if ( mSimulationResults.contains( scheme->id() ) )
    message = mMesh->exportSimulationResults( mSimulationResults.value( scheme->id() ), meshFileName );
  else
  {
    ReosHydraulicSimulation *sim = simulation( scheme );
    if ( sim )
    {
      std::unique_ptr<ReosHydraulicSimulationResults> results( sim->loadSimulationResults( scheme->id() ) );
      if ( results )
        message = mMesh->exportSimulationResults( results.get(), meshFileName );
    }
  }

  if ( message.type == ReosModule::Error )
  {
    mNetwork->message( message, false );
    return message;
  }

  QMap<QString, ReosEncodedElement> scalarSymbologies;
  QMap<QString, ReosEncodedElement> vectorSymbologies;

  const QStringList scalarIds = mMesh->datasetIds();
  for ( const QString &id : scalarIds )
  {
    const QString groupName = mMesh->datasetName( id );
    ReosEncodedElement symbology = mMesh->datasetScalarGroupSymbology( id );
    scalarSymbologies.insert( groupName, symbology );
  }

  const QStringList vectorIds = mMesh->vectorDatasetIds();
  for ( const QString &id : vectorIds )
  {
    const QString groupName = mMesh->datasetName( id );
    ReosEncodedElement symbology = mMesh->datasetVectorGroupSymbology( id );
    vectorSymbologies.insert( groupName, symbology );
  }

  ReosDuration timeStep = simulation( scheme )->representative2DTimeStep();
  timeStep.setAdaptedUnit();

  if ( !mNetwork->gisEngine()->createProjectFile( fileName, keepLayers ) )
  {
    message.type = ReosModule::Error;
    message.text = tr( "Unable to create a QGIS project to location \"%1\"" ).arg( fileName );
    mNetwork->message( message, false );
    return message;
  }

  mNetwork->gisEngine()->addMeshLayerToExistingProject(
    fileName,
    meshName,
    meshFileName,
    mMesh->wireFrameSymbology(),
    scalarSymbologies,
    vectorSymbologies,
    timeStep,
    message );

  if ( message.type == ReosModule::Simple )
  {
    message.addText( tr( "Hydraulic scheme \"%1\" of hydraulic structure \"%2\" successfully exported." ).
                     arg( scheme->schemeName()->value(), elementNameParameter()->value() ) );
  }
  mNetwork->message( message, false );
  return message;
}

QString ReosHydraulicStructure2D::currentActivatedMeshDataset() const
{
  if ( !mMesh )
    return QString();
  return mMesh->currentdScalarDatasetId();
}

QString ReosHydraulicStructure2D::currentActivatedVectorMeshDataset() const
{
  if ( !mMesh )
    return QString();
  return  mMesh->currentdVectorDatasetId();
}

ReosHydraulicSimulationResults::DatasetType ReosHydraulicStructure2D::currentActivatedDatasetResultType() const
{
  if ( mMesh )
  {
    const QString currentDatasetId = mMesh->currentdScalarDatasetId();
    for ( ReosHydraulicSimulationResults *results : mSimulationResults )
    {
      if ( results )
      {
        for ( int ri = 0; ri < results->groupCount(); ri++ )
        {
          if ( results->groupId( ri ) == currentDatasetId )
            return results->datasetType( ri );
        }
      }
    }
  }

  return ReosHydraulicSimulationResults::DatasetType::NoType;
}

QString ReosHydraulicStructure2D::currentDatasetName() const
{
  if ( !mMesh )
    return QString();
  return mMesh->datasetName( mMesh->currentdScalarDatasetId() );
}

void ReosHydraulicStructure2D::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  if ( mPolylinesStructures && !hasCapability( DefinedExternally ) )
    element.addEncodedData( QStringLiteral( "structure" ), mPolylinesStructures->encode() );

  if ( mMeshGenerator )
    element.addEncodedData( QStringLiteral( "mesh-generator" ), mMeshGenerator->encode() );
  if ( mMeshResolutionController )
    element.addEncodedData( QStringLiteral( "mesh-resolution-controller" ), mMeshResolutionController->encode() );
  if ( mTopographyCollection )
    element.addEncodedData( QStringLiteral( "topography-collection" ), mTopographyCollection->encode() );

  QDir dir( context.projectPath() );
  QString hydDir = context.projectName() + QStringLiteral( "-hydr-struct" );
  dir.mkdir( hydDir );
  dir.cd( hydDir );
  dir.mkdir( directory() );
  dir.cd( directory() );

  if ( !hasCapability( DefinedExternally ) )
    mMesh->save( dir.path() );

  if ( hasCapability( GeometryEditable ) )
  {
    element.addData( QStringLiteral( "boundaries-vertices" ), mBoundaryVertices );
    element.addData( QStringLiteral( "hole-vertices" ), mHolesVertices );
    element.addEncodedData( QStringLiteral( "mesh-quality-parameters" ), mMesh->qualityMeshParameters().encode() );
    if ( mRoughnessStructure )
      element.addEncodedData( QStringLiteral( "roughness-structure" ), mRoughnessStructure->encode() );
  }

  QList<ReosEncodedElement> encodedSimulations;
  for ( ReosHydraulicSimulation *sim : mSimulations )
    encodedSimulations.append( sim->encode() );
  element.addListEncodedData( QStringLiteral( "simulations" ), encodedSimulations );

  element.addData( QStringLiteral( "current-mesh-dataset-id" ), mMesh->currentdScalarDatasetId() );
  element.addData( QStringLiteral( "current-mesh-vector-dataset-id" ), mMesh->currentdVectorDatasetId() );

  element.addData( QStringLiteral( "scalar-results-symbologies" ), mMesh->datasetScalarSymbologies() );
  element.addData( QStringLiteral( "vector-results-symbologies" ), mMesh->datasetVectorSymbologies() );

  ReosMesh::WireFrameSettings wireframeSettings = mMesh->wireFrameSettings();
  element.addData( QStringLiteral( "wire-frame-enabled" ), wireframeSettings.enabled );
  element.addData( QStringLiteral( "wire-frame-color" ), wireframeSettings.color );
  element.addData( QStringLiteral( "wire-frame-width" ), wireframeSettings.width );

  element.addEncodedData( QStringLiteral( "3d-map-settings" ), m3dMapSettings.encode() );
  element.addEncodedData( QStringLiteral( "3d-terrain-settings" ), m3dTerrainSettings.encode() );

  element.addData( QStringLiteral( "mesh-need-to-be-generated" ), mMeshNeedToBeGenerated );

  element.addEncodedData( QStringLiteral( "profiles-collection" ), mProfilesCollection->encode() );

  element.addData( QStringLiteral( "boundary-condition-ids" ), mBoundaryConditions );

  element.addData( QStringLiteral( "capabilities" ), mCapabilities );

  if ( mStructureImporterSource )
  {
    element.addEncodedData( QStringLiteral( "structure-importer" ), mStructureImporterSource->encode( mNetwork->context() ) );
  }
}

ReosRoughnessStructure *ReosHydraulicStructure2D::roughnessStructure() const
{
  return mRoughnessStructure.get();
}

QDir ReosHydraulicStructure2D::structureDirectory() const
{
  return QDir( mNetwork->context().projectPath() + '/' + mNetwork->context().projectName() + QStringLiteral( "-hydr-struct" ) + '/' + directory() );
}

QList<ReosHydraulicStructureBoundaryCondition *> ReosHydraulicStructure2D::boundaryConditions() const
{
  QMap<QString, ReosHydraulicStructureBoundaryCondition *> ret;
  for ( const QString &bcId : mBoundaryConditions )
    ret.insert( bcId, boundaryConditionNetWorkElement( bcId ) );

  return ret.values();
}

QStringList ReosHydraulicStructure2D::boundaryConditionId() const
{
  return mBoundaryConditions.values();
}

ReosHydraulicSimulation *ReosHydraulicStructure2D::currentSimulation() const
{
  if ( mCurrentSimulationIndex < 0 || mCurrentSimulationIndex >= mSimulations.count() )
    return nullptr;

  return mSimulations.at( mCurrentSimulationIndex );

}

int ReosHydraulicStructure2D::currentSimulationIndex() const
{
  return mCurrentSimulationIndex;
}

QStringList ReosHydraulicStructure2D::simulationNames() const
{
  QStringList names;

  for ( ReosHydraulicSimulation *sim : mSimulations )
    names.append( sim->name() );

  return names;
}

void ReosHydraulicStructure2D::setCurrentSimulation( int index )
{
  ReosHydraulicSimulation *currentSim = currentSimulation();
  if ( currentSim )
    disconnect( currentSim, &ReosHydraulicSimulation::timeStepChanged, this, &ReosHydraulicStructure2D::timeStepChanged );

  mCurrentSimulationIndex = index;
  currentSim = currentSimulation();

  if ( currentSim )
    connect( currentSim, &ReosHydraulicSimulation::timeStepChanged, this, &ReosHydraulicStructure2D::timeStepChanged );

  emit currentSimulationChanged();
}

bool ReosHydraulicStructure2D::addSimulation( const QString &key )
{
  ReosHydraulicSimulation *sim = ReosSimulationEngineRegistery::instance()->createSimulation( key, this );
  if ( sim )
  {
    mSimulations.append( sim );
    setCurrentSimulation( mSimulations.count() - 1 );
    return true;
  }

  return false;
}

void ReosHydraulicStructure2D::removeSimulation( int index )
{
  setCurrentSimulation( index - 1 );
  mSimulations.at( index )->deleteLater();
  mSimulations.removeAt( index );
}

ReosHydraulicSimulation *ReosHydraulicStructure2D::simulation( ReosHydraulicScheme *scheme ) const
{
  const ReosEncodedElement elem = scheme->restoreElementConfig( id() );

  QString simulationId;

  if ( !elem.getData( QStringLiteral( "current-simulation-id" ), simulationId ) )
    return currentSimulation();

  int simIndex = simulationIndexFromId( simulationId );
  if ( simIndex != -1 )
    return mSimulations.at( simIndex );
  else
    return nullptr;
}

void ReosHydraulicStructure2D::updateCalculationContext( const ReosCalculationContext &context )
{
  QList<ReosHydraulicStructureBoundaryCondition *> boundaries = boundaryConditions();
  for ( ReosHydraulicStructureBoundaryCondition *bc : boundaries )
  {
    if ( bc )
    {
      switch ( bc->conditionType() )
      {
        case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
          bc->updateCalculationContextFromDownstream( context );
          break;
        case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
          bc->updateCalculationContextFromUpstream( context, nullptr, true );
          break;
        case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
        case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
          break;
      }
    }
  }
}

Reos3DMapSettings ReosHydraulicStructure2D::map3dSettings() const
{
  return m3dMapSettings;
}

void ReosHydraulicStructure2D::setMap3dSettings( const Reos3DMapSettings &value )
{
  m3dMapSettings = value;
}

Reos3DTerrainSettings ReosHydraulicStructure2D::terrain3DSettings() const
{
  return m3dTerrainSettings;
}

void ReosHydraulicStructure2D::setTerrain3DSettings( const Reos3DTerrainSettings &settings )
{
  m3dTerrainSettings = settings;
}

void ReosHydraulicStructure2D::onBoundaryConditionAdded( const QString &bid )
{
  mBoundaryConditions.insert( bid );
  ReosHydraulicStructureBoundaryCondition *newBc = new ReosHydraulicStructureBoundaryCondition( this, bid, mNetwork->context() );
  mNetwork->addElement( newBc );
  emit boundariesChanged();
}

void ReosHydraulicStructure2D::onBoundaryConditionRemoved( const QString &bid )
{
  mBoundaryConditions.remove( bid );
  mNetwork->removeElement( boundaryConditionNetWorkElement( bid ) );
  emit boundariesChanged();
}

void ReosHydraulicStructure2D::onExtrernalBoundaryConditionRemoved( const QString &bcId )
{
  mBoundaryConditions.remove( bcId );
  emit boundariesChanged();
}

void ReosHydraulicStructure2D::onExtrernalBoundaryConditionAdded( const QString &bcId )
{
  mBoundaryConditions.insert( bcId );
  emit boundariesChanged();
}

void ReosHydraulicStructure2D::onGeometryStructureChange()
{
  mBoundaryVertices.clear();
  mHolesVertices.clear();
}

void ReosHydraulicStructure2D::onFlowsFromSolverReceived( const QDateTime &time, const QStringList &boundId, const QList<double> &values )
{
  for ( int i = 0; i < boundId.count(); ++i )
  {
    const QString &bId = boundId.at( i );
    ReosHydraulicStructureBoundaryCondition *bound = boundaryConditionNetWorkElement( bId );
    if ( bound )
      bound->outputHydrograph()->setValue( time, values.at( i ) );
  }
}

ReosTopographyCollection *ReosHydraulicStructure2D::topographyCollecion() const
{
  return mTopographyCollection;
}

QString ReosHydraulicStructure2D::terrainMeshDatasetId() const
{
  if ( !mMesh )
    return QString();
  return mMesh->verticesElevationDatasetId();
}

double ReosHydraulicStructure2D::terrainElevationAt( const QPointF &position )
{
  if ( !mMesh )
    return std::numeric_limits<double>::quiet_NaN();
  return mMesh->datasetScalarValueAt( terrainMeshDatasetId(), position );
}

void ReosHydraulicStructure2D::activateResultDatasetGroup( const QString &id )
{
  if ( mMesh )
    mMesh->activateDataset( id );
}

void ReosHydraulicStructure2D::activateResultVectorDatasetGroup( const QString &id )
{
  if ( mMesh )
    mMesh->activateVectorDataset( id );
}

QStringList ReosHydraulicStructure2D::meshDatasetIds() const
{
  if ( mMesh )
    return mMesh->datasetIds();

  return QStringList();
}

QStringList ReosHydraulicStructure2D::meshVectorDatasetIds() const
{
  if ( mMesh )
    return mMesh->vectorDatasetIds();

  return QStringList();
}

QString ReosHydraulicStructure2D::meshDatasetName( const QString &id ) const
{
  if ( mMesh )
    return mMesh->datasetName( id );

  return QString();
}

ReosSimulationPreparationProcess *ReosHydraulicStructure2D::getPreparationProcessSimulation( const ReosSimulationData &simData, const ReosCalculationContext &context, ReosModule::Message &message )
{
  if ( mMeshNeedToBeGenerated )
  {
    message.type = ReosModule::Error;
    message.addText( tr( "The mesh need to be regenerated" ) );
    return nullptr;
  }
  if ( !currentSimulation() )
  {
    message.type = ReosModule::Error;
    message.addText( tr( "Current simulation not defined" ) );
    return nullptr;
  }
  if ( mNetwork->projectFileName().isEmpty() )
  {
    message.type = ReosModule::Error;
    message.addText( tr( "Project must be saved at least one time." ) );
    return nullptr;
  }

  return new ReosSimulationPreparationProcess( this, currentSimulation(), simData, context );
}

ReosSimulationPreparationProcess *ReosHydraulicStructure2D::getPreparationProcessSimulation( const ReosSimulationData &simData, const ReosCalculationContext &context, ReosModule::Message &message, const QDir &directory )
{
  std::unique_ptr<ReosSimulationPreparationProcess> ret( getPreparationProcessSimulation( simData, context, message ) );
  ret->setDestination( directory );
  return ret.release();
}

ReosSimulationProcess *ReosHydraulicStructure2D::createSimulationProcess( const ReosCalculationContext &context, QString &error )
{
  if ( mMeshNeedToBeGenerated )
  {
    error = tr( "The mesh need to be regenerated" );
    return nullptr;
  }

  if ( !currentSimulation() )
  {
    error = tr( "Current simulation not defined" );
    return nullptr;
  }

  QString schemeId = context.schemeId();

  if ( processFromScheme( context.schemeId() ) )
  {
    error = tr( "A simulation with the current hydraulic scheme is currently running." );
    return nullptr;
  }

  QPointer<ReosHydraulicSimulation> sim = currentSimulation();

  ReosSimulationProcess *process = mSimulationProcesses.emplace( schemeId, sim->getProcess( context ) ).first->second.get();

  connect( process, &ReosProcess::finished, sim, [this, sim, schemeId]
  {
    auto it = mSimulationProcesses.find( schemeId );
    if ( it != mSimulationProcesses.end() )
    {
      bool success = false;
      ReosSimulationProcess *process = it->second.get();
      success = it->second->isSuccessful();
      onSimulationFinished( sim, schemeId, process, success );

      mSimulationProcesses.erase( it );

      emit simulationFinished( success );
    }

  } );

  return process;
}

ReosSimulationProcess *ReosHydraulicStructure2D::simulationProcess( const ReosCalculationContext &context ) const
{
  return processFromScheme( context.schemeId() );
}

bool ReosHydraulicStructure2D::hasSimulationRunning() const
{
  if ( mSimulationProcesses.empty() )
    return false;

  for ( auto &proc : mSimulationProcesses )
    if ( !proc.second->isFinished() )
      return true;

  return true;
}

bool ReosHydraulicStructure2D::runSimulation( const ReosCalculationContext &context )
{
  QTextStream txtStream( stdout );
  QElapsedTimer timer;
  timer.start();
  txtStream << QStringLiteral( "*********************************************************************************" ) << Qt::endl;
  txtStream << tr( "Start running simulation for hydraulic structure \"%1\"" ).arg( elementName() ) << Qt::endl;
  txtStream << QStringLiteral( "*********************************************************************************" ) << Qt::endl;

  ReosModule::Message message;
  const ReosSimulationData simData = simulationData( context.schemeId(), message );

  removeResults( context );
  updateResults( context.schemeId() );

  ReosHydraulicScheme *scheme = mNetwork->scheme( context.schemeId() );
  if ( currentSimulation() )
  {
    currentSimulation()->saveConfiguration( scheme );
  }
  else
  {
    txtStream << tr( "No simulation set for this structure and the current scheme. Simulation aborded." ) << Qt::endl;
    return false;
  }

  txtStream << tr( "Start preparation of simulation" ) << Qt::endl;
  std::unique_ptr<ReosSimulationPreparationProcess> preparationProcess( getPreparationProcessSimulation( simData, context, message ) );
  if ( !preparationProcess )
  {
    txtStream << tr( "Simulation did not start for following reason:\n\n%1" ).arg( message.text ) << Qt::endl;
    return false;
  }
  std::unique_ptr<QEventLoop> loop( new QEventLoop );
  connect( preparationProcess.get(), &ReosProcess::finished, loop.get(), &QEventLoop::quit );
  connect( preparationProcess.get(), &ReosProcess::sendInformation, this, [&]( const QString & mess )
  {
    txtStream << mess << Qt::endl;
  } );
  preparationProcess->startOnOtherThread();
  loop->exec();

  if ( !preparationProcess->isSuccessful() )
  {
    txtStream << tr( "Something get wrong during simulation preparation.\nPlease, check the model." ) << Qt::endl;
    return false;
  }
  txtStream << tr( "Preparation of the simulation is succesfull. Time Elapsed: %1 ms" ).arg( timer.elapsed() ) << Qt::endl;

  ReosCalculationContext effCalcContext = preparationProcess->calculationContext();

  txtStream << QStringLiteral( "*********************************************************************************" ) << Qt::endl;
  txtStream << tr( "Start simulation" ) << Qt::endl;
  QString error;
  ReosSimulationProcess *process( createSimulationProcess( effCalcContext, error ) );
  if ( !process )
  {
    txtStream << tr( "Simulation did not start for following reason:\n\n%1" ).arg( error ) << Qt::endl;
    return false;
  }

  loop.reset( new QEventLoop );
  bool isSuccess;
  connect( this, &ReosHydraulicStructure2D::simulationFinished, loop.get(), [&]( bool success )
  {
    isSuccess = success;
    loop->quit();
  } );
  connect( process, &ReosProcess::sendInformation, this, [&]( const QString & mess )
  {
    txtStream << mess << Qt::endl;
  } );

  process->startOnOtherThread();
  loop->exec();

  txtStream << tr( "Simulation is finished. Time Elapsed: %1 s" ).arg( QString::number( static_cast<double>( timer.elapsed() ) / 1000.0, 'f', 2 ) ) << Qt::endl;

  return isSuccess;
}

bool ReosHydraulicStructure2D::hasResults() const
{
  int schemeCount = mNetwork->schemeCount();
  for ( int i = 0; i < schemeCount; ++i )
  {
    ReosHydraulicScheme *scheme = mNetwork->scheme( i );
    const ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );
    QString simulationId;

    encodedElement.getData( QStringLiteral( "current-simulation-id" ), simulationId );

    int simIndex = simulationIndexFromId( simulationId );

    if ( simIndex == -1 )
      continue;

    ReosHydraulicSimulation *sim = mSimulations.at( simIndex );

    if ( sim->hasResult( scheme->id() ) )
      return true;
  }

  return false;
}

bool ReosHydraulicStructure2D::hasResults( const QString &schemeId ) const
{
  return mSimulationResults.contains( schemeId );
}

bool ReosHydraulicStructure2D::hasResults( const ReosHydraulicScheme *scheme ) const
{
  if ( !scheme )
    return false;

  return mSimulationResults.contains( scheme->id() );
}

QDateTime ReosHydraulicStructure2D::resultsRunDateTime( const QString &schemeId ) const
{
  if ( mSimulationResults.contains( schemeId ) )
    return mSimulationResults.value( schemeId )->runDateTime();
  else
    return QDateTime();
}

int ReosHydraulicStructure2D::resultsTimeStepCount( const QString &schemeId ) const
{
  if ( mSimulationResults.contains( schemeId ) )
  {
    ReosHydraulicSimulationResults *results = mSimulationResults.value( schemeId );
    if ( results && results->groupCount() > 0 )
      return mSimulationResults.value( schemeId )->datasetCount( 0 );
  }

  return 0;
}

double ReosHydraulicStructure2D::resultsValueAt( const QDateTime &time,
    const ReosSpatialPosition &position,
    ReosHydraulicSimulationResults::DatasetType datasetType,
    const QString &schemeId )
{
  if ( mSimulationResults.contains( schemeId ) )
  {
    ReosHydraulicSimulationResults *results = mSimulationResults.value( schemeId );
    if ( results )
      return results->interpolateResultOnMesh( mMesh.get(), position, time, datasetType );
  }

  return std::numeric_limits<double>::quiet_NaN();
}

QString ReosHydraulicStructure2D::resultsUnits( ReosHydraulicSimulationResults::DatasetType datasetType, const QString &schemeId )
{
  if ( mSimulationResults.contains( schemeId ) )
    return mSimulationResults.value( schemeId )->unitString( datasetType );

  return QString();
}

bool ReosHydraulicStructure2D::rasterizeResult(
  const QDateTime &time,
  ReosHydraulicSimulationResults::DatasetType datasetType,
  const QString &schemeId,
  const QString &fileName,
  const QString &destinationCrs,
  double resolution )
{
  if ( mSimulationResults.contains( schemeId ) )
  {
    ReosHydraulicSimulationResults *results = mSimulationResults.value( schemeId );
    if ( results )
      return results->rasterizeResultFromMesh( mMesh.get(), fileName, time, datasetType, destinationCrs, resolution );
  }

  return false;
}

void ReosHydraulicStructure2D::removeAllResults()
{
  mMesh->setSimulationResults( nullptr, mNetwork->gisEngine()->crs() );
  qDeleteAll( mSimulationResults );
  mSimulationResults.clear();

  int schemeCount = mNetwork->schemeCount();
  for ( int i = 0; i < schemeCount; ++i )
  {
    ReosHydraulicScheme *scheme = mNetwork->scheme( i );
    const ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );
    QString simulationId;

    encodedElement.getData( QStringLiteral( "current-simulation-id" ), simulationId );

    int simIndex = simulationIndexFromId( simulationId );

    if ( simIndex == -1 )
      continue;

    ReosHydraulicSimulation *sim = mSimulations.at( simIndex );
    if ( sim )
      sim->removeResults( scheme->id() );

    updateResults( scheme->id() );
  }
}

void ReosHydraulicStructure2D::removeResults( const ReosCalculationContext &context )
{
  const QString &schemeId = context.schemeId();

  if ( mSimulationResults.contains( schemeId ) )
  {
    ReosHydraulicSimulationResults *resToRemove = mSimulationResults.value( schemeId );
    if ( resToRemove == mCurrentResult )
      setResultsOnStructure( nullptr );

    delete mSimulationResults.value( schemeId ); // replace by deleteLater() ?
    mSimulationResults.remove( schemeId );

    ReosHydraulicScheme *scheme = mNetwork->scheme( schemeId );
    if ( scheme )
    {
      ReosHydraulicSimulation *sim = simulation( scheme );
      sim->removeResults( schemeId );
    }
  }
}

void ReosHydraulicStructure2D::clearResults( const QString &schemeId )
{
  ReosHydraulicSimulationResults *simulationResults = mSimulationResults.value( schemeId, nullptr );
  if ( simulationResults )
  {
    if ( mCurrentResult == simulationResults )
      setResultsOnStructure( nullptr );

    mSimulationResults.remove( schemeId );
    simulationResults->deleteLater();
  }
}

ReosHydraulicSimulationResults *ReosHydraulicStructure2D::results( ReosHydraulicScheme *scheme )
{
  ReosHydraulicSimulation *sim = simulation( scheme );
  if ( !sim )
    return nullptr;

  if ( !hasResults( scheme ) )
  {
    loadResult( sim, scheme->id() );
    if ( !hasResults() )
      return nullptr;
  }

  return mSimulationResults.value( scheme->id() );
}

void ReosHydraulicStructure2D::initConnection()
{
  if ( mMesh )
  {
    mMesh->enableVertexElevationDataset( tr( "Terrain elevation" ) );
    mMesh->activateDataset( mMesh->verticesElevationDatasetId() );
    mMesh->setVerticaleSCale( m3dMapSettings.verticalExaggeration() );
  }

  connect( mPolylinesStructures.get(), &ReosDataObject::dataChanged, this, [this]
  {
    mMeshNeedToBeGenerated = hasCapability( ReosHydraulicStructure2D::GeometryEditable );
    if ( mMeshGenerator && mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( mPolylinesStructures.get(), &ReosPolylinesStructure::boundaryConditionAdded, this, &ReosHydraulicStructure2D::onBoundaryConditionAdded );
  connect( mPolylinesStructures.get(), &ReosPolylinesStructure::boundaryConditionRemoved, this, &ReosHydraulicStructure2D::onBoundaryConditionRemoved );

  if ( mMeshResolutionController )
  {
    connect( mMeshResolutionController, &ReosDataObject::dataChanged, this, [this]
    {
      mMeshNeedToBeGenerated = hasCapability( ReosHydraulicStructure2D::GeometryEditable );
      if ( mMeshGenerator && mMeshGenerator->autoUpdateParameter()->value() )
        generateMeshInPlace();
    } );
  }

  if ( mMeshGenerator )
  {
    connect( mMeshGenerator, &ReosDataObject::dataChanged, this, [this]
    {
      if ( mMeshGenerator->autoUpdateParameter()->value() )
        generateMeshInPlace();
    } );
  }

  if ( mMesh && mTopographyCollection )
  {
    connect( this, &ReosHydraulicStructure2D::meshGenerated, mTopographyCollection, [this]
    {
      if ( mTopographyCollection->autoApply()->value() )
      {
        std::unique_ptr<ReosProcess> process( mMesh->applyTopographyOnVertices( mTopographyCollection ) );
        process->processOnThisThread();
      }
    } );
  }

  if ( mTimeWindowSettings )
  {
    connect( mTimeWindowSettings, &ReosTimeWindowSettings::dataChanged, this, &ReosHydraulicStructure2D::timeWindowChanged );
  }

  if ( mMesh && mProfilesCollection )
  {
    connect( mMesh.get(), &ReosDataObject::dataChanged, mProfilesCollection, &ReosHydraulicStructureProfilesCollection::clearProfiles );
  }

  connect( this, &ReosHydraulicStructure2D::simulationResultChanged, mNetwork, &ReosHydraulicNetwork::timeStepChanged );
}

void ReosHydraulicStructure2D::generateMeshInPlace()
{
  std::unique_ptr<ReosMeshGeneratorProcess> process( getGenerateMeshProcess() );
  process->start();
}

QString ReosHydraulicStructure2D::directory() const
{
  return id().split( ':' ).last();
}

ReosHydraulicStructureBoundaryCondition *ReosHydraulicStructure2D::boundaryConditionNetWorkElement( const QString &boundaryId ) const
{
  const QList<ReosHydraulicNetworkElement *> hydrElems = mNetwork->hydraulicNetworkElements( ReosHydraulicStructureBoundaryCondition::staticType() );
  for ( ReosHydraulicNetworkElement *hydrElem : hydrElems )
  {
    ReosHydraulicStructureBoundaryCondition *bcElem = qobject_cast<ReosHydraulicStructureBoundaryCondition *>( hydrElem );
    if ( bcElem && bcElem->boundaryConditionId() == boundaryId )
      return bcElem;
  }
  return nullptr;
}

ReosMeshGenerator *ReosHydraulicStructure2D::meshGenerator() const
{
  return mMeshGenerator;
}

void ReosHydraulicStructure2D::generateMesh()
{
  std::unique_ptr<ReosMeshGeneratorProcess> process(
    mMeshGenerator->getGenerateMeshProcess( mPolylinesStructures.get(), mMeshResolutionController, mMesh->crs() ) );
  process->start();
  removeAllResults();
  onMeshGenerated( process->meshResult() );
}

QPolygonF ReosHydraulicStructure2D::domain( const QString &crs ) const
{
  return mPolylinesStructures->boundary( crs );
}

ReosPolylinesStructure *ReosHydraulicStructure2D::geometryStructure() const
{
  return mPolylinesStructures.get();
}

ReosMeshResolutionController *ReosHydraulicStructure2D::meshResolutionController() const
{
  return mMeshResolutionController;
}

ReosMesh *ReosHydraulicStructure2D::mesh() const
{
  return mMesh.get();
}

ReosMeshGeneratorProcess *ReosHydraulicStructure2D::getGenerateMeshProcess()
{
  std::unique_ptr<ReosMeshGeneratorProcess> process(
    mMeshGenerator->getGenerateMeshProcess( mPolylinesStructures.get(), mMeshResolutionController, mMesh->crs() ) );
  ReosMeshGeneratorProcess *processP = process.get();

  connect( processP, &ReosProcess::finished, this, [this, processP]
  {
    if ( mMesh && processP->isSuccessful() )
    {
      removeAllResults();
      onMeshGenerated( processP->meshResult() );
    }
  } );

  return process.release();
}

ReosSimulationData ReosHydraulicStructure2D::simulationData( const QString &schemeId, ReosModule::Message &message ) const
{
  ReosSimulationData ret;

  ret.holesVertices = mHolesVertices;

  if ( !mBoundaryVertices.isEmpty() )
  {

    int boundarySegCount = mPolylinesStructures->boundary().count();
    Q_ASSERT( mBoundaryVertices.count() ==  boundarySegCount );

    QVector<ReosSimulationData::BoundaryVertices> vertexToCondition( boundarySegCount );

    for ( int i = 0; i < boundarySegCount; ++i )
    {
      ReosSimulationData::BoundaryVertices boundVert;
      boundVert.verticesIndex = mBoundaryVertices.at( i );
      QString bcid = mPolylinesStructures->boundaryClassId( i );
      boundVert.boundaryCondition = boundaryConditionNetWorkElement( bcid );

      vertexToCondition[i] = boundVert;
    }

    ret.boundaryVertices = vertexToCondition;
  }

  ret.meshData = mMesh->meshDataFrame();

  if ( mRoughnessStructure )
  {
    ret.roughnessValues.reset( mRoughnessStructure->structure()->values( mesh()->crs() ) );
    ret.defaultRoughness = mRoughnessStructure->defaultRoughness()->value();
  }

  ret.coordinateTransformer = mNetwork->gisEngine()->getCoordinateTransformer();

  ReosHydraulicScheme *scheme = mNetwork->scheme( schemeId );
  if ( scheme )
  {
    ReosHydraulicSimulation *sim = simulation( scheme );
    if ( sim )
      message = sim->prepareSimulationData( ret, schemeId );
  }
  else
  {
    message.type = ReosModule::Error;
    message.addText( "No hydraulic scheme" );
  }

  return ret;
}

void ReosHydraulicStructure2D::onMeshGenerated( const ReosMeshFrameData &meshData )
{
  mMesh->generateMesh( meshData );

  mBoundaryVertices = meshData.boundaryVertices;
  mHolesVertices = meshData.holesVertices;
  mMeshNeedToBeGenerated = false;

  emit meshGenerated();
  emit dataChanged();
}

void ReosHydraulicStructure2D::onSimulationFinished( ReosHydraulicSimulation *simulation, const QString &schemeId, ReosSimulationProcess *process, bool success )
{
  if ( !simulation )
    return;

  simulation->saveSimulationResult( schemeId, process, success );

  //! Now we can remove the old one if still presents
  if ( mSimulationResults.contains( schemeId ) )
  {
    mSimulationResults.value( schemeId )->deleteLater();
    mSimulationResults.remove( schemeId );
  }

  updateResults( schemeId );
}

void ReosHydraulicStructure2D::loadResult( ReosHydraulicSimulation *simulation, const QString &schemeId )
{
  ReosHydraulicSimulationResults *simulationResults = mSimulationResults.value( schemeId, nullptr );

  if ( simulationResults )
  {
    simulationResults->deleteLater();
    mSimulationResults.remove( schemeId );
  }

  ReosHydraulicSimulationResults *simResults = simulation->loadSimulationResults( schemeId, this );
  if ( simResults )
    mSimulationResults.insert( schemeId, simResults );
}

void ReosHydraulicStructure2D::setResultsOnStructure( ReosHydraulicSimulationResults *simResults )
{
  if ( !mesh() )
    return;

  mesh()->setSimulationResults( simResults, mNetwork->gisEngine()->crs() );
  emit timeWindowChanged();
  emit timeStepChanged();

  // First we clean boundaries condition
  const QList<ReosHydraulicStructureBoundaryCondition *> boundaries = boundaryConditions();
  for ( ReosHydraulicStructureBoundaryCondition *bc : boundaries )
  {
    switch ( bc->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
        if ( bc->outputHydrograph() )
        {
          bc->setHydrographFromModel( nullptr );
          bc->outputHydrograph()->clear();
        }
        break;
    }
  }

  if ( simResults )
  {
    QString currentVectorDatasetId = mMesh->currentdVectorDatasetId();
    ReosHydraulicSimulationResults::DatasetType currentType = currentActivatedDatasetResultType();

    QString waterLevelId;
    QString currentActivatedId = mMesh->currentdScalarDatasetId();

    for ( int i = 0; i < simResults->groupCount(); ++i )
    {
      ReosHydraulicSimulationResults::DatasetType type = simResults->datasetType( i );
      const QString groupId = simResults->groupId( i );

      if ( type == ReosHydraulicSimulationResults::DatasetType::WaterLevel )
        waterLevelId = groupId;

      if ( type == currentType )
        currentActivatedId = groupId;
    }

    mMesh->activateDataset( currentActivatedId, false );
    mMesh->activateVectorDataset( currentVectorDatasetId, false );
    mMesh->setVerticalDataset3DId( waterLevelId, true );

    const QMap<QString, ReosHydrograph *> outputHydrographs = simResults->outputHydrographs();

    for ( ReosHydraulicStructureBoundaryCondition *bc : boundaries )
    {
      if ( bc->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::OutputLevel ||
           bc->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally )
      {
        bc->outputHydrograph()->clear();
        if ( outputHydrographs.contains( bc->boundaryConditionId() ) )
          bc->setHydrographFromModel( outputHydrographs.value( bc->boundaryConditionId() ) );
      }
    }
  }

  emit simulationResultChanged();
}

void ReosHydraulicStructure2D::updateResults( const QString &schemeId )
{
  if ( mNetwork->calculationContext().schemeId() != schemeId )
    return;

  QElapsedTimer timer;
  timer.start();

  if ( !mSimulationResults.contains( schemeId ) &&
       currentSimulation() &&
       currentSimulation()->hasResult( schemeId ) )
    loadResult( currentSimulation(), schemeId );

  if ( mCurrentResult != mSimulationResults.value( schemeId ) )
  {
    mCurrentResult = mSimulationResults.value( schemeId );
    setResultsOnStructure( mCurrentResult );
  }

}

ReosSimulationProcess *ReosHydraulicStructure2D::processFromScheme( const QString &schemeId ) const
{
  auto it = mSimulationProcesses.find( schemeId );
  if ( it == mSimulationProcesses.end() )
    return nullptr;

  return it->second.get();
}

void ReosHydraulicStructure2D::activateMeshTerrain()
{
  if ( mMesh )
    mMesh->activateDataset( mMesh->verticesElevationDatasetId() );
}

void ReosHydraulicStructure2D::deactivateMeshScalar()
{
  if ( mMesh )
    mMesh->activateDataset( QString() );
}

void ReosHydraulicStructure2D::deactivateMeshVector()
{
  if ( mMesh )
    mMesh->activateVectorDataset( QString() );
}

ReosHydraulicStructure2D::~ReosHydraulicStructure2D() = default;

ReosHydraulicStructure2D *ReosHydraulicStructure2D::create( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  std::unique_ptr<ReosHydraulicStructure2D> elem( new ReosHydraulicStructure2D( encodedElement, context ) );
  context.network()->addElement( elem.get(), false );

  return elem.release();
}

ReosHydraulicStructure2D *ReosHydraulicStructure2D::create( ReosStructureImporter *structureImporter, const ReosHydraulicNetworkContext &context )
{
  if ( !structureImporter || !structureImporter->isValid() )
  {
    return nullptr;
  }

  ReosGisEngine *gisEngine = context.network()->gisEngine();
  bool takeCrs = context.network()->elementsCount() == 0 &&
                 gisEngine->layersCount() == 0;

  std::unique_ptr<ReosHydraulicStructure2D> elem( new ReosHydraulicStructure2D( structureImporter, context ) );
  context.network()->addElement( elem.get(), true );

  if ( takeCrs && elem->mesh() )
  {
    QString crs = elem->mesh()->crs();
    if ( gisEngine->crsIsValid( crs ) )
      gisEngine->setCrs( crs );
  }

  return elem.release();
}

void ReosHydraulicStructure2D::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );
  if ( currentSimulation() )
    encodedElement.addData( QStringLiteral( "current-simulation-id" ), currentSimulation()->id() );

  encodedElement.addEncodedData( QStringLiteral( "time-window-settings" ), mTimeWindowSettings->encode() );

  scheme->saveElementConfig( id(), encodedElement );

  for ( ReosHydraulicSimulation *sim : mSimulations )
    sim->saveConfiguration( scheme );
}

int ReosHydraulicStructure2D::simulationIndexFromId( const QString &simId ) const
{
  for ( int i = 0; i < mSimulations.count(); ++i )
  {
    const ReosHydraulicSimulation *sim = mSimulations.at( i );
    if ( sim->id() == simId )
      return i;
  }

  return -1;
}

void ReosHydraulicStructure2D::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  for ( ReosHydraulicSimulation *sim : std::as_const( mSimulations ) )
    sim->restoreConfiguration( scheme );

  QString simulationId;
  if ( encodedElement.getData( QStringLiteral( "current-simulation-id" ), simulationId ) )
    setCurrentSimulation( simulationIndexFromId( simulationId ) );

  updateResults( scheme->id() );

  mTimeWindowSettings->decode( encodedElement.getEncodedData( QStringLiteral( "time-window-settings" ) ) );
}

ReosMapExtent ReosHydraulicStructure2D::extent() const
{
  return ReosMapExtent( domain(), mPolylinesStructures->crs() );
}

ReosDuration ReosHydraulicStructure2D::currentElementTimeStep() const
{
  if ( mCurrentSimulationIndex >= 0 && mSimulations.at( mCurrentSimulationIndex ) )
  {
    return  mSimulations.at( mCurrentSimulationIndex )->representativeTimeStep();
  }

  return ReosDuration( qint64( 0 ) );
}

ReosDuration ReosHydraulicStructure2D::mapTimeStep() const
{
  if ( mCurrentSimulationIndex >= 0 && mSimulations.at( mCurrentSimulationIndex ) )
  {
    return  mSimulations.at( mCurrentSimulationIndex )->representative2DTimeStep();
  }

  return ReosDuration();
}

ReosTimeWindow ReosHydraulicStructure2D::timeWindow() const
{
  ReosTimeWindow tw;

  if ( hasCapability( ReosHydraulicStructure2D::DefinedExternally ) &&
       mTimeWindowSettings->useExternalDefinedTimeWindow()->value() &&
       mCurrentSimulationIndex >= 0 )
  {
    return mSimulations.at( mCurrentSimulationIndex )->externalTimeWindow();
  }
  else if ( mTimeWindowSettings->automaticallyDefined() )
  {
    const QList<ReosHydraulicStructureBoundaryCondition *> bcs = boundaryConditions();
    for ( ReosHydraulicStructureBoundaryCondition *bc : bcs )
    {
      switch ( mTimeWindowSettings->combineMethod() )
      {
        case ReosTimeWindowSettings::Intersection:
          tw = tw.intersection( bc->timeWindow() );
          break;
        case ReosTimeWindowSettings::Union:
          tw = tw.unite( bc->timeWindow() );
          break;
      }
    }
  }

  return mTimeWindowSettings->timeWindow( tw );
}

QIcon ReosHydraulicStructure2D::icon() const
{
  return QIcon( QStringLiteral( ":/images/hydraulicStructure2D.svg" ) );
}

ReosHydraulicNetworkElementCompatibilty ReosHydraulicStructure2D::checkCompatiblity( ReosHydraulicScheme *scheme ) const
{
  ReosHydraulicSimulation *sim = currentSimulation();
  if ( sim )
    return sim->checkCompatiblity( scheme );

  return ReosHydraulicNetworkElementCompatibilty();
}

QFileInfoList ReosHydraulicStructure2D::cleanScheme( ReosHydraulicScheme *scheme )
{
  QFileInfoList ret;
  for ( ReosHydraulicSimulation *sim : std::as_const( mSimulations ) )
  {
    ret.append( sim->cleanScheme( scheme ) );
  }

  return ret;
}

ReosHydraulicNetworkElement *ReosHydraulicStructure2dFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  return ReosHydraulicStructure2D::create( encodedElement, context );
}

ReosRoughnessStructure::ReosRoughnessStructure( const QString &mCrs )
  : mStructure( ReosPolygonStructure::createPolygonStructure( mCrs ) )
  , mDefaultRoughness( new ReosParameterDouble( tr( "Default roughness" ) ) )
{
  mDefaultRoughness->setDisplayPrecision( 3 );
  mDefaultRoughness->setValue( 0.03 );
}

ReosRoughnessStructure::ReosRoughnessStructure( const ReosEncodedElement &encodedElement )
  : mStructure( ReosPolygonStructure::createPolygonStructure( encodedElement.getEncodedData( QStringLiteral( "structure" ) ) ) )
  , mDefaultRoughness( ReosParameterDouble::decode( encodedElement.getEncodedData( QStringLiteral( "default-roughness" ) ), false, tr( "Default roughness" ), this ) )
{}

ReosEncodedElement ReosRoughnessStructure::encode() const
{
  ReosEncodedElement encodedElement( QStringLiteral( "roughness-structure" ) );

  encodedElement.addEncodedData( QStringLiteral( "structure" ), mStructure->encode() );
  encodedElement.addEncodedData( QStringLiteral( "default-roughness" ), mDefaultRoughness->encode() );

  return encodedElement;
}

ReosParameterDouble *ReosRoughnessStructure::defaultRoughness() const
{
  return mDefaultRoughness;
}

ReosPolygonStructure *ReosRoughnessStructure::structure() const
{
  return mStructure.get();
}

