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
#include "reosgmshgenerator.h"
#include "reostopographycollection.h"
#include "reoshydraulicsimulation.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoshydraulicsimulation.h"
#include "reoscalculationcontext.h"
#include "reoshydraulicsimulationresults.h"
#include "reoshydraulicscheme.h"

#include <QProcess>
#include <QDir>
#include <QDebug>

ReosHydraulicStructure2D::ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( context.network() )
  , mMeshGenerator( new ReosGmshGenerator( this ) )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( domain, crs ) )
  , mMeshResolutionController( new ReosMeshResolutionController( this, crs ) )
  , mTopographyCollection( ReosTopographyCollection::createTopographyCollection( context.network()->getGisEngine(), this ) )
  , mMesh( ReosMesh::createMeshFrame( crs ) )
  , mRoughnessStructure( new ReosRoughnessStructure( crs ) )
  , mHydraulicNetworkContext( context )
{
  init();
}

ReosHydraulicStructure2D::ReosHydraulicStructure2D(
  const ReosEncodedElement &encodedElement,
  const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( encodedElement, context.network() )
  , mMeshGenerator( ReosMeshGenerator::createMeshGenerator( encodedElement.getEncodedData( QStringLiteral( "mesh-generator" ) ), this ) )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( encodedElement.getEncodedData( QStringLiteral( "structure" ) ) ) )
  , mTopographyCollection( ReosTopographyCollection::createTopographyCollection( encodedElement.getEncodedData( QStringLiteral( "topography-collection" ) ), context.network()->getGisEngine(), this ) )
  , mRoughnessStructure( new ReosRoughnessStructure( encodedElement.getEncodedData( QStringLiteral( "roughness-structure" ) ) ) )
  , m3dMapSettings( encodedElement.getEncodedData( "3d-map-setings" ) )
  , mHydraulicNetworkContext( context )
{
  if ( encodedElement.hasEncodedData( QStringLiteral( "mesh-resolution-controller" ) ) )
    mMeshResolutionController = new ReosMeshResolutionController( encodedElement.getEncodedData( QStringLiteral( "mesh-resolution-controller" ) ), this );
  else
    mMeshResolutionController = new ReosMeshResolutionController( this );

  mMesh.reset( ReosMesh::createMeshFrameFromFile( structureDirectory().path() ) );
  init();

  mMesh->setQualityMeshParameter( encodedElement.getEncodedData( QStringLiteral( "mesh-quality-parameters" ) ) );

  encodedElement.getData( QStringLiteral( "boundaries-vertices" ), mBoundaryVertices );
  encodedElement.getData( QStringLiteral( "hole-vertices" ), mHolesVertices );
  mMesh->setBoundariesVertices( mBoundaryVertices );
  mMesh->setHolesVertices( mHolesVertices );

  //if the boundary condition have associated elements in the network, attache it, if not creates them
  const QStringList boundaryIdList = mPolylinesStructures->classes();
  for ( const QString &bcId : boundaryIdList )
  {
    ReosHydraulicStructureBoundaryCondition *bc = boundaryConditionNetWorkElement( bcId );
    if ( bc )
      bc->attachStructure( this );
    else
      mNetWork->addElement( new ReosHydraulicStructureBoundaryCondition( this, bcId, mHydraulicNetworkContext ) );
  }

  const QList<ReosEncodedElement> encodedSimulations = encodedElement.getListEncodedData( QStringLiteral( "simulations" ) );
  for ( const ReosEncodedElement &elem : encodedSimulations )
  {
    ReosHydraulicSimulation *sim = ReosSimulationEngineRegistery::instance()->createSimulation( elem, this );
    if ( sim )
      mSimulations.append( sim );
  }

  encodedElement.getData( QStringLiteral( "current-simulation-index" ), mCurrentSimulationIndex );
  if ( mCurrentSimulationIndex >= mSimulations.count() )
    mCurrentSimulationIndex = mSimulations.count() - 1;

  encodedElement.getData( QStringLiteral( "terrain-symbology" ), mTerrainSymbology );
  mMesh->setDatasetScalarGroupSymbology( ReosEncodedElement( mTerrainSymbology ), mTerrainDatasetId );
  QMap<int, QByteArray> resultSymbol;
  encodedElement.getData( QStringLiteral( "scalar-results-symbologies" ), resultSymbol );
  const QList<int> typeKeys = resultSymbol.keys();
  for ( int type :  typeKeys )
    mResultScalarDatasetSymbologies.insert( static_cast<ResultType>( type ), resultSymbol.value( type ) );

  QString currentActivatedMeshDataset;
  encodedElement.getData( QStringLiteral( "current-mesh-dataset-id" ), currentActivatedMeshDataset );
  mMesh->activateDataset( currentActivatedMeshDataset );
}

QVector<QVector<QVector<int> > > ReosHydraulicStructure2D::holesVertices() const
{
  return mHolesVertices;
}

QString ReosHydraulicStructure2D::currentActivatedMeshDataset() const
{
  return mMesh->currentdScalarDatasetId();
}

ReosHydraulicSimulationResults::DatasetType ReosHydraulicStructure2D::currentActivatedDatasetResultType() const
{
  const QString currentDatasetId =  mMesh->currentdScalarDatasetId();
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

  return ReosHydraulicSimulationResults::DatasetType::None;
}

QString ReosHydraulicStructure2D::currentDatasetName() const
{
  return mMesh->datasetName( mMesh->currentdScalarDatasetId() );
}

void ReosHydraulicStructure2D::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  element.addEncodedData( QStringLiteral( "structure" ), mPolylinesStructures->encode() );
  element.addEncodedData( QStringLiteral( "mesh-generator" ), mMeshGenerator->encode() );
  element.addEncodedData( QStringLiteral( "mesh-resolution-controller" ), mMeshResolutionController->encode() );
  element.addEncodedData( QStringLiteral( "topography-collection" ), mTopographyCollection->encode() );

  QDir dir( context.projectPath() );
  QString hydDir = context.projectName() + QStringLiteral( "-hydr-struct" );
  dir.mkdir( hydDir );
  dir.cd( hydDir );
  dir.mkdir( directory() );
  dir.cd( directory() );

  element.addData( QStringLiteral( "boundaries-vertices" ), mBoundaryVertices );
  element.addData( QStringLiteral( "hole-vertices" ), mHolesVertices );
  element.addEncodedData( QStringLiteral( "mesh-quality-parameters" ), mMesh->qualityMeshParameters().encode() );
  element.addEncodedData( QStringLiteral( "roughness-structure" ), mRoughnessStructure->encode() );

  QList<ReosEncodedElement> encodedSimulations;
  for ( ReosHydraulicSimulation *sim : mSimulations )
    encodedSimulations.append( sim->encode() );
  element.addListEncodedData( QStringLiteral( "simulations" ), encodedSimulations );
  element.addData( QStringLiteral( "current-simulation-index" ), mCurrentSimulationIndex );

  element.addData( QStringLiteral( "current-mesh-dataset-id" ), mMesh->currentdScalarDatasetId() );

  getSymbologiesFromMesh( context.network()->calculationContext().schemeId() );
  element.addData( QStringLiteral( "terrain-symbology" ), mTerrainSymbology );
  const QList<ResultType> typeKeys = mResultScalarDatasetSymbologies.keys();
  QMap<int, QByteArray> resultSymbol;
  for ( ResultType type : typeKeys )
    resultSymbol.insert( int( type ), mResultScalarDatasetSymbologies.value( type ) );
  element.addData( QStringLiteral( "scalar-results-symbologies" ), resultSymbol );

  element.addEncodedData( "3d-map-setings", m3dMapSettings.encode() );
}

ReosRoughnessStructure *ReosHydraulicStructure2D::roughnessStructure() const
{
  return mRoughnessStructure.get();
}

QDir ReosHydraulicStructure2D::structureDirectory() const
{
  return QDir( mHydraulicNetworkContext.projectPath() + '/' + mHydraulicNetworkContext.projectName() + QStringLiteral( "-hydr-struct" ) + '/' + directory() );
}

QList<ReosHydraulicStructureBoundaryCondition *> ReosHydraulicStructure2D::boundaryConditions() const
{
  QList<ReosHydraulicStructureBoundaryCondition *> ret;
  const QStringList boundaryIdList = mPolylinesStructures->classes();
  for ( const QString &bcId : boundaryIdList )
    ret.append( boundaryConditionNetWorkElement( bcId ) );

  return ret;
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
  mCurrentSimulationIndex = index;
  emit currentSimulationChanged();
}

bool ReosHydraulicStructure2D::addSimulation( const QString key )
{
  ReosHydraulicSimulation *sim = ReosSimulationEngineRegistery::instance()->createSimulation( key, this );
  if ( sim )
  {
    mSimulations.append( sim );
    mCurrentSimulationIndex = mSimulations.count() - 1;
    return true;
  }

  return false;
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
  mNetWork->addElement( new ReosHydraulicStructureBoundaryCondition( this, bid, mNetWork->context() ) );
  emit boundaryChanged();
}

void ReosHydraulicStructure2D::onBoundaryConditionRemoved( const QString &bid )
{
  mNetWork->removeElement( boundaryConditionNetWorkElement( bid ) );
  emit boundaryChanged();
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
  return mTerrainDatasetId;
}

void ReosHydraulicStructure2D::activateResultDatasetGroup( const QString &id )
{
  mMesh->activateDataset( id );
}

void ReosHydraulicStructure2D::activateResultDatasetGroup( ReosHydraulicSimulationResults::DatasetType datasetType )
{

}

QStringList ReosHydraulicStructure2D::meshDatasetIds() const
{
  return mMesh->datasetIds();
}

QString ReosHydraulicStructure2D::meshDatasetName( const QString &id ) const
{
  return mMesh->datasetName( id );
}

ReosProcess *ReosHydraulicStructure2D::getPreparationProcessSimulation( const ReosCalculationContext &context )
{
  if ( !currentSimulation() )
    return nullptr;

  return new ReosSimulationPreparationProcess( this, currentSimulation(), context );
}

ReosProcess *ReosHydraulicStructure2D::getPreparationProcessSimulation( const ReosCalculationContext &context, const QDir &directory )
{
  if ( !currentSimulation() )
    return nullptr;

  std::unique_ptr<ReosSimulationPreparationProcess> ret( new ReosSimulationPreparationProcess( this, currentSimulation(), context ) );
  ret->setDestination( directory );
  return ret.release();
}

ReosSimulationProcess *ReosHydraulicStructure2D::startSimulation( const ReosCalculationContext &context )
{
  if ( !currentSimulation() )
    return nullptr;

  QString schemeId = context.schemeId();

  if ( processFromScheme( context.schemeId() ) )
    return nullptr;

  QPointer<ReosHydraulicSimulation> sim = currentSimulation();

  ReosSimulationProcess *process = mSimulationProcesses.emplace( schemeId, sim->getProcess( this, context ) ).first->second.get();

  connect( process, &ReosProcess::finished, sim, [this, sim, schemeId]
  {
    auto it = mSimulationProcesses.find( schemeId );
    bool success = false;
    if ( it != mSimulationProcesses.end() )
    {
      ReosSimulationProcess *process = it->second.get();
      const QMap<QString, ReosHydrograph *> hydrographs = process->outputHydrographs();
      const QList<QString> hydIds = hydrographs.keys();
      for ( const QString &id : hydIds )
      {
        const QList<ReosHydraulicStructureBoundaryCondition *> bounds = boundaryConditions();
        for ( ReosHydraulicStructureBoundaryCondition *bc : bounds )
        {
          if ( bc->boundaryConditionId() == id )
            bc->outputHydrograph()->copyFrom( hydrographs.value( id ) );
        }
      }

      success = it->second->isSuccessful();

      mSimulationProcesses.erase( it );
    }

    onSimulationFinished( sim, schemeId, success );
  } );

  //Store the current symbology per data type
  getSymbologiesFromMesh( schemeId );
  setResultsOnStructure( nullptr );
  emit simulationResultChanged();

  process->startOnOtherThread();

  return process;
}

ReosSimulationProcess *ReosHydraulicStructure2D::simulationProcess( const ReosCalculationContext &context ) const
{
  return processFromScheme( context.schemeId() );
}

bool ReosHydraulicStructure2D::hasSimulationRunning() const
{
  return !mSimulationProcesses.empty();
}

bool ReosHydraulicStructure2D::hasResult( const ReosCalculationContext &context )
{
  return mSimulationResults.contains( context.schemeId() );
}

QDateTime ReosHydraulicStructure2D::resultsDateTime( const ReosCalculationContext &context ) const
{
  if ( mSimulationResults.contains( context.schemeId() ) )
    return mSimulationResults.value( context.schemeId() )->runDateTime();
  else
    return QDateTime();
}

int ReosHydraulicStructure2D::resultsTimeStepCount( const ReosCalculationContext &context ) const
{
  if ( mSimulationResults.contains( context.schemeId() ) )
  {
    ReosHydraulicSimulationResults *results = mSimulationResults.value( context.schemeId() );
    if ( results && results->groupCount() > 0 )
      return mSimulationResults.value( context.schemeId() )->datasetCount( 0 );
  }

  return 0;
}

double ReosHydraulicStructure2D::resultsValueAt( const QDateTime &time,
    const ReosSpatialPosition &position,
    ReosHydraulicSimulationResults::DatasetType datasetType,
    const ReosCalculationContext &context )
{
  if ( mSimulationResults.contains( context.schemeId() ) )
  {
    ReosHydraulicSimulationResults *results = mSimulationResults.value( context.schemeId() );
    if ( results )
      return results->interpolateResultOnMesh( mMesh.get(), position, time, datasetType );
  }

  return std::numeric_limits<double>::quiet_NaN();
}

QString ReosHydraulicStructure2D::resultsUnits( ReosHydraulicSimulationResults::DatasetType datasetType, const ReosCalculationContext &context )
{
  if ( mSimulationResults.contains( context.schemeId() ) )
    return mSimulationResults.value( context.schemeId() )->unitString( datasetType );

  return QString();
}

void ReosHydraulicStructure2D::init()
{
  mTerrainDatasetId = mMesh->enableVertexElevationDataset( tr( "Terrain elevation" ) );
  mMesh->setVerticaleSCale( m3dMapSettings.verticalExaggeration() );

  connect( mPolylinesStructures.get(), &ReosDataObject::dataChanged, this, [this]
  {
    if ( mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( mPolylinesStructures.get(), &ReosPolylinesStructure::boundaryConditionAdded, this, &ReosHydraulicStructure2D::onBoundaryConditionAdded );
  connect( mPolylinesStructures.get(), &ReosPolylinesStructure::boundaryConditionRemoved, this, &ReosHydraulicStructure2D::onBoundaryConditionRemoved );

  connect( mMeshResolutionController, &ReosDataObject::dataChanged, this, [this]
  {
    if ( mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( mMeshGenerator, &ReosDataObject::dataChanged, this, [this]
  {
    if ( mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( this, &ReosHydraulicStructure2D::meshGenerated, mTopographyCollection, [this]
  {
    if ( mTopographyCollection->autoApply()->value() )
      mMesh->applyTopographyOnVertices( mTopographyCollection );
  } );
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

ReosHydraulicStructureBoundaryCondition *ReosHydraulicStructure2D::boundaryConditionNetWorkElement( const QString boundaryId ) const
{
  const QList<ReosHydraulicNetworkElement *> hydrElems = mNetWork->getElements( ReosHydraulicStructureBoundaryCondition::staticType() );
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
      onMeshGenerated( processP->meshResult() );
    }
  } );

  return process.release();
}

QVector<ReosHydraulicStructure2D::BoundaryVertices> ReosHydraulicStructure2D::boundaryVertices() const
{
  if ( mBoundaryVertices.isEmpty() )
    return QVector<ReosHydraulicStructure2D::BoundaryVertices>();

  int boundarySegCount = mPolylinesStructures->boundary().count();
  Q_ASSERT( mBoundaryVertices.count() ==  boundarySegCount );

  QVector<BoundaryVertices> vertexToCondition( boundarySegCount );

  for ( int i = 0; i < boundarySegCount; ++i )
  {
    BoundaryVertices boundVert;
    boundVert.verticesIndex = mBoundaryVertices.at( i );
    QString bcid = mPolylinesStructures->boundaryClassId( i );
    boundVert.boundaryCondition = boundaryConditionNetWorkElement( bcid );

    vertexToCondition[i] = boundVert;
  }

  return vertexToCondition;
}


void ReosHydraulicStructure2D::onMeshGenerated( const ReosMeshFrameData &meshData )
{
  mMesh->generateMesh( meshData );

  mBoundaryVertices = meshData.boundaryVertices;
  mHolesVertices = meshData.holesVertices;

  emit meshGenerated();
  emit dataChanged();
}

void ReosHydraulicStructure2D::getSymbologiesFromMesh( const QString &schemeId ) const
{
  mTerrainSymbology = mMesh->datasetScalarGroupSymbology( mMesh->verticesElevationDatasetId() ).bytes();

  ReosHydraulicSimulationResults *simulationResults = mSimulationResults.value( schemeId, nullptr );

  if ( !simulationResults )
    return;

  for ( int i = 0; i < simulationResults->groupCount(); ++i )
  {
    const QString id = simulationResults->groupId( i );
    if ( mMesh->hasDatasetGroupIndex( id ) )
    {
      ReosEncodedElement encodedSymb = mMesh->datasetScalarGroupSymbology( id );
      mResultScalarDatasetSymbologies.insert( simulationResults->datasetType( i ), encodedSymb.bytes() );
    }
  }
}

void ReosHydraulicStructure2D::onSimulationFinished( ReosHydraulicSimulation *simulation, const QString &schemeId, bool success )
{
  if ( !simulation )
  {
    emit simulationFinished();
    return;
  }
  simulation->saveSimulationResult( this, schemeId, success );

  //! Now we can remove the old one
  mSimulationResults.value( schemeId )->deleteLater();
  mSimulationResults.remove( schemeId );

  if ( mNetWork->calculationContext().schemeId() == schemeId )
    updateCurrentResults( schemeId );

  emit simulationFinished();
}

void ReosHydraulicStructure2D::loadResult( ReosHydraulicSimulation *simulation, const QString &schemeId )
{
  ReosHydraulicSimulationResults *simulationResults = mSimulationResults.value( schemeId, nullptr );

  if ( simulationResults )
  {
    simulationResults->deleteLater();
    mSimulationResults.remove( schemeId );
  }

  ReosHydraulicSimulationResults *simResults = simulation->loadSimulationResults( this, schemeId );
  mSimulationResults.insert( schemeId, simResults );
}

void ReosHydraulicStructure2D::setResultsOnStructure( ReosHydraulicSimulationResults *simResults )
{
  mesh()->setSimulationResults( simResults );

  const QList<ReosHydraulicStructureBoundaryCondition *> boundaries = boundaryConditions();
  for ( ReosHydraulicStructureBoundaryCondition *bc : boundaries )
  {
    switch ( bc->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
        bc->outputHydrograph()->clear();
        break;
    }
  }

  if ( !simResults )
  {
    mMesh->setVerticalDataset3DId( QString(), false );
    mMesh->activateDataset( QString() );
    return;
  }

  if ( simResults )
  {
    QString currentDatasetId = mMesh->currentdScalarDatasetId();
    ReosHydraulicSimulationResults::DatasetType currentType = currentActivatedDatasetResultType();

    if ( currentDatasetId.isEmpty() )
    {
      const QStringList ids = mesh()->datasetIds();
      if ( ids.count() > 1 )
        currentDatasetId = ids.at( 1 );
      else if ( ids.count() != 0 )
        currentDatasetId = ids.first();
    }

    QString waterLevelId;
    QString currentActivatedId = mMesh->verticesElevationDatasetId();

    //Restore the current symbology per data type
    for ( int i = 0; i < simResults->groupCount(); ++i )
    {
      ResultType type = simResults->datasetType( i );
      const ReosEncodedElement encodedSymb( mResultScalarDatasetSymbologies.value( type ) );
      const QString groupId = simResults->groupId( i );
      mMesh->setDatasetScalarGroupSymbology( encodedSymb, groupId );

      if ( type == ResultType::WaterLevel )
        waterLevelId = groupId;

      if ( type == currentType )
        currentActivatedId = groupId;
    }
    mMesh->setVerticalDataset3DId( waterLevelId, false );
    mMesh->activateDataset( currentActivatedId );

    const QMap<QString, ReosHydrograph *> outputHydrographs = simResults->outputHydrographs();
    const QList<ReosHydraulicStructureBoundaryCondition *> boundaries = boundaryConditions();

    for ( ReosHydraulicStructureBoundaryCondition *bc : boundaries )
    {
      if ( bc->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::OutputLevel )
      {
        bc->outputHydrograph()->clear();
        if ( outputHydrographs.contains( bc->boundaryConditionId() ) )
          bc->outputHydrograph()->copyFrom( outputHydrographs.value( bc->boundaryConditionId() ) );
      }
    }
  }

  emit simulationResultChanged();
}

void ReosHydraulicStructure2D::updateCurrentResults( const QString &schemeId )
{
  //Store the current symbology per data type
  getSymbologiesFromMesh( schemeId );

  if ( currentSimulation() && currentSimulation()->hasResult( this, schemeId ) )
  {
    if ( !mSimulationResults.contains( schemeId ) )
      loadResult( currentSimulation(), schemeId );

    setResultsOnStructure( mSimulationResults.value( schemeId ) );
  }
  else
  {
    setResultsOnStructure( nullptr );
    activateResultDatasetGroup( mTerrainDatasetId );
    emit simulationResultChanged();
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
  mMesh->activateDataset( mTerrainDatasetId );
  mMesh->setDatasetScalarGroupSymbology( ReosEncodedElement( mTerrainSymbology ), mTerrainDatasetId );
}

void ReosHydraulicStructure2D::deactivateMeshScalar()
{
  mTerrainSymbology = mMesh->datasetScalarGroupSymbology( mTerrainDatasetId ).bytes();
  mMesh->activateDataset( QString() );
}

ReosHydraulicStructure2D *ReosHydraulicStructure2D::create( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  return new ReosHydraulicStructure2D( encodedElement, context );

}

void ReosHydraulicStructure2D::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  getSymbologiesFromMesh( scheme->id() );

  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );
  encodedElement.addData( QStringLiteral( "current-simulation-index" ), mCurrentSimulationIndex );
  scheme->saveElementConfig( id(), encodedElement );

  //stor ethe current symbology from the scheme that will change
}

void ReosHydraulicStructure2D::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  ReosEncodedElement encodedElement = scheme->restoreElementConfig( id() );

  int simulationIndex = -1;
  encodedElement.getData( QStringLiteral( "current-simulation-index" ), simulationIndex );

  if ( simulationIndex == -1 )
    return;

  mCurrentSimulationIndex = simulationIndex;
  updateCurrentResults( scheme->id() );

  emit currentSimulationChanged();

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
