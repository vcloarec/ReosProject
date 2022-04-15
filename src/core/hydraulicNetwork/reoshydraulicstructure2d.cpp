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

#include <QProcess>
#include <QDir>

ReosHydraulicStructure2D::ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( context.network() )
  , mMeshGenerator( new ReosGmshGenerator( this ) )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( domain, crs ) )
  , mMeshResolutionController( new ReosMeshResolutionController( this, crs ) )
  , mTopographyCollecion( ReosTopographyCollection::createTopographyCollection( context.network()->getGisEngine(), this ) )
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
  , mTopographyCollecion( ReosTopographyCollection::createTopographyCollection( encodedElement.getEncodedData( QStringLiteral( "topography-collection" ) ), context.network()->getGisEngine(), this ) )
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

  encodedElement.getData( QStringLiteral( "current-mesh-dataset-id" ), mCurrentActivatedMeshDataset );
  mMesh->activateDataset( mCurrentActivatedMeshDataset );
}

QString ReosHydraulicStructure2D::currentActivatedMeshDataset() const
{
  return mCurrentActivatedMeshDataset;
}

bool ReosHydraulicStructure2D::hasResults() const
{
  return ( mSimulationResults && mSimulationResults->groupCount() > 0 );
}

void ReosHydraulicStructure2D::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  element.addEncodedData( QStringLiteral( "structure" ), mPolylinesStructures->encode() );
  element.addEncodedData( QStringLiteral( "mesh-generator" ), mMeshGenerator->encode() );
  element.addEncodedData( QStringLiteral( "mesh-resolution-controller" ), mMeshResolutionController->encode() );
  element.addEncodedData( QStringLiteral( "topography-collection" ), mTopographyCollecion->encode() );

  QDir dir( context.projectPath() );
  QString hydDir = context.projectName() + QStringLiteral( "-hydr-struct" );
  dir.mkdir( hydDir );
  dir.cd( hydDir );
  dir.mkdir( directory() );
  dir.cd( directory() );

  mMesh->save( dir.path() );

  element.addData( QStringLiteral( "boundaries-vertices" ), mBoundaryVertices );
  element.addData( QStringLiteral( "hole-vertices" ), mHolesVertices );
  element.addEncodedData( QStringLiteral( "mesh-quality-parameters" ), mMesh->qualityMeshParameters().encode() );
  element.addEncodedData( QStringLiteral( "roughness-structure" ), mRoughnessStructure->encode() );

  QList<ReosEncodedElement> encodedSimulations;
  for ( ReosHydraulicSimulation *sim : mSimulations )
    encodedSimulations.append( sim->encode() );
  element.addListEncodedData( QStringLiteral( "simulations" ), encodedSimulations );
  element.addData( QStringLiteral( "current-simulation-index" ), mCurrentSimulationIndex );

  element.addData( QStringLiteral( "current-mesh-dataset-id" ), mCurrentActivatedMeshDataset );

  getSymbologiesFromMesh();
  element.addData( QStringLiteral( "terrain-symbology" ), mTerrainSymbology );
  const QList<ResultType> typeKeys = mResultScalarDatasetSymbologies.keys();
  QMap<int, QByteArray> resultSymbol;
  for ( ResultType type : typeKeys )
    resultSymbol.insert( int( type ), mResultScalarDatasetSymbologies.value( type ) );
  element.addData( QStringLiteral( "scalar-results-symbologies" ), resultSymbol );

  element.addEncodedData( "3d-map-setings", m3dMapSettings.encode() );
}

bool ReosHydraulicStructure2D::isSimulationInProgress() const
{
  return ( mCurrentProcess && !mCurrentProcess->isFinished() ) ;
}

ReosRoughnessStructure *ReosHydraulicStructure2D::roughnessStructure() const
{
  return mRoughnessStructure.get();
}

QDir ReosHydraulicStructure2D::structureDirectory()
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

  if ( currentSimulation() && currentSimulation()->hasResult( this, context ) )
  {
    loadResult( currentSimulation(), context );
    activateResultDatasetGroup();
  }
  else
  {
    activateResultDatasetGroup( mTerrainDatasetId );
    emit simulationResultChanged();
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
}

void ReosHydraulicStructure2D::onBoundaryConditionRemoved( const QString &bid )
{
  mNetWork->removeElement( boundaryConditionNetWorkElement( bid ) );
}

void ReosHydraulicStructure2D::onGeometryStructureChange()
{
  mBoundaryVertices.clear();
  mHolesVertices.clear();
}

void ReosHydraulicStructure2D::onMessageFromSolverReceived( const QString &message )
{
  emit simulationTextAdded( message );
}

ReosTopographyCollection *ReosHydraulicStructure2D::topographyCollecion() const
{
  return mTopographyCollecion;
}

QString ReosHydraulicStructure2D::terrainMeshDatasetId() const
{
  return mTerrainDatasetId;
}

void ReosHydraulicStructure2D::activateResultDatasetGroup( const QString &id )
{
  QString effId = id;
  if ( effId.isEmpty() )
    effId = mCurrentActivatedMeshDataset;
  else
    mCurrentActivatedMeshDataset = id;

  mMesh->activateDataset( effId );
}

QStringList ReosHydraulicStructure2D::meshDatasetIds() const
{
  return mMesh->datasetIds();
}

QString ReosHydraulicStructure2D::meshDatasetName( const QString &id ) const
{
  return mMesh->datasetName( id );
}


ReosProcess *ReosHydraulicStructure2D::prepareSimulation()
{
  if ( !currentSimulation() )
    return nullptr;
  ReosCalculationContext context = mNetWork->calculationContext();

  return new ReosSimulationPreparationProcess( this, currentSimulation(), context );

}


ReosSimulationProcess *ReosHydraulicStructure2D::startSimulation()
{
  if ( mCurrentProcess || !currentSimulation() )
    return nullptr;

  QPointer<ReosHydraulicSimulation> sim = currentSimulation();

  ReosCalculationContext context = mNetWork->calculationContext();
  mCurrentProcess.reset( sim->getProcess( this, context ) );

  connect( mCurrentProcess.get(), &ReosSimulationProcess::sendInformation, this, &ReosHydraulicStructure2D::onMessageFromSolverReceived );
  connect( mCurrentProcess.get(), &ReosProcess::finished, sim, [this, sim, context]
  {
    mCurrentProcess->deleteLater();
    mCurrentProcess.release();
    if ( !sim.isNull() )
      onSimulationFinished( sim, context );
  } );

  if ( mSimulationResults )
    mSimulationResults->deleteLater();
  mSimulationResults = nullptr;
  mMesh->setSimulationResults( nullptr );

  emit simulationResultChanged();

  mCurrentProcess->startOnOtherThread();

  return mCurrentProcess.get();
}

ReosSimulationProcess *ReosHydraulicStructure2D::currentProcess() const
{
  return mCurrentProcess.get();
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

  connect( this, &ReosHydraulicStructure2D::meshGenerated, mTopographyCollecion, [this]
  {
    if ( mTopographyCollecion->autoApply()->value() )
      mMesh->applyTopographyOnVertices( mTopographyCollecion );
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

void ReosHydraulicStructure2D::getSymbologiesFromMesh() const
{
  if ( !mSimulationResults )
    return;

  mTerrainSymbology = mMesh->datasetScalarGroupSymbology( mMesh->verticesElevationDatasetId() ).bytes();

  for ( int i = 0; i < mSimulationResults->groupCount(); ++i )
  {
    const QString id = mSimulationResults->groupId( i );
    ReosEncodedElement encodedSymb = mMesh->datasetScalarGroupSymbology( id );
    mResultScalarDatasetSymbologies.insert( mSimulationResults->datasetType( i ), encodedSymb.bytes() );
  }
}

void ReosHydraulicStructure2D::onSimulationFinished( ReosHydraulicSimulation *simulation, const ReosCalculationContext &context )
{
  if ( !simulation )
    return;

  //Store the current symbology per data type
  getSymbologiesFromMesh();

  loadResult( simulation, context );
}

void ReosHydraulicStructure2D::loadResult( ReosHydraulicSimulation *simulation, const ReosCalculationContext &context )
{
  if ( mSimulationResults )
  {
    mSimulationResults->deleteLater();
    mSimulationResults = nullptr;
  }

  mSimulationResults = simulation->createResults( this, context );

  if ( !mSimulationResults )
    return;

  mesh()->setSimulationResults( mSimulationResults );

  if ( mCurrentActivatedMeshDataset.isEmpty() )
  {
    QStringList ids = mesh()->datasetIds();
    if ( ids.count() > 1 )
      mCurrentActivatedMeshDataset = ids.at( 1 );
    else if ( ids.count() != 0 )
      mCurrentActivatedMeshDataset = ids.first();
  }

  if ( mSimulationResults )
  {
    QString waterLevelId;
    //Restore the current symbology per data type
    for ( int i = 0; i < mSimulationResults->groupCount(); ++i )
    {
      ResultType type = mSimulationResults->datasetType( i );
      ReosEncodedElement encodedSymb( mResultScalarDatasetSymbologies.value( type ) );
      QString groupId = mSimulationResults->groupId( i );
      mMesh->setDatasetScalarGroupSymbology( encodedSymb, groupId );

      if ( type == ResultType::WaterLevel )
        waterLevelId = groupId;

    }

    mMesh->setVerticalDataset3DId( waterLevelId );
  }

  emit simulationResultChanged();
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
