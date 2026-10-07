/***************************************************************************
                      test_telemac.cpp
                     --------------------------------------
Date                 : 04-08-2023
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include <filesystem>
#include <QObject>
#include <QtTest/QtTest>

#include "reosapplication.h"
#include "reoscoremodule.h"
#include "reoshydraulicscheme.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosparameter.h"
#include "reossettings.h"
#include "reostimeseries.h"

#include "reostelemac2dsimulation.h"
#include "reostelemacsteeringfile.h"
#include "reostelemacstructureimportersource.h"
#include "reostelemacboundaries.h"
#include "reosselafin.h"

#include "reos_testutils.h"

class ReosTelemacTesting : public QObject
{
    Q_OBJECT

  private slots:
    void initTestCase();
    void cleanupTestCase();

    void reoSelafin();

    void exisingSteeringFile();
    void telemacBoundaries();

    void buildStructure();

    void importStructure();

  private:
    ReosCoreModule *coreModule;
    QTemporaryDir projectDir;
};

void ReosTelemacTesting::initTestCase()
{
  int argc = 0;
  QVERIFY( !ReosApplication::initializationReos( argc, nullptr, "reos_tests" ) );
  coreModule = new ReosCoreModule( this );
  // To avoid shared library/Qt meta-object duplication issue, we replace the Telemac engine factory loaded from plugin with the build in place one.
  ReosSimulationEngineRegistery::instance()->registerEngineFactory( new ReosTelemac2DSimulationEngineFactory() );
  coreModule->gisEngine()->setCrs( ReosGisEngine::crsFromEPSG( 32620 ) );
  ReosTelemac2DSimulationEngineFactory::initializeSettingsStatic();

#ifdef _WIN32
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/engine/telemac/cpu-usage-count" ), 1 );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-configuration" ), QStringLiteral( "win_no_mpi" ) );
#endif
}

void ReosTelemacTesting::cleanupTestCase()
{}

void ReosTelemacTesting::reoSelafin()
{
  ReosSelafin selafinFile( testFile( "/telemac/bridge/geo_bridge.slf" ) );
  QList<int> ipobo;
  std::unique_ptr<ReosMesh> mesh( selafinFile.loadMeshFrame( ipobo ) );

  int verticesCount = mesh->vertexCount();
}


void ReosTelemacTesting::exisingSteeringFile()
{
  ReosTelemacSteeringFile steeringFile( testFile( "/telemac/bridge/t2d_bridge.cas" ) );
  steeringFile.open();
  QCOMPARE( 106, steeringFile.lineCount() );
  QCOMPARE( 47, steeringFile.keyCount() );

  QCOMPARE( QStringLiteral( "'geo_bridge.cli'" ), steeringFile.value( QStringLiteral( "BOUNDARY CONDITIONS FILE" ) ) );

  steeringFile.setKey( QStringLiteral( "BOUNDARY CONDITIONS FILE" ), QStringLiteral( "'geo_bridge2.cli'" ) );
  QCOMPARE( 106, steeringFile.lineCount() );
  QCOMPARE( 47, steeringFile.keyCount() );

  steeringFile.setKey( QStringLiteral( "DUMMY_KEY" ), QStringLiteral( "XXXXX" ) );

  QCOMPARE( 107, steeringFile.lineCount() );
  QCOMPARE( 48, steeringFile.keyCount() );

  QString newSteeringFilePath = tempFile( "new_steering_file.cas" );
  steeringFile.save( newSteeringFilePath );

  ReosTelemacSteeringFile steeringFile_2( newSteeringFilePath );
  steeringFile_2.open();
  QCOMPARE( 107, steeringFile_2.lineCount() );
  QCOMPARE( 48, steeringFile_2.keyCount() );
  QCOMPARE( QStringLiteral( "'geo_bridge2.cli'" ), steeringFile_2.value( QStringLiteral( "BOUNDARY CONDITIONS FILE" ) ) );
  QCOMPARE( QStringLiteral( "XXXXX" ), steeringFile_2.value( QStringLiteral( "DUMMY_KEY" ) ) );
}

void ReosTelemacTesting::telemacBoundaries()
{
  ReosCoreModule::Message message;
  std::unique_ptr<ReosMesh> mesh( ReosMesh::createMeshFrameFromFile( testFile( "/telemac/bridge/geo_bridge.slf" ), QString(), message ) );
  ReosTelemacBoundaries boundaries( mesh.get(), testFile( "/telemac/bridge/geo_bridge.cli" ) );
  QCOMPARE( 250, boundaries.boundaryVertexCount() );
  QCOMPARE( QPolygonF( { QPointF( 1000.0, 0.0 ), QPointF( 1000.0, 250.0 ), QPointF( 0.0, 250.0 ), QPointF( 0.0, 0.0 ) } ), boundaries.envelop() );

  QCOMPARE( boundaries.boundaryConditionTypes().count(), 2 );
  QCOMPARE( boundaries.boundaryConditionTypes().at( 0 ), ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
  QCOMPARE( boundaries.boundaryConditionTypes().at( 1 ), ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QCOMPARE( boundaries.liquidDomainSegmentIndex().count(), 2 );
  QCOMPARE( boundaries.liquidDomainSegmentIndex().at( 0 ), QSet<int>( { 0 } ) );
  QCOMPARE( boundaries.liquidDomainSegmentIndex().at( 1 ), QSet<int>( { 2 } ) );

  ReosTelemacLiquidBoundaries liquidBoundaries( testFile( "/telemac/bridge/t2d_bridge.liq" ) );

  ReosTelemacLiquidBoundaries::TelemacLiquidBoundary *bcInputFlow = liquidBoundaries.boundaryCondition( 1, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( !bcInputFlow );
  bcInputFlow = liquidBoundaries.boundaryCondition( 2, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( bcInputFlow );
  QCOMPARE( bcInputFlow->rank, 2 );
  QCOMPARE( bcInputFlow->type, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QCOMPARE( bcInputFlow->time.count(), 7 );
  QCOMPARE( bcInputFlow->value.count(), 7 );

  QCOMPARE( bcInputFlow->time.at( 0 ), 0.0 );
  QCOMPARE( bcInputFlow->value.at( 0 ), 0.0 );

  QCOMPARE( bcInputFlow->time.at( 1 ), 100.0 );
  QCOMPARE( bcInputFlow->value.at( 1 ), 20.0 );

  QCOMPARE( bcInputFlow->time.at( 2 ), 6100.0 );
  QCOMPARE( bcInputFlow->value.at( 2 ), 20.0 );

  QCOMPARE( bcInputFlow->time.at( 3 ), 6600.0 );
  QCOMPARE( bcInputFlow->value.at( 3 ), 120.0 );

  QCOMPARE( bcInputFlow->time.at( 4 ), 17600.0 );
  QCOMPARE( bcInputFlow->value.at( 4 ), 120.0 );

  QCOMPARE( bcInputFlow->time.at( 5 ), 18200.0 );
  QCOMPARE( bcInputFlow->value.at( 5 ), 20.0 );

  QCOMPARE( bcInputFlow->time.at( 6 ), 90000 );
  QCOMPARE( bcInputFlow->value.at( 6 ), 20.0 );

  liquidBoundaries = ReosTelemacLiquidBoundaries( testFile( "/telemac/estimation/t2d_estimation.qsl" ) );

  bcInputFlow = liquidBoundaries.boundaryCondition( 1, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( !bcInputFlow );
  bcInputFlow = liquidBoundaries.boundaryCondition( 2, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( bcInputFlow );
  QCOMPARE( bcInputFlow->rank, 2 );
  QCOMPARE( bcInputFlow->type, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QCOMPARE( bcInputFlow->time.count(), 4 );
  QCOMPARE( bcInputFlow->value.count(), 4 );

  QCOMPARE( bcInputFlow->time.at( 0 ), 0.0 );
  QCOMPARE( bcInputFlow->value.at( 0 ), 1.0 );

  QCOMPARE( bcInputFlow->time.at( 1 ), 20.0 );
  QCOMPARE( bcInputFlow->value.at( 1 ), 50.0 );

  QCOMPARE( bcInputFlow->time.at( 2 ), 10000.0 );
  QCOMPARE( bcInputFlow->value.at( 2 ), 50.0 );

  QCOMPARE( bcInputFlow->time.at( 3 ), 50000.0 );
  QCOMPARE( bcInputFlow->value.at( 3 ), 50.0 );


  ReosTelemacLiquidBoundaries::TelemacLiquidBoundary *bcLevel = liquidBoundaries.boundaryCondition( 2, ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
  QVERIFY( !bcLevel );
  bcLevel = liquidBoundaries.boundaryCondition( 1, ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
  QVERIFY( bcLevel );
  QCOMPARE( bcLevel->rank, 1 );
  QCOMPARE( bcLevel->type, ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
  QCOMPARE( bcLevel->time.count(), 4 );
  QCOMPARE( bcLevel->value.count(), 4 );

  QCOMPARE( bcLevel->time.at( 0 ), 0.0 );
  QCOMPARE( bcLevel->value.at( 0 ), 0.5 );

  QCOMPARE( bcLevel->time.at( 1 ), 20.0 );
  QCOMPARE( bcLevel->value.at( 1 ), 0.5 );

  QCOMPARE( bcLevel->time.at( 2 ), 10000.0 );
  QCOMPARE( bcLevel->value.at( 2 ), 0.5 );

  QCOMPARE( bcLevel->time.at( 3 ), 50000.0 );
  QCOMPARE( bcLevel->value.at( 3 ), 0.5 );
}

void ReosTelemacTesting::buildStructure()
{
  QPolygonF domain;
  domain << QPointF( 495537.14930738694965839, 1996798.24522213893942535 );
  domain << QPointF( 495541.044451420661062, 1996649.45072005619294941 );
  domain << QPointF( 495816.04162018373608589, 1996651.00877766986377537 );
  domain << QPointF( 495819.15773541055386886, 1996435.21779820742085576 );
  domain << QPointF( 495994.43921691790455952, 1996437.55488462746143341 );
  domain << QPointF( 495988.20698646450182423, 1996813.04676946741528809 );
  ReosHydraulicNetworkContext context = coreModule->hydraulicNetwork()->context();
  ReosHydraulicStructure2D *hydraulicStructure = new ReosHydraulicStructure2D( domain, coreModule->gisEngine()->crsFromEPSG( 32620 ), context );
  QVERIFY( hydraulicStructure );
  coreModule->hydraulicNetwork()->addElement( hydraulicStructure );

  // Boundary condition
  ReosGeometryStructureVertex *vert1 = hydraulicStructure->geometryStructure()->searchForVertex( ReosMapExtent( ReosSpatialPosition( 495537, 1996798 ), ReosSpatialPosition( 495538, 1996799 ) ) );
  QVERIFY( vert1 );

  ReosGeometryStructureVertex *vert2 = hydraulicStructure->geometryStructure()->searchForVertex( ReosMapExtent( ReosSpatialPosition( 495542, 1996649 ), ReosSpatialPosition( 495541, 1996650 ) ) );
  QVERIFY( vert2 );

  ReosGeometryStructureVertex *vert3 = hydraulicStructure->geometryStructure()->searchForVertex( ReosMapExtent( ReosSpatialPosition( 495819, 1996435 ), ReosSpatialPosition( 495820, 1996436 ) ) );
  QVERIFY( vert3 );

  ReosGeometryStructureVertex *vert4 = hydraulicStructure->geometryStructure()->searchForVertex( ReosMapExtent( ReosSpatialPosition( 495994., 1996437 ), ReosSpatialPosition( 495995, 1996438 ) ) );
  QVERIFY( vert4 );

  hydraulicStructure->geometryStructure()->addBoundaryCondition( vert1, vert2, QString( "Upstream" ) );
  hydraulicStructure->geometryStructure()->addBoundaryCondition( vert3, vert4, QString( "Downstream" ) );

  const QList<ReosHydraulicStructureBoundaryCondition *> bcList = hydraulicStructure->boundaryConditions();
  QCOMPARE( bcList.count(), 2 );
  ReosHydraulicStructureBoundaryCondition *upstreamBC = nullptr;
  ReosHydraulicStructureBoundaryCondition *downstreamBC = nullptr;
  for ( ReosHydraulicStructureBoundaryCondition *bc : bcList )
  {
    if ( bc->elementName() == QString( "Upstream" ) )
      upstreamBC = bc;
    if ( bc->elementName() == QString( "Downstream" ) )
      downstreamBC = bc;
  }

  QVERIFY( upstreamBC );
  QVERIFY( downstreamBC );

  upstreamBC->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );

  std::unique_ptr<ReosHydrograph> usHyd( new ReosHydrograph() );
  usHyd->setValue( QDateTime( QDate( 2022, 01, 01 ), QTime( 0, 0, 0 ), Qt::UTC ), 0 );
  usHyd->setValue( QDateTime( QDate( 2022, 01, 01 ), QTime( 0, 30, 0 ), Qt::UTC ), 100 );
  usHyd->setValue( QDateTime( QDate( 2022, 01, 01 ), QTime( 1, 0, 0 ), Qt::UTC ), 100 );
  upstreamBC->gaugedHydrographsStore()->addHydrograph( usHyd.release() );
  upstreamBC->setGaugedHydrographIndex( 0 );
  upstreamBC->setInternalHydrographOrigin( ReosHydrographJunction::GaugedHydrograph );

  downstreamBC->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
  downstreamBC->constantWaterElevation()->setValue( 2 );

  ReosMeshResolutionController *meshResolControl = hydraulicStructure->meshResolutionController();
  QVERIFY( meshResolControl );
  QCOMPARE( hydraulicStructure->mesh()->vertexCount(), 0 );

  meshResolControl->defaultSize()->setValue( 50 );
  hydraulicStructure->generateMesh();
  QCOMPARE( hydraulicStructure->mesh()->vertexCount(), 86 );

  hydraulicStructure->mesh()->applyConstantZValue( 0, coreModule->gisEngine()->crsFromEPSG( 32620 ) );
  QCOMPARE( hydraulicStructure->mesh()->vertexElevation( 0 ), 0 );

  ReosHydraulicScheme *scheme = coreModule->hydraulicNetwork()->currentScheme();

  // Telemac simulation
  QVERIFY( hydraulicStructure->addSimulation( QStringLiteral( "telemac2D" ) ) );
  ReosTelemac2DSimulation *telemacSim = dynamic_cast<ReosTelemac2DSimulation *>( hydraulicStructure->currentSimulation() );

  QVERIFY( telemacSim );
  telemacSim->setEquation( ReosTelemac2DSimulation::Equation::FiniteVolume );
  telemacSim->setVolumeFiniteEquation( ReosTelemac2DSimulation::VolumeFiniteScheme::HLLC );
  telemacSim->timeStep()->setValue( ReosDuration( 1.0, ReosDuration::minute ) );
  telemacSim->outputPeriodResult2D()->setValue( 5 );
  telemacSim->outputPeriodResultHydrograph()->setValue( 1 );

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity );

  ReosTelemac2DInitialConstantWaterLevel *initialConstantWaterLevel = qobject_cast<ReosTelemac2DInitialConstantWaterLevel *>( telemacSim->initialCondition() );
  QVERIFY( initialConstantWaterLevel );
  initialConstantWaterLevel->initialWaterLevel()->setValue( 2.0 );

  ReosModule::Message message;
  ReosSimulationData simData = hydraulicStructure->simulationData( scheme->id(), message );

  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( simData.boundaryVertices.count(), 6 );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::None );

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::FromOtherSimulation );

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::FromOtherSimulation );
  simData = hydraulicStructure->simulationData( scheme->id(), message );
  QVERIFY( message.type == ReosModule::Error );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::None );

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::Interpolation );
  simData = hydraulicStructure->simulationData( scheme->id(), message );
  QVERIFY( message.type == ReosModule::Error );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::None );

  ReosTelemac2DInitialConditionFromInterpolation *interCi = dynamic_cast<ReosTelemac2DInitialConditionFromInterpolation *>( telemacSim->initialCondition() );
  interCi->firstValue()->setValue( 4 );
  interCi->secondValue()->setValue( 3 );
  QPolygonF interLine;
  interLine << QPointF( 495593.83, 1996741.39 );
  interCi->setLine( interLine, coreModule->gisEngine()->crsFromEPSG( 32620 ) );
  simData = hydraulicStructure->simulationData( scheme->id(), message );
  QVERIFY( message.type == ReosModule::Error );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::None );

  interLine << QPointF( 495888.53, 1996727.71 );
  interCi->setLine( interLine, coreModule->gisEngine()->crsFromEPSG( 32620 ) );
  simData = hydraulicStructure->simulationData( scheme->id(), message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::Vertex );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::Vertex );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::Vertex );
  int vertexCount = 86;
  QCOMPARE( simData.waterLevelIni.count(), vertexCount );
  QCOMPARE( simData.waterDepthIni.count(), vertexCount );
  QCOMPARE( simData.velocityIni.count(), vertexCount * 2 );

  for ( int vi = 0; vi < vertexCount; ++vi )
  {
    QVERIFY( simData.waterLevelIni.at( vi ) <= 4 && simData.waterLevelIni.at( vi ) >= 3 );
    QVERIFY( simData.waterDepthIni.at( vi ) <= 4 && simData.waterDepthIni.at( vi ) >= 3 );
    QVERIFY( simData.velocityIni.at( vi ) == 0 );
  }

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::LastTimeStep );
  simData = hydraulicStructure->simulationData( scheme->id(), message );
  QVERIFY( message.type == ReosModule::Error );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::None );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::None );

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity );

  coreModule->saveProject( projectDir.filePath( "telemac_model" ) );
  QVERIFY( hydraulicStructure->runSimulation( coreModule->hydraulicNetwork()->currentScheme()->calculationContext() ) );

  QVERIFY( hydraulicStructure->hasResults() );
  ReosHydraulicSimulationResults *result = hydraulicStructure->results( coreModule->hydraulicNetwork()->currentScheme() );
  QVERIFY( result );
  QCOMPARE( result->groupCount(), 3 );
  QCOMPARE( result->datasetCount( 0 ), 13 );

  telemacSim->setInitialCondition( ReosTelemac2DInitialCondition::Type::LastTimeStep );
  simData = hydraulicStructure->simulationData( scheme->id(), message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( simData.waterDepthIniLocation, ReosSimulationData::Vertex );
  QCOMPARE( simData.waterLevelIniLocation, ReosSimulationData::Vertex );
  QCOMPARE( simData.velocityIniLocation, ReosSimulationData::Vertex );
  QCOMPARE( simData.waterLevelIni.count(), vertexCount );
  QCOMPARE( simData.waterDepthIni.count(), vertexCount );
  QCOMPARE( simData.velocityIni.count(), vertexCount * 2 );
  for ( int vi = 0; vi < vertexCount; ++vi )
  {
    QVERIFY( simData.waterLevelIni.at( vi ) <= 4 && simData.waterLevelIni.at( vi ) >= 1.95 );
    QVERIFY( simData.waterDepthIni.at( vi ) <= 4 && simData.waterDepthIni.at( vi ) >= 1.95 );
    QVERIFY( simData.velocityIni.at( vi ) != 0 );
  }

  coreModule->saveProject( projectDir.filePath( "telemac_model" ) );
  QVERIFY( hydraulicStructure->runSimulation( coreModule->hydraulicNetwork()->currentScheme()->calculationContext() ) );

  QVERIFY( hydraulicStructure->hasResults() );
  result = hydraulicStructure->results( coreModule->hydraulicNetwork()->currentScheme() );
  QVERIFY( result );
  QCOMPARE( result->groupCount(), 3 );
  QCOMPARE( result->datasetCount( 0 ), 13 );
}

void ReosTelemacTesting::importStructure()
{
  ReosHydraulicNetworkContext context = coreModule->hydraulicNetwork()->context();
  std::unique_ptr<ReosStructureImporterSource> importerSource( new ReosTelemacStructureImporterSource( testFile( "/telemac/bridge/t2d_bridge.cas" ), context ) );

  std::unique_ptr<ReosStructureImporter> importer( importerSource->createImporter() );
  QVERIFY( importer );

  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.get(), coreModule->hydraulicNetwork()->context() );
  QVERIFY( structure );
}

QTEST_MAIN( ReosTelemacTesting )
#include "test_telemac.moc"
