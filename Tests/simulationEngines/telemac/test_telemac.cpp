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
#include<QtTest/QtTest>
#include <QObject>
#include <filesystem>

#include "reoshydraulicstructure2d.h"
#include "reoscoremodule.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosparameter.h"
#include "reostimeseries.h"
#include "reoshydraulicscheme.h"
#include "reossettings.h"
#include "reosapplication.h"

#include "reostelemac2dsimulation.h"

class ReosTelemacTesting : public QObject
{
    Q_OBJECT

  private slots:
    void initTestCase();
    void cleanupTestCase();
    void buildStructure();

  private:
    ReosCoreModule *coreModule;
    QTemporaryDir projectDir;

};

void ReosTelemacTesting::initTestCase()
{
  int argc = 0;
  QVERIFY( !ReosApplication::initializationReos( argc, nullptr, "reos_tests" ) );
  coreModule = new ReosCoreModule( this );
  coreModule->gisEngine()->setCrs( ReosGisEngine::crsFromEPSG( 32620 ) );
  ReosTelemac2DSimulationEngineFactory::initializeSettingsStatic();

#ifdef _WIN32
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/engine/telemac/cpu-usage-count" ), 1 );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-configuration" ), QStringLiteral( "win_no_mpi" ) );
#endif
}

void ReosTelemacTesting::cleanupTestCase()
{

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
  ReosHydraulicStructure2D *hydraulicStructure =
    new ReosHydraulicStructure2D( domain, coreModule->gisEngine()->crsFromEPSG( 32620 ), context );
  QVERIFY( hydraulicStructure );
  coreModule->hydraulicNetwork()->addElement( hydraulicStructure );

  // Boundary condition
  ReosGeometryStructureVertex *vert1 =
    hydraulicStructure->geometryStructure()->searchForVertex(
      ReosMapExtent( ReosSpatialPosition( 495537, 1996798 ),
                     ReosSpatialPosition( 495538, 1996799 ) ) );
  QVERIFY( vert1 );

  ReosGeometryStructureVertex *vert2 =
    hydraulicStructure->geometryStructure()->searchForVertex(
      ReosMapExtent( ReosSpatialPosition( 495542, 1996649 ),
                     ReosSpatialPosition( 495541, 1996650 ) ) );
  QVERIFY( vert2 );

  ReosGeometryStructureVertex *vert3 =
    hydraulicStructure->geometryStructure()->searchForVertex(
      ReosMapExtent( ReosSpatialPosition( 495819, 1996435 ),
                     ReosSpatialPosition( 495820, 1996436 ) ) );
  QVERIFY( vert3 );

  ReosGeometryStructureVertex *vert4 =
    hydraulicStructure->geometryStructure()->searchForVertex(
      ReosMapExtent( ReosSpatialPosition( 495994., 1996437 ),
                     ReosSpatialPosition( 495995, 1996438 ) ) );
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
  qobject_cast<ReosTelemac2DInitialConstantWaterLevel>( telemacSim->initialCondition() ).initialWaterLevel()->setValue( 2.0 );

  coreModule->saveProject( projectDir.filePath( "telemac_model" ) );

  QVERIFY( hydraulicStructure->runSimulation( coreModule->hydraulicNetwork()->currentScheme()->calculationContext() ) );

  QVERIFY( hydraulicStructure->hasResults() );
  ReosHydraulicSimulationResults *result = hydraulicStructure->results( coreModule->hydraulicNetwork()->currentScheme() );
  QVERIFY( result );
  QCOMPARE( result->groupCount(), 3 );
  QCOMPARE( result->datasetCount( 0 ), 13 );
}

QTEST_MAIN( ReosTelemacTesting )
#include "test_telemac.moc"
