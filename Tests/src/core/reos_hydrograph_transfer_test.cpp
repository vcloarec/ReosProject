/***************************************************************************
  reos_hydrograph_transfer_test.cpp

 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
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

#include "reos_testutils.h"
#include "reoshydrographrouting.h"
#include "reoshydrographsource.h"
#include "reoshydrograph.h"
#include "reostransferfunction.h"
#include "reosrunoffmodel.h"
#include "reosgisengine.h"

#define WAITING_TIME_FOR_LOOP 100


class ReosHydrographTransferTest: public QObject
{
    Q_OBJECT
  private slots:
    void initTestCase();

    void test_junction();
    void test_classicMuskingumRouting();
    void test_watershed_and_routing();

  private:
    ReosHydrographSourceFixed mSource1;
    ReosHydrographSourceFixed mSource2;
    ReosHydrographSourceFixed mSource3;

    ReosGisEngine gisEngine;

    QEventLoop loop;
    QTimer timer;
};

void ReosHydrographTransferTest::initTestCase()
{
  std::unique_ptr<ReosHydrograph> hydrograph1 = std::make_unique<ReosHydrograph>();
  hydrograph1->setReferenceTime( QDateTime( QDate( 2020, 01, 01 ), QTime( 10, 0, 0 ) ) );
  hydrograph1->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph1->setValue( ReosDuration( 30, ReosDuration::minute ), 2 );
  hydrograph1->setValue( ReosDuration( 40, ReosDuration::minute ), 5 );
  hydrograph1->setValue( ReosDuration( 55, ReosDuration::minute ), 3 );
  hydrograph1->setValue( ReosDuration( 1.5, ReosDuration::hour ), 0 );
  mSource1.setHydrograph( hydrograph1.release() );

  std::unique_ptr<ReosHydrograph> hydrograph2 = std::make_unique<ReosHydrograph>();
  hydrograph2->setReferenceTime( QDateTime( QDate( 2020, 01, 01 ), QTime( 9, 30, 0 ) ) );
  hydrograph2->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph2->setValue( ReosDuration( 30, ReosDuration::minute ), 1 );
  hydrograph2->setValue( ReosDuration( 45, ReosDuration::minute ), 3 );
  hydrograph2->setValue( ReosDuration( 50, ReosDuration::minute ), 4 );
  hydrograph2->setValue( ReosDuration( 2, ReosDuration::hour ), 0 );
  mSource2.setHydrograph( hydrograph2.release() );

  std::unique_ptr<ReosHydrograph> hydrograph3 = std::make_unique<ReosHydrograph>();
  hydrograph3->setReferenceTime( QDateTime( QDate( 2020, 01, 01 ), QTime( 10, 15, 0 ) ) );
  hydrograph3->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph3->setValue( ReosDuration( 10, ReosDuration::minute ), 4 );
  hydrograph3->setValue( ReosDuration( 15, ReosDuration::minute ), 3 );
  hydrograph3->setValue( ReosDuration( 25, ReosDuration::minute ), 1 );
  hydrograph3->setValue( ReosDuration( 45, ReosDuration::hour ), 0 );
  mSource3.setHydrograph( hydrograph3.release() );


  connect( &timer, &QTimer::timeout, &loop, &QEventLoop::quit );
}

void ReosHydrographTransferTest::test_junction()
{
  ReosCalculationContext context;
  ReosHydrographJunction junction{ QPointF()};

  ReosHydrograph *inputHydrograph_1 = mSource1.outputHydrograph( );
  ReosHydrograph *inputHydrograph_2 = mSource2.outputHydrograph( );
  ReosHydrograph *inputHydrograph_3 = mSource3.outputHydrograph( );

  ReosHydrographRoutingLink transfer1;
  transfer1.setInputHydrographSource( &mSource1 );
  transfer1.setHydrographDestination( &junction );

  junction.updateCalculationContext( context );
  ReosHydrograph *junctionHydrograph = junction.outputHydrograph( );

  QVERIFY( junctionHydrograph );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( junctionHydrograph->valueCount(), inputHydrograph_1->valueCount() );

  for ( int i = 0; i < inputHydrograph_1->valueCount(); ++i )
  {
    QCOMPARE( junctionHydrograph->valueAt( i ), inputHydrograph_1->valueAt( i ) );
    QVERIFY( junctionHydrograph->timeAt( i ) == inputHydrograph_1->timeAt( i ) );
  }

  ReosHydrographRoutingLink transfer2;
  transfer2.setInputHydrographSource( &mSource2 );
  transfer2.setHydrographDestination( &junction );

  junction.updateCalculationContext( context );
  junctionHydrograph = junction.outputHydrograph( );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( junctionHydrograph->valueCount(), 8 );

  for ( int i = 0; i < inputHydrograph_1->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_1->timeAt( i );
    QCOMPARE( junctionHydrograph->valueAtTime( time ),
              inputHydrograph_1->valueAtTime( time ) + inputHydrograph_2->valueAtTime( time ) );
  }

  for ( int i = 0; i < inputHydrograph_2->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_2->timeAt( i );
    QCOMPARE( junctionHydrograph->valueAtTime( time ),
              inputHydrograph_1->valueAtTime( time ) + inputHydrograph_2->valueAtTime( time ) );
  }

  ReosHydrographJunction junction2{ QPointF()};

  ReosHydrographRoutingLink transfer3;
  transfer3.setInputHydrographSource( &mSource3 );
  transfer3.setHydrographDestination( &junction );

  ReosHydrographRoutingLink transfer4;
  transfer4.setInputHydrographSource( &junction );
  transfer4.setHydrographDestination( &junction2 );

  junction2.updateCalculationContext( context );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  ReosHydrograph *junctionHydrograph2 = junction2.outputHydrograph( );

  for ( int i = 0; i < inputHydrograph_1->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_1->timeAt( i );
    QCOMPARE( junctionHydrograph2->valueAtTime( time ),
              inputHydrograph_1->valueAtTime( time ) +
              inputHydrograph_2->valueAtTime( time ) +
              inputHydrograph_3->valueAtTime( time ) );
  }

  for ( int i = 0; i < inputHydrograph_2->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_2->timeAt( i );
    QCOMPARE( junctionHydrograph2->valueAtTime( time ),
              inputHydrograph_1->valueAtTime( time ) +
              inputHydrograph_2->valueAtTime( time ) +
              inputHydrograph_3->valueAtTime( time ) );
  }

  for ( int i = 0; i < inputHydrograph_3->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_3->timeAt( i );
    QCOMPARE( junctionHydrograph2->valueAtTime( time ),
              inputHydrograph_1->valueAtTime( time ) +
              inputHydrograph_2->valueAtTime( time ) +
              inputHydrograph_3->valueAtTime( time ) );
  }
}

void ReosHydrographTransferTest::test_classicMuskingumRouting()
{
  ReosModule rootModule;
  ReosHydrographRoutingMethodFactories::instantiate( &rootModule );
  ReosCalculationContext context;

  ReosHydrographRoutingLink routing;
  QVERIFY( routing.setCurrentRoutingMethod( ReosHydrographRoutingMethodMuskingum::staticType() ) );
  QVERIFY( routing.currentRoutingMethod() );
  QVERIFY( routing.currentRoutingMethod()->type() == ReosHydrographRoutingMethodMuskingum::staticType() );

  routing.setInputHydrographSource( &mSource1 );
  routing.updateCalculationContext( context );
  QVERIFY( routing.outputHydrograph() );
}

void ReosHydrographTransferTest::test_watershed_and_routing()
{
  // build rainfalls
  ReosModule root;
  ReosIdfFormulaRegistery::instantiate( &root );
  ReosIdfFormulaRegistery *idfRegistery = ReosIdfFormulaRegistery::instance();
  idfRegistery->registerFormula( new ReosIdfFormulaMontana );
  ReosTransferFunctionFactories::instantiate( &root );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionNashUnitHydrographFactory );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionLinearReservoirFactory );

  ReosHydrographRoutingMethodFactories::instantiate( &root );

  ReosIntensityDurationCurve idCurve;
  idCurve.addInterval( ReosDuration( 5, ReosDuration::minute ), ReosDuration( 1, ReosDuration::hour ) );
  idCurve.createParameters( 0, idfRegistery->formula( QStringLiteral( "Montana" ) ), ReosDuration::minute, ReosDuration::minute );
  idCurve.setCurrentFormula( QStringLiteral( "Montana" ) );
  idCurve.setupFormula( idfRegistery );
  ReosParameterDouble *a = idCurve.currentParameters( 0 )->parameter( 0 );
  ReosParameterDouble *b = idCurve.currentParameters( 0 )->parameter( 1 );
  a->setValue( 4.78 );
  b->setValue( 0.322 );
  ReosRainfallChicagoItem chicagoRainfallItem( "chicago", QString() );
  ReosRainfallAlternatingBlockItem alternateRainfallItem( "alternate", QString() );

  chicagoRainfallItem.data()->timeStepParameter()->setValue( ReosDuration( 5, ReosDuration::minute ) );
  chicagoRainfallItem.data()->totalDuration()->setValue( ReosDuration( 1, ReosDuration::hour ) );
  chicagoRainfallItem.data()->setIntensityDurationCurve( &idCurve );

  alternateRainfallItem.data()->timeStepParameter()->setValue( ReosDuration( 5, ReosDuration::minute ) );
  alternateRainfallItem.data()->totalDuration()->setValue( ReosDuration( 30, ReosDuration::minute ) );
  alternateRainfallItem.data()->setIntensityDurationCurve( &idCurve );

  QCOMPARE( chicagoRainfallItem.data()->valueCount(), 12 );
  QCOMPARE( alternateRainfallItem.data()->valueCount(), 6 );
  QVERIFY( equal( chicagoRainfallItem.data()->valueAt( 5 ), 14.234, 0.001 ) );
  QVERIFY( equal( alternateRainfallItem.data()->valueAt( 2 ), 8.539, 0.001 ) );

  ReosWatershedTree watershedTree( &gisEngine );

  ReosWatershed *watershed1 = watershedTree.addWatershed( new ReosWatershed() );
  watershed1->area()->setValue( ReosArea( 10, ReosArea::km2 ) );
  watershed1->concentrationTime()->setValue( ReosDuration( 2, ReosDuration::hour ) );
  ReosRunoffConstantCoefficientModel runoffConstantCoefficientModel_1( "test_1" );
  runoffConstantCoefficientModel_1.coefficient()->setValue( 0.5 );
  watershed1->runoffModels()->addRunoffModel( &runoffConstantCoefficientModel_1 );
  watershed1->setCurrentTransferFunction( ReosTransferFunctionNashUnitHydrograph::staticType() );

  ReosWatershed *watershed2 = watershedTree.addWatershed( new ReosWatershed() );
  watershed2->area()->setValue( ReosArea( 5, ReosArea::km2 ) );
  watershed2->concentrationTime()->setValue( ReosDuration( 1, ReosDuration::hour ) );
  ReosRunoffConstantCoefficientModel runoffConstantCoefficientModel_2( "test_2" );
  runoffConstantCoefficientModel_2.coefficient()->setValue( 0.3 );
  watershed2->runoffModels()->addRunoffModel( &runoffConstantCoefficientModel_1 );
  watershed2->setCurrentTransferFunction( ReosTransferFunctionNashUnitHydrograph::staticType() );

  ReosWatershed *watershed3 = watershedTree.addWatershed( new ReosWatershed() );
  watershed3->area()->setValue( ReosArea( 1, ReosArea::km2 ) );
  watershed3->concentrationTime()->setValue( ReosDuration( 0.5, ReosDuration::hour ) );
  ReosRunoffConstantCoefficientModel runoffConstantCoefficientModel_3( "test_3" );
  runoffConstantCoefficientModel_2.coefficient()->setValue( 0.3 );
  watershed3->runoffModels()->addRunoffModel( &runoffConstantCoefficientModel_1 );
  watershed3->setCurrentTransferFunction( ReosTransferFunctionNashUnitHydrograph::staticType() );

  ReosMeteorologicModelsCollection meteoCollection;
  QCOMPARE( meteoCollection.modelCount(), 1 ); //by default one model is present
  meteoCollection.meteorologicModel( 0 )->associate( watershed1, &chicagoRainfallItem );
  meteoCollection.meteorologicModel( 0 )->associate( watershed2, &chicagoRainfallItem );
  meteoCollection.meteorologicModel( 0 )->associate( watershed3, &chicagoRainfallItem );
  ReosMeteorologicModel *meteoModel = meteoCollection.meteorologicModel( 0 );
  ReosSerieRainfall *rainfall = meteoModel->associatedRainfall( watershed1 );
  QVERIFY( rainfall );
  QCOMPARE( rainfall->valueCount(), 12 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 72.34254782834526 );

  meteoCollection.addMeteorologicModel( "model_2" );
  QCOMPARE( meteoCollection.modelCount(), 2 );
  meteoCollection.meteorologicModel( 1 )->associate( watershed1, &alternateRainfallItem );
  meteoCollection.meteorologicModel( 1 )->associate( watershed2, &alternateRainfallItem );
  meteoCollection.meteorologicModel( 1 )->associate( watershed3, &alternateRainfallItem );

  ReosMeteorologicModel *meteoModel_2 = meteoCollection.meteorologicModel( 1 );
  rainfall = meteoModel_2->associatedRainfall( watershed1 );
  QVERIFY( rainfall );
  QCOMPARE( rainfall->valueCount(), 6 );
  QCOMPARE( rainfall->valueWithMode( 5, ReosTimeSerieConstantInterval::Cumulative ), 42.0128468263 );


  ReosRunoffHydrographsStore runoffStore( &meteoCollection );
  runoffStore.setWatershed( watershed1 );
  ReosHydrograph *tempHyd = runoffStore.hydrograph( meteoModel );
  ReosHydrograph *tempHyd2 = runoffStore.hydrograph( meteoModel_2 );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  ReosHydrograph expectedHydrograh_watershed1_runoff_model_1;
  ReosHydrograph expectedHydrograh_watershed1_runoff_model_2;
  expectedHydrograh_watershed1_runoff_model_1.copyFrom( tempHyd );
  expectedHydrograh_watershed1_runoff_model_2.copyFrom( tempHyd2 );
  QCOMPARE( expectedHydrograh_watershed1_runoff_model_1.valueCount(), 98 );
  QCOMPARE( expectedHydrograh_watershed1_runoff_model_2.valueCount(), 92 );

  runoffStore.setWatershed( watershed2 );
  tempHyd = runoffStore.hydrograph( meteoModel );
  tempHyd2 = runoffStore.hydrograph( meteoModel_2 );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  ReosHydrograph expectedHydrograh_watershed2_runoff_model_1;
  ReosHydrograph expectedHydrograh_watershed2_runoff_model_2;
  expectedHydrograh_watershed2_runoff_model_1.copyFrom( tempHyd );
  expectedHydrograh_watershed2_runoff_model_2.copyFrom( tempHyd2 );
  QCOMPARE( expectedHydrograh_watershed2_runoff_model_1.valueCount(), 55 );
  QCOMPARE( expectedHydrograh_watershed2_runoff_model_2.valueCount(), 49 );

  ReosHydrograph sum_model_1;
  sum_model_1.copyFrom( &expectedHydrograh_watershed1_runoff_model_1 );
  sum_model_1.addOther( &expectedHydrograh_watershed2_runoff_model_1 );
  ReosHydrograph sum_model_2;
  sum_model_2.copyFrom( &expectedHydrograh_watershed1_runoff_model_2 );
  sum_model_2.addOther( &expectedHydrograh_watershed2_runoff_model_2 );


  ReosHydrographNodeWatershed watershedNode1( watershed1, &meteoCollection );
  watershedNode1.name()->setValue( "watershed node 1" );
  ReosHydrographNodeWatershed watershedNode2( watershed2, &meteoCollection );
  watershedNode2.name()->setValue( "watershed node 2" );
  ReosHydrographNodeWatershed watershedNode3( watershed3, &meteoCollection );
  watershedNode3.name()->setValue( "watershed node 3" );

  ReosHydrographJunction junction1( QPointF( 0, 0 ) );
  junction1.name()->setValue( "junction 1" );
  ReosHydrographJunction junction2( QPointF( 10, 0 ) );
  junction2.name()->setValue( "junction 2" );

  ReosHydrographJunction junction3( QPointF( 10, 0 ) );
  junction2.name()->setValue( "junction 3" );

  ReosHydrographRoutingLink link1;
  link1.setInputHydrographSource( &watershedNode1 );
  link1.setHydrographDestination( &junction1 );
  ReosHydrographRoutingLink *link2 = new ReosHydrographRoutingLink;
  link2->setInputHydrographSource( &watershedNode2 );
  link2->setHydrographDestination( &junction1 );
  ReosHydrographRoutingLink link3;
  link3.setInputHydrographSource( &junction1 );
  link3.setHydrographDestination( &junction2 );

  QVERIFY( junction2.outputHydrograph() );
  QVERIFY( junction2.outputHydrograph()->valueCount() == 0 );

  ReosCalculationContext context;
  context.setMeteorologicModel( meteoModel );
  watershedNode1.updateCalculationContext( context );


  QVERIFY( watershedNode1.outputHydrograph()->valueCount() == 0 ); //calculation will be updated only when back to an event loop
  QVERIFY( watershedNode2.outputHydrograph()->valueCount() == 0 );
  QVERIFY( junction1.outputHydrograph()->valueCount() == 0 );
  QVERIFY( junction2.outputHydrograph()->valueCount() == 0 );
  QVERIFY( junction3.outputHydrograph()->valueCount() == 0 );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  //move the link 2 to junction 3 instead of junction 1
  link2->setHydrographDestination( &junction3 );
  link1.updateCalculationContext( context );
  link2->updateCalculationContext( context );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *junction3.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );

  //back the link 2 to junction 1
  link2->setHydrographDestination( &junction1 );
  link1.updateCalculationContext( context ); //test the update of context in a parallel but sharing same network
  junction3.updateCalculationContext( context ); //need to

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // remove the link 2
  link2->destroy();
  link1.updateCalculationContext( context );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // replace the removed link 2 by link 4
  ReosHydrographRoutingLink link4;
  link4.setInputHydrographSource( &watershedNode2 );
  link4.setHydrographDestination( &junction1 );
  link1.updateCalculationContext( context );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // Change the routing methof of link 3
  link3.setCurrentRoutingMethod( ReosHydrographRoutingMethodMuskingum::staticType() );
  link3.name()->setValue( "link 3" );
  ReosHydrographRoutingMethodMuskingum *muskingumRouting = qobject_cast<ReosHydrographRoutingMethodMuskingum *>( link3.currentRoutingMethod() );
  Q_ASSERT( muskingumRouting );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 142 ); //junction 2 hads now a hydrograph transform by Muskingum method -->more values
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // change parameter of the routing methos
  muskingumRouting->kParameter()->setValue( ReosDuration( 0.2, ReosDuration::hour ) );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 107 ); //junction 2 hads now a hydrograph transform by Muskingum method -->more values
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // Set back the link 3 with direct routing
  link3.setCurrentRoutingMethod( ReosHydrographRoutingMethodDirect::staticType() );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // Set a gauged hydrograph
  ReosHydrograph *gaugedHydrograph = new ReosHydrograph;
  gaugedHydrograph->copyFrom( mSource1.outputHydrograph() );
  gaugedHydrograph->setReferenceTime( watershedNode1.outputHydrograph()->referenceTime() );
  watershed1->gaugedHydrographs()->addHydrograph( gaugedHydrograph );

  watershedNode1.setGaugedHydrographIndex( 0 );
  watershedNode1.setInternalHydrographOrigin( ReosHydrographNodeWatershed::GaugedHydrograph );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  ReosHydrograph expectedHyd;
  expectedHyd.copyFrom( gaugedHydrograph );
  expectedHyd.addOther( &expectedHydrograh_watershed2_runoff_model_1 );

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 5 );
  QVERIFY( *watershedNode1.outputHydrograph() == *gaugedHydrograph );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *junction1.outputHydrograph() == expectedHyd );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *junction2.outputHydrograph() == expectedHyd );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  // back with a runoff hydrograph
  watershedNode1.setInternalHydrographOrigin( ReosHydrographNodeWatershed::RunoffHydrograph );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  //change the model of calculation
  context.setMeteorologicModel( meteoModel_2 );
  junction1.updateCalculationContext( context );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 92 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_2 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 49 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_2 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 92 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_2 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 92 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_2 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );

  //change the rainfall of meteo model (set same as first model)
  meteoCollection.meteorologicModel( 1 )->associate( watershed1, &chicagoRainfallItem );
  meteoCollection.meteorologicModel( 1 )->associate( watershed2, &chicagoRainfallItem );
  junction1.updateCalculationContext( context );

  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();

  QCOMPARE( watershedNode1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *watershedNode1.outputHydrograph() == expectedHydrograh_watershed1_runoff_model_1 );
  QCOMPARE( watershedNode2.outputHydrograph()->valueCount(), 55 );
  QVERIFY( *watershedNode2.outputHydrograph() == expectedHydrograh_watershed2_runoff_model_1 );
  QCOMPARE( junction1.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction1.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction2.outputHydrograph()->valueCount(), 98 );
  QVERIFY( *junction2.outputHydrograph() == sum_model_1 );
  QCOMPARE( junction3.outputHydrograph()->valueCount(), 0 );
}

QTEST_MAIN( ReosHydrographTransferTest )
#include "reos_hydrograph_transfer_test.moc"
