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
#include "reoshydrographtransfer.h"
#include "reoshydrographsource.h"
#include "reoshydrograph.h"
#include "reosmuskingumclassicrouting.h"


class ReosHydrographTransferTest: public QObject
{
    Q_OBJECT
  private slots:
    void initTestCase();

    void test_junction();
    void test_classicMuskingumRouting();

  private:
    ReosHydrographSourceFixed mSource1;
    ReosHydrographSourceFixed mSource2;
    ReosHydrographSourceFixed mSource3;
};

void ReosHydrographTransferTest::initTestCase()
{
  std::unique_ptr<ReosHydrograph> hydrograph1 = std::make_unique<ReosHydrograph>();
  hydrograph1->referenceTime()->setValue( QDateTime( QDate( 2020, 01, 01 ), QTime( 10, 0, 0 ) ) );
  hydrograph1->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph1->setValue( ReosDuration( 30, ReosDuration::minute ), 2 );
  hydrograph1->setValue( ReosDuration( 40, ReosDuration::minute ), 5 );
  hydrograph1->setValue( ReosDuration( 55, ReosDuration::minute ), 3 );
  hydrograph1->setValue( ReosDuration( 1.5, ReosDuration::hour ), 0 );
  mSource1.setHydrograph( hydrograph1.release() );

  std::unique_ptr<ReosHydrograph> hydrograph2 = std::make_unique<ReosHydrograph>();
  hydrograph2->referenceTime()->setValue( QDateTime( QDate( 2020, 01, 01 ), QTime( 9, 30, 0 ) ) );
  hydrograph2->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph2->setValue( ReosDuration( 30, ReosDuration::minute ), 1 );
  hydrograph2->setValue( ReosDuration( 45, ReosDuration::minute ), 3 );
  hydrograph2->setValue( ReosDuration( 50, ReosDuration::minute ), 4 );
  hydrograph2->setValue( ReosDuration( 2, ReosDuration::hour ), 0 );
  mSource2.setHydrograph( hydrograph2.release() );

  std::unique_ptr<ReosHydrograph> hydrograph3 = std::make_unique<ReosHydrograph>();
  hydrograph3->referenceTime()->setValue( QDateTime( QDate( 2020, 01, 01 ), QTime( 10, 15, 0 ) ) );
  hydrograph3->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph3->setValue( ReosDuration( 10, ReosDuration::minute ), 4 );
  hydrograph3->setValue( ReosDuration( 15, ReosDuration::minute ), 3 );
  hydrograph3->setValue( ReosDuration( 25, ReosDuration::minute ), 1 );
  hydrograph3->setValue( ReosDuration( 45, ReosDuration::hour ), 0 );
  mSource3.setHydrograph( hydrograph3.release() );
}

void ReosHydrographTransferTest::test_junction()
{
  ReosCalculationContext context;
  ReosHydrographJunction junction{ QPointF()};

  ReosHydrograph *inputHydrograph_1 = mSource1.outputHydrograph( );
  ReosHydrograph *inputHydrograph_2 = mSource2.outputHydrograph( );
  ReosHydrograph *inputHydrograph_3 = mSource3.outputHydrograph( );

  ReosHydrographRouting transfer1;
  transfer1.setInputHydrographSource( &mSource1 );
  transfer1.setHydrographDestination( &junction );

  junction.updateCalculation( context );
  ReosHydrograph *junctionHydrograph = junction.outputHydrograph();

  QVERIFY( junctionHydrograph );
  QCOMPARE( junctionHydrograph->valueCount(), inputHydrograph_1->valueCount() );

  for ( int i = 0; i < inputHydrograph_1->valueCount(); ++i )
  {
    QCOMPARE( junctionHydrograph->valueAt( i ), inputHydrograph_1->valueAt( i ) );
    QVERIFY( junctionHydrograph->timeAt( i ) == inputHydrograph_1->timeAt( i ) );
  }

  ReosHydrographRouting transfer2;
  transfer2.setInputHydrographSource( &mSource2 );
  transfer2.setHydrographDestination( &junction );

  junction.updateCalculation( context );

  junctionHydrograph = junction.outputHydrograph( );

  QCOMPARE( junctionHydrograph->valueCount(), 8 );

  for ( int i = 0; i < inputHydrograph_1->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_1->timeAt( i );
    QCOMPARE( junctionHydrograph->valueAtTime( time ), inputHydrograph_1->valueAtTime( time ) + inputHydrograph_2->valueAtTime( time ) );
  }

  for ( int i = 0; i < inputHydrograph_2->valueCount(); ++i )
  {
    QDateTime time = inputHydrograph_2->timeAt( i );
    QCOMPARE( junctionHydrograph->valueAtTime( time ), inputHydrograph_1->valueAtTime( time ) + inputHydrograph_2->valueAtTime( time ) );
  }

  ReosHydrographJunction junction2{ QPointF()};

  ReosHydrographRouting transfer3;
  transfer3.setInputHydrographSource( &mSource3 );
  transfer3.setHydrographDestination( &junction );

  ReosHydrographRouting transfer4;
  transfer4.setInputHydrographSource( &junction );
  transfer4.setHydrographDestination( &junction2 );

  junction2.updateCalculation( context );

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

  ReosHydrographRouting routing;
  QVERIFY( routing.setCurrentRoutingMethod( ReosMuskingumClassicRouting::typeString() ) );
  QVERIFY( routing.currentRoutingMethod() );
  QVERIFY( routing.currentRoutingMethod()->type() == ReosMuskingumClassicRouting::typeString() );

  routing.setInputHydrographSource( &mSource1 );
  routing.updateCalculation( context );
  QVERIFY( routing.outputHydrograph() );
}

QTEST_MAIN( ReosHydrographTransferTest )
#include "reos_hydrograph_transfer_test.moc"
