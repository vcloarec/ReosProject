/***************************************************************************
  reos_hydroportail_test.cpp - %{Cpp:License:ClassName}

 ---------------------
 begin                : 10.6.2023
 copyright            : (C) 2023 by Vincent Cloarec
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
#include "reosdataprovider.h"
#include "reostimeseriesprovider.h"
#include "reoshydrograph.h"
#include "reosvortexioprovider.h"

class ReosVortexIoTest: public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();
    void createHydrograph();
    void missingData();
    void rhin();
    void badRequest();

};

void ReosVortexIoTest::createProvider()
{
  QString key = QStringLiteral( "vortexio" );
  QStringList available_provider = ReosDataProviderRegistery::instance()->providers( ReosHydrograph::staticType() );

  QVERIFY( available_provider.contains( key ) );

  QString uri = QStringLiteral( "maeslstrom_key::fr-V720001002::2023-12-01::2023-12-31" );

  ReosVortexIoProvider provider;
  provider.setDataSource( uri );
  provider.load();
  QVERIFY( provider.isValid() );
}

void ReosVortexIoTest::createHydrograph()
{
  ReosHydrograph invalid_1( nullptr, "vortexio", QStringLiteral( "maeslstrom_key::2018-01-01::2023-12-31" ) );
  QVERIFY( !invalid_1.isValid() );

  ReosHydrograph hydrograph( nullptr, "vortexio", QStringLiteral( "maeslstrom_key::fr-V720001002::2018-01-01::2023-12-31" ) );
  QVERIFY( hydrograph.isValid() );
  QVERIFY( hydrograph.metadata().contains( QStringLiteral( "request_status" ) ) );
  QCOMPARE( hydrograph.metadata().value( QStringLiteral( "request_status" ) ), 200 );

  int valueCount = hydrograph.valueCount();

  for ( int i = 0; i < valueCount - 1; ++i )
    QVERIFY( hydrograph.timeAt( i ) < hydrograph.timeAt( i + 1 ) );

  QCOMPARE( valueCount, 122565 );
  QCOMPARE( hydrograph.referenceTime(), QDateTime( QDate( 2018, 1, 1 ), QTime( 0, 5, 0 ), Qt::UTC ) );
  QCOMPARE( hydrograph.valueAt( 0 ), 2960.0 );
  QCOMPARE( hydrograph.valueAt( 1 ), 2970.0 );
  QCOMPARE( hydrograph.valueAt( 120000 ), 1580.0 );

  QVERIFY( hydrograph.relativeTimeAt( 0 ) == ReosDuration( qint64( 0 ) ) );
  QVERIFY( hydrograph.relativeTimeAt( 1 ) == ReosDuration( 600, ReosDuration::second ) );
  QVERIFY( hydrograph.relativeTimeAt( 12000 ) == ReosDuration( 13152000, ReosDuration::second ) );

  ReosEncodeContext context;
  context.setEncodeRelativePath( false );
  ReosEncodedElement encodedHyd = hydrograph.encode( context );

  std::unique_ptr<ReosHydrograph> decodedHyd( ReosHydrograph::decode( encodedHyd, context ) );

  for ( int i = 0; i < valueCount - 1; ++i )
    QVERIFY( decodedHyd->timeAt( i ) < hydrograph.timeAt( i + 1 ) );

  QCOMPARE( valueCount, 122565 );
  QCOMPARE( decodedHyd->referenceTime(), QDateTime( QDate( 2018, 1, 1 ), QTime( 0, 5, 0 ), Qt::UTC ) );
  QCOMPARE( decodedHyd->valueAt( 0 ), 2960.0 );
  QCOMPARE( decodedHyd->valueAt( 1 ), 2970.0 );
  QCOMPARE( decodedHyd->valueAt( 120000 ), 1580.0 );

  QVERIFY( decodedHyd->relativeTimeAt( 0 ) == ReosDuration( qint64( 0 ) ) );
  QVERIFY( decodedHyd->relativeTimeAt( 1 ) == ReosDuration( 600, ReosDuration::second ) );
  QVERIFY( decodedHyd->relativeTimeAt( 12000 ) == ReosDuration( 13152000, ReosDuration::second ) );

}

void ReosVortexIoTest::missingData()
{
  ReosHydrograph hydrograph( nullptr, "vortexio", QStringLiteral( "maeslstrom_key::fr-J261401002::2006-05-13::2006-08-12" ) );
  QVERIFY( hydrograph.metadata().contains( QStringLiteral( "request_status" ) ) );
  QCOMPARE( hydrograph.metadata().value( QStringLiteral( "request_status" ) ), 200 );

  QVector<double> values = hydrograph.constData();
  int valueCount = hydrograph.valueCount();

  for ( int i = 0; i < valueCount - 1; ++i )
    QVERIFY( hydrograph.timeAt( i ) < hydrograph.timeAt( i + 1 ) );

  QCOMPARE( valueCount, 22 );

  ReosFloat64GridBlock constStep = hydrograph.toConstantTimeStep( ReosDuration( 5.0, ReosDuration::hour ) );
  QCOMPARE( constStep.values().size(), 438 );
  QCOMPARE( constStep.values().at( 24 ), 0.8128527004909984 );
  QVERIFY( std::isnan( constStep.values().at( 25 ) ) );
  QVERIFY( std::isnan( constStep.values().at( 418 ) ) );
  QCOMPARE( constStep.values().at( 419 ), 0.2998231132075472 );

}


void ReosVortexIoTest::rhin()
{
  ReosHydrograph hydrograph( nullptr, "vortexio", QStringLiteral( "maeslstrom_key::fr-A021005050::1990-01-01::2000-01-01" ) );

  QVERIFY( hydrograph.metadata().contains( QStringLiteral( "request_status" ) ) );
  QCOMPARE( hydrograph.metadata().value( QStringLiteral( "request_status" ) ), 200 );

  QVector<double> values = hydrograph.constData();
  int valueCount = hydrograph.valueCount();

  for ( int i = 0; i < valueCount - 1; ++i )
    QVERIFY( hydrograph.timeAt( i ) < hydrograph.timeAt( i + 1 ) );

  QCOMPARE( valueCount, 0 );
}

void ReosVortexIoTest::badRequest()
{
  QString uri =  QStringLiteral( "maeslstrom_key::fr-A236003301::2022-06-17::2024-06-16" );
  ReosHydrograph hydrograph( nullptr, "vortexio", uri );
  QVariantMap metadata = hydrograph.metadata();

  QVERIFY( metadata.contains( QStringLiteral( "request_status" ) ) );
  QCOMPARE( metadata.value( QStringLiteral( "request_status" ) ), 500 );
}

QTEST_MAIN( ReosVortexIoTest )
#include "reos_vortexio_test.moc"
