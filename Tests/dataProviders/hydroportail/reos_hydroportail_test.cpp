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
#include "reoshydroportailprovider.h"

class ReosHydroportailTest: public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();
    void createHydrograph();

};

void ReosHydroportailTest::createProvider()
{
  QString file( testFile( QStringLiteral( "hydroportail/J261401002_Q.csv" ) ) );
  std::unique_ptr<ReosTimeSerieVariableTimeStepProvider> provider(
    qobject_cast<ReosTimeSerieVariableTimeStepProvider *>( ReosDataProviderRegistery::instance()->createCompatibleProvider( file, ReosHydrograph::staticType() ) ) );

  QVERIFY( provider );

  QCOMPARE( provider->key(), ReosHydroportailProvider::staticKey() );
  QCOMPARE( provider->fileSuffixes(), QStringList() << "csv" );

  QVariantMap parameters = ReosDataProviderRegistery::instance()->uriParameters( QStringLiteral( "hydroportail" ), ReosHydrograph::staticType() );
  QVERIFY( parameters.contains( QStringLiteral( "file-path" ) ) );
  parameters.insert( QStringLiteral( "file-path" ), file );
  bool ok = false;
  provider->setDataSource( ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "hydroportail" ), ReosHydrograph::staticType(), parameters, ok ) );
  QVERIFY( ok );

  QCOMPARE( provider->valueCount(), 3689 );
}

void ReosHydroportailTest::createHydrograph()
{
  ReosHydrograph hydrograph( nullptr, "hydroportail", testFile( QStringLiteral( "hydroportail/J261401002_Q.csv" ) ) );

  QCOMPARE( hydrograph.valueCount(), 3689 );
  QCOMPARE( hydrograph.referenceTime(), QDateTime( QDate( 2020, 1, 1 ), QTime( 0, 1, 0 ), Qt::UTC ) );
  QCOMPARE( hydrograph.valueAt( 0 ), 3.94 );
  QCOMPARE( hydrograph.valueAt( 1 ), 3.83 );
  QCOMPARE( hydrograph.valueAt( 3688 ), 8.4 );
  QVERIFY( hydrograph.relativeTimeAt( 0 ) == ReosDuration( qint64( 0 ) ) );
  QVERIFY( hydrograph.relativeTimeAt( 1 ) == ReosDuration( 19, ReosDuration::hour ) + ReosDuration( 1, ReosDuration::minute ) );
  QVERIFY( hydrograph.relativeTimeAt( 3688 ) == ReosDuration( 365, ReosDuration::day ) + ReosDuration( 23, ReosDuration::hour )  + ReosDuration( 53, ReosDuration::minute ) );

  ReosEncodeContext context;
  context.setEncodeRelativePath( false );
  ReosEncodedElement encodedHyd = hydrograph.encode( context );

  std::unique_ptr<ReosHydrograph> decodedHyd( ReosHydrograph::decode( encodedHyd, context ) );

  QCOMPARE( decodedHyd->valueCount(), 3689 );
  QCOMPARE( decodedHyd->referenceTime(), QDateTime( QDate( 2020, 1, 1 ), QTime( 0, 1, 0 ), Qt::UTC ) );
  QCOMPARE( decodedHyd->valueAt( 0 ), 3.94 );
  QCOMPARE( decodedHyd->valueAt( 1 ), 3.83 );
  QCOMPARE( decodedHyd->valueAt( 3688 ), 8.4 );
  QVERIFY( decodedHyd->relativeTimeAt( 0 ) == ReosDuration( qint64( 0 ) ) );
  QVERIFY( decodedHyd->relativeTimeAt( 1 ) == ReosDuration( 19, ReosDuration::hour ) + ReosDuration( 1, ReosDuration::minute ) );
  QVERIFY( decodedHyd->relativeTimeAt( 3688 ) == ReosDuration( 365, ReosDuration::day ) + ReosDuration( 23, ReosDuration::hour )  + ReosDuration( 53, ReosDuration::minute ) );


  ReosHydrograph hydrograph_ls( nullptr, "hydroportail", testFile( QStringLiteral( "hydroportail/J261401002_Q_ls.csv" ) ) );

  QCOMPARE( hydrograph_ls.valueCount(), 3689 );
  QCOMPARE( hydrograph_ls.referenceTime(), QDateTime( QDate( 2020, 1, 1 ), QTime( 0, 1, 0 ), Qt::UTC ) );
  QCOMPARE( hydrograph_ls.valueAt( 0 ), 3.94 );
  QCOMPARE( hydrograph_ls.valueAt( 1 ), 3.83 );
  QCOMPARE( hydrograph_ls.valueAt( 3688 ), 8.4 );
  QVERIFY( hydrograph_ls.relativeTimeAt( 0 ) == ReosDuration( qint64( 0 ) ) );
  QVERIFY( hydrograph_ls.relativeTimeAt( 1 ) == ReosDuration( 19, ReosDuration::hour ) + ReosDuration( 1, ReosDuration::minute ) );
  QVERIFY( hydrograph_ls.relativeTimeAt( 3688 ) == ReosDuration( 365, ReosDuration::day ) + ReosDuration( 23, ReosDuration::hour )  + ReosDuration( 53, ReosDuration::minute ) );

  ReosHydrograph hydrograph_mm3( nullptr, "hydroportail", testFile( QStringLiteral( "hydroportail/J261401002_Q_mm3.csv" ) ) );

  QCOMPARE( hydrograph_ls.valueCount(), 3689 );
  QCOMPARE( hydrograph_ls.referenceTime(), QDateTime( QDate( 2020, 1, 1 ), QTime( 0, 1, 0 ), Qt::UTC ) );
  QCOMPARE( hydrograph_ls.valueAt( 0 ), 3.94 );
  QCOMPARE( hydrograph_ls.valueAt( 1 ), 3.83 );
  QCOMPARE( hydrograph_ls.valueAt( 3688 ), 8.4 );
  QVERIFY( hydrograph_ls.relativeTimeAt( 0 ) == ReosDuration( qint64( 0 ) ) );
  QVERIFY( hydrograph_ls.relativeTimeAt( 1 ) == ReosDuration( 19, ReosDuration::hour ) + ReosDuration( 1, ReosDuration::minute ) );
  QVERIFY( hydrograph_ls.relativeTimeAt( 3688 ) == ReosDuration( 365, ReosDuration::day ) + ReosDuration( 23, ReosDuration::hour )  + ReosDuration( 53, ReosDuration::minute ) );
}

QTEST_MAIN( ReosHydroportailTest )
#include "reos_hydroportail_test.moc"
