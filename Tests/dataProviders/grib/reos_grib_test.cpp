/***************************************************************************
                      reos_raster_test.cpp
                     --------------------------------------
Date                 : 04-09-2020
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

#include "reos_testutils.h"
#include "reosgribprovider.h"
#include "reosgriddedrainitem.h"

class ReosGribTest: public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();

    void griddedRainInFolder();
    void griddedRainInFile();
};

void ReosGribTest::createProvider()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  std::unique_ptr<ReosGriddedRainfallProvider> provider( new ReosGribGriddedRainfallProvider );

  ReosModule::Message message;
  ReosGriddedRainfallProvider::Details details = provider->details( "lkhkjh", message );
  QVERIFY( message.type == ReosModule::Error );

  message = ReosModule::Message();
  details = provider->details( gribFile, message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( details.availableVariables.count(), 15 );

  QString variable = QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" );
  QVERIFY( details.availableVariables.contains( variable ) );

  provider->setDataSource(
    ReosGribGriddedRainfallProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeHeight ) );

  QCOMPARE( provider->count(), 3 );

  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 2 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 2 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 15, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();
  QVERIFY( extent.isValid() );
}

void ReosGribTest::griddedRainInFolder()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  QString variable( QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( ReosGribGriddedRainfallProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeHeight ),
                             ReosGribGriddedRainfallProvider::staticKey() ) );

  QVERIFY( rainfall->isValid() );

  QCOMPARE( rainfall->gridCount(), 3 );

  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 2 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 2 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 15, 0, 0 ), Qt::UTC ) );

  rainfall->intensityValues( 0 );
  rainfall->intensityValues( 1 );
  rainfall->intensityValues( 2 );
}

void ReosGribTest::griddedRainInFile()
{
  QString gribFile( testFile( QStringLiteral( "grib/W_fr-meteofrance,MODEL,AROME+0025+SP1+00H06H_C_LFPW_202211161200--.grib2" ) ) );
  QString variable( QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( ReosGribGriddedRainfallProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeHeight ),
                             ReosGribGriddedRainfallProvider::staticKey() ) );

  QVERIFY( rainfall->isValid() );

  QCOMPARE( rainfall->gridCount(), 6 );

  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 2022, 11, 16 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 2022, 11, 16 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 2022, 11, 16 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 2022, 11, 16 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 2 ), QDateTime( QDate( 2022, 11, 16 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 2 ), QDateTime( QDate( 2022, 11, 16 ), QTime( 15, 0, 0 ), Qt::UTC ) );

  rainfall->intensityValues( 0 );
  rainfall->intensityValues( 1 );
  rainfall->intensityValues( 2 );
}

QTEST_MAIN( ReosGribTest )
#include "reos_grib_test.moc"
