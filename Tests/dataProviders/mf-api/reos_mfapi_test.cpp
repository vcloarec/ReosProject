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
#include "reosgisengine.h"
#include "reosgriddedrainitem.h"
#include "reosmapextent.h"
#include "reosmeteofranceapi.h"
#include "reosmeteofrancearomeprovider.h"

class ReosMFApitest: public QObject
{
    Q_OBJECT

  private slots:
    void meteoFranceApi();
    void aromeProvider();
    void aromeGriddedRain();
};

void ReosMFApitest::meteoFranceApi()
{
  ReosMeteoFranceApi api( METEOFRANCE_API_KEY_FILE );

  ReosMeteoFranceApiArome *aromeService = api.aromeService();
  QVERIFY( aromeService );

  ReosMeteoFranceApiArome::Model model( {QStringLiteral( "FRANCE" ), QStringLiteral( "001" )} );
  QString error;
  aromeService->connectToServiceBlocking( model, error );

  QList<QDateTime> runs = aromeService->availableRuns();
  QVERIFY( !runs.isEmpty() );

  ReosMeteoFranceApiArome::RunInfo runInfo = aromeService->runInfoBlocking( runs.at( runs.count() - 2 ) );
  QString crs = ReosGisEngine::crsFromEPSG( 4326 );
  QVERIFY( runInfo.extent == ReosMapExtent( ReosSpatialPosition( {-12.0, 37.5}, crs ), ReosSpatialPosition( {16.0, 55.4}, crs ) ) );
  QCOMPARE( runInfo.frameCount, 51 );

  ReosMapExtent extent( ReosSpatialPosition( {8.0, 45.5}, crs ), ReosSpatialPosition( {9.0, 46}, crs ) );
  aromeService->requestFrameBlocking( extent, runs.last(), 0 );
}

void ReosMFApitest::aromeProvider()
{
  const QString keyFileName = QStringLiteral( "/home/vincent/dev/Lekan_local/meteofrance-api-key" );
  const QString crs = ReosGisEngine::crsFromEPSG( 4326 );
  const ReosMapExtent extent( ReosSpatialPosition( {8.0, 48}, crs ), ReosSpatialPosition( {10.0, 50}, crs ) );
  const QString uri = ReosMeteoFranceAromeApiProvider::uri( keyFileName, QStringLiteral( "FRANCE" ), QStringLiteral( "001" ), extent, 1 );

  std::unique_ptr<ReosMeteoFranceAromeApiProvider> provider( new ReosMeteoFranceAromeApiProvider() );

  QEventLoop loop;
  connect( provider.get(), &ReosDataProvider::loadingFinished, &loop, &QEventLoop::quit );
  provider->setDataSource( uri );
  loop.exec();

  QCOMPARE( provider->count(), 51 );
  ReosRasterExtent providerExtent = provider->extent();
  QCOMPARE( providerExtent.xCellCount(), 201 );
  QCOMPARE( providerExtent.yCellCount(), 201 );
  for ( int i = 0; i < provider->count(); ++i )
    QCOMPARE( provider->data( i ).count(), 40401 );
}

void ReosMFApitest::aromeGriddedRain()
{
  QString keyFileName = METEOFRANCE_API_KEY_FILE;
  QString crs = ReosGisEngine::crsFromEPSG( 4326 );
  ReosMapExtent extent( ReosSpatialPosition( {8.0, 48}, crs ), ReosSpatialPosition( {10.0, 50}, crs ) );
  const QString uri = ReosMeteoFranceAromeApiProvider::uri( keyFileName, QStringLiteral( "FRANCE" ), QStringLiteral( "001" ), extent, 1 );

  QEventLoop loop;
  std::unique_ptr<ReosGriddedRainfall> rainfall( new ReosGriddedRainfall( uri, ReosMeteoFranceAromeApiProvider::staticKey() ) );
  connect( rainfall.get(), &ReosGriddedRainfall::loadingFinished, &loop, &QEventLoop::quit );
  QVERIFY( rainfall->dataProvider() );
  loop.exec();

  QCOMPARE( rainfall->gridCount(), 51 );
  ReosRasterExtent rainfallExtent = rainfall->rasterExtent();
  QCOMPARE( rainfallExtent.xCellCount(), 201 );
  QCOMPARE( rainfallExtent.yCellCount(), 201 );
  for ( int i = 0; i < rainfall->gridCount(); ++i )
    QCOMPARE( rainfall->intensityValues( i ).count(), 40401 );
}

QTEST_MAIN( ReosMFApitest )
#include "reos_mfapi_test.moc"
