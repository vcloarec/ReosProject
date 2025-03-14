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
#include "reosgriddedrainitem.h"
#include "reosgriddedrainfallprovider.h"
#include "reosera5provider.h"
#include "reosnetcdfutils.h"
#include "reosgisengine.h"
#include "reoswatershed.h"
#include "reosgriddeddata.h"

class ReosEra5Test: public QObject
{
    Q_OBJECT

  private slots:
    void initTestCase();
    void createProvider();
    void createGridData();
    void griddedDataOnWatersed();
    void timeWindow();

  private:
    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;
};

void ReosEra5Test::initTestCase()
{
  ReosIdfFormulaRegistery::instantiate( &mRootModule );
  mGisEngine = new ReosGisEngine( &mRootModule );
}

void ReosEra5Test::createProvider()
{
  std::unique_ptr<ReosDataProvider> compatibleProvider( ReosDataProviderRegistery::instance()->createCompatibleProvider(
        ERA5_FILES_PATH + QStringLiteral( "/nc.nc" ), ReosGriddedRainfall::staticType() ) );
  QVERIFY( !compatibleProvider );

  const QVariantMap &uriParamDescription = ReosDataProviderRegistery::instance()->uriParameters( QStringLiteral( "era5" ), ReosEra5Provider::dataType() );

  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = QString( ERA5_FILES_PATH );
  uriParam[QStringLiteral( "var-short-name" )] = QStringLiteral( "tp" );

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "era5" ), ReosEra5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  compatibleProvider.reset( ReosDataProviderRegistery::instance()->createCompatibleProvider( uri, ReosGriddedData::staticType() ) );
  QVERIFY( compatibleProvider );

  ReosGriddedDataProvider *provider = qobject_cast<ReosGriddedDataProvider *>( compatibleProvider.get() );
  provider->setDataSource( uri );
  QVERIFY( provider->isValid() );

  QCOMPARE( provider->count(), 3624 );
  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 50 ), QDateTime( QDate( 1990, 1, 3 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 50 ), QDateTime( QDate( 1990, 1, 3 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();

  QCOMPARE( extent.xCellSize(), 0.25 );
  QCOMPARE( extent.yCellSize(), -0.25 );
  QCOMPARE( extent.xCellCount(), 63 );
  QCOMPARE( extent.yCellCount(), 41 );

  QVector<double> values = provider->data( 0 );
  QCOMPARE( values.at( 50 ), 1.112854089824677e-05 );
  QCOMPARE( values.at( 60 ), 3.561133087438655e-06 );

  values = provider->data( 50 );
  QCOMPARE( values.at( 50 ), 5.341699631158416e-06 );
  QCOMPARE( values.at( 60 ), 6.343268312000456e-06 );

  double min = 0, max = 0;
  QVERIFY( !provider->getDirectMinMax( min, max ) );
  provider->calculateMinMax( min, max );
  QCOMPARE( min, 0.0 );
  QCOMPARE( max, 0.0072928667068481445 );
}

void ReosEra5Test::createGridData()
{
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = QString( ERA5_FILES_PATH );
  uriParam[QStringLiteral( "var-short-name" )] = QStringLiteral( "tp" );

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "era5" ), ReosEra5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  std::unique_ptr<ReosGriddedData> griddedData =
    std::make_unique<ReosGriddedData>( uri, QStringLiteral( "era5" ) );

  QCOMPARE( griddedData->gridCount(), 3624 );
  QCOMPARE( griddedData->startTime( 0 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( griddedData->endTime( 0 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( griddedData->startTime( 1 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( griddedData->endTime( 1 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( griddedData->startTime( 50 ), QDateTime( QDate( 1990, 1, 3 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( griddedData->endTime( 50 ), QDateTime( QDate( 1990, 1, 3 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = griddedData->rasterExtent();

  QCOMPARE( extent.xCellSize(), 0.25 );
  QCOMPARE( extent.yCellSize(), -0.25 );
  QCOMPARE( extent.xCellCount(), 63 );
  QCOMPARE( extent.yCellCount(), 41 );

  QVector<double> values = griddedData->values( 0 );
  QCOMPARE( values.at( 50 ), 1.112854089824677e-05 );
  QCOMPARE( values.at( 60 ), 3.561133087438655e-06 );

  values = griddedData->values( 50 );
  QCOMPARE( values.at( 50 ), 5.341699631158416e-06 );
  QCOMPARE( values.at( 60 ), 6.343268312000456e-06 );

  double min = 0, max = 0;
  QVERIFY( !griddedData->getDirectMinMaxValue( min, max ) );
  griddedData->calculateMinMaxValue( min, max );
  QCOMPARE( min, 0.0 );
  QCOMPARE( max, 0.0072928667068481445 );
}

void ReosEra5Test::griddedDataOnWatersed()
{
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = QString( ERA5_FILES_PATH );
  uriParam[QStringLiteral( "var-short-name" )] = QStringLiteral( "tp" );

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "era5" ), ReosEra5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  std::unique_ptr<ReosGriddedData> griddedData =
    std::make_unique<ReosGriddedData>( uri, QStringLiteral( "era5" ) );


  QPolygonF watershed_poly;
  watershed_poly  << QPointF( 279856., 6309772. )
                  << QPointF( 346425., 6320051. )
                  << QPointF( 348884., 6252486. )
                  << QPointF( 283670., 6251741. );

  ReosWatershed watershed;
  mGisEngine->setCrs( ReosGisEngine::crsFromEPSG( 9794 ) );
  watershed.setGeographicalContext( mGisEngine );
  watershed.setDelineating( watershed_poly );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, griddedData.get() ) );

  gridOnWs->preCalculate();

  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 3624 );
  QCOMPARE( values.at( 58 ), 0.0001061439977543495 );
}

void ReosEra5Test::timeWindow()
{
  QVariantMap uriParams;
  uriParams[QStringLiteral( "file-or-dir-path" )] = ERA5_FILES_PATH;
  uriParams[QStringLiteral( "var-short-name" )] = QStringLiteral( "tp" );
  uriParams[QStringLiteral( "start-date-time" )] = QDateTime( QDate( 1990, 2, 3 ), QTime( 1, 2, 3 ), Qt::UTC );
  uriParams[QStringLiteral( "end-date-time" )] = QDateTime( QDate( 1990, 4, 6 ), QTime( 12, 5, 0 ), Qt::UTC );
  bool ok = false;
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "era5" ), ReosGriddedData::staticType(), uriParams, ok );
  QVERIFY( ok );

  std::unique_ptr<ReosGriddedData> griddedData = std::make_unique<ReosGriddedData>( uri, QStringLiteral( "era5" ) );

  QCOMPARE( 1498, griddedData->gridCount() );
  QPair<QDateTime, QDateTime> timeExtent = griddedData->timeExtent();
  QCOMPARE( timeExtent.first, QDateTime( QDate( 1990, 2, 3 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( timeExtent.second, QDateTime( QDate( 1990, 4, 6 ), QTime( 12, 0, 0 ), Qt::UTC ) );
}



QTEST_MAIN( ReosEra5Test )
#include "reos_era5_test.moc"
