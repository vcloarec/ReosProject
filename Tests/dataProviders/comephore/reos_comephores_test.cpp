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
#include "reosseriesrainfall.h"
#include "reosnetcdfutils.h"
#include "reosgisengine.h"
#include "reoswatershed.h"

class ReosComephoreTest: public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();
    void createRainfallFromTif();
    void netcdfFile();
    void dupplicateFrames();
    void createRainfallFromNetCdf();
    void netCdfFolder();
    void timeWindow();
    void missingIndex();
    void griddedDataOnWatersed();

  private:
    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;
};

void ReosComephoreTest::createProvider()
{
  std::unique_ptr<ReosDataProvider> compatibleProvider( ReosDataProviderRegistery::instance()->createCompatibleProvider(
        COMEPHORE_FILES_PATH + QStringLiteral( "/tif.tif" ), ReosGriddedRainfall::staticType() ) );
  QVERIFY( !compatibleProvider );

  compatibleProvider.reset( ReosDataProviderRegistery::instance()->createCompatibleProvider(
                              COMEPHORE_FILES_PATH + QStringLiteral( "/tif" ), ReosGriddedRainfall::staticType() ) );
  QVERIFY( compatibleProvider );

  ReosGriddedRainfallProvider *provider = qobject_cast<ReosGriddedRainfallProvider *>( compatibleProvider.get() );

  QString comephoresPath( COMEPHORE_FILES_PATH + QStringLiteral( "/tif.tif" ) );
  provider->setDataSource( comephoresPath );
  QVERIFY( !provider->isValid() );

  comephoresPath = QString( COMEPHORE_FILES_PATH + QStringLiteral( "/tif" ) );
  provider->setDataSource( comephoresPath );
  QVERIFY( provider->isValid() );

  QCOMPARE( provider->count(), 73 );
  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 50 ), QDateTime( QDate( 2018, 02, 20 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 50 ), QDateTime( QDate( 2018, 02, 20 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();

  QCOMPARE( extent.xCellSize(), 1000.0 );
  QCOMPARE( extent.yCellSize(), -1000.0 );
  QCOMPARE( extent.xCellCount(), 101 );
  QCOMPARE( extent.yCellCount(), 170 );

  QVector<double> values = provider->data( 70 );
  QCOMPARE( values.at( 8581 ), 2.2 );
  QCOMPARE( values.at( 8584 ), 3.0 );
  QVERIFY( std::isnan( values.last() ) );

  double min = 0, max = 0;
  QVERIFY( !provider->getDirectMinMax( min, max ) );
  provider->calculateMinMax( min, max );
  QCOMPARE( min, 0.1 );
  QCOMPARE( max, 12.4 );

}

void ReosComephoreTest::createRainfallFromTif()
{
  std::unique_ptr<ReosGriddedRainfall> rainfall =
    std::make_unique<ReosGriddedRainfall>( testFile( QStringLiteral( "comephore/tif_files/a_day_january_1997/" ) ), QStringLiteral( "comephore" ) );

  QCOMPARE( rainfall->gridCount(), 25 );
  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 1997, 01, 31 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 1997, 01, 31 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 1997, 01, 31 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 1997, 01, 31 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 20 ), QDateTime( QDate( 1997, 01, 31 ), QTime( 20, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 20 ), QDateTime( QDate( 1997, 01, 31 ), QTime( 21, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = rainfall->rasterExtent();

  QCOMPARE( extent.xCellSize(), 1000.0 );
  QCOMPARE( extent.yCellSize(), -1000.0 );
  QCOMPARE( extent.xCellCount(), 1536 );
  QCOMPARE( extent.yCellCount(), 1536 );

  QVector<double> values = rainfall->intensityValues( 20 );
  QCOMPARE( values.at( 8581 ), std::numeric_limits<double>::quiet_NaN() );
  QCOMPARE( values.at( 8584 ), std::numeric_limits<double>::quiet_NaN() );
  QVERIFY( std::isnan( values.last() ) );

  double min, max;
  QVERIFY( !rainfall->getDirectMinMaxValue( min, max ) );
  rainfall->calculateMinMaxValue( min, max );
  QCOMPARE( min, 0.2 );
  QCOMPARE( max, 1.9 );
}

void ReosComephoreTest::netcdfFile()
{
  ReosNetCdfFile file( COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc/comephore_1km-1h_202001.nc" ), false );
  QVERIFY( file.isValid() );

  QVERIFY( file.hasVariable( QStringLiteral( "RR" ) ) );
  QCOMPARE( file.variableDimensionCount( QStringLiteral( "RR" ) ), 3 );

  QStringList dimensionNames = file.variableDimensionNames( QStringLiteral( "RR" ) );
  QVERIFY( dimensionNames.contains( QStringLiteral( "time" ) ) );
  QVERIFY( dimensionNames.contains( QStringLiteral( "X" ) ) );
  QVERIFY( dimensionNames.contains( QStringLiteral( "Y" ) ) );

  QCOMPARE( file.dimensionLength( QStringLiteral( "time" ) ), 744 );
  QCOMPARE( file.dimensionLength( QStringLiteral( "X" ) ), 1536 );
  QCOMPARE( file.dimensionLength( QStringLiteral( "Y" ) ), 1536 );

  QString proj4Crs = file.globalStringAttributeValue( QStringLiteral( "crs_proj4_string" ) );
  QString crs = ReosGisEngine::crsFromProj( proj4Crs );
  QVERIFY( ReosGisEngine::crsIsValid( crs ) );

  double nw_latitude = file.globalDoubleAttributeValue( QStringLiteral( "nw_corner_latitude" ) );
  QCOMPARE( nw_latitude, 53.67 );
  double nw_longitude = file.globalDoubleAttributeValue( QStringLiteral( "nw_corner_longitude" ) );
  QCOMPARE( nw_longitude, -9.965 );
  QString crsWGS84 = ReosGisEngine::crsFromEPSG( 4326 );
  ReosSpatialPosition nw_position( nw_longitude, nw_latitude, crsWGS84 );

  std::unique_ptr<ReosDataProvider> compatibleProvider( ReosDataProviderRegistery::instance()->createCompatibleProvider(
        COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc/comephore_1km-1h_202001.nc" ), ReosGriddedRainfall::staticType() ) );
  QVERIFY( compatibleProvider );

  QCOMPARE( compatibleProvider->key(), QStringLiteral( "comephore::gridded-precipitation" ) );

  ReosGriddedRainfallProvider *provider = qobject_cast<ReosGriddedRainfallProvider *>( compatibleProvider.get() );
  provider->setDataSource( COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc/comephore_XXXX.nc" ) );
  QVERIFY( !provider->isValid() );

  provider->setDataSource( COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc/comephore_1km-1h_202001.nc" ) );
  QVERIFY( provider->isValid() );

  QCOMPARE( provider->count(), 744 );
  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 50 ), QDateTime( QDate( 2020, 01, 03 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 50 ), QDateTime( QDate( 2020, 01, 03 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  QVector<double> vals = provider->data( 0 );
  QCOMPARE( vals.count(), 1536 * 1536 );
  QCOMPARE( vals.at( 576802 ), 0.2 );
  QCOMPARE( vals.at( 994187 ), 0.2 );

  vals = provider->data( 200 );
  QCOMPARE( vals.count(), 1536 * 1536 );
  QCOMPARE( vals.at( 695080 ), 1.3 );
  QCOMPARE( vals.at( 725983 ), 1.5 );

  int rowMin = 350;
  int rowMax = 360;
  int colMin = 760;
  int colMax = 770;

  QVector<double> vals_ex = provider->dataInGridExtent( 200, rowMin, rowMax, colMin, colMax );
  QCOMPARE( vals_ex.count(), ( colMax - colMin + 1 ) * ( rowMax - rowMin + 1 ) );
  for ( int row = 0; row < rowMax - rowMin + 1 ; row++ )
    for ( int col = 0; col < colMax - colMin; col++ )
    {
      int oriIndex = ( col + colMin ) + ( row  + rowMin )  * 1536;
      int index = col + row * ( colMax - colMin + 1 );
      QCOMPARE( vals.at( oriIndex ), vals_ex.at( index ) );
    }
}

void ReosComephoreTest::dupplicateFrames()
{
  std::unique_ptr<ReosGriddedRainfall> rainfall =
    std::make_unique<ReosGriddedRainfall>( COMEPHORE_FILES_PATH +
        QStringLiteral( "/comephore_nc/comephore_1km-1h_202008.nc" ),
        QStringLiteral( "comephore" ) );

  QCOMPARE( rainfall->gridCount(), 744 );
  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 2020, 8, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 2020, 8, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 2020, 8, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 2020, 8, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 50 ), QDateTime( QDate( 2020, 8, 03 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 50 ), QDateTime( QDate( 2020, 8, 03 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 743 ), QDateTime( QDate( 2020, 8, 31 ), QTime( 23, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 743 ), QDateTime( QDate( 2020, 9, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );

  QVERIFY( rainfall->minimumTimeStep().valueMilliSecond() == 3600000 );
}

void ReosComephoreTest::createRainfallFromNetCdf()
{
  std::unique_ptr<ReosGriddedRainfall> rainfall =
    std::make_unique<ReosGriddedRainfall>( COMEPHORE_FILES_PATH +
        QStringLiteral( "/comephore_nc/comephore_1km-1h_202001.nc" ),
        QStringLiteral( "comephore" ) );

  QCOMPARE( rainfall->gridCount(), 744 );
  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 50 ), QDateTime( QDate( 2020, 01, 03 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 50 ), QDateTime( QDate( 2020, 01, 03 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = rainfall->rasterExtent();

  QCOMPARE( extent.xCellSize(), 1000.0 );
  QCOMPARE( extent.yCellSize(), -1000.0 );
  QCOMPARE( extent.xCellCount(), 1536 );
  QCOMPARE( extent.yCellCount(), 1536 );

  QVector<double> values = rainfall->intensityValues( 200 );
  QCOMPARE( values.at( 695080 ), 1.3 );
  QCOMPARE( values.at( 725983 ), 1.5 );
  QVERIFY( std::isnan( values.last() ) );

  double min, max;
  QVERIFY( !rainfall->getDirectMinMaxValue( min, max ) );

  // calculation too long to keep in test...
  /*rainfall->calculateMinMaxValue( min, max );
  QCOMPARE( min, 0.1 );
  QCOMPARE( max, 89.90 );*/
}

void ReosComephoreTest::netCdfFolder()
{
  std::unique_ptr<ReosDataProvider> compatibleProvider( ReosDataProviderRegistery::instance()->createCompatibleProvider(
        COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc" ), ReosGriddedRainfall::staticType() ) );
  QVERIFY( compatibleProvider );

  ReosGriddedRainfallProvider *provider = qobject_cast<ReosGriddedRainfallProvider *>( compatibleProvider.get() );

  QString comephoresPath( COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc" ) );
  provider->setDataSource( comephoresPath );

  QVERIFY( provider->isValid() );

  QCOMPARE( provider->count(), 8784 );
  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 2020, 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 2020, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 2020, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 2020, 1, 1 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 8000 ), QDateTime( QDate( 2020, 11, 29 ), QTime( 8, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 8000 ), QDateTime( QDate( 2020, 11, 29 ), QTime( 9, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();

  QCOMPARE( extent.xCellSize(), 1000.0 );
  QCOMPARE( extent.yCellSize(), -1000.0 );
  QCOMPARE( extent.xCellCount(), 1536 );
  QCOMPARE( extent.yCellCount(), 1536 );

  QVector<double> values = provider->data( 700 );
  QCOMPARE( values.count(), 1536 * 1536 );
  QCOMPARE( values.at( 866655 ), 0.4 );
  QCOMPARE( values.at( 894433 ), 0.3 );
  QVERIFY( std::isnan( values.last() ) );

  for ( int i = 0; i < provider->count(); ++i )
    QVERIFY( provider->startTime( i ).isValid() );

  QVERIFY( provider->hasCapability( ReosGriddedRainfallProvider::SubGridExtract ) );
  QVERIFY( provider->hasCapability( ReosGriddedRainfallProvider::QualificationValue ) );

  values = provider->dataInGridExtent( 700, 560, 570, 340, 360 );
  QCOMPARE( values.count(), 231 );
  QVERIFY( equal( values.at( 12 ), 0.3, 0.01 ) );

  values = provider->qualifData( 700 );
  QCOMPARE( values.count(), 1536 * 1536 );
  QCOMPARE( values.at( 866655 ), 91 );
}

void ReosComephoreTest::timeWindow()
{
  QVariantMap uriParams;
  uriParams[QStringLiteral( "file-or-dir-path" )] = COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc" );
  uriParams[QStringLiteral( "start-date-time" )] = QDateTime( QDate( 2020, 2, 3 ), QTime( 1, 2, 3 ), Qt::UTC );
  uriParams[QStringLiteral( "end-date-time" )] = QDateTime( QDate( 2020, 5, 6 ), QTime( 12, 5, 0 ), Qt::UTC );
  bool ok = false;
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "comephore" ), ReosGriddedRainfall::staticType(), uriParams, ok );

  QVERIFY( ok );

  std::unique_ptr<ReosGriddedRainfall> rainfall = std::make_unique<ReosGriddedRainfall>( uri, QStringLiteral( "comephore" ) );

  QCOMPARE( 2242, rainfall->gridCount() );
  QPair<QDateTime, QDateTime> timeExtent = rainfall->timeExtent();
  QCOMPARE( timeExtent.first, QDateTime( QDate( 2020, 2, 3 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( timeExtent.second, QDateTime( QDate( 2020, 5, 6 ), QTime( 12, 0, 0 ), Qt::UTC ) );
}

void ReosComephoreTest::missingIndex()
{
  QVariantMap uriParams;
  uriParams[QStringLiteral( "file-or-dir-path" )] = COMEPHORE_FILES_PATH + QStringLiteral( "/negative_values" );

  bool ok = false;
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "comephore" ), ReosGriddedRainfall::staticType(), uriParams, ok );

  QVERIFY( ok );

  std::unique_ptr<ReosGriddedRainfall> rainfall = std::make_unique<ReosGriddedRainfall>( uri, QStringLiteral( "comephore" ) );

  QCOMPARE( 1487, rainfall->gridCount() );


  int missingIndex = rainfall->dataIndex( QDateTime( QDate( 2019, 12, 31 ), QTime( 23, 30, 0 ), Qt::UTC ) );
  QCOMPARE( -1, missingIndex );

  ReosRasterExtent extent = rainfall->rasterExtent();

  ReosRasterMemory<double> rainValues = ReosRasterMemory<double>( extent.yCellCount(), extent.xCellCount() );
  rainValues.setValues( rainfall->values( -1 ) );

  QPolygonF delineating;
  delineating << QPointF( 541413, 6242700 ) << QPointF( 568692, 6242215 ) << QPointF( 550460, 6229044 );

  ReosWatershed ws( delineating, delineating.at( 0 ), ReosGisEngine::crsFromEPSG( 2154 ) );

  std::unique_ptr<ReosSeriesRainfallFromGriddedOnWatershed> onWs = std::unique_ptr<ReosSeriesRainfallFromGriddedOnWatershed>( ReosSeriesRainfallFromGriddedOnWatershed::create( & ws, rainfall.get() ) );
}

void ReosComephoreTest::griddedDataOnWatersed()
{
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = COMEPHORE_FILES_PATH + QStringLiteral( "/comephore_nc" );

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "comephore" ), ReosGriddedRainfall::staticType(), uriParam, ok );
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


QTEST_MAIN( ReosComephoreTest )
#include "reos_comephores_test.moc"
