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
#include <QtTest/QtTest>
#include <QObject>
#include <QPair>
#include <qtestcase.h>

#include "reos_testutils.h"
#include "reosgribprovider.h"
#include "reosgriddedrainitem.h"
#include "reoseccodesreader.h"
#include "reoswatershed.h"
#include "reosgisengine.h"

class ReosGribTest : public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();

    void griddedRainInFolder();
    void griddedRainInFile();
    void eccodesReader();
    void aromeGribFiles();
    void aromePiGribFiles();
    void ecmwfGribFiles();
    void ERA5GribFiles();
    void uri();
    void griddedAromePiaf();
    void ERA5RecentGribFiles();
};

void ReosGribTest::createProvider()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  std::unique_ptr<ReosGriddedDataProvider> provider( qobject_cast<ReosGriddedDataProvider *>( ReosDataProviderRegistery::instance()->createCompatibleProvider( gribFile, ReosGriddedData::staticType() ) ) );

  ReosModule::Message message;
  ReosGriddedDataProvider::FileDetails details = provider->details( "lkhkjh", message );
  QVERIFY( details.availableVariables.isEmpty() );
  QVERIFY( message.type == ReosModule::Error );

  message = ReosModule::Message();
  details = provider->details( gribFile, message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( details.availableVariables.count(), 15 );

  QString variable = QStringLiteral( "Total Precipitation" );
  QVERIFY( details.availableVariables.contains( variable ) );


  provider->setDataSource( ReosGribGriddedDataProvider::uri( gribFile, variable, ReosGribGriddedDataProvider::ValueType::Cumulative ) );

  QCOMPARE( provider->count(), 3 );

  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 2 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 14, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 2 ), QDateTime( QDate( 2022, 11, 12 ), QTime( 15, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();
  QVERIFY( extent.isValid() );

  double min = 0, max = 0;
  QVERIFY( !provider->getDirectMinMax( min, max ) );
  provider->calculateMinMax( min, max );
  QCOMPARE( min, -0.001953125 );
  QCOMPARE( max, 63.595703125 );
}

void ReosGribTest::griddedRainInFolder()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  QString variable( QStringLiteral( "Total Precipitation" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( ReosGribGriddedDataProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeOnTimeStep ), ReosGribGriddedDataProvider::staticKey() )
  );

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

  double min = 0, max = 0;
  QVERIFY( !rainfall->getDirectMinMaxValue( min, max ) );
  rainfall->calculateMinMaxValue( min, max );
  QCOMPARE( min, -0.001953125 );
  QCOMPARE( max, 63.595703125 );
}

void ReosGribTest::griddedRainInFile()
{
  QString gribFile( testFile( QStringLiteral( "grib/W_fr-meteofrance,MODEL,AROME+0025+SP1+00H06H_C_LFPW_202211161200--.grib2" ) ) );
  QString variable( QStringLiteral( "Total Precipitation" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( ReosGribGriddedDataProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeOnTimeStep ), ReosGribGriddedDataProvider::staticKey() )
  );

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

void ReosGribTest::eccodesReader()
{
  QString fileName =  QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/ecmwf/000-oper.grib2" );

  const QList<ReosEcCodesReader::Variable> variables = ReosEcCodesReader::variables( fileName );

  QVariantMap keys;
  keys.insert( "shortName", "vsw" );
  keys.insert( "topLevel", 0 );

  ReosEcCodesReader reader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 1, reader.frameCount() );

  QPair<int, int> range = reader.stepRange( 0 );
  ReosEcCodesReader::StepType stepType = reader.stepType( 0 );
  QPair<int, int> expected( 0, 0 );
  QCOMPARE( expected, range );
  QVERIFY( stepType == ReosEcCodesReader::Instant );

  keys.clear();
  keys.insert( "shortName", "vsw" );
  keys.insert( "topLevel", "0" );

  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 1, reader.frameCount() );

  keys.clear();
  keys.insert( "shortName", "blabla" );
  keys.insert( "topLevel", "0" );

  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 0, reader.frameCount() );

  keys.clear();
  keys.insert( "shortName", "vsw" );
  keys.insert( "topLevel", 1 );

  keys.clear();
  keys.insert( "shortName", "sot" );
  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 4, reader.frameCount() );

  QDateTime refTime = reader.dataTime( 0 );
  QDateTime validTime = reader.validityTime( 0 );

  keys.clear();
  keys.insert( "shortName", "tp" );
  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 1, reader.frameCount() );

  refTime = reader.dataTime( 0 );
  validTime = reader.validityTime( 0 );

  QCOMPARE( QDateTime( QDate( 2025, 2, 17 ), QTime( 0, 0 ), Qt::UTC ), refTime );
  QCOMPARE( QDateTime( QDate( 2025, 2, 17 ), QTime( 0, 0 ), Qt::UTC ), validTime );

  range = reader.stepRange( 0 );
  stepType = reader.stepType( 0 );
  expected = QPair<int, int>( 0, 0 );
  QCOMPARE( expected, range );
  QVERIFY( stepType == ReosEcCodesReader::Accum );

  ReosRasterMemory<double> raster = reader.values( 0 );
  QCOMPARE( raster.values().size(), 1038240 );

  ReosRasterExtent extent = reader.extent( 0 );

  fileName = QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/318-oper.grib2" );

  keys.clear();
  keys.insert( "shortName", "tp" );
  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 1, reader.frameCount() );

  refTime = reader.dataTime( 0 );
  validTime = reader.validityTime( 0 );

  QCOMPARE( QDateTime( QDate( 2025, 2, 17 ), QTime( 0, 0 ), Qt::UTC ), refTime );
  QCOMPARE( QDateTime( QDate( 2025, 3, 02 ), QTime( 6, 0 ), Qt::UTC ), validTime );

  range = reader.stepRange( 0 );
  stepType = reader.stepType( 0 );
  expected = QPair<int, int>( 0, 318 );
  QCOMPARE( expected, range );
  QVERIFY( stepType == ReosEcCodesReader::Accum );

  fileName =  QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/AROME_2025-04-02T12_15_00Z.grib2" );
  keys.clear();
  keys.insert( "shortName", "tp" );
  reader = ReosEcCodesReader( fileName, keys );
  raster = reader.values( 0 );
  QCOMPARE( raster.values().size(), 5016591 );
  extent = reader.extent( 0 );
  QCOMPARE( extent.width(), 28.01 );
  QVERIFY( extent.crs() != "" );

  refTime = reader.dataTime( 0 );
  validTime = reader.validityTime( 0 );

  QCOMPARE( QDateTime( QDate( 2025, 4, 2 ), QTime( 12, 0 ), Qt::UTC ), refTime );
  QCOMPARE( QDateTime( QDate( 2025, 4, 2 ), QTime( 12, 15 ), Qt::UTC ), validTime );

  range = reader.stepRange( 0 );
  stepType = reader.stepType( 0 );
  expected = QPair<int, int>( 0, 15 );
  QCOMPARE( expected, range );
  QVERIFY( stepType == ReosEcCodesReader::Accum );
}

void ReosGribTest::aromeGribFiles()
{
  QString gribFile( QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/arome" ) );
  QVariantMap keys;
  keys.clear();
  keys.insert( "shortName", "tp" );
  bool ok = false;
  QVariantMap uriParams;
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  std::unique_ptr<ReosGriddedData> rainfall( new ReosGriddedData( uri, ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( rainfall->isValid() );

  QCOMPARE( rainfall->gridCount(), 5 );

  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 2025, 04, 01 ), QTime( 06, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 2025, 04, 01 ), QTime( 07, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 2025, 04, 01 ), QTime( 07, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 2025, 04, 01 ), QTime( 8, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 2 ), QDateTime( QDate( 2025, 04, 01 ), QTime( 8, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 2 ), QDateTime( QDate( 2025, 04, 01 ), QTime( 9, 0, 0 ), Qt::UTC ) );

  QVector<double> values = rainfall->values( 0 );
  QVERIFY( std::isnan( values.at( 11000 ) ) );
  QVERIFY( equal( values.at( 6995 ), 0.01318359375, 0.000001 ) );

  values = rainfall->values( 2 );
  QVERIFY( std::isnan( values.at( 11000 ) ) );
  QVERIFY( equal( values.at( 6995 ), 0.008056640625, 0.000001 ) );
}

void ReosGribTest::aromePiGribFiles()
{
  QString gribFile( QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/arome-pi" ) );
  QVariantMap keys;
  keys.clear();
  keys.insert( "shortName", "tp" );
  bool ok = false;
  QVariantMap uriParams;
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  std::unique_ptr<ReosGriddedData> rainfall( new ReosGriddedData( uri, "grib" ) );

  QVERIFY( rainfall->isValid() );

  QCOMPARE( rainfall->gridCount(), 4 );

  QPolygonF watershed_poly;
  watershed_poly << QPointF( 279856., 6309772. ) << QPointF( 346425., 6320051. ) << QPointF( 348884., 6252486. ) << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, rainfall.get() ) );

  gridOnWs->preCalculate();
  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 4 );
}

void ReosGribTest::ERA5GribFiles()
{
  QString gribFile( QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/1990-02.grib" ) );
  QVariantMap keys;
  keys.clear();
  keys.insert( "shortName", "tp" );
  bool ok = false;
  QVariantMap uriParams;
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  QString uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  std::unique_ptr<ReosGriddedData> dataset( new ReosGriddedData( uri, "grib" ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 672 );

  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 1990, 01, 31 ), QTime( 23, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 2 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 2 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );

  QPolygonF watershed_poly;
  watershed_poly << QPointF( 279856., 6309772. ) << QPointF( 346425., 6320051. ) << QPointF( 348884., 6252486. ) << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );
  watershed.calculateArea();

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, dataset.get() ) );

  gridOnWs->preCalculate();
  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 672 );

  keys.clear();
  keys.insert( "shortName", "2t" );
  uriParams.clear();
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  uriParams.insert( QStringLiteral( "all-instantaneous-frames" ), true );
  uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  dataset.reset( new ReosGriddedData( uri, ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 672 );

  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 1990, 01, 31 ), QTime( 23, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 2 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 2 ), QDateTime( QDate( 1990, 02, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs_2( ReosSeriesFromGriddedDataOnWatershed::createWithTimeStep( &watershed, dataset.get(), ReosDuration( 1.0, ReosDuration::hour ) ) );

  gridOnWs_2->preCalculate();
  QVector<double> valuesOnWs = gridOnWs_2->constData();
  QCOMPARE( valuesOnWs.count(), 672 );
}


void ReosGribTest::ecmwfGribFiles()
{
  QString gribFile( QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/ecmwf" ) );

  QString shortName = "tp";
  QVariantMap keys;
  keys.insert( "shortName", shortName );
  std::unique_ptr<ReosGriddedData> dataset( new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys ), ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );
  QCOMPARE( dataset->gridCount(), 2 );

  shortName = "2t";
  keys.clear();
  keys.insert( "shortName", shortName );
  dataset.reset( new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys ), ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 2 );

  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 6, 0, 0 ), Qt::UTC ) );

  QPolygonF watershed_poly;
  watershed_poly << QPointF( 279856., 6309772. ) << QPointF( 346425., 6320051. ) << QPointF( 348884., 6252486. ) << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs_1( ReosSeriesFromGriddedDataOnWatershed::createWithTimeStep( &watershed, dataset.get(), ReosDuration( 1.0, ReosDuration::hour ) ) );

  gridOnWs_1->preCalculate();
  QVector<double> values = dataset->values( 1 );
  QVector<double> valuesOnWs = gridOnWs_1->constData();
  QCOMPARE( valuesOnWs.count(), 6 );
  QVERIFY( !std::isnan( valuesOnWs.at( 0 ) ) );

  keys.clear();
  keys.insert( "shortName", "vsw" );
  keys.insert( "topLevel", "0" );
  dataset.reset( new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys ), ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 2 );

  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 6, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 2 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 6, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 2 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 6, 0, 0 ), Qt::UTC ) );


  values = dataset->values( 0 );
  QVERIFY( equal( values.at( 43607 ), 0.728271484375, 1e-10 ) );

  values = dataset->values( 1 );
  QVERIFY( equal( values.at( 43607 ), 0.728271484375, 1e-10 ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs_2( ReosSeriesFromGriddedDataOnWatershed::createWithTimeStep( &watershed, dataset.get(), ReosDuration( 1.0, ReosDuration::hour ) ) );

  gridOnWs_2->preCalculate();
  values = dataset->values( 1 );
  valuesOnWs = gridOnWs_2->constData();
  QCOMPARE( valuesOnWs.count(), 6 );
  QVERIFY( !std::isnan( valuesOnWs.at( 0 ) ) );
}

void ReosGribTest::uri()
{
  ReosDataProviderRegistery::instance()->uriParameters( "grib", ReosGriddedData::staticType() );
}

void ReosGribTest::ERA5RecentGribFiles()
{
  QString gribFile( QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/era-recent/2025-10-04.grib" ) );

  const QList<ReosEcCodesReader::Variable> variables = ReosEcCodesReader::variables( gribFile );


  QVariantMap keys;
  keys.clear();
  keys.insert( "shortName", "tp" );
  bool ok = false;
  QVariantMap uriParams;
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  uriParams.insert( QStringLiteral( "cumulative-on-day" ), true );
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  std::unique_ptr<ReosGriddedData> dataset( new ReosGriddedData( uri, "grib" ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 24 );

  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 2025, 10, 03 ), QTime( 23, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 2 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 2 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 2, 0, 0 ), Qt::UTC ) );

  QPolygonF watershed_poly;
  watershed_poly << QPointF( 279856., 6309772. ) << QPointF( 346425., 6320051. ) << QPointF( 348884., 6252486. ) << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );
  watershed.calculateArea();

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, dataset.get() ) );

  gridOnWs->preCalculate();
  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 24 );

  keys.clear();
  keys.insert( "shortName", "2t" );
  dataset.reset( new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys, false, true ), ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 24 );

  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 2025, 10, 03 ), QTime( 23, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 2 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 2 ), QDateTime( QDate( 2025, 10, 04 ), QTime( 2, 0, 0 ), Qt::UTC ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs_2( ReosSeriesFromGriddedDataOnWatershed::createWithTimeStep( &watershed, dataset.get(), ReosDuration( 1.0, ReosDuration::hour ) ) );

  gridOnWs_2->preCalculate();
  values = dataset->values( 1 );
  QVector<double> valuesOnWs = gridOnWs_2->constData();
  QCOMPARE( valuesOnWs.count(), 24 );
}


void ReosGribTest::griddedAromePiaf()
{
  QString gribFile( QStringLiteral(GRIB_TEST_FILES_PATH) + QStringLiteral( "/arome-piaf/" ) );
  QString shortName = "tp";
  QVariantMap keys;
  keys.insert( "shortName", shortName );
  std::unique_ptr<ReosGriddedRainfall> dataset( new ReosGriddedRainfall( ReosGribGriddedDataProvider::uri( gribFile, keys ), ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 39 );


  QCOMPARE( dataset->startTime( 0 ), QDateTime( QDate( 2025, 10, 16 ), QTime( 21, 40, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 0 ), QDateTime( QDate( 2025, 10, 16 ), QTime( 21, 45, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 1 ), QDateTime( QDate( 2025, 10, 16 ), QTime( 21, 45, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 1 ), QDateTime( QDate( 2025, 10, 16 ), QTime( 21, 50, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->startTime( 2 ), QDateTime( QDate( 2025, 10, 16 ), QTime( 21, 50, 0 ), Qt::UTC ) );
  QCOMPARE( dataset->endTime( 2 ), QDateTime( QDate( 2025, 10, 16 ), QTime( 21, 55, 0 ), Qt::UTC ) );

  QPolygonF watershed_poly;
  watershed_poly << QPointF( 279856., 6309772. ) << QPointF( 346425., 6320051. ) << QPointF( 348884., 6252486. ) << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );
  watershed.calculateArea();

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, dataset.get() ) );

  gridOnWs->preCalculate();
  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 39 );

  QVector<QPointF> pointsForValues;
  pointsForValues << QPointF( 640361, 6025606 ) << QPointF( 949009.1, 7120534.4 );

  QVector<double> valuesOnPoint;
  for ( int index = 0; index < dataset->gridCount(); index++ )
  {
    valuesOnPoint = dataset->valuesAtPositions( index, pointsForValues, ReosGisEngine::crsFromEPSG( 2154 ) );
    QCOMPARE( valuesOnPoint.count(), pointsForValues.count() );
  }

  valuesOnPoint = dataset->valuesAtPositions( 0, pointsForValues, ReosGisEngine::crsFromEPSG( 2154 ) );

  QVERIFY( equal( 0.8299560546875, valuesOnPoint[0], 0.0001 ) );
  QVERIFY( equal( 0.6778564453125, valuesOnPoint[1], 0.0001 ) );
}


QTEST_MAIN( ReosGribTest )
#include "reos_grib_test.moc"
