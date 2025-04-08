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
#include <QPair>
#include <qtestcase.h>

#include "reos_testutils.h"
#include "reosgribprovider.h"
#include "reosgriddedrainitem.h"
#include "reoseccodesreader.h"
#include "reoswatershed.h"
#include "reosgisengine.h"

class ReosGribTest: public QObject
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
    void uri();
};

void ReosGribTest::createProvider()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  std::unique_ptr<ReosGriddedDataProvider> provider(
    qobject_cast<ReosGriddedDataProvider *>( ReosDataProviderRegistery::instance()->createCompatibleProvider( gribFile, ReosGriddedData::staticType() ) ) );

  ReosModule::Message message;
  ReosGriddedDataProvider::FileDetails details = provider->details( "lkhkjh", message );
  QVERIFY( details.availableVariables.isEmpty() );
  QVERIFY( message.type == ReosModule::Error );

  message = ReosModule::Message();
  details = provider->details( gribFile, message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( details.availableVariables.count(), 15 );

  QString variable = QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" );
  QVERIFY( details.availableVariables.contains( variable ) );

  provider->setDataSource(
    ReosGribGriddedDataProvider::uri( gribFile, variable, ReosGribGriddedDataProvider::ValueType::CumulativeOnTimeStep ) );

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
  QCOMPARE( min, 0.0009765625 );
  QCOMPARE( max, 63.595703125 );
}

void ReosGribTest::griddedRainInFolder()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  QString variable( QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( ReosGribGriddedDataProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeOnTimeStep ),
                             ReosGribGriddedDataProvider::staticKey() ) );

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
  QCOMPARE( min, 0.0009765625 );
  QCOMPARE( max, 63.595703125 );
}

void ReosGribTest::griddedRainInFile()
{
  QString gribFile( testFile( QStringLiteral( "grib/W_fr-meteofrance,MODEL,AROME+0025+SP1+00H06H_C_LFPW_202211161200--.grib2" ) ) );
  QString variable( QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( ReosGribGriddedDataProvider::uri( gribFile, variable, ReosGriddedRainfallProvider::ValueType::CumulativeOnTimeStep ),
                             ReosGribGriddedDataProvider::staticKey() ) );

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
  QString fileName = testFile( QStringLiteral( "grib/ecmwf/000-oper.grib2" ) );

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

  QDateTime refTime = reader.referenceTime( 0 );
  QDateTime validTime = reader.validityTime( 0 );

  keys.clear();
  keys.insert( "shortName", "tp" );
  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 1, reader.frameCount() );

  refTime = reader.referenceTime( 0 );
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

  fileName = "/home/vincent/318-oper.grib2";

  keys.clear();
  keys.insert( "shortName", "tp" );
  reader = ReosEcCodesReader( fileName, keys );
  QVERIFY( reader.isValid() );
  QCOMPARE( 1, reader.frameCount() );

  refTime = reader.referenceTime( 0 );
  validTime = reader.validityTime( 0 );

  QCOMPARE( QDateTime( QDate( 2025, 2, 17 ), QTime( 0, 0 ), Qt::UTC ), refTime );
  QCOMPARE( QDateTime( QDate( 2025, 3, 02 ), QTime( 6, 0 ), Qt::UTC ), validTime );

  range = reader.stepRange( 0 );
  stepType = reader.stepType( 0 );
  expected = QPair<int, int>( 0, 318 );
  QCOMPARE( expected, range );
  QVERIFY( stepType == ReosEcCodesReader::Accum );

  fileName = "/home/vincent/AROME_2025-04-02T12_15_00Z.grib2";
  keys.clear();
  keys.insert( "shortName", "tp" );
  reader = ReosEcCodesReader( fileName, keys );
  raster = reader.values( 0 );
  QCOMPARE( raster.values().size(), 5016591 );
  extent = reader.extent( 0 );
  QCOMPARE( extent.width(), 28.01 );
  QVERIFY( extent.crs() != "" );

  refTime = reader.referenceTime( 0 );
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
  QString gribFile( testFile( QStringLiteral( "grib/arome" ) ) );
  QVariantMap keys;
  keys.clear();
  keys.insert( "shortName", "tp" );
  bool ok = false;
  QVariantMap uriParams;
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  std::unique_ptr<ReosGriddedData> rainfall(
    new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys ),
                         ReosGribGriddedDataProvider::staticKey() ) );

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
  QString gribFile( testFile( QStringLiteral( "grib/arome-pi" ) ) );
  QVariantMap keys;
  keys.clear();
  keys.insert( "shortName", "tp" );
  bool ok = false;
  QVariantMap uriParams;
  uriParams.insert( QStringLiteral( "file-or-dir-path" ), gribFile );
  uriParams.insert( QStringLiteral( "grib-keys" ), keys );
  const QString uri = ReosDataProviderRegistery::instance()->buildUri( "grib", ReosGriddedData::staticType(), uriParams, ok );
  std::unique_ptr<ReosGriddedData> rainfall(
    new ReosGriddedData( uri, "grib" ) );

  QVERIFY( rainfall->isValid() );

  QCOMPARE( rainfall->gridCount(), 4 );

  QPolygonF watershed_poly;
  watershed_poly  << QPointF( 279856., 6309772. )
                  << QPointF( 346425., 6320051. )
                  << QPointF( 348884., 6252486. )
                  << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, rainfall.get() ) );

  gridOnWs->preCalculate();
  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 4 );
}

void ReosGribTest::ecmwfGribFiles()
{
  QString gribFile( testFile( QStringLiteral( "grib/ecmwf" ) ) );
  QString shortName = "tp";
  QVariantMap keys;
  keys.insert( "shortName", shortName );
  std::unique_ptr<ReosGriddedData> rainfall(
    new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys ),
                         ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( rainfall->isValid() );

  QCOMPARE( rainfall->gridCount(), 2 );

  QCOMPARE( rainfall->startTime( 0 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 0 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->startTime( 1 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rainfall->endTime( 1 ), QDateTime( QDate( 2025, 02, 17 ), QTime( 6, 0, 0 ), Qt::UTC ) );


  QVector<double> values = rainfall->values( 0 );
  QVERIFY( equal( values.at( 6453 ), 6.866455078125e-05, 1e-10 ) );

  values = rainfall->values( 1 );
  QVERIFY( equal( values.at( 6453 ), 8.58306884765625e-05, 1e-10 ) );


  keys.clear();
  keys.insert( "shortName", "vsw" );
  keys.insert( "topLevel", "0" );
  std::unique_ptr<ReosGriddedData> dataset(
    new ReosGriddedData( ReosGribGriddedDataProvider::uri( gribFile, keys ),
                         ReosGribGriddedDataProvider::staticKey() ) );

  QVERIFY( dataset->isValid() );

  QCOMPARE( dataset->gridCount(), 3 );

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

  values = dataset->values( 2 );
  QVERIFY( equal( values.at( 43607 ), 0.728271484375, 1e-10 ) );

  QPolygonF watershed_poly;
  watershed_poly  << QPointF( 279856., 6309772. )
                  << QPointF( 346425., 6320051. )
                  << QPointF( 348884., 6252486. )
                  << QPointF( 283670., 6251741. );

  ReosWatershed watershed( watershed_poly, QPointF(), ReosGisEngine::crsFromEPSG( 9794 ) );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, dataset.get() ) );

  gridOnWs->preCalculate();
  values = dataset->values( 1 );
  QVector<double> valuesOnWs = gridOnWs->constData();
  QCOMPARE( valuesOnWs.count(), 2 );
  QVERIFY( !std::isnan( valuesOnWs.at( 0 ) ) );
}

void ReosGribTest::uri()
{
  ReosDataProviderRegistery::instance()->uriParameters( "grib", ReosGriddedData::staticType() );
}




QTEST_MAIN( ReosGribTest )
#include "reos_grib_test.moc"
