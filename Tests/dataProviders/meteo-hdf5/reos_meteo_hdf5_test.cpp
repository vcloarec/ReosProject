/***************************************************************************
                      reos_meteo_hdf5_test.cpp
                     --------------------------------------
Date                 : 14-03-2025
Copyright            : (C) 2025 by Vincent Cloarec
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
#include "reosmeteohdf5.h"
#include "reoswatershed.h"
#include "reosgisengine.h"

class ReosMeteoHdf5Test: public QObject
{
    Q_OBJECT

  private slots:
    void initTestCase();
    void createProvider();
    void createGridData();
    void rainOnWatershed();
    void rainOnWatershed_missing_file();

  private:
    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;

};

void ReosMeteoHdf5Test::initTestCase()
{
  ReosIdfFormulaRegistery::instantiate( &mRootModule );
  mGisEngine = new ReosGisEngine( &mRootModule );
}


void ReosMeteoHdf5Test::createProvider()
{
  QString pathToFiles( testFile( QStringLiteral( "MF-mosaique" ) ) );
  std::unique_ptr<ReosGriddedDataProvider> provider(
    qobject_cast<ReosGriddedDataProvider *>( ReosDataProviderRegistery::instance()->createCompatibleProvider( pathToFiles, ReosGriddedData::staticType() ) ) );
  QVERIFY( provider );
  QVERIFY( provider->key().contains( METEO_HDF5_KEY ) );


  const QVariantMap &uriParamDescription = ReosDataProviderRegistery::instance()->uriParameters( QStringLiteral( "meteo-hdf5" ), ReosMeteoHdf5Provider::dataType() );
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = pathToFiles;

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "meteo-hdf5" ), ReosMeteoHdf5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  provider.reset( qobject_cast<ReosGriddedDataProvider *>( ReosDataProviderRegistery::instance()->createCompatibleProvider( uri, ReosGriddedData::staticType() ) ) );
  QVERIFY( provider );
  provider->setDataSource( uri );
  provider->load();
  QVERIFY( provider->isValid() );

  QVector<double> values = provider->data( 0 );

  double min, max;
  provider->calculateMinMax( min, max );
  QCOMPARE( min, 0.0 );
  QCOMPARE( max, 19.15 );
}


void ReosMeteoHdf5Test::createGridData()
{
  QString pathToFiles( testFile( QStringLiteral( "MF-mosaique" ) ) );
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = pathToFiles;

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "meteo-hdf5" ), ReosMeteoHdf5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  std::unique_ptr<ReosGriddedData> griddedRain =
    std::make_unique<ReosGriddedData>( uri, QStringLiteral( "meteo-hdf5" ) );

  QCOMPARE( griddedRain->gridCount(), 3 );
  QCOMPARE( griddedRain->startTime( 0 ), QDateTime( QDate( 2025, 03, 13 ), QTime( 15, 55, 0 ), Qt::UTC ) );
  QCOMPARE( griddedRain->endTime( 0 ), QDateTime( QDate( 2025, 03, 13 ), QTime( 16, 00, 0 ), Qt::UTC ) );
  QCOMPARE( griddedRain->startTime( 1 ), QDateTime( QDate( 2025, 03, 13 ), QTime( 16, 00, 0 ), Qt::UTC ) );
  QCOMPARE( griddedRain->endTime( 1 ), QDateTime( QDate( 2025, 03, 13 ), QTime( 16, 05, 0 ), Qt::UTC ) );
  QCOMPARE( griddedRain->startTime( 2 ), QDateTime( QDate( 2025, 03, 13 ), QTime( 16, 05, 0 ), Qt::UTC ) );
  QCOMPARE( griddedRain->endTime( 2 ), QDateTime( QDate( 2025, 03, 13 ), QTime( 16, 10, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = griddedRain->rasterExtent();

  QVERIFY( equal( extent.xCellSize(), 500.0, 0.001 ) );
  QVERIFY( equal( extent.yCellSize(), -500.0, 0.001 ) );
  QCOMPARE( extent.xCellCount(), 3472 );
  QCOMPARE( extent.yCellCount(), 3472 );

  double min = 0, max = 0;
  QVERIFY( !griddedRain->getDirectMinMaxValue( min, max ) );
  griddedRain->calculateMinMaxValue( min, max );
  QCOMPARE( min, 0.0 );
  QCOMPARE( max, 19.15 );
}

void ReosMeteoHdf5Test::rainOnWatershed()
{
  QString pathToFiles( testFile( QStringLiteral( "MF-mosaique" ) ) );
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = pathToFiles;

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "meteo-hdf5" ), ReosMeteoHdf5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  std::unique_ptr<ReosGriddedData> griddedRain =
    std::make_unique<ReosGriddedData>( uri, QStringLiteral( "meteo-hdf5" ) );
  QPolygonF watershed_poly;

  watershed_poly  << QPointF( 5.48210223811306463, 47.49347814632444909 )
                  << QPointF( 5.05990854288562986, 47.96360649690521427 )
                  << QPointF( 4.64508941002024667, 47.68706040832829274 )
                  << QPointF( 4.94007190450229672, 47.03994256105829663 );

  ReosWatershed watershed;
  mGisEngine->setCrs( ReosGisEngine::crsFromEPSG( 4326 ) );
  watershed.setGeographicalContext( mGisEngine );
  watershed.setDelineating( watershed_poly );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, griddedRain.get() ) );

  gridOnWs->preCalculate();

  QVector<double> values = gridOnWs->constData();
  QCOMPARE( values.count(), 3 );
  QVERIFY( equal( values.at( 0 ), 0.1312763475149827, 6 ) );
  QVERIFY( equal( values.at( 1 ), 0.1244901610017982, 6 ) );
  QVERIFY( equal( values.at( 2 ), 0.116361515127953, 6 ) );

  ReosDuration timeStep = gridOnWs->timeStep();
  QVERIFY( timeStep == ReosDuration( 5.0, ReosDuration::minute ) );


  watershed_poly.clear();
  watershed_poly  << QPointF( 3.37651104703155402, 43.78221043303267379 )
                  << QPointF( 3.33046804374244498, 43.86118930728770948 )
                  << QPointF( 3.26493238212655967, 43.80943293862696208 )
                  << QPointF( 3.28476112076931459, 43.76002913217806167 );

  ReosWatershed watershed_2( watershed_poly, QPointF( 0, 0 ), ReosGisEngine::crsFromEPSG( 4326 ) );
  watershed_2.setGeographicalContext( mGisEngine );

  gridOnWs.reset( ReosSeriesFromGriddedDataOnWatershed::create( &watershed_2, griddedRain.get() ) );

  gridOnWs->preCalculate();

  values = gridOnWs->constData();
  QCOMPARE( values.count(), 3 );
  QVERIFY( equal( values.at( 0 ), 0.0396993016085, 6 ) );
  QVERIFY( equal( values.at( 1 ), 0.02716157205240172, 6 ) );
  QVERIFY( equal( values.at( 2 ), 0.01061135371179039, 6 ) );
  timeStep = gridOnWs->timeStep();
  QVERIFY( timeStep == ReosDuration( 5.0, ReosDuration::minute ) );


}

void ReosMeteoHdf5Test::rainOnWatershed_missing_file()
{
  QString pathToFiles( testFile( QStringLiteral( "MF-mosaique-missing" ) ) );
  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = pathToFiles;

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "meteo-hdf5" ), ReosMeteoHdf5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  std::unique_ptr<ReosGriddedData> griddedRain =
    std::make_unique<ReosGriddedData>( uri, QStringLiteral( "meteo-hdf5" ) );
  QPolygonF watershed_poly;

  watershed_poly  << QPointF( 5.48210223811306463, 47.49347814632444909 )
                  << QPointF( 5.05990854288562986, 47.96360649690521427 )
                  << QPointF( 4.64508941002024667, 47.68706040832829274 )
                  << QPointF( 4.94007190450229672, 47.03994256105829663 );

  ReosWatershed watershed;
  mGisEngine->setCrs( ReosGisEngine::crsFromEPSG( 4326 ) );
  watershed.setGeographicalContext( mGisEngine );
  watershed.setDelineating( watershed_poly );

  std::unique_ptr<ReosSeriesFromGriddedDataOnWatershed> gridOnWs( ReosSeriesFromGriddedDataOnWatershed::create( &watershed, griddedRain.get() ) );

  gridOnWs->preCalculate();

  QVector<double> values = gridOnWs->constData();
  QVERIFY( equal( values.at( 0 ), 0.1312763475149827, 6 ) );
  QCOMPARE( values.at( 1 ), std::numeric_limits<double>::quiet_NaN() );
  QVERIFY( equal( values.at( 2 ), 0.116361515127953, 6 ) );

  ReosDuration timeStep = gridOnWs->timeStep();
  QVERIFY( timeStep == ReosDuration( 5.0, ReosDuration::minute ) );


  watershed_poly.clear();
  watershed_poly  << QPointF( 3.37651104703155402, 43.78221043303267379 )
                  << QPointF( 3.33046804374244498, 43.86118930728770948 )
                  << QPointF( 3.26493238212655967, 43.80943293862696208 )
                  << QPointF( 3.28476112076931459, 43.76002913217806167 );

  ReosWatershed watershed_2( watershed_poly, QPointF( 0, 0 ), ReosGisEngine::crsFromEPSG( 4326 ) );
  watershed_2.setGeographicalContext( mGisEngine );

  gridOnWs.reset( ReosSeriesFromGriddedDataOnWatershed::create( &watershed_2, griddedRain.get() ) );

  gridOnWs->preCalculate();

  values = gridOnWs->constData();
  QCOMPARE( values.count(), 3 );
  QVERIFY( equal( values.at( 0 ), 0.0396993016085, 6 ) );
  QCOMPARE( values.at( 1 ), std::numeric_limits<double>::quiet_NaN() );
  QVERIFY( equal( values.at( 2 ), 0.01061135371179039, 6 ) );
  timeStep = gridOnWs->timeStep();
  QVERIFY( timeStep == ReosDuration( 5.0, ReosDuration::minute ) );
}


QTEST_MAIN( ReosMeteoHdf5Test )
#include "reos_meteo_hdf5_test.moc"


