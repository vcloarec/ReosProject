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
#include <QtTest>
#include <QObject>
#include <QPolygonF>
#include <QPointF>

#include "reos_testutils.h"
#include "reosgisengine.h"
#include "reoswatershed.h"
#include "reoslandusedata.h"
#include "reosmemoryraster.h"


class ReosCdslccTest : public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();
    void createLandUseData();

    void landUsedClassified();

    void landUseClassifiedDataOnOverlappingPolygon();

  private:
    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;
};

void ReosCdslccTest::createProvider()
{
  std::unique_ptr<ReosDataProvider> compatibleProvider( ReosDataProviderRegistery::instance()->createCompatibleProvider( testFile( "/cdslcc/finistere.nc" ), ReosLandUseData::staticType() ) );
  QVERIFY( compatibleProvider );
}

void ReosCdslccTest::createLandUseData()
{
  ReosLandUseData landUseData( testFile( "/cdslcc/finistere.nc" ), "cdslcc" );

  ReosRasterExtent extent = landUseData.extent();

  QCOMPARE( extent.xCellCount(), 475 );
  QCOMPARE( extent.yCellCount(), 400 );
  QVERIFY( equal( extent.xCellSize(), 0.002777777777, 0.00001 ) );
  QVERIFY( equal( extent.yCellSize(), -0.002777777777, 0.00001 ) );

  QCOMPARE( landUseData.timeWindow().start(), QDateTime::fromString( "2022-01-01T00:00:00", Qt::ISODate ) );
  QCOMPARE( landUseData.timeWindow().end(), QDateTime::fromString( "2022-12-31T23:59:59", Qt::ISODate ) );

  QVERIFY( !extent.crs().isEmpty() );
  QVector<int> data = landUseData.data();

  QCOMPARE( data.count(), 190000 );
  QCOMPARE( data.at( 5000 ), 210 );

  QPolygonF poly(
    { QPointF( -3.97270903899460048, 48.38251575588691367 ),
      QPointF( -3.97270903899460048, 48.42045504313829696 ),
      QPointF( -3.90834437753827046, 48.42045504313829696 ),
      QPointF( -3.90834437753827046, 48.38251575588691367 ) }
  );

  ReosMapExtent requestedExtent( poly, ReosGisEngine::crsFromEPSG( 4326 ) );

  ReosRasterExtent outputExtent;
  data = landUseData.data( requestedExtent, outputExtent );

  QVERIFY( equal( outputExtent.xMapOrigin(), -3.9750000, 0.00001 ) );
  QVERIFY( equal( outputExtent.yMapOrigin(), 48.42222222, 0.00001 ) );
  QCOMPARE( outputExtent.xCellCount(), 24 );
  QCOMPARE( outputExtent.yCellCount(), 15 );

  QCOMPARE( data.at( 0 ), 30 );
  QCOMPARE( data.at( 1 ), 30 );
  QCOMPARE( data.at( 2 ), 10 );
  QCOMPARE( data.at( 358 ), 130 );
  QCOMPARE( data.at( 359 ), 30 );

  QPolygonF poly_2154(
    { QPointF( 157590.36536302175954916, 6777140.74498519208282232 ),
      QPointF( 157590.36536302175954916, 6786265.45279560890048742 ),
      QPointF( 160684.05454721508431248, 6786265.45279560890048742 ),
      QPointF( 160684.05454721508431248, 6777140.74498519208282232 ) }
  );

  ReosMapExtent requestedExtent_2154( poly_2154, ReosGisEngine::crsFromEPSG( 2154 ) );

  data = landUseData.data( requestedExtent_2154, outputExtent );

  QVERIFY( equal( outputExtent.xMapOrigin(), -4.2777777778, 0.00001 ) );
  QVERIFY( equal( outputExtent.yMapOrigin(), 47.955555556, 0.00001 ) );
  QCOMPARE( outputExtent.xCellCount(), 20 );
  QCOMPARE( outputExtent.yCellCount(), 31 );

  QCOMPARE( data.at( 0 ), 11 );
  QCOMPARE( data.at( 1 ), 11 );
  QCOMPARE( data.at( 2 ), 11 );
  QCOMPARE( data.at( 3 ), 30 );
  QCOMPARE( data.at( 618 ), 11 );
  QCOMPARE( data.at( 619 ), 190 );


  ReosWatershed watershed( poly, QPointF(), ReosGisEngine::crsFromEPSG( 4326 ) );

  ReosLandUseDataOnWatershed landUseOnWatershed( &landUseData, &watershed );

  QMap<int, double> distribution = landUseOnWatershed.landUseDistribution();

  //check the sum of all the value of distribution
  double sum = 0;
  for ( auto it = distribution.begin(); it != distribution.end(); ++it )
    sum += it.value();
  QVERIFY( equal( sum, 1.0, 0.001 ) );
}
void ReosCdslccTest::landUsedClassified()
{
  ReosLandUseData landUseData( testFile( "/cdslcc/finistere.nc" ), "cdslcc" );

QPolygonF poly_4326(
    { QPointF( -3.84144997523527909, 48.55008770843649302 ),
      QPointF( -3.84361688954929814, 48.4746790903087188 ),
      QPointF( -3.69843363051016993, 48.49548146772330171 ),
      QPointF( -3.70666790490343834, 48.55875536569259765 ) }
  );
  ReosWatershed watershed( poly_4326, QPointF(), ReosGisEngine::crsFromEPSG( 4326 ) );

  ReosLandUseDataOnWatershed landUseOnWatershed( &landUseData, &watershed );

  QVector<QMap<int, double>> distribution = landUseOnWatershed.landUseDistributionByArea(testFile( "/cdslcc/morlaix_area_distr.tiff" ),6);

  //check the sum of all the values of distribution
  QCOMPARE( distribution.size(), 6 );

  for ( auto it = distribution.begin(); it != distribution.end(); ++it )
  {
    double sum = 0;
    if (it->isEmpty())
      continue;
    for ( auto innerIt = it->begin(); innerIt != it->end(); ++innerIt )
      sum += innerIt.value();
    QVERIFY( equal( sum, 1.0, 0.001 ) );
  } 
  
}

void ReosCdslccTest::landUseClassifiedDataOnOverlappingPolygon()
{
  ReosLandUseData landUseData( testFile( "/cdslcc/finistere.nc" ), "cdslcc" );

  QPolygonF overlappingPoly(
    { QPointF(-3.8872053941118816, 48.52095596478442019),
      QPointF(-3.82564508173136897, 48.52698284851398114),
      QPointF(-3.81186934749237327, 48.50158383851083244),
      QPointF(-3.80885590562759324, 48.46068712748881069),
      QPointF(-3.8734296598728859, 48.45035532680956436),
      QPointF(-3.88935785258672473, 48.4537992603693155),
      QPointF(-3.8872053941118816, 48.52095596478442019) }
  );

  ReosWatershed watershed( overlappingPoly, QPointF(), ReosGisEngine::crsFromEPSG( 4326 ) );

  ReosLandUseDataOnWatershed landUseOnWatershed( &landUseData, &watershed );

  QVector<QMap<int, double>> distribution = landUseOnWatershed.landUseDistributionByArea(testFile( "/cdslcc/morlaix_area_distr.tiff" ),6);

  //check the sum of all the values of distribution
  QCOMPARE( distribution.size(), 6 );

  for ( auto it = distribution.begin(); it != distribution.end(); ++it )
  {
    double sum = 0;
    if (it->isEmpty())
      continue;
    for ( auto innerIt = it->begin(); innerIt != it->end(); ++innerIt )
      sum += innerIt.value();
    QVERIFY( equal( sum, 1.0, 0.001 ) );
  } 
}



QTEST_MAIN( ReosCdslccTest )
#include "reos_cdslcc_test.moc"
