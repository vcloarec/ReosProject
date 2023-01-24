/***************************************************************************
                      reos_geometry_test.cpp
                     --------------------------------------
Date                 : October-2020
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

#include "reosgeometryutils.h"
#include "reosgisengine.h"
#include "reos_testutils.h"

class ReosGeometryTest: public QObject
{
    Q_OBJECT
  private slots:

    void initTestCase();
    void polygonInteractions();
    void polygonPolylineInteractions();
    void gridReprojection();

    void rasterizePolygon();

  private:
    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;

};
void ReosGeometryTest::initTestCase()
{
  mGisEngine = new ReosGisEngine( &mRootModule );
}

void ReosGeometryTest::polygonInteractions()
{
  QPolygonF polygon1;
  QPolygonF polygon2;
  QPolygonF polygon3;
  polygon1 << QPointF( 0, 0 ) << QPointF( 0, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon2 << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon3 << QPointF( 0, 0 ) << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 0, 2 );

  QCOMPARE( ReosInclusionType::Partial, ReosGeometryUtils::polygonIsInsidePolygon( polygon1, polygon2 ) );
  QCOMPARE( ReosInclusionType::Partial, ReosGeometryUtils::polygonIsInsidePolygon( polygon1, polygon3 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon2, polygon1 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon3, polygon1 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polygonIsInsidePolygon( polygon2, polygon3 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polygonIsInsidePolygon( polygon3, polygon2 ) );

  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon1, polygon1 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon2, polygon2 ) );
}


void ReosGeometryTest::polygonPolylineInteractions()
{
  QPolygonF polygon1;
  QPolygonF polygon2;
  QPolygonF polygon3;
  polygon1 << QPointF( 0, 0 ) << QPointF( 0, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon2 << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon3 << QPointF( 0, 0 ) << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 0, 2 );

  QPolygonF polyline1;
  QPolygonF polyline2;
  QPolygonF polyline3;
  polyline1 << QPointF( -1, 1 ) << QPointF( 3, 1 );
  polyline2 << QPointF( 0.25, 1 ) << QPointF( .75, 1 );
  polyline3 << QPointF( 0, 1 ) << QPointF( 1, 1 );

  QCOMPARE( ReosInclusionType::Partial, ReosGeometryUtils::polylineIsInsidePolygon( polyline1, polygon1 ) );

  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline2, polygon1 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polylineIsInsidePolygon( polyline2, polygon2 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline2, polygon3 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline3, polygon1 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polylineIsInsidePolygon( polyline3, polygon2 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline3, polygon3 ) );
}

void ReosGeometryTest::gridReprojection()
{
  QString projCrs = ReosGisEngine::crsFromEPSG( 32620 );
  QString geoCrs = ReosGisEngine::crsFromEPSG( 4326 );

  ReosRasterExtent geoExtent( -56.8624999999999972, 16.5124999999999993, 82, 72, 0.02500000000000003608, -0.02499999999999999792 );
  geoExtent.setCrs( geoCrs );

  ReosMapExtent destinationExtent = ReosGisEngine::transformExtent( geoExtent, projCrs );
  ReosRasterExtent projExtent;
  bool success;
  ReosRasterMemory<QList<QPair<double, QPoint>>> result =
    ReosGisEngine::transformRasterExtent( geoExtent, destinationExtent, 2000, 2000, projExtent, success );

  QCOMPARE( result.rowCount(), 104 );
  QCOMPARE( result.columnCount(), 114 );

  for ( int r = 0; r < result.rowCount(); ++r )
  {
    for ( int c = 0; r < result.columnCount(); ++r )
    {
      const QList<QPair<double, QPoint>> pairs = result.value( r, c );
      if ( !pairs.isEmpty() )
      {
        double sum = 0;
        for ( const QPair<double, QPoint> &pair : pairs )
        {
          sum += pair.first;
        }

        QVERIFY( equal( sum, 1.0000, 0.0001 ) );
      }
    }
  }
}

void ReosGeometryTest::rasterizePolygon()
{
  ReosRasterExtent rasterExtent( 0, 100, 100, 100, 1, -1 );
  QPolygonF poly;
  poly << QPointF( 10, 0.5 );
  poly << QPointF( 88, 2 );
  poly << QPointF( 90.25, 50.5 );
  poly << QPointF( 0.5, 0.5 );

  ReosRasterExtent finalExtent;
  int xOri, yOri;
  ReosRasterMemory<double> rasterized = ReosGeometryUtils::rasterizePolygon( poly, rasterExtent, finalExtent, xOri, yOri, false );

  QVERIFY( equal( finalExtent.xMapOrigin(), 0.0, 0.001 ) );
  QVERIFY( equal( finalExtent.yMapOrigin(), 51.0, 0.001 ) );
  QCOMPARE( finalExtent.xCellCount(), 91 );
  QCOMPARE( finalExtent.yCellCount(), 51 );

  int validCount = 0;
  QVector<double> values = rasterized.values();

  for ( double v : std::as_const( values ) )
  {
    if ( v == 1.0 )
      validCount++;
  }

  QCOMPARE( validCount, 2116 );

  rasterized = ReosGeometryUtils::rasterizePolygon( poly, rasterExtent, finalExtent, xOri, yOri, true );
  QVERIFY( equal( finalExtent.xMapOrigin(), 0.0, 0.001 ) );
  QVERIFY( equal( finalExtent.yMapOrigin(), 51.0, 0.001 ) );
  QCOMPARE( finalExtent.xCellCount(), 91 );
  QCOMPARE( finalExtent.yCellCount(), 51 );

  int fullCount = 0;
  int partialCount = 0;
  values = rasterized.values();

  for ( double v : std::as_const( values ) )
  {
    if ( equal( v, 1.0, 0.000001 ) )
      fullCount++;
    else if ( v > 0.0 )
      partialCount++;
  }

  QCOMPARE( fullCount, 1999 );
  QCOMPARE( partialCount, 277 );

  rasterExtent = ReosRasterExtent( 100, 0, 100, 100, 1, -1 );

/// Attempt with a raster extent taht do not ontersect the polygon
  rasterized = ReosGeometryUtils::rasterizePolygon( poly, rasterExtent, finalExtent, xOri, yOri, false );
  QCOMPARE( finalExtent.xCellCount(), 1 );
  QCOMPARE( finalExtent.yCellCount(), 1 );
  validCount = 0;
  values = rasterized.values();

  for ( double v : std::as_const( values ) )
  {
    if ( v == 1.0 )
      validCount++;
  }
  QCOMPARE( validCount, 0 );

  rasterized = ReosGeometryUtils::rasterizePolygon( poly, rasterExtent, finalExtent, xOri, yOri, true );
  fullCount = 0;
  partialCount = 0;
  values = rasterized.values();

  for ( double v : std::as_const( values ) )
  {
    if ( equal( v, 1.0, 0.000001 ) )
      fullCount++;
    else if ( v > 0.0 )
      partialCount++;
  }

  QCOMPARE( fullCount, 0 );
  QCOMPARE( partialCount, 0 );
}


QTEST_MAIN( ReosGeometryTest )
#include "reos_geometry_test.moc"
