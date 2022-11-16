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

#include "reosgisengine.h"
#include "reosdigitalelevationmodel.h"
#include "reos_testutils.h"

class ReosDemTesting: public QObject
{
    Q_OBJECT
  private slots:
    void raster_DEM();

  private:
    ReosGisEngine gisEngine;
};

void ReosDemTesting::raster_DEM()
{
  QString layerId = gisEngine.addRasterLayer( test_file( "filledDem.tiff" ).c_str(), "raster" );

  QVERIFY( ! gisEngine.getTopDigitalElevationModel() );

  gisEngine.registerLayerAsDigitalElevationModel( layerId );
  std::unique_ptr<ReosDigitalElevationModel> dem( gisEngine.getTopDigitalElevationModel() );
  QVERIFY( dem );

  QCOMPARE( dem->elevationAt( QPointF( 5, 5 ) ), 5.00054216385 );

  ReosRasterExtent rasterExtent;
  float maxValue = 0;
  ReosRasterMemory<float> rasterMemory( dem->extractMemoryRasterSimplePrecision( ReosMapExtent( 2, 3, 5, 7 ), rasterExtent, maxValue ) );

  QVERIFY( rasterExtent.isValid() );
  QCOMPARE( rasterExtent.xCellCount(), 3 );
  QCOMPARE( rasterExtent.yCellCount(), 4 );
  QCOMPARE( rasterExtent.xMapOrigin(), 2 );
  QCOMPARE( rasterExtent.yMapOrigin(), 7 );
  QCOMPARE( rasterExtent.xCellSize(), 1 );
  QCOMPARE( rasterExtent.yCellSize(), -1 );

  QCOMPARE( rasterMemory.rowCount(), 4 );
  QCOMPARE( rasterMemory.columnCount(), 3 );
  QCOMPARE( rasterMemory.value( 0, 0 ), 5.00028324127 );
  QCOMPARE( rasterMemory.value( 3, 2 ), 5.00042486191 );

  QPolygonF polyline;
  polyline << QPointF( 0.55, 10.9 ) << QPointF( 10.5, 10.7 )
           << QPointF( 0.4, 5.8 ) << QPointF( 10.4, 1.5 );

  QPolygonF profile = dem->elevationOnPolyline( polyline );

  QPolygonF expectedProfile;
  expectedProfile << QPointF( 0.000000000000000, 5.000000000000000 ) << QPointF( 0.957846721044250, 5.000000000000000 )
                  << QPointF( 1.957644767138169, 5.000000000000000 ) << QPointF( 2.957442813232088, 5.000000000000000 )
                  << QPointF( 3.957240859326008, 5.000000000000000 ) << QPointF( 4.957038905419926, 3.000000000000000 )
                  << QPointF( 5.956836951513846, 5.000000000000000 ) << QPointF( 6.956634997607765, 5.000000000000000 )
                  << QPointF( 7.956433043701684, 5.000000000000000 ) << QPointF( 8.956231089795601, 5.000000000000000 )
                  << QPointF( 9.952009847262008, 5.000000000000000 ) << QPointF( 11.375508424254138, 5.000141620635986 )
                  << QPointF( 12.275216536120253, 5.000141620635986 ) << QPointF( 13.611416702258047, 5.000141620635986 )
                  << QPointF( 14.511124814124162, 5.000141620635986 ) << QPointF( 15.847324980261956, 5.000241756439209 )
                  << QPointF( 16.747033092128071, 5.000200271606445 ) << QPointF( 18.083233258265864, 5.000341892242432 )
                  << QPointF( 18.982941370131982, 5.000283241271973 ) << QPointF( 20.319141536269775, 5.000141620635986 )
                  << QPointF( 21.177872843040994, 5.000000000000000 ) << QPointF( 22.306917274576858, 5.000141620635986 )
                  << QPointF( 23.620614212816310, 5.000283241271973 ) << QPointF( 24.539283400396343, 5.000424861907959 )
                  << QPointF( 25.852980338635795, 5.000424861907959 ) << QPointF( 26.771649526215832, 5.000424861907959 )
                  << QPointF( 27.690318713795868, 5.000424861907959 ) << QPointF( 29.004015652035317, 5.000283241271973 )
                  << QPointF( 29.922684839615354, 5.000283241271973 ) << QPointF( 31.236381777854803, 5.000141620635986 )
                  << QPointF( 32.063184046676838, 5.000000000000000 );

  QCOMPARE( profile, expectedProfile );

  QPolygonF polygon;
  polygon << QPointF( 1, 1 ) << QPointF( 1, 5 ) << QPointF( 2, 5 ) << QPointF( 9, 9 ) << QPointF( 1, 9 );

  QVERIFY( equal( dem->averageElevationInPolygon( polygon, QString() ), 5.0002, 0.0001 ) );
}

QTEST_MAIN( ReosDemTesting )
#include "reos_dem_test.moc"
