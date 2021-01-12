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
  ReosRasterMemory<float> rasterMemory( dem->extractMemoryRasterSimplePrecision( ReosMapExtent( 2, 3, 5, 7 ), rasterExtent ) );

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
}

QTEST_MAIN( ReosDemTesting )
#include "reos_dem_test.moc"
