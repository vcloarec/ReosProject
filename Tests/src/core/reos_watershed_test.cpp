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

#include "reoswatersheddelineating.h"
#include "reos_testutils.h"

class ReosWatersehdTest: public QObject
{
    Q_OBJECT
  private slots:
    void watershedDelineating();

  private:
    ReosModule rootModule;
    ReosGisEngine gisEngine;
};

void ReosWatersehdTest::watershedDelineating()
{
  ReosWatershedDelineating watershedDelineating( &rootModule, &gisEngine );

  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForDownstream );

  QVERIFY( ! watershedDelineating.hasValidDigitalElevationModel() );

  // add raster layer and register it as DEM
  QString layerId = gisEngine.addRasterLayer( test_file( "DEM_for_watershed.tif" ).c_str(), QStringLiteral( "raster_DEM" ) );
  QVERIFY( ! watershedDelineating.setDigitalElevationModelDEM( layerId ) );
  QVERIFY( ! watershedDelineating.hasValidDigitalElevationModel() );
  QVERIFY( gisEngine.registerLayerAsDigitalElevationModel( layerId ) );
  QCOMPARE( gisEngine.layerType( layerId ), ReosGisEngine::RasterLayer );
  QMap<QString, QString> demList = gisEngine.digitalElevationModelRasterList();
  QCOMPARE( demList[layerId], QStringLiteral( "raster_DEM" ) );

  QVERIFY( watershedDelineating.setDigitalElevationModelDEM( layerId ) );

  QVERIFY( watershedDelineating.hasValidDigitalElevationModel() );

  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForDownstream );


}

QTEST_MAIN( ReosWatersehdTest )
#include "reos_watershed_test.moc"
