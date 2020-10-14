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
  // create watershed delinating module
  ReosWatershedDelineating watershedDelineating( &rootModule, &gisEngine );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::NoDigitalElevationModel );

  QVERIFY( ! watershedDelineating.hasValidDigitalElevationModel() );

  // add raster layer and register it as DEM
  QString layerId = gisEngine.addRasterLayer( test_file( "DEM_for_watershed.tif" ).c_str(), QStringLiteral( "raster_DEM" ) );
  QCOMPARE( gisEngine.layerType( layerId ), ReosGisEngine::RasterLayer );
  // attempt to add it to the wateshed delineating but fail because the DEM is not registered
  QVERIFY( ! watershedDelineating.setDigitalElevationModelDEM( layerId ) );
  QVERIFY( ! watershedDelineating.hasValidDigitalElevationModel() );
  // register the DEM
  QVERIFY( gisEngine.registerLayerAsDigitalElevationModel( layerId ) );
  QMap<QString, QString> demList = gisEngine.digitalElevationModelRasterList();
  QCOMPARE( demList[layerId], QStringLiteral( "raster_DEM" ) );

  // Try again with the registered DEM
  QVERIFY( watershedDelineating.setDigitalElevationModelDEM( layerId ) );
  QVERIFY( watershedDelineating.hasValidDigitalElevationModel() );

  // Module ready for the next step
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForDownstream );

  // Set a downstream line
  QPolygonF downstreamLine( {QPointF( 661598.75, 1792949.65 )} );

  // Add only one point line --> fails and still waiting
  QVERIFY( ! watershedDelineating.setDownstreamLine( downstreamLine ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForDownstream );

  downstreamLine.append( QPointF( 661753, 1792949.65 ) );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForExtent );

  // Extent that do not contain the downstream line
  ReosMapExtent extent( 661556.31, 1792979.81, 661781.55,  1793062.83 );
  QVERIFY( !watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForExtent );

  std::unique_ptr<ReosProcess> process;
  process.reset( watershedDelineating.delineatingProcess() );

  QVERIFY( !process );

  extent = ReosMapExtent( 661553.33, 1792734.46, 661780.44, 1792964.54 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );

  process.reset( watershedDelineating.delineatingProcess() );
  QVERIFY( process );

  process->start();

  QVERIFY( process->isSuccessful() );

}

QTEST_MAIN( ReosWatersehdTest )
#include "reos_watershed_test.moc"
