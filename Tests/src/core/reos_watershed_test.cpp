/***************************************************************************
                      reos_watershed_test.cpp
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

#include <fstream>

#include "reoswatersheddelineating.h"
#include "reoswatershedtree.h"
#include "reos_testutils.h"


class ReosWatersehdTest: public QObject
{
    Q_OBJECT
  private slots:
    void watershedDelineating();
    void watershedDelineatingWithBurningLine();
    void watershdDelineatingMultiWatershed();
    void inclusion();
    void watershedInteractions();

  private:
    ReosModule rootModule;
    ReosGisEngine gisEngine;
};


void ReosWatersehdTest::inclusion()
{
  QPolygonF poly1;
  poly1 << QPointF( 0, 0 ) << QPointF( 0, 5 ) << QPointF( 5, 5 ) << QPointF( 5, 0 );
  ReosWatershed watershed1( poly1, QPointF( 0, 2.5 ), ReosWatershed::Manual );

  QPolygonF poly2;
  poly2 << QPointF( 1, 0 ) << QPointF( 1, 5 ) << QPointF( 5, 5 ) << QPointF( 5, 0 );
  ReosWatershed watershed2( poly2, QPointF( 1, 2.5 ), ReosWatershed::Manual );

  QPolygonF poly3;
  poly3 << QPointF( 1, 1 ) << QPointF( 1, 4 ) << QPointF( 4, 4 ) << QPointF( 4, 1 );
  ReosWatershed watershed3( poly3, QPointF( 1, 2.5 ), ReosWatershed::Manual );

  QPolygonF poly4;
  poly4 << QPointF( 0, 2 ) << QPointF( 0, 3 ) << QPointF( 5, 3 ) << QPointF( 4, 2 );
  ReosWatershed watershed4( poly4, QPointF( 0, 2.5 ), ReosWatershed::Manual );

  QPolygonF poly5;
  poly5 << QPointF( 5, 2 ) << QPointF( 5, 3 ) << QPointF( 10, 3 ) << QPointF( 10, 2 );
  ReosWatershed watershed5( poly5, QPointF( 5, 2.5 ), ReosWatershed::Manual );

  ReosInclusionType inclusion = watershed3.isContainedBy( watershed1 );
  QCOMPARE( inclusion, ReosInclusionType::Total );

  inclusion = watershed3.isContainedBy( watershed2 );
  QCOMPARE( inclusion, ReosInclusionType::Total );

  inclusion = watershed1.isContainedBy( watershed2 );
  QCOMPARE( inclusion, ReosInclusionType::Partial );

  inclusion = watershed2.isContainedBy( watershed1 );
  QCOMPARE( inclusion, ReosInclusionType::Total );

  inclusion = watershed1.isContainedBy( watershed4 );
  QCOMPARE( inclusion, ReosInclusionType::Partial );
  inclusion = watershed4.isContainedBy( watershed1 );
  QCOMPARE( inclusion, ReosInclusionType::Total );
  inclusion = watershed4.isContainedBy( watershed2 );
  QCOMPARE( inclusion, ReosInclusionType::Partial );
  inclusion = watershed4.isContainedBy( watershed3 );
  QCOMPARE( inclusion, ReosInclusionType::Partial );

  inclusion = watershed5.isContainedBy( watershed1 );
  QCOMPARE( inclusion, ReosInclusionType::None );
}


void ReosWatersehdTest::watershedInteractions()
{
  ReosWatershedTree watershedTree;

  QPolygonF poly1;
  poly1 << QPointF( 0, 0 ) << QPointF( 0, 50 ) << QPointF( 100, 50 ) << QPointF( 100, 0 );

  ReosWatershed *watershed_1 = watershedTree.addWatershed( new ReosWatershed( poly1, QPointF( 0, 25 ), ReosWatershed::Manual ) );

  QVERIFY( watershed_1 );
  QCOMPARE( watershedTree.watershedCount(), 1 );
  QCOMPARE( watershed_1->downstreamWatershed(), nullptr );
  QCOMPARE( watershed_1->directUpstreamWatershedCount(), 0 );

  QPolygonF poly2;
  poly2 << QPointF( 75, -25 ) << QPointF( 75, 75 ) << QPointF( 125, 75 ) << QPointF( 125, -25 );

  ReosWatershed *watershed_2 = watershedTree.addWatershed( new ReosWatershed( poly2, QPointF( 75, 25 ), ReosWatershed::Manual ), true );
  QVERIFY( watershed_2 );
  QCOMPARE( watershed_1->downstreamWatershed(), nullptr );
  QCOMPARE( watershed_1->directUpstreamWatershedCount(), 2 ); //new one + residual watershed
  QCOMPARE( watershedTree.watershedCount(), 3 );
  QVERIFY( poly2 != watershed_2->delineating() ); //adding watershed 2 has to result to modify is delineating
  poly2.clear();
  poly2 << QPointF( 75, 0 ) << QPointF( 75, 50 ) << QPointF( 100, 50 ) << QPointF( 100, 0 );
  QCOMPARE( poly2, watershed_2->delineating() );
  ReosWatershed *residual = watershed_1->residualWatershed();
  QVERIFY( residual );
  QPolygonF residualDelineating;
  residualDelineating << QPointF( 0, 0 ) << QPointF( 0, 50 ) << QPointF( 75, 50 ) << QPointF( 75, 0 );
  QCOMPARE( residualDelineating, residual->delineating() );
  QCOMPARE( residual->outletPoint(), watershed_1->outletPoint() );

  QPolygonF poly3;
  poly3 << QPointF( 50, -25 ) << QPointF( 50, 75 ) << QPointF( 125, 75 ) << QPointF( 125, -25 );

  ReosWatershed *watershed_3 = watershedTree.addWatershed( new ReosWatershed( poly3, QPointF( 50, 25 ), ReosWatershed::Manual ), true );
  QVERIFY( watershed_3 );
  QVERIFY( poly3 != watershed_3->delineating() );
  poly3.clear();
  poly3 << QPointF( 50, 0 ) << QPointF( 50, 50 ) << QPointF( 100, 50 ) << QPointF( 100, 0 );
  QCOMPARE( poly3, watershed_3->delineating() );
  QCOMPARE( watershed_1->downstreamWatershed(), nullptr );
  QCOMPARE( watershed_2->downstreamWatershed(), watershed_3 );
  QCOMPARE( watershed_1->directUpstreamWatershedCount(), 2 ); //new one + residual watershed
  QCOMPARE( watershed_2->directUpstreamWatershedCount(), 0 );
  QCOMPARE( watershed_3->directUpstreamWatershedCount(), 2 ); //watershed_2 + residual watershed
  QCOMPARE( watershedTree.watershedCount(), 5 );
  residualDelineating.clear();
  residualDelineating << QPointF( 0, 0 ) << QPointF( 0, 50 ) << QPointF( 50, 50 ) << QPointF( 50, 0 );
  QCOMPARE( residualDelineating, watershed_1->residualWatershed()->delineating() );
  residual = watershed_3->residualWatershed();
  residualDelineating.clear();
  residualDelineating << QPointF( 50, 0 ) << QPointF( 50, 50 ) << QPointF( 75, 50 ) << QPointF( 75, 0 );
  QCOMPARE( residualDelineating, residual->delineating() );

}

void ReosWatersehdTest::watershedDelineating()
{
  // create watershed delinating module
  ReosWatershedTree watershedStore;
  ReosWatershedDelineating watershedDelineating( &rootModule, &watershedStore, &gisEngine );
  ModuleProcessControler controler( &watershedDelineating );
  ReosWatershedItemModel itemModel( &watershedStore );

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

  downstreamLine.prepend( QPointF( 661753, 1792949.65 ) );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForExtent );

  // Extent that do not contain the downstream line
  ReosMapExtent extent( 661556.31, 1792979.81, 661781.55,  1793062.83 );
  QVERIFY( !watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForExtent );

  QVERIFY( !watershedDelineating.startDelineating() );

  // Start the good delineating
  extent = ReosMapExtent( 661553.33, 1792734.46, 661780.44, 1792964.54 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();

  QPolygonF polygonWatershed = watershedDelineating.lastWatershedDelineated();

  QPolygonF polygonWatershedTest = QPolygonF(
  {
    QPointF( 661598.50, 1792950.50 ), QPointF( 661598.50, 1792949.50 ), QPointF( 661597.50, 1792949.50 ), QPointF( 661597.50, 1792948.50 ), QPointF( 661596.50, 1792948.50 ), QPointF( 661596.50, 1792947.50 ), QPointF( 661595.50, 1792947.50 ), QPointF( 661595.50, 1792946.50 ), QPointF( 661594.50, 1792946.50 ), QPointF( 661594.50, 1792945.50 ),
    QPointF( 661593.50, 1792945.50 ), QPointF( 661593.50, 1792944.50 ), QPointF( 661592.50, 1792944.50 ), QPointF( 661592.50, 1792943.50 ), QPointF( 661591.50, 1792943.50 ), QPointF( 661591.50, 1792942.50 ), QPointF( 661590.50, 1792942.50 ), QPointF( 661590.50, 1792941.50 ), QPointF( 661589.50, 1792941.50 ), QPointF( 661589.50, 1792940.50 ),
    QPointF( 661588.50, 1792940.50 ), QPointF( 661588.50, 1792939.50 ), QPointF( 661587.50, 1792939.50 ), QPointF( 661587.50, 1792938.50 ), QPointF( 661586.50, 1792938.50 ), QPointF( 661586.50, 1792937.50 ), QPointF( 661585.50, 1792937.50 ), QPointF( 661585.50, 1792936.50 ), QPointF( 661584.50, 1792936.50 ), QPointF( 661584.50, 1792935.50 ),
    QPointF( 661583.50, 1792935.50 ), QPointF( 661583.50, 1792934.50 ), QPointF( 661582.50, 1792934.50 ), QPointF( 661582.50, 1792933.50 ), QPointF( 661581.50, 1792933.50 ), QPointF( 661581.50, 1792932.50 ), QPointF( 661580.50, 1792932.50 ), QPointF( 661580.50, 1792931.50 ), QPointF( 661576.50, 1792931.50 ), QPointF( 661576.50, 1792932.50 ),
    QPointF( 661575.50, 1792932.50 ), QPointF( 661575.50, 1792931.50 ), QPointF( 661574.50, 1792931.50 ), QPointF( 661574.50, 1792928.50 ), QPointF( 661573.50, 1792928.50 ), QPointF( 661573.50, 1792927.50 ), QPointF( 661574.50, 1792927.50 ), QPointF( 661574.50, 1792926.50 ), QPointF( 661572.50, 1792926.50 ), QPointF( 661572.50, 1792924.50 ),
    QPointF( 661573.50, 1792924.50 ), QPointF( 661573.50, 1792921.50 ), QPointF( 661572.50, 1792921.50 ), QPointF( 661572.50, 1792920.50 ), QPointF( 661571.50, 1792920.50 ), QPointF( 661571.50, 1792919.50 ), QPointF( 661570.50, 1792919.50 ), QPointF( 661570.50, 1792918.50 ), QPointF( 661569.50, 1792918.50 ), QPointF( 661569.50, 1792917.50 ),
    QPointF( 661568.50, 1792917.50 ), QPointF( 661568.50, 1792916.50 ), QPointF( 661567.50, 1792916.50 ), QPointF( 661567.50, 1792914.50 ), QPointF( 661565.50, 1792914.50 ), QPointF( 661565.50, 1792913.50 ), QPointF( 661564.50, 1792913.50 ), QPointF( 661564.50, 1792912.50 ), QPointF( 661563.50, 1792912.50 ), QPointF( 661563.50, 1792910.50 ),
    QPointF( 661565.50, 1792910.50 ), QPointF( 661565.50, 1792909.50 ), QPointF( 661567.50, 1792909.50 ), QPointF( 661567.50, 1792908.50 ), QPointF( 661572.50, 1792908.50 ), QPointF( 661572.50, 1792904.50 ), QPointF( 661574.50, 1792904.50 ), QPointF( 661574.50, 1792902.50 ), QPointF( 661576.50, 1792902.50 ), QPointF( 661576.50, 1792900.50 ),
    QPointF( 661577.50, 1792900.50 ), QPointF( 661577.50, 1792899.50 ), QPointF( 661576.50, 1792899.50 ), QPointF( 661576.50, 1792898.50 ), QPointF( 661575.50, 1792898.50 ), QPointF( 661575.50, 1792893.50 ), QPointF( 661579.50, 1792893.50 ), QPointF( 661579.50, 1792892.50 ), QPointF( 661581.50, 1792892.50 ), QPointF( 661581.50, 1792891.50 ),
    QPointF( 661580.50, 1792891.50 ), QPointF( 661580.50, 1792890.50 ), QPointF( 661579.50, 1792890.50 ), QPointF( 661579.50, 1792888.50 ), QPointF( 661580.50, 1792888.50 ), QPointF( 661580.50, 1792885.50 ), QPointF( 661581.50, 1792885.50 ), QPointF( 661581.50, 1792882.50 ), QPointF( 661583.50, 1792882.50 ), QPointF( 661583.50, 1792879.50 ),
    QPointF( 661584.50, 1792879.50 ), QPointF( 661584.50, 1792878.50 ), QPointF( 661585.50, 1792878.50 ), QPointF( 661585.50, 1792873.50 ), QPointF( 661584.50, 1792873.50 ), QPointF( 661584.50, 1792871.50 ), QPointF( 661582.50, 1792871.50 ), QPointF( 661582.50, 1792870.50 ), QPointF( 661581.50, 1792870.50 ), QPointF( 661581.50, 1792866.50 ),
    QPointF( 661582.50, 1792866.50 ), QPointF( 661582.50, 1792859.50 ), QPointF( 661583.50, 1792859.50 ), QPointF( 661583.50, 1792857.50 ), QPointF( 661582.50, 1792857.50 ), QPointF( 661582.50, 1792853.50 ), QPointF( 661584.50, 1792853.50 ), QPointF( 661584.50, 1792849.50 ), QPointF( 661585.50, 1792849.50 ), QPointF( 661585.50, 1792846.50 ),
    QPointF( 661584.50, 1792846.50 ), QPointF( 661584.50, 1792845.50 ), QPointF( 661585.50, 1792845.50 ), QPointF( 661585.50, 1792840.50 ), QPointF( 661584.50, 1792840.50 ), QPointF( 661584.50, 1792839.50 ), QPointF( 661585.50, 1792839.50 ), QPointF( 661585.50, 1792838.50 ), QPointF( 661586.50, 1792838.50 ), QPointF( 661586.50, 1792835.50 ),
    QPointF( 661585.50, 1792835.50 ), QPointF( 661585.50, 1792834.50 ), QPointF( 661584.50, 1792834.50 ), QPointF( 661584.50, 1792833.50 ), QPointF( 661583.50, 1792833.50 ), QPointF( 661583.50, 1792831.50 ), QPointF( 661584.50, 1792831.50 ), QPointF( 661584.50, 1792830.50 ), QPointF( 661585.50, 1792830.50 ), QPointF( 661585.50, 1792829.50 ),
    QPointF( 661586.50, 1792829.50 ), QPointF( 661586.50, 1792828.50 ), QPointF( 661585.50, 1792828.50 ), QPointF( 661585.50, 1792825.50 ), QPointF( 661586.50, 1792825.50 ), QPointF( 661586.50, 1792824.50 ), QPointF( 661587.50, 1792824.50 ), QPointF( 661587.50, 1792823.50 ), QPointF( 661586.50, 1792823.50 ), QPointF( 661586.50, 1792820.50 ),
    QPointF( 661588.50, 1792820.50 ), QPointF( 661588.50, 1792817.50 ), QPointF( 661587.50, 1792817.50 ), QPointF( 661587.50, 1792816.50 ), QPointF( 661588.50, 1792816.50 ), QPointF( 661588.50, 1792815.50 ), QPointF( 661589.50, 1792815.50 ), QPointF( 661589.50, 1792814.50 ), QPointF( 661591.50, 1792814.50 ), QPointF( 661591.50, 1792813.50 ),
    QPointF( 661597.50, 1792813.50 ), QPointF( 661597.50, 1792814.50 ), QPointF( 661601.50, 1792814.50 ), QPointF( 661601.50, 1792813.50 ), QPointF( 661602.50, 1792813.50 ), QPointF( 661602.50, 1792810.50 ), QPointF( 661601.50, 1792810.50 ), QPointF( 661601.50, 1792809.50 ), QPointF( 661602.50, 1792809.50 ), QPointF( 661602.50, 1792806.50 ),
    QPointF( 661601.50, 1792806.50 ), QPointF( 661601.50, 1792799.50 ), QPointF( 661600.50, 1792799.50 ), QPointF( 661600.50, 1792797.50 ), QPointF( 661602.50, 1792797.50 ), QPointF( 661602.50, 1792795.50 ), QPointF( 661601.50, 1792795.50 ), QPointF( 661601.50, 1792794.50 ), QPointF( 661602.50, 1792794.50 ), QPointF( 661602.50, 1792792.50 ),
    QPointF( 661603.50, 1792792.50 ), QPointF( 661603.50, 1792791.50 ), QPointF( 661604.50, 1792791.50 ), QPointF( 661604.50, 1792790.50 ), QPointF( 661606.50, 1792790.50 ), QPointF( 661606.50, 1792787.50 ), QPointF( 661609.50, 1792787.50 ), QPointF( 661609.50, 1792786.50 ), QPointF( 661610.50, 1792786.50 ), QPointF( 661610.50, 1792784.50 ),
    QPointF( 661611.50, 1792784.50 ), QPointF( 661611.50, 1792783.50 ), QPointF( 661613.50, 1792783.50 ), QPointF( 661613.50, 1792782.50 ), QPointF( 661614.50, 1792782.50 ), QPointF( 661614.50, 1792781.50 ), QPointF( 661618.50, 1792781.50 ), QPointF( 661618.50, 1792779.50 ), QPointF( 661619.50, 1792779.50 ), QPointF( 661619.50, 1792778.50 ),
    QPointF( 661620.50, 1792778.50 ), QPointF( 661620.50, 1792777.50 ), QPointF( 661622.50, 1792777.50 ), QPointF( 661622.50, 1792776.50 ), QPointF( 661623.50, 1792776.50 ), QPointF( 661623.50, 1792775.50 ), QPointF( 661624.50, 1792775.50 ), QPointF( 661624.50, 1792774.50 ), QPointF( 661627.50, 1792774.50 ), QPointF( 661627.50, 1792775.50 ),
    QPointF( 661628.50, 1792775.50 ), QPointF( 661628.50, 1792772.50 ), QPointF( 661629.50, 1792772.50 ), QPointF( 661629.50, 1792771.50 ), QPointF( 661631.50, 1792771.50 ), QPointF( 661631.50, 1792770.50 ), QPointF( 661636.50, 1792770.50 ), QPointF( 661636.50, 1792769.50 ), QPointF( 661637.50, 1792769.50 ), QPointF( 661637.50, 1792770.50 ),
    QPointF( 661638.50, 1792770.50 ), QPointF( 661638.50, 1792769.50 ), QPointF( 661642.50, 1792769.50 ), QPointF( 661642.50, 1792767.50 ), QPointF( 661644.50, 1792767.50 ), QPointF( 661644.50, 1792766.50 ), QPointF( 661646.50, 1792766.50 ), QPointF( 661646.50, 1792767.50 ), QPointF( 661647.50, 1792767.50 ), QPointF( 661647.50, 1792764.50 ),
    QPointF( 661648.50, 1792764.50 ), QPointF( 661648.50, 1792763.50 ), QPointF( 661649.50, 1792763.50 ), QPointF( 661649.50, 1792762.50 ), QPointF( 661650.50, 1792762.50 ), QPointF( 661650.50, 1792761.50 ), QPointF( 661649.50, 1792761.50 ), QPointF( 661649.50, 1792758.50 ), QPointF( 661650.50, 1792758.50 ), QPointF( 661650.50, 1792757.50 ),
    QPointF( 661651.50, 1792757.50 ), QPointF( 661651.50, 1792756.50 ), QPointF( 661652.50, 1792756.50 ), QPointF( 661652.50, 1792755.50 ), QPointF( 661653.50, 1792755.50 ), QPointF( 661653.50, 1792754.50 ), QPointF( 661654.50, 1792754.50 ), QPointF( 661654.50, 1792753.50 ), QPointF( 661655.50, 1792753.50 ), QPointF( 661655.50, 1792752.50 ),
    QPointF( 661656.50, 1792752.50 ), QPointF( 661656.50, 1792751.50 ), QPointF( 661657.50, 1792751.50 ), QPointF( 661657.50, 1792750.50 ), QPointF( 661658.50, 1792750.50 ), QPointF( 661658.50, 1792749.50 ), QPointF( 661659.50, 1792749.50 ), QPointF( 661659.50, 1792748.50 ), QPointF( 661660.50, 1792748.50 ), QPointF( 661660.50, 1792747.50 ),
    QPointF( 661661.50, 1792747.50 ), QPointF( 661661.50, 1792746.50 ), QPointF( 661662.50, 1792746.50 ), QPointF( 661662.50, 1792745.50 ), QPointF( 661663.50, 1792745.50 ), QPointF( 661663.50, 1792743.50 ), QPointF( 661664.50, 1792743.50 ), QPointF( 661664.50, 1792742.50 ), QPointF( 661665.50, 1792742.50 ), QPointF( 661665.50, 1792741.50 ),
    QPointF( 661666.50, 1792741.50 ), QPointF( 661666.50, 1792740.50 ), QPointF( 661667.50, 1792740.50 ), QPointF( 661667.50, 1792739.50 ), QPointF( 661666.50, 1792739.50 ), QPointF( 661666.50, 1792737.50 ), QPointF( 661665.50, 1792737.50 ), QPointF( 661665.50, 1792736.50 ), QPointF( 661664.50, 1792736.50 ), QPointF( 661664.50, 1792735.50 ),
    QPointF( 661663.50, 1792735.50 ), QPointF( 661663.50, 1792734.50 ), QPointF( 661662.50, 1792734.50 ), QPointF( 661662.50, 1792733.50 ), QPointF( 661675.50, 1792733.50 ), QPointF( 661675.50, 1792734.50 ), QPointF( 661676.50, 1792734.50 ), QPointF( 661676.50, 1792735.50 ), QPointF( 661677.50, 1792735.50 ), QPointF( 661677.50, 1792736.50 ),
    QPointF( 661678.50, 1792736.50 ), QPointF( 661678.50, 1792735.50 ), QPointF( 661680.50, 1792735.50 ), QPointF( 661680.50, 1792733.50 ), QPointF( 661684.50, 1792733.50 ), QPointF( 661684.50, 1792736.50 ), QPointF( 661685.50, 1792736.50 ), QPointF( 661685.50, 1792737.50 ), QPointF( 661686.50, 1792737.50 ), QPointF( 661686.50, 1792736.50 ),
    QPointF( 661687.50, 1792736.50 ), QPointF( 661687.50, 1792737.50 ), QPointF( 661688.50, 1792737.50 ), QPointF( 661688.50, 1792738.50 ), QPointF( 661689.50, 1792738.50 ), QPointF( 661689.50, 1792740.50 ), QPointF( 661690.50, 1792740.50 ), QPointF( 661690.50, 1792741.50 ), QPointF( 661691.50, 1792741.50 ), QPointF( 661691.50, 1792742.50 ),
    QPointF( 661692.50, 1792742.50 ), QPointF( 661692.50, 1792741.50 ), QPointF( 661696.50, 1792741.50 ), QPointF( 661696.50, 1792743.50 ), QPointF( 661697.50, 1792743.50 ), QPointF( 661697.50, 1792744.50 ), QPointF( 661698.50, 1792744.50 ), QPointF( 661698.50, 1792743.50 ), QPointF( 661706.50, 1792743.50 ), QPointF( 661706.50, 1792744.50 ),
    QPointF( 661708.50, 1792744.50 ), QPointF( 661708.50, 1792745.50 ), QPointF( 661712.50, 1792745.50 ), QPointF( 661712.50, 1792744.50 ), QPointF( 661717.50, 1792744.50 ), QPointF( 661717.50, 1792743.50 ), QPointF( 661719.50, 1792743.50 ), QPointF( 661719.50, 1792748.50 ), QPointF( 661718.50, 1792748.50 ), QPointF( 661718.50, 1792750.50 ),
    QPointF( 661721.50, 1792750.50 ), QPointF( 661721.50, 1792751.50 ), QPointF( 661725.50, 1792751.50 ), QPointF( 661725.50, 1792750.50 ), QPointF( 661730.50, 1792750.50 ), QPointF( 661730.50, 1792749.50 ), QPointF( 661731.50, 1792749.50 ), QPointF( 661731.50, 1792748.50 ), QPointF( 661733.50, 1792748.50 ), QPointF( 661733.50, 1792749.50 ),
    QPointF( 661734.50, 1792749.50 ), QPointF( 661734.50, 1792752.50 ), QPointF( 661736.50, 1792752.50 ), QPointF( 661736.50, 1792753.50 ), QPointF( 661738.50, 1792753.50 ), QPointF( 661738.50, 1792752.50 ), QPointF( 661741.50, 1792752.50 ), QPointF( 661741.50, 1792751.50 ), QPointF( 661743.50, 1792751.50 ), QPointF( 661743.50, 1792752.50 ),
    QPointF( 661746.50, 1792752.50 ), QPointF( 661746.50, 1792753.50 ), QPointF( 661747.50, 1792753.50 ), QPointF( 661747.50, 1792754.50 ), QPointF( 661748.50, 1792754.50 ), QPointF( 661748.50, 1792755.50 ), QPointF( 661749.50, 1792755.50 ), QPointF( 661749.50, 1792756.50 ), QPointF( 661748.50, 1792756.50 ), QPointF( 661748.50, 1792757.50 ),
    QPointF( 661746.50, 1792757.50 ), QPointF( 661746.50, 1792758.50 ), QPointF( 661745.50, 1792758.50 ), QPointF( 661745.50, 1792759.50 ), QPointF( 661746.50, 1792759.50 ), QPointF( 661746.50, 1792760.50 ), QPointF( 661747.50, 1792760.50 ), QPointF( 661747.50, 1792761.50 ), QPointF( 661748.50, 1792761.50 ), QPointF( 661748.50, 1792762.50 ),
    QPointF( 661749.50, 1792762.50 ), QPointF( 661749.50, 1792763.50 ), QPointF( 661752.50, 1792763.50 ), QPointF( 661752.50, 1792764.50 ), QPointF( 661753.50, 1792764.50 ), QPointF( 661753.50, 1792765.50 ), QPointF( 661754.50, 1792765.50 ), QPointF( 661754.50, 1792764.50 ), QPointF( 661759.50, 1792764.50 ), QPointF( 661759.50, 1792768.50 ),
    QPointF( 661758.50, 1792768.50 ), QPointF( 661758.50, 1792769.50 ), QPointF( 661757.50, 1792769.50 ), QPointF( 661757.50, 1792774.50 ), QPointF( 661759.50, 1792774.50 ), QPointF( 661759.50, 1792775.50 ), QPointF( 661760.50, 1792775.50 ), QPointF( 661760.50, 1792778.50 ), QPointF( 661761.50, 1792778.50 ), QPointF( 661761.50, 1792780.50 ),
    QPointF( 661760.50, 1792780.50 ), QPointF( 661760.50, 1792784.50 ), QPointF( 661761.50, 1792784.50 ), QPointF( 661761.50, 1792787.50 ), QPointF( 661762.50, 1792787.50 ), QPointF( 661762.50, 1792793.50 ), QPointF( 661761.50, 1792793.50 ), QPointF( 661761.50, 1792795.50 ), QPointF( 661760.50, 1792795.50 ), QPointF( 661760.50, 1792796.50 ),
    QPointF( 661759.50, 1792796.50 ), QPointF( 661759.50, 1792797.50 ), QPointF( 661758.50, 1792797.50 ), QPointF( 661758.50, 1792802.50 ), QPointF( 661757.50, 1792802.50 ), QPointF( 661757.50, 1792803.50 ), QPointF( 661755.50, 1792803.50 ), QPointF( 661755.50, 1792807.50 ), QPointF( 661754.50, 1792807.50 ), QPointF( 661754.50, 1792808.50 ),
    QPointF( 661753.50, 1792808.50 ), QPointF( 661753.50, 1792809.50 ), QPointF( 661752.50, 1792809.50 ), QPointF( 661752.50, 1792810.50 ), QPointF( 661753.50, 1792810.50 ), QPointF( 661753.50, 1792812.50 ), QPointF( 661755.50, 1792812.50 ), QPointF( 661755.50, 1792813.50 ), QPointF( 661756.50, 1792813.50 ), QPointF( 661756.50, 1792814.50 ),
    QPointF( 661755.50, 1792814.50 ), QPointF( 661755.50, 1792815.50 ), QPointF( 661754.50, 1792815.50 ), QPointF( 661754.50, 1792819.50 ), QPointF( 661753.50, 1792819.50 ), QPointF( 661753.50, 1792823.50 ), QPointF( 661752.50, 1792823.50 ), QPointF( 661752.50, 1792822.50 ), QPointF( 661749.50, 1792822.50 ), QPointF( 661749.50, 1792823.50 ),
    QPointF( 661748.50, 1792823.50 ), QPointF( 661748.50, 1792822.50 ), QPointF( 661745.50, 1792822.50 ), QPointF( 661745.50, 1792821.50 ), QPointF( 661744.50, 1792821.50 ), QPointF( 661744.50, 1792826.50 ), QPointF( 661743.50, 1792826.50 ), QPointF( 661743.50, 1792830.50 ), QPointF( 661744.50, 1792830.50 ), QPointF( 661744.50, 1792832.50 ),
    QPointF( 661745.50, 1792832.50 ), QPointF( 661745.50, 1792833.50 ), QPointF( 661744.50, 1792833.50 ), QPointF( 661744.50, 1792836.50 ), QPointF( 661745.50, 1792836.50 ), QPointF( 661745.50, 1792838.50 ), QPointF( 661744.50, 1792838.50 ), QPointF( 661744.50, 1792839.50 ), QPointF( 661743.50, 1792839.50 ), QPointF( 661743.50, 1792840.50 ),
    QPointF( 661742.50, 1792840.50 ), QPointF( 661742.50, 1792841.50 ), QPointF( 661741.50, 1792841.50 ), QPointF( 661741.50, 1792842.50 ), QPointF( 661740.50, 1792842.50 ), QPointF( 661740.50, 1792845.50 ), QPointF( 661741.50, 1792845.50 ), QPointF( 661741.50, 1792846.50 ), QPointF( 661742.50, 1792846.50 ), QPointF( 661742.50, 1792845.50 ),
    QPointF( 661745.50, 1792845.50 ), QPointF( 661745.50, 1792846.50 ), QPointF( 661749.50, 1792846.50 ), QPointF( 661749.50, 1792847.50 ), QPointF( 661750.50, 1792847.50 ), QPointF( 661750.50, 1792848.50 ), QPointF( 661751.50, 1792848.50 ), QPointF( 661751.50, 1792849.50 ), QPointF( 661752.50, 1792849.50 ), QPointF( 661752.50, 1792852.50 ),
    QPointF( 661751.50, 1792852.50 ), QPointF( 661751.50, 1792853.50 ), QPointF( 661752.50, 1792853.50 ), QPointF( 661752.50, 1792854.50 ), QPointF( 661753.50, 1792854.50 ), QPointF( 661753.50, 1792855.50 ), QPointF( 661754.50, 1792855.50 ), QPointF( 661754.50, 1792856.50 ), QPointF( 661756.50, 1792856.50 ), QPointF( 661756.50, 1792860.50 ),
    QPointF( 661755.50, 1792860.50 ), QPointF( 661755.50, 1792861.50 ), QPointF( 661754.50, 1792861.50 ), QPointF( 661754.50, 1792862.50 ), QPointF( 661753.50, 1792862.50 ), QPointF( 661753.50, 1792863.50 ), QPointF( 661752.50, 1792863.50 ), QPointF( 661752.50, 1792864.50 ), QPointF( 661753.50, 1792864.50 ), QPointF( 661753.50, 1792867.50 ),
    QPointF( 661754.50, 1792867.50 ), QPointF( 661754.50, 1792868.50 ), QPointF( 661756.50, 1792868.50 ), QPointF( 661756.50, 1792870.50 ), QPointF( 661757.50, 1792870.50 ), QPointF( 661757.50, 1792872.50 ), QPointF( 661752.50, 1792872.50 ), QPointF( 661752.50, 1792873.50 ), QPointF( 661753.50, 1792873.50 ), QPointF( 661753.50, 1792874.50 ),
    QPointF( 661754.50, 1792874.50 ), QPointF( 661754.50, 1792875.50 ), QPointF( 661755.50, 1792875.50 ), QPointF( 661755.50, 1792876.50 ), QPointF( 661756.50, 1792876.50 ), QPointF( 661756.50, 1792877.50 ), QPointF( 661757.50, 1792877.50 ), QPointF( 661757.50, 1792878.50 ), QPointF( 661758.50, 1792878.50 ), QPointF( 661758.50, 1792879.50 ),
    QPointF( 661759.50, 1792879.50 ), QPointF( 661759.50, 1792884.50 ), QPointF( 661760.50, 1792884.50 ), QPointF( 661760.50, 1792885.50 ), QPointF( 661759.50, 1792885.50 ), QPointF( 661759.50, 1792888.50 ), QPointF( 661760.50, 1792888.50 ), QPointF( 661760.50, 1792887.50 ), QPointF( 661761.50, 1792887.50 ), QPointF( 661761.50, 1792888.50 ),
    QPointF( 661762.50, 1792888.50 ), QPointF( 661762.50, 1792889.50 ), QPointF( 661763.50, 1792889.50 ), QPointF( 661763.50, 1792899.50 ), QPointF( 661762.50, 1792899.50 ), QPointF( 661762.50, 1792905.50 ), QPointF( 661756.50, 1792905.50 ), QPointF( 661756.50, 1792906.50 ), QPointF( 661754.50, 1792906.50 ), QPointF( 661754.50, 1792907.50 ),
    QPointF( 661751.50, 1792907.50 ), QPointF( 661751.50, 1792908.50 ), QPointF( 661752.50, 1792908.50 ), QPointF( 661752.50, 1792909.50 ), QPointF( 661753.50, 1792909.50 ), QPointF( 661753.50, 1792912.50 ), QPointF( 661756.50, 1792912.50 ), QPointF( 661756.50, 1792913.50 ), QPointF( 661757.50, 1792913.50 ), QPointF( 661757.50, 1792914.50 ),
    QPointF( 661758.50, 1792914.50 ), QPointF( 661758.50, 1792920.50 ), QPointF( 661757.50, 1792920.50 ), QPointF( 661757.50, 1792923.50 ), QPointF( 661760.50, 1792923.50 ), QPointF( 661760.50, 1792922.50 ), QPointF( 661762.50, 1792922.50 ), QPointF( 661762.50, 1792921.50 ), QPointF( 661763.50, 1792921.50 ), QPointF( 661763.50, 1792920.50 ),
    QPointF( 661765.50, 1792920.50 ), QPointF( 661765.50, 1792924.50 ), QPointF( 661764.50, 1792924.50 ), QPointF( 661764.50, 1792925.50 ), QPointF( 661762.50, 1792925.50 ), QPointF( 661762.50, 1792927.50 ), QPointF( 661763.50, 1792927.50 ), QPointF( 661763.50, 1792928.50 ), QPointF( 661766.50, 1792928.50 ), QPointF( 661766.50, 1792929.50 ),
    QPointF( 661767.50, 1792929.50 ), QPointF( 661767.50, 1792932.50 ), QPointF( 661768.50, 1792932.50 ), QPointF( 661768.50, 1792934.50 ), QPointF( 661767.50, 1792934.50 ), QPointF( 661767.50, 1792935.50 ), QPointF( 661766.50, 1792935.50 ), QPointF( 661766.50, 1792936.50 ), QPointF( 661765.50, 1792936.50 ), QPointF( 661765.50, 1792937.50 ),
    QPointF( 661764.50, 1792937.50 ), QPointF( 661764.50, 1792938.50 ), QPointF( 661763.50, 1792938.50 ), QPointF( 661763.50, 1792939.50 ), QPointF( 661762.50, 1792939.50 ), QPointF( 661762.50, 1792940.50 ), QPointF( 661761.50, 1792940.50 ), QPointF( 661761.50, 1792941.50 ), QPointF( 661760.50, 1792941.50 ), QPointF( 661760.50, 1792942.50 ),
    QPointF( 661759.50, 1792942.50 ), QPointF( 661759.50, 1792943.50 ), QPointF( 661757.50, 1792943.50 ), QPointF( 661757.50, 1792945.50 ), QPointF( 661756.50, 1792945.50 ), QPointF( 661756.50, 1792947.50 ), QPointF( 661755.50, 1792947.50 ), QPointF( 661755.50, 1792948.50 ), QPointF( 661754.50, 1792948.50 ), QPointF( 661754.50, 1792949.50 ),
    QPointF( 661753.50, 1792949.50 ), QPointF( 661753.50, 1792950.50 ), QPointF( 661690.50, 1792950.50 ), QPointF( 661690.50, 1792951.50 ), QPointF( 661687.50, 1792951.50 ), QPointF( 661687.50, 1792952.50 ), QPointF( 661685.50, 1792952.50 ), QPointF( 661685.50, 1792953.50 ), QPointF( 661682.50, 1792953.50 ), QPointF( 661682.50, 1792954.50 ),
    QPointF( 661681.50, 1792954.50 ), QPointF( 661681.50, 1792953.50 ), QPointF( 661680.50, 1792953.50 ), QPointF( 661680.50, 1792952.50 ), QPointF( 661679.50, 1792952.50 ), QPointF( 661679.50, 1792953.50 ), QPointF( 661678.50, 1792953.50 ), QPointF( 661678.50, 1792954.50 ), QPointF( 661677.50, 1792954.50 ), QPointF( 661677.50, 1792953.50 ),
    QPointF( 661676.50, 1792953.50 ), QPointF( 661676.50, 1792952.50 ), QPointF( 661675.50, 1792952.50 ), QPointF( 661675.50, 1792951.50 ), QPointF( 661674.50, 1792951.50 ), QPointF( 661674.50, 1792950.50 ), QPointF( 661669.50, 1792950.50 ), QPointF( 661669.50, 1792951.50 ), QPointF( 661667.50, 1792951.50 ), QPointF( 661667.50, 1792950.50 ),
    QPointF( 661599.50, 1792950.50 )} );

  QCOMPARE( polygonWatershed, polygonWatershedTest );

  QPolygonF streamLine = watershedDelineating.lastStreamLine();

  QPolygonF streamLinetest( {QPointF( 661675.00, 1792734.00 ), QPointF( 661680.00, 1792739.00 ), QPointF( 661681.00, 1792739.00 ), QPointF( 661682.00, 1792740.00 ), QPointF( 661682.00, 1792742.00 ), QPointF( 661681.00, 1792743.00 ), QPointF( 661681.00, 1792745.00 ), QPointF( 661679.00, 1792747.00 ), QPointF( 661681.00, 1792749.00 ), QPointF( 661681.00, 1792750.00 ),
                             QPointF( 661682.00, 1792751.00 ), QPointF( 661681.00, 1792752.00 ), QPointF( 661681.00, 1792753.00 ), QPointF( 661683.00, 1792755.00 ), QPointF( 661680.00, 1792758.00 ), QPointF( 661679.00, 1792757.00 ), QPointF( 661677.00, 1792757.00 ), QPointF( 661676.00, 1792758.00 ), QPointF( 661670.00, 1792758.00 ), QPointF( 661669.00, 1792759.00 ),
                             QPointF( 661668.00, 1792759.00 ), QPointF( 661667.00, 1792760.00 ), QPointF( 661664.00, 1792760.00 ), QPointF( 661663.00, 1792761.00 ), QPointF( 661662.00, 1792760.00 ), QPointF( 661661.00, 1792761.00 ), QPointF( 661659.00, 1792761.00 ), QPointF( 661658.00, 1792762.00 ), QPointF( 661654.00, 1792762.00 ), QPointF( 661653.00, 1792763.00 ),
                             QPointF( 661653.00, 1792765.00 ), QPointF( 661650.00, 1792768.00 ), QPointF( 661650.00, 1792769.00 ), QPointF( 661649.00, 1792770.00 ), QPointF( 661649.00, 1792773.00 ), QPointF( 661645.00, 1792777.00 ), QPointF( 661645.00, 1792778.00 ), QPointF( 661646.00, 1792779.00 ), QPointF( 661642.00, 1792783.00 ), QPointF( 661643.00, 1792784.00 ),
                             QPointF( 661643.00, 1792785.00 ), QPointF( 661652.00, 1792794.00 ), QPointF( 661653.00, 1792794.00 ), QPointF( 661654.00, 1792795.00 ), QPointF( 661656.00, 1792795.00 ), QPointF( 661658.00, 1792797.00 ), QPointF( 661658.00, 1792799.00 ), QPointF( 661664.00, 1792805.00 ), QPointF( 661664.00, 1792806.00 ), QPointF( 661658.00, 1792812.00 ),
                             QPointF( 661663.00, 1792817.00 ), QPointF( 661665.00, 1792817.00 ), QPointF( 661673.00, 1792825.00 ), QPointF( 661673.00, 1792826.00 ), QPointF( 661675.00, 1792828.00 ), QPointF( 661674.00, 1792829.00 ), QPointF( 661674.00, 1792831.00 ), QPointF( 661675.00, 1792832.00 ), QPointF( 661675.00, 1792833.00 ), QPointF( 661672.00, 1792836.00 ),
                             QPointF( 661672.00, 1792839.00 ), QPointF( 661671.00, 1792840.00 ), QPointF( 661671.00, 1792841.00 ), QPointF( 661670.00, 1792842.00 ), QPointF( 661670.00, 1792843.00 ), QPointF( 661671.00, 1792844.00 ), QPointF( 661671.00, 1792845.00 ), QPointF( 661670.00, 1792846.00 ), QPointF( 661670.00, 1792849.00 ), QPointF( 661671.00, 1792850.00 ),
                             QPointF( 661669.00, 1792852.00 ), QPointF( 661670.00, 1792853.00 ), QPointF( 661672.00, 1792853.00 ), QPointF( 661674.00, 1792855.00 ), QPointF( 661674.00, 1792857.00 ), QPointF( 661673.00, 1792858.00 ), QPointF( 661672.00, 1792858.00 ), QPointF( 661671.00, 1792859.00 ), QPointF( 661671.00, 1792860.00 ), QPointF( 661667.00, 1792864.00 ),
                             QPointF( 661667.00, 1792865.00 ), QPointF( 661666.00, 1792866.00 ), QPointF( 661666.00, 1792867.00 ), QPointF( 661665.00, 1792868.00 ), QPointF( 661665.00, 1792870.00 ), QPointF( 661664.00, 1792871.00 ), QPointF( 661664.00, 1792875.00 ), QPointF( 661665.00, 1792876.00 ), QPointF( 661665.00, 1792879.00 ), QPointF( 661669.00, 1792883.00 ),
                             QPointF( 661669.00, 1792884.00 ), QPointF( 661670.00, 1792885.00 ), QPointF( 661670.00, 1792886.00 ), QPointF( 661669.00, 1792887.00 ), QPointF( 661669.00, 1792891.00 ), QPointF( 661671.00, 1792893.00 ), QPointF( 661671.00, 1792895.00 ), QPointF( 661675.00, 1792899.00 ), QPointF( 661673.00, 1792901.00 ), QPointF( 661673.00, 1792902.00 ),
                             QPointF( 661668.00, 1792907.00 ), QPointF( 661668.00, 1792909.00 ), QPointF( 661673.00, 1792914.00 ), QPointF( 661673.00, 1792917.00 ), QPointF( 661674.00, 1792918.00 ), QPointF( 661673.00, 1792919.00 ), QPointF( 661673.00, 1792923.00 ), QPointF( 661672.00, 1792924.00 ), QPointF( 661672.00, 1792925.00 ), QPointF( 661668.00, 1792929.00 ),
                             QPointF( 661670.00, 1792931.00 ), QPointF( 661670.00, 1792933.00 ), QPointF( 661668.00, 1792935.00 ), QPointF( 661668.00, 1792936.00 ), QPointF( 661666.00, 1792938.00 ), QPointF( 661666.00, 1792941.00 ), QPointF( 661665.00, 1792942.00 ), QPointF( 661664.00, 1792941.00 ), QPointF( 661663.00, 1792942.00 ), QPointF( 661663.00, 1792944.00 ),
                             QPointF( 661660.00, 1792947.00 ), QPointF( 661660.00, 1792948.00 ), QPointF( 661659.00, 1792949.00 ), QPointF( 661659.00, 1792950.00 )} );

  QCOMPARE( streamLine, streamLinetest );

  //! Watershed exceed predfined extent--> predefined extent not valid, need to set another one
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingWithBroughtBackExtent );

  //! Restart with a bigger extent
  extent = ReosMapExtent( 661553.33, 1792690, 661780.44, 1792964.54 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  bool needAdjusting;
  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  watershedDelineating.storeWatershed( true );
  QVERIFY( watershedStore.watershedCount() == 1 );
  QVERIFY( watershedStore.masterWatershedCount() == 1 );
  QVERIFY( itemModel.rowCount( QModelIndex() ) == 1 );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForDownstream );

  //! Attempt to delineate an upstream watershed
  downstreamLine.clear();
  //! Partially contained line --> not possible
  downstreamLine << QPointF( 661560, 1792750 ) << QPointF( 661760, 1792900 );

  QVERIFY( !watershedDelineating.setDownstreamLine( downstreamLine ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForDownstream );

  downstreamLine.clear();
  downstreamLine << QPointF( 661637, 1792840 ) << QPointF( 661703.5, 1792843.86 );

  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );

  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();

  polygonWatershed = watershedDelineating.lastWatershedDelineated();

  polygonWatershedTest = QPolygonF(
  {
    QPointF( 661658.50, 1792842.50 ), QPointF( 661658.50, 1792841.50 ), QPointF( 661656.50, 1792841.50 ), QPointF( 661656.50, 1792842.50 ), QPointF( 661655.50, 1792842.50 ), QPointF( 661655.50, 1792843.50 ), QPointF( 661653.50, 1792843.50 ), QPointF( 661653.50, 1792842.50 ), QPointF( 661652.50, 1792842.50 ), QPointF( 661652.50, 1792841.50 ),
    QPointF( 661645.50, 1792841.50 ), QPointF( 661645.50, 1792840.50 ), QPointF( 661636.50, 1792840.50 ), QPointF( 661636.50, 1792839.50 ), QPointF( 661635.50, 1792839.50 ), QPointF( 661635.50, 1792838.50 ), QPointF( 661634.50, 1792838.50 ), QPointF( 661634.50, 1792837.50 ), QPointF( 661633.50, 1792837.50 ), QPointF( 661633.50, 1792836.50 ),
    QPointF( 661630.50, 1792836.50 ), QPointF( 661630.50, 1792835.50 ), QPointF( 661628.50, 1792835.50 ), QPointF( 661628.50, 1792836.50 ), QPointF( 661625.50, 1792836.50 ), QPointF( 661625.50, 1792835.50 ), QPointF( 661622.50, 1792835.50 ), QPointF( 661622.50, 1792834.50 ), QPointF( 661621.50, 1792834.50 ), QPointF( 661621.50, 1792833.50 ),
    QPointF( 661619.50, 1792833.50 ), QPointF( 661619.50, 1792832.50 ), QPointF( 661618.50, 1792832.50 ), QPointF( 661618.50, 1792831.50 ), QPointF( 661613.50, 1792831.50 ), QPointF( 661613.50, 1792830.50 ), QPointF( 661611.50, 1792830.50 ), QPointF( 661611.50, 1792828.50 ), QPointF( 661610.50, 1792828.50 ), QPointF( 661610.50, 1792827.50 ),
    QPointF( 661607.50, 1792827.50 ), QPointF( 661607.50, 1792826.50 ), QPointF( 661606.50, 1792826.50 ), QPointF( 661606.50, 1792827.50 ), QPointF( 661602.50, 1792827.50 ), QPointF( 661602.50, 1792826.50 ), QPointF( 661600.50, 1792826.50 ), QPointF( 661600.50, 1792825.50 ), QPointF( 661599.50, 1792825.50 ), QPointF( 661599.50, 1792824.50 ),
    QPointF( 661598.50, 1792824.50 ), QPointF( 661598.50, 1792823.50 ), QPointF( 661599.50, 1792823.50 ), QPointF( 661599.50, 1792822.50 ), QPointF( 661600.50, 1792822.50 ), QPointF( 661600.50, 1792820.50 ), QPointF( 661599.50, 1792820.50 ), QPointF( 661599.50, 1792818.50 ), QPointF( 661601.50, 1792818.50 ), QPointF( 661601.50, 1792813.50 ),
    QPointF( 661602.50, 1792813.50 ), QPointF( 661602.50, 1792810.50 ), QPointF( 661601.50, 1792810.50 ), QPointF( 661601.50, 1792809.50 ), QPointF( 661602.50, 1792809.50 ), QPointF( 661602.50, 1792806.50 ), QPointF( 661601.50, 1792806.50 ), QPointF( 661601.50, 1792799.50 ), QPointF( 661600.50, 1792799.50 ), QPointF( 661600.50, 1792797.50 ),
    QPointF( 661602.50, 1792797.50 ), QPointF( 661602.50, 1792795.50 ), QPointF( 661601.50, 1792795.50 ), QPointF( 661601.50, 1792794.50 ), QPointF( 661602.50, 1792794.50 ), QPointF( 661602.50, 1792792.50 ), QPointF( 661603.50, 1792792.50 ), QPointF( 661603.50, 1792791.50 ), QPointF( 661604.50, 1792791.50 ), QPointF( 661604.50, 1792790.50 ),
    QPointF( 661606.50, 1792790.50 ), QPointF( 661606.50, 1792787.50 ), QPointF( 661609.50, 1792787.50 ), QPointF( 661609.50, 1792786.50 ), QPointF( 661610.50, 1792786.50 ), QPointF( 661610.50, 1792784.50 ), QPointF( 661611.50, 1792784.50 ), QPointF( 661611.50, 1792783.50 ), QPointF( 661613.50, 1792783.50 ), QPointF( 661613.50, 1792782.50 ),
    QPointF( 661614.50, 1792782.50 ), QPointF( 661614.50, 1792781.50 ), QPointF( 661618.50, 1792781.50 ), QPointF( 661618.50, 1792779.50 ), QPointF( 661619.50, 1792779.50 ), QPointF( 661619.50, 1792778.50 ), QPointF( 661620.50, 1792778.50 ), QPointF( 661620.50, 1792777.50 ), QPointF( 661622.50, 1792777.50 ), QPointF( 661622.50, 1792776.50 ),
    QPointF( 661623.50, 1792776.50 ), QPointF( 661623.50, 1792775.50 ), QPointF( 661624.50, 1792775.50 ), QPointF( 661624.50, 1792774.50 ), QPointF( 661627.50, 1792774.50 ), QPointF( 661627.50, 1792775.50 ), QPointF( 661628.50, 1792775.50 ), QPointF( 661628.50, 1792772.50 ), QPointF( 661629.50, 1792772.50 ), QPointF( 661629.50, 1792771.50 ),
    QPointF( 661631.50, 1792771.50 ), QPointF( 661631.50, 1792770.50 ), QPointF( 661636.50, 1792770.50 ), QPointF( 661636.50, 1792769.50 ), QPointF( 661637.50, 1792769.50 ), QPointF( 661637.50, 1792770.50 ), QPointF( 661638.50, 1792770.50 ), QPointF( 661638.50, 1792769.50 ), QPointF( 661642.50, 1792769.50 ), QPointF( 661642.50, 1792767.50 ),
    QPointF( 661644.50, 1792767.50 ), QPointF( 661644.50, 1792766.50 ), QPointF( 661646.50, 1792766.50 ), QPointF( 661646.50, 1792767.50 ), QPointF( 661647.50, 1792767.50 ), QPointF( 661647.50, 1792764.50 ), QPointF( 661648.50, 1792764.50 ), QPointF( 661648.50, 1792763.50 ), QPointF( 661649.50, 1792763.50 ), QPointF( 661649.50, 1792762.50 ),
    QPointF( 661650.50, 1792762.50 ), QPointF( 661650.50, 1792761.50 ), QPointF( 661649.50, 1792761.50 ), QPointF( 661649.50, 1792758.50 ), QPointF( 661650.50, 1792758.50 ), QPointF( 661650.50, 1792757.50 ), QPointF( 661651.50, 1792757.50 ), QPointF( 661651.50, 1792756.50 ), QPointF( 661652.50, 1792756.50 ), QPointF( 661652.50, 1792755.50 ),
    QPointF( 661653.50, 1792755.50 ), QPointF( 661653.50, 1792754.50 ), QPointF( 661654.50, 1792754.50 ), QPointF( 661654.50, 1792753.50 ), QPointF( 661655.50, 1792753.50 ), QPointF( 661655.50, 1792752.50 ), QPointF( 661656.50, 1792752.50 ), QPointF( 661656.50, 1792751.50 ), QPointF( 661657.50, 1792751.50 ), QPointF( 661657.50, 1792750.50 ),
    QPointF( 661658.50, 1792750.50 ), QPointF( 661658.50, 1792749.50 ), QPointF( 661659.50, 1792749.50 ), QPointF( 661659.50, 1792748.50 ), QPointF( 661660.50, 1792748.50 ), QPointF( 661660.50, 1792747.50 ), QPointF( 661661.50, 1792747.50 ), QPointF( 661661.50, 1792746.50 ), QPointF( 661662.50, 1792746.50 ), QPointF( 661662.50, 1792745.50 ),
    QPointF( 661663.50, 1792745.50 ), QPointF( 661663.50, 1792743.50 ), QPointF( 661664.50, 1792743.50 ), QPointF( 661664.50, 1792742.50 ), QPointF( 661665.50, 1792742.50 ), QPointF( 661665.50, 1792741.50 ), QPointF( 661666.50, 1792741.50 ), QPointF( 661666.50, 1792740.50 ), QPointF( 661667.50, 1792740.50 ), QPointF( 661667.50, 1792739.50 ),
    QPointF( 661666.50, 1792739.50 ), QPointF( 661666.50, 1792737.50 ), QPointF( 661665.50, 1792737.50 ), QPointF( 661665.50, 1792736.50 ), QPointF( 661664.50, 1792736.50 ), QPointF( 661664.50, 1792735.50 ), QPointF( 661663.50, 1792735.50 ), QPointF( 661663.50, 1792734.50 ), QPointF( 661662.50, 1792734.50 ), QPointF( 661662.50, 1792733.50 ),
    QPointF( 661661.50, 1792733.50 ), QPointF( 661661.50, 1792729.50 ), QPointF( 661662.50, 1792729.50 ), QPointF( 661662.50, 1792728.50 ), QPointF( 661664.50, 1792728.50 ), QPointF( 661664.50, 1792725.50 ), QPointF( 661665.50, 1792725.50 ), QPointF( 661665.50, 1792724.50 ), QPointF( 661666.50, 1792724.50 ), QPointF( 661666.50, 1792723.50 ),
    QPointF( 661667.50, 1792723.50 ), QPointF( 661667.50, 1792722.50 ), QPointF( 661668.50, 1792722.50 ), QPointF( 661668.50, 1792721.50 ), QPointF( 661669.50, 1792721.50 ), QPointF( 661669.50, 1792722.50 ), QPointF( 661671.50, 1792722.50 ), QPointF( 661671.50, 1792723.50 ), QPointF( 661672.50, 1792723.50 ), QPointF( 661672.50, 1792724.50 ),
    QPointF( 661671.50, 1792724.50 ), QPointF( 661671.50, 1792727.50 ), QPointF( 661674.50, 1792727.50 ), QPointF( 661674.50, 1792728.50 ), QPointF( 661677.50, 1792728.50 ), QPointF( 661677.50, 1792729.50 ), QPointF( 661680.50, 1792729.50 ), QPointF( 661680.50, 1792730.50 ), QPointF( 661681.50, 1792730.50 ), QPointF( 661681.50, 1792731.50 ),
    QPointF( 661682.50, 1792731.50 ), QPointF( 661682.50, 1792733.50 ), QPointF( 661683.50, 1792733.50 ), QPointF( 661683.50, 1792734.50 ), QPointF( 661684.50, 1792734.50 ), QPointF( 661684.50, 1792736.50 ), QPointF( 661685.50, 1792736.50 ), QPointF( 661685.50, 1792737.50 ), QPointF( 661686.50, 1792737.50 ), QPointF( 661686.50, 1792736.50 ),
    QPointF( 661687.50, 1792736.50 ), QPointF( 661687.50, 1792737.50 ), QPointF( 661688.50, 1792737.50 ), QPointF( 661688.50, 1792738.50 ), QPointF( 661689.50, 1792738.50 ), QPointF( 661689.50, 1792740.50 ), QPointF( 661690.50, 1792740.50 ), QPointF( 661690.50, 1792741.50 ), QPointF( 661691.50, 1792741.50 ), QPointF( 661691.50, 1792742.50 ),
    QPointF( 661692.50, 1792742.50 ), QPointF( 661692.50, 1792741.50 ), QPointF( 661696.50, 1792741.50 ), QPointF( 661696.50, 1792743.50 ), QPointF( 661697.50, 1792743.50 ), QPointF( 661697.50, 1792744.50 ), QPointF( 661698.50, 1792744.50 ), QPointF( 661698.50, 1792743.50 ), QPointF( 661706.50, 1792743.50 ), QPointF( 661706.50, 1792744.50 ),
    QPointF( 661708.50, 1792744.50 ), QPointF( 661708.50, 1792745.50 ), QPointF( 661712.50, 1792745.50 ), QPointF( 661712.50, 1792744.50 ), QPointF( 661717.50, 1792744.50 ), QPointF( 661717.50, 1792743.50 ), QPointF( 661719.50, 1792743.50 ), QPointF( 661719.50, 1792748.50 ), QPointF( 661718.50, 1792748.50 ), QPointF( 661718.50, 1792750.50 ),
    QPointF( 661721.50, 1792750.50 ), QPointF( 661721.50, 1792751.50 ), QPointF( 661725.50, 1792751.50 ), QPointF( 661725.50, 1792750.50 ), QPointF( 661730.50, 1792750.50 ), QPointF( 661730.50, 1792749.50 ), QPointF( 661731.50, 1792749.50 ), QPointF( 661731.50, 1792748.50 ), QPointF( 661733.50, 1792748.50 ), QPointF( 661733.50, 1792749.50 ),
    QPointF( 661734.50, 1792749.50 ), QPointF( 661734.50, 1792752.50 ), QPointF( 661736.50, 1792752.50 ), QPointF( 661736.50, 1792753.50 ), QPointF( 661738.50, 1792753.50 ), QPointF( 661738.50, 1792752.50 ), QPointF( 661741.50, 1792752.50 ), QPointF( 661741.50, 1792751.50 ), QPointF( 661743.50, 1792751.50 ), QPointF( 661743.50, 1792752.50 ),
    QPointF( 661746.50, 1792752.50 ), QPointF( 661746.50, 1792753.50 ), QPointF( 661747.50, 1792753.50 ), QPointF( 661747.50, 1792754.50 ), QPointF( 661748.50, 1792754.50 ), QPointF( 661748.50, 1792755.50 ), QPointF( 661749.50, 1792755.50 ), QPointF( 661749.50, 1792756.50 ), QPointF( 661748.50, 1792756.50 ), QPointF( 661748.50, 1792757.50 ),
    QPointF( 661746.50, 1792757.50 ), QPointF( 661746.50, 1792758.50 ), QPointF( 661745.50, 1792758.50 ), QPointF( 661745.50, 1792759.50 ), QPointF( 661746.50, 1792759.50 ), QPointF( 661746.50, 1792760.50 ), QPointF( 661747.50, 1792760.50 ), QPointF( 661747.50, 1792761.50 ), QPointF( 661748.50, 1792761.50 ), QPointF( 661748.50, 1792762.50 ),
    QPointF( 661749.50, 1792762.50 ), QPointF( 661749.50, 1792763.50 ), QPointF( 661752.50, 1792763.50 ), QPointF( 661752.50, 1792764.50 ), QPointF( 661753.50, 1792764.50 ), QPointF( 661753.50, 1792765.50 ), QPointF( 661754.50, 1792765.50 ), QPointF( 661754.50, 1792764.50 ), QPointF( 661759.50, 1792764.50 ), QPointF( 661759.50, 1792768.50 ),
    QPointF( 661758.50, 1792768.50 ), QPointF( 661758.50, 1792769.50 ), QPointF( 661757.50, 1792769.50 ), QPointF( 661757.50, 1792774.50 ), QPointF( 661759.50, 1792774.50 ), QPointF( 661759.50, 1792775.50 ), QPointF( 661760.50, 1792775.50 ), QPointF( 661760.50, 1792778.50 ), QPointF( 661761.50, 1792778.50 ), QPointF( 661761.50, 1792780.50 ),
    QPointF( 661760.50, 1792780.50 ), QPointF( 661760.50, 1792784.50 ), QPointF( 661761.50, 1792784.50 ), QPointF( 661761.50, 1792787.50 ), QPointF( 661762.50, 1792787.50 ), QPointF( 661762.50, 1792793.50 ), QPointF( 661761.50, 1792793.50 ), QPointF( 661761.50, 1792795.50 ), QPointF( 661760.50, 1792795.50 ), QPointF( 661760.50, 1792796.50 ),
    QPointF( 661759.50, 1792796.50 ), QPointF( 661759.50, 1792797.50 ), QPointF( 661758.50, 1792797.50 ), QPointF( 661758.50, 1792802.50 ), QPointF( 661757.50, 1792802.50 ), QPointF( 661757.50, 1792803.50 ), QPointF( 661755.50, 1792803.50 ), QPointF( 661755.50, 1792807.50 ), QPointF( 661754.50, 1792807.50 ), QPointF( 661754.50, 1792808.50 ),
    QPointF( 661753.50, 1792808.50 ), QPointF( 661753.50, 1792809.50 ), QPointF( 661752.50, 1792809.50 ), QPointF( 661752.50, 1792810.50 ), QPointF( 661753.50, 1792810.50 ), QPointF( 661753.50, 1792812.50 ), QPointF( 661755.50, 1792812.50 ), QPointF( 661755.50, 1792813.50 ), QPointF( 661756.50, 1792813.50 ), QPointF( 661756.50, 1792814.50 ),
    QPointF( 661755.50, 1792814.50 ), QPointF( 661755.50, 1792815.50 ), QPointF( 661754.50, 1792815.50 ), QPointF( 661754.50, 1792819.50 ), QPointF( 661753.50, 1792819.50 ), QPointF( 661753.50, 1792823.50 ), QPointF( 661752.50, 1792823.50 ), QPointF( 661752.50, 1792822.50 ), QPointF( 661749.50, 1792822.50 ), QPointF( 661749.50, 1792823.50 ),
    QPointF( 661748.50, 1792823.50 ), QPointF( 661748.50, 1792822.50 ), QPointF( 661745.50, 1792822.50 ), QPointF( 661745.50, 1792821.50 ), QPointF( 661744.50, 1792821.50 ), QPointF( 661744.50, 1792826.50 ), QPointF( 661743.50, 1792826.50 ), QPointF( 661743.50, 1792830.50 ), QPointF( 661744.50, 1792830.50 ), QPointF( 661744.50, 1792832.50 ),
    QPointF( 661745.50, 1792832.50 ), QPointF( 661745.50, 1792833.50 ), QPointF( 661744.50, 1792833.50 ), QPointF( 661744.50, 1792836.50 ), QPointF( 661745.50, 1792836.50 ), QPointF( 661745.50, 1792838.50 ), QPointF( 661744.50, 1792838.50 ), QPointF( 661744.50, 1792839.50 ), QPointF( 661743.50, 1792839.50 ), QPointF( 661743.50, 1792840.50 ),
    QPointF( 661742.50, 1792840.50 ), QPointF( 661742.50, 1792841.50 ), QPointF( 661741.50, 1792841.50 ), QPointF( 661741.50, 1792842.50 ), QPointF( 661740.50, 1792842.50 ), QPointF( 661740.50, 1792844.50 ), QPointF( 661738.50, 1792844.50 ), QPointF( 661738.50, 1792845.50 ), QPointF( 661736.50, 1792845.50 ), QPointF( 661736.50, 1792846.50 ),
    QPointF( 661734.50, 1792846.50 ), QPointF( 661734.50, 1792847.50 ), QPointF( 661733.50, 1792847.50 ), QPointF( 661733.50, 1792848.50 ), QPointF( 661731.50, 1792848.50 ), QPointF( 661731.50, 1792849.50 ), QPointF( 661729.50, 1792849.50 ), QPointF( 661729.50, 1792850.50 ), QPointF( 661728.50, 1792850.50 ), QPointF( 661728.50, 1792849.50 ),
    QPointF( 661727.50, 1792849.50 ), QPointF( 661727.50, 1792850.50 ), QPointF( 661726.50, 1792850.50 ), QPointF( 661726.50, 1792851.50 ), QPointF( 661725.50, 1792851.50 ), QPointF( 661725.50, 1792852.50 ), QPointF( 661724.50, 1792852.50 ), QPointF( 661724.50, 1792853.50 ), QPointF( 661723.50, 1792853.50 ), QPointF( 661723.50, 1792854.50 ),
    QPointF( 661721.50, 1792854.50 ), QPointF( 661721.50, 1792853.50 ), QPointF( 661717.50, 1792853.50 ), QPointF( 661717.50, 1792854.50 ), QPointF( 661708.50, 1792854.50 ), QPointF( 661708.50, 1792853.50 ), QPointF( 661707.50, 1792853.50 ), QPointF( 661707.50, 1792852.50 ), QPointF( 661706.50, 1792852.50 ), QPointF( 661706.50, 1792851.50 ),
    QPointF( 661705.50, 1792851.50 ), QPointF( 661705.50, 1792850.50 ), QPointF( 661704.50, 1792850.50 ), QPointF( 661704.50, 1792849.50 ), QPointF( 661703.50, 1792849.50 ), QPointF( 661703.50, 1792848.50 ), QPointF( 661702.50, 1792848.50 ), QPointF( 661702.50, 1792847.50 ), QPointF( 661701.50, 1792847.50 ), QPointF( 661701.50, 1792846.50 ),
    QPointF( 661700.50, 1792846.50 ), QPointF( 661700.50, 1792845.50 ), QPointF( 661699.50, 1792845.50 ), QPointF( 661699.50, 1792844.50 ), QPointF( 661695.50, 1792844.50 ), QPointF( 661695.50, 1792843.50 ), QPointF( 661678.50, 1792843.50 ), QPointF( 661678.50, 1792842.50 ), QPointF( 661668.50, 1792842.50 ), QPointF( 661668.50, 1792843.50 ),
    QPointF( 661664.50, 1792843.50 ), QPointF( 661664.50, 1792844.50 ), QPointF( 661662.50, 1792844.50 ), QPointF( 661662.50, 1792843.50 ), QPointF( 661659.50, 1792843.50 ), QPointF( 661659.50, 1792842.50 )} );

  QCOMPARE( polygonWatershed, polygonWatershedTest );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  watershedDelineating.storeWatershed( true );
  QVERIFY( watershedStore.masterWatershedCount() == 1 );
  QVERIFY( watershedStore.watershedCount() == 3 ); //including residual watershed

  QVERIFY( itemModel.rowCount( QModelIndex() ) == 1 );
  QVERIFY( itemModel.rowCount( itemModel.index( 0, 0, QModelIndex() ) ) == 2 ); //including residual watershed
}

//----------------------------------------------------------------------------------------------------------------------------
//----------------------------------------------------------------------------------------------------------------------------

void ReosWatersehdTest::watershedDelineatingWithBurningLine()
{
  // create watershed delineating module
  ReosWatershedTree watershedStore;
  ReosWatershedDelineating watershedDelineating( &rootModule, &watershedStore, &gisEngine );
  ModuleProcessControler controler( &watershedDelineating );


  // add raster layer and register it as DEM
  QString layerId = gisEngine.addRasterLayer( test_file( "DEM_for_watershed.tif" ).c_str(), QStringLiteral( "raster_DEM" ) );
  QCOMPARE( gisEngine.layerType( layerId ), ReosGisEngine::RasterLayer );
  QVERIFY( gisEngine.registerLayerAsDigitalElevationModel( layerId ) );
  QMap<QString, QString> demList = gisEngine.digitalElevationModelRasterList();
  QVERIFY( watershedDelineating.setDigitalElevationModelDEM( layerId ) );
  QVERIFY( watershedDelineating.hasValidDigitalElevationModel() );

  QPolygonF burningLine;
  burningLine << QPointF( 661560, 1792870 ) << QPointF( 661670, 1792930 );

  QList<QPolygonF> burningLines;
  burningLines << burningLine;
  watershedDelineating.setBurningLines( burningLines );

  // Set a downstream line
  QPolygonF downstreamLine;
  downstreamLine << QPointF( 661598.75, 1792949.65 ) << QPointF( 661753, 1792949.65 );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingForExtent );

  // Start the good delineating
  ReosMapExtent extent = ReosMapExtent( 661553.33, 1792734.46, 661780.44, 1792964.54 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();


  QPolygonF polygonWatershed = watershedDelineating.lastWatershedDelineated();

  QPolygonF polygonWatershedTest = QPolygonF(
  {
    QPointF( 661598.50, 1792950.50 ), QPointF( 661598.50, 1792949.50 ), QPointF( 661597.50, 1792949.50 ), QPointF( 661597.50, 1792948.50 ), QPointF( 661596.50, 1792948.50 ), QPointF( 661596.50, 1792947.50 ), QPointF( 661595.50, 1792947.50 ), QPointF( 661595.50, 1792946.50 ), QPointF( 661594.50, 1792946.50 ), QPointF( 661594.50, 1792945.50 ),
    QPointF( 661593.50, 1792945.50 ), QPointF( 661593.50, 1792944.50 ), QPointF( 661592.50, 1792944.50 ), QPointF( 661592.50, 1792943.50 ), QPointF( 661591.50, 1792943.50 ), QPointF( 661591.50, 1792942.50 ), QPointF( 661590.50, 1792942.50 ), QPointF( 661590.50, 1792941.50 ), QPointF( 661589.50, 1792941.50 ), QPointF( 661589.50, 1792940.50 ),
    QPointF( 661588.50, 1792940.50 ), QPointF( 661588.50, 1792939.50 ), QPointF( 661587.50, 1792939.50 ), QPointF( 661587.50, 1792938.50 ), QPointF( 661586.50, 1792938.50 ), QPointF( 661586.50, 1792937.50 ), QPointF( 661585.50, 1792937.50 ), QPointF( 661585.50, 1792936.50 ), QPointF( 661584.50, 1792936.50 ), QPointF( 661584.50, 1792935.50 ),
    QPointF( 661583.50, 1792935.50 ), QPointF( 661583.50, 1792934.50 ), QPointF( 661582.50, 1792934.50 ), QPointF( 661582.50, 1792933.50 ), QPointF( 661581.50, 1792933.50 ), QPointF( 661581.50, 1792932.50 ), QPointF( 661580.50, 1792932.50 ), QPointF( 661580.50, 1792931.50 ), QPointF( 661576.50, 1792931.50 ), QPointF( 661576.50, 1792932.50 ),
    QPointF( 661575.50, 1792932.50 ), QPointF( 661575.50, 1792931.50 ), QPointF( 661574.50, 1792931.50 ), QPointF( 661574.50, 1792928.50 ), QPointF( 661573.50, 1792928.50 ), QPointF( 661573.50, 1792927.50 ), QPointF( 661574.50, 1792927.50 ), QPointF( 661574.50, 1792926.50 ), QPointF( 661572.50, 1792926.50 ), QPointF( 661572.50, 1792924.50 ),
    QPointF( 661573.50, 1792924.50 ), QPointF( 661573.50, 1792921.50 ), QPointF( 661572.50, 1792921.50 ), QPointF( 661572.50, 1792920.50 ), QPointF( 661571.50, 1792920.50 ), QPointF( 661571.50, 1792919.50 ), QPointF( 661570.50, 1792919.50 ), QPointF( 661570.50, 1792918.50 ), QPointF( 661569.50, 1792918.50 ), QPointF( 661569.50, 1792917.50 ),
    QPointF( 661568.50, 1792917.50 ), QPointF( 661568.50, 1792916.50 ), QPointF( 661567.50, 1792916.50 ), QPointF( 661567.50, 1792914.50 ), QPointF( 661565.50, 1792914.50 ), QPointF( 661565.50, 1792913.50 ), QPointF( 661564.50, 1792913.50 ), QPointF( 661564.50, 1792912.50 ), QPointF( 661563.50, 1792912.50 ), QPointF( 661563.50, 1792910.50 ),
    QPointF( 661565.50, 1792910.50 ), QPointF( 661565.50, 1792909.50 ), QPointF( 661567.50, 1792909.50 ), QPointF( 661567.50, 1792908.50 ), QPointF( 661572.50, 1792908.50 ), QPointF( 661572.50, 1792904.50 ), QPointF( 661574.50, 1792904.50 ), QPointF( 661574.50, 1792902.50 ), QPointF( 661576.50, 1792902.50 ), QPointF( 661576.50, 1792900.50 ),
    QPointF( 661577.50, 1792900.50 ), QPointF( 661577.50, 1792899.50 ), QPointF( 661576.50, 1792899.50 ), QPointF( 661576.50, 1792898.50 ), QPointF( 661575.50, 1792898.50 ), QPointF( 661575.50, 1792893.50 ), QPointF( 661579.50, 1792893.50 ), QPointF( 661579.50, 1792892.50 ), QPointF( 661581.50, 1792892.50 ), QPointF( 661581.50, 1792891.50 ),
    QPointF( 661580.50, 1792891.50 ), QPointF( 661580.50, 1792890.50 ), QPointF( 661579.50, 1792890.50 ), QPointF( 661579.50, 1792888.50 ), QPointF( 661578.50, 1792888.50 ), QPointF( 661578.50, 1792887.50 ), QPointF( 661577.50, 1792887.50 ), QPointF( 661577.50, 1792885.50 ), QPointF( 661576.50, 1792885.50 ), QPointF( 661576.50, 1792884.50 ),
    QPointF( 661575.50, 1792884.50 ), QPointF( 661575.50, 1792883.50 ), QPointF( 661574.50, 1792883.50 ), QPointF( 661574.50, 1792882.50 ), QPointF( 661573.50, 1792882.50 ), QPointF( 661573.50, 1792881.50 ), QPointF( 661572.50, 1792881.50 ), QPointF( 661572.50, 1792880.50 ), QPointF( 661571.50, 1792880.50 ), QPointF( 661571.50, 1792879.50 ),
    QPointF( 661570.50, 1792879.50 ), QPointF( 661570.50, 1792878.50 ), QPointF( 661569.50, 1792878.50 ), QPointF( 661569.50, 1792877.50 ), QPointF( 661568.50, 1792877.50 ), QPointF( 661568.50, 1792876.50 ), QPointF( 661567.50, 1792876.50 ), QPointF( 661567.50, 1792875.50 ), QPointF( 661565.50, 1792875.50 ), QPointF( 661565.50, 1792874.50 ),
    QPointF( 661564.50, 1792874.50 ), QPointF( 661564.50, 1792875.50 ), QPointF( 661563.50, 1792875.50 ), QPointF( 661563.50, 1792874.50 ), QPointF( 661562.50, 1792874.50 ), QPointF( 661562.50, 1792873.50 ), QPointF( 661561.50, 1792873.50 ), QPointF( 661561.50, 1792872.50 ), QPointF( 661560.50, 1792872.50 ), QPointF( 661560.50, 1792869.50 ),
    QPointF( 661562.50, 1792869.50 ), QPointF( 661562.50, 1792868.50 ), QPointF( 661565.50, 1792868.50 ), QPointF( 661565.50, 1792869.50 ), QPointF( 661566.50, 1792869.50 ), QPointF( 661566.50, 1792868.50 ), QPointF( 661569.50, 1792868.50 ), QPointF( 661569.50, 1792867.50 ), QPointF( 661570.50, 1792867.50 ), QPointF( 661570.50, 1792866.50 ),
    QPointF( 661571.50, 1792866.50 ), QPointF( 661571.50, 1792865.50 ), QPointF( 661572.50, 1792865.50 ), QPointF( 661572.50, 1792864.50 ), QPointF( 661575.50, 1792864.50 ), QPointF( 661575.50, 1792865.50 ), QPointF( 661576.50, 1792865.50 ), QPointF( 661576.50, 1792866.50 ), QPointF( 661577.50, 1792866.50 ), QPointF( 661577.50, 1792867.50 ),
    QPointF( 661578.50, 1792867.50 ), QPointF( 661578.50, 1792868.50 ), QPointF( 661579.50, 1792868.50 ), QPointF( 661579.50, 1792867.50 ), QPointF( 661581.50, 1792867.50 ), QPointF( 661581.50, 1792866.50 ), QPointF( 661582.50, 1792866.50 ), QPointF( 661582.50, 1792859.50 ), QPointF( 661583.50, 1792859.50 ), QPointF( 661583.50, 1792857.50 ),
    QPointF( 661582.50, 1792857.50 ), QPointF( 661582.50, 1792853.50 ), QPointF( 661584.50, 1792853.50 ), QPointF( 661584.50, 1792849.50 ), QPointF( 661585.50, 1792849.50 ), QPointF( 661585.50, 1792846.50 ), QPointF( 661584.50, 1792846.50 ), QPointF( 661584.50, 1792845.50 ), QPointF( 661585.50, 1792845.50 ), QPointF( 661585.50, 1792840.50 ),
    QPointF( 661584.50, 1792840.50 ), QPointF( 661584.50, 1792839.50 ), QPointF( 661585.50, 1792839.50 ), QPointF( 661585.50, 1792838.50 ), QPointF( 661586.50, 1792838.50 ), QPointF( 661586.50, 1792835.50 ), QPointF( 661585.50, 1792835.50 ), QPointF( 661585.50, 1792834.50 ), QPointF( 661584.50, 1792834.50 ), QPointF( 661584.50, 1792833.50 ),
    QPointF( 661583.50, 1792833.50 ), QPointF( 661583.50, 1792831.50 ), QPointF( 661584.50, 1792831.50 ), QPointF( 661584.50, 1792830.50 ), QPointF( 661585.50, 1792830.50 ), QPointF( 661585.50, 1792829.50 ), QPointF( 661586.50, 1792829.50 ), QPointF( 661586.50, 1792828.50 ), QPointF( 661585.50, 1792828.50 ), QPointF( 661585.50, 1792825.50 ),
    QPointF( 661586.50, 1792825.50 ), QPointF( 661586.50, 1792824.50 ), QPointF( 661587.50, 1792824.50 ), QPointF( 661587.50, 1792823.50 ), QPointF( 661586.50, 1792823.50 ), QPointF( 661586.50, 1792820.50 ), QPointF( 661588.50, 1792820.50 ), QPointF( 661588.50, 1792817.50 ), QPointF( 661587.50, 1792817.50 ), QPointF( 661587.50, 1792816.50 ),
    QPointF( 661588.50, 1792816.50 ), QPointF( 661588.50, 1792815.50 ), QPointF( 661589.50, 1792815.50 ), QPointF( 661589.50, 1792814.50 ), QPointF( 661591.50, 1792814.50 ), QPointF( 661591.50, 1792813.50 ), QPointF( 661597.50, 1792813.50 ), QPointF( 661597.50, 1792814.50 ), QPointF( 661601.50, 1792814.50 ), QPointF( 661601.50, 1792813.50 ),
    QPointF( 661602.50, 1792813.50 ), QPointF( 661602.50, 1792810.50 ), QPointF( 661601.50, 1792810.50 ), QPointF( 661601.50, 1792809.50 ), QPointF( 661602.50, 1792809.50 ), QPointF( 661602.50, 1792806.50 ), QPointF( 661601.50, 1792806.50 ), QPointF( 661601.50, 1792799.50 ), QPointF( 661600.50, 1792799.50 ), QPointF( 661600.50, 1792797.50 ),
    QPointF( 661602.50, 1792797.50 ), QPointF( 661602.50, 1792795.50 ), QPointF( 661601.50, 1792795.50 ), QPointF( 661601.50, 1792794.50 ), QPointF( 661602.50, 1792794.50 ), QPointF( 661602.50, 1792792.50 ), QPointF( 661603.50, 1792792.50 ), QPointF( 661603.50, 1792791.50 ), QPointF( 661604.50, 1792791.50 ), QPointF( 661604.50, 1792790.50 ),
    QPointF( 661606.50, 1792790.50 ), QPointF( 661606.50, 1792787.50 ), QPointF( 661609.50, 1792787.50 ), QPointF( 661609.50, 1792786.50 ), QPointF( 661610.50, 1792786.50 ), QPointF( 661610.50, 1792784.50 ), QPointF( 661611.50, 1792784.50 ), QPointF( 661611.50, 1792783.50 ), QPointF( 661613.50, 1792783.50 ), QPointF( 661613.50, 1792782.50 ),
    QPointF( 661614.50, 1792782.50 ), QPointF( 661614.50, 1792781.50 ), QPointF( 661618.50, 1792781.50 ), QPointF( 661618.50, 1792779.50 ), QPointF( 661619.50, 1792779.50 ), QPointF( 661619.50, 1792778.50 ), QPointF( 661620.50, 1792778.50 ), QPointF( 661620.50, 1792777.50 ), QPointF( 661622.50, 1792777.50 ), QPointF( 661622.50, 1792776.50 ),
    QPointF( 661623.50, 1792776.50 ), QPointF( 661623.50, 1792775.50 ), QPointF( 661624.50, 1792775.50 ), QPointF( 661624.50, 1792774.50 ), QPointF( 661627.50, 1792774.50 ), QPointF( 661627.50, 1792775.50 ), QPointF( 661628.50, 1792775.50 ), QPointF( 661628.50, 1792772.50 ), QPointF( 661629.50, 1792772.50 ), QPointF( 661629.50, 1792771.50 ),
    QPointF( 661631.50, 1792771.50 ), QPointF( 661631.50, 1792770.50 ), QPointF( 661636.50, 1792770.50 ), QPointF( 661636.50, 1792769.50 ), QPointF( 661637.50, 1792769.50 ), QPointF( 661637.50, 1792770.50 ), QPointF( 661638.50, 1792770.50 ), QPointF( 661638.50, 1792769.50 ), QPointF( 661642.50, 1792769.50 ), QPointF( 661642.50, 1792767.50 ),
    QPointF( 661644.50, 1792767.50 ), QPointF( 661644.50, 1792766.50 ), QPointF( 661646.50, 1792766.50 ), QPointF( 661646.50, 1792767.50 ), QPointF( 661647.50, 1792767.50 ), QPointF( 661647.50, 1792764.50 ), QPointF( 661648.50, 1792764.50 ), QPointF( 661648.50, 1792763.50 ), QPointF( 661649.50, 1792763.50 ), QPointF( 661649.50, 1792762.50 ),
    QPointF( 661650.50, 1792762.50 ), QPointF( 661650.50, 1792761.50 ), QPointF( 661649.50, 1792761.50 ), QPointF( 661649.50, 1792758.50 ), QPointF( 661650.50, 1792758.50 ), QPointF( 661650.50, 1792757.50 ), QPointF( 661651.50, 1792757.50 ), QPointF( 661651.50, 1792756.50 ), QPointF( 661652.50, 1792756.50 ), QPointF( 661652.50, 1792755.50 ),
    QPointF( 661653.50, 1792755.50 ), QPointF( 661653.50, 1792754.50 ), QPointF( 661654.50, 1792754.50 ), QPointF( 661654.50, 1792753.50 ), QPointF( 661655.50, 1792753.50 ), QPointF( 661655.50, 1792752.50 ), QPointF( 661656.50, 1792752.50 ), QPointF( 661656.50, 1792751.50 ), QPointF( 661657.50, 1792751.50 ), QPointF( 661657.50, 1792750.50 ),
    QPointF( 661658.50, 1792750.50 ), QPointF( 661658.50, 1792749.50 ), QPointF( 661659.50, 1792749.50 ), QPointF( 661659.50, 1792748.50 ), QPointF( 661660.50, 1792748.50 ), QPointF( 661660.50, 1792747.50 ), QPointF( 661661.50, 1792747.50 ), QPointF( 661661.50, 1792746.50 ), QPointF( 661662.50, 1792746.50 ), QPointF( 661662.50, 1792745.50 ),
    QPointF( 661663.50, 1792745.50 ), QPointF( 661663.50, 1792743.50 ), QPointF( 661664.50, 1792743.50 ), QPointF( 661664.50, 1792742.50 ), QPointF( 661665.50, 1792742.50 ), QPointF( 661665.50, 1792741.50 ), QPointF( 661666.50, 1792741.50 ), QPointF( 661666.50, 1792740.50 ), QPointF( 661667.50, 1792740.50 ), QPointF( 661667.50, 1792739.50 ),
    QPointF( 661666.50, 1792739.50 ), QPointF( 661666.50, 1792737.50 ), QPointF( 661665.50, 1792737.50 ), QPointF( 661665.50, 1792736.50 ), QPointF( 661664.50, 1792736.50 ), QPointF( 661664.50, 1792735.50 ), QPointF( 661663.50, 1792735.50 ), QPointF( 661663.50, 1792734.50 ), QPointF( 661662.50, 1792734.50 ), QPointF( 661662.50, 1792733.50 ),
    QPointF( 661675.50, 1792733.50 ), QPointF( 661675.50, 1792734.50 ), QPointF( 661676.50, 1792734.50 ), QPointF( 661676.50, 1792735.50 ), QPointF( 661677.50, 1792735.50 ), QPointF( 661677.50, 1792736.50 ), QPointF( 661678.50, 1792736.50 ), QPointF( 661678.50, 1792735.50 ), QPointF( 661680.50, 1792735.50 ), QPointF( 661680.50, 1792733.50 ),
    QPointF( 661684.50, 1792733.50 ), QPointF( 661684.50, 1792736.50 ), QPointF( 661685.50, 1792736.50 ), QPointF( 661685.50, 1792737.50 ), QPointF( 661686.50, 1792737.50 ), QPointF( 661686.50, 1792736.50 ), QPointF( 661687.50, 1792736.50 ), QPointF( 661687.50, 1792737.50 ), QPointF( 661688.50, 1792737.50 ), QPointF( 661688.50, 1792738.50 ),
    QPointF( 661689.50, 1792738.50 ), QPointF( 661689.50, 1792740.50 ), QPointF( 661690.50, 1792740.50 ), QPointF( 661690.50, 1792741.50 ), QPointF( 661691.50, 1792741.50 ), QPointF( 661691.50, 1792742.50 ), QPointF( 661692.50, 1792742.50 ), QPointF( 661692.50, 1792741.50 ), QPointF( 661696.50, 1792741.50 ), QPointF( 661696.50, 1792743.50 ),
    QPointF( 661697.50, 1792743.50 ), QPointF( 661697.50, 1792744.50 ), QPointF( 661698.50, 1792744.50 ), QPointF( 661698.50, 1792743.50 ), QPointF( 661706.50, 1792743.50 ), QPointF( 661706.50, 1792744.50 ), QPointF( 661708.50, 1792744.50 ), QPointF( 661708.50, 1792745.50 ), QPointF( 661712.50, 1792745.50 ), QPointF( 661712.50, 1792744.50 ),
    QPointF( 661717.50, 1792744.50 ), QPointF( 661717.50, 1792743.50 ), QPointF( 661719.50, 1792743.50 ), QPointF( 661719.50, 1792748.50 ), QPointF( 661718.50, 1792748.50 ), QPointF( 661718.50, 1792750.50 ), QPointF( 661721.50, 1792750.50 ), QPointF( 661721.50, 1792751.50 ), QPointF( 661725.50, 1792751.50 ), QPointF( 661725.50, 1792750.50 ),
    QPointF( 661730.50, 1792750.50 ), QPointF( 661730.50, 1792749.50 ), QPointF( 661731.50, 1792749.50 ), QPointF( 661731.50, 1792748.50 ), QPointF( 661733.50, 1792748.50 ), QPointF( 661733.50, 1792749.50 ), QPointF( 661734.50, 1792749.50 ), QPointF( 661734.50, 1792752.50 ), QPointF( 661736.50, 1792752.50 ), QPointF( 661736.50, 1792753.50 ),
    QPointF( 661738.50, 1792753.50 ), QPointF( 661738.50, 1792752.50 ), QPointF( 661741.50, 1792752.50 ), QPointF( 661741.50, 1792751.50 ), QPointF( 661743.50, 1792751.50 ), QPointF( 661743.50, 1792752.50 ), QPointF( 661746.50, 1792752.50 ), QPointF( 661746.50, 1792753.50 ), QPointF( 661747.50, 1792753.50 ), QPointF( 661747.50, 1792754.50 ),
    QPointF( 661748.50, 1792754.50 ), QPointF( 661748.50, 1792755.50 ), QPointF( 661749.50, 1792755.50 ), QPointF( 661749.50, 1792756.50 ), QPointF( 661748.50, 1792756.50 ), QPointF( 661748.50, 1792757.50 ), QPointF( 661746.50, 1792757.50 ), QPointF( 661746.50, 1792758.50 ), QPointF( 661745.50, 1792758.50 ), QPointF( 661745.50, 1792759.50 ),
    QPointF( 661746.50, 1792759.50 ), QPointF( 661746.50, 1792760.50 ), QPointF( 661747.50, 1792760.50 ), QPointF( 661747.50, 1792761.50 ), QPointF( 661748.50, 1792761.50 ), QPointF( 661748.50, 1792762.50 ), QPointF( 661749.50, 1792762.50 ), QPointF( 661749.50, 1792763.50 ), QPointF( 661752.50, 1792763.50 ), QPointF( 661752.50, 1792764.50 ),
    QPointF( 661753.50, 1792764.50 ), QPointF( 661753.50, 1792765.50 ), QPointF( 661754.50, 1792765.50 ), QPointF( 661754.50, 1792764.50 ), QPointF( 661759.50, 1792764.50 ), QPointF( 661759.50, 1792768.50 ), QPointF( 661758.50, 1792768.50 ), QPointF( 661758.50, 1792769.50 ), QPointF( 661757.50, 1792769.50 ), QPointF( 661757.50, 1792774.50 ),
    QPointF( 661759.50, 1792774.50 ), QPointF( 661759.50, 1792775.50 ), QPointF( 661760.50, 1792775.50 ), QPointF( 661760.50, 1792778.50 ), QPointF( 661761.50, 1792778.50 ), QPointF( 661761.50, 1792780.50 ), QPointF( 661760.50, 1792780.50 ), QPointF( 661760.50, 1792784.50 ), QPointF( 661761.50, 1792784.50 ), QPointF( 661761.50, 1792787.50 ),
    QPointF( 661762.50, 1792787.50 ), QPointF( 661762.50, 1792793.50 ), QPointF( 661761.50, 1792793.50 ), QPointF( 661761.50, 1792795.50 ), QPointF( 661760.50, 1792795.50 ), QPointF( 661760.50, 1792796.50 ), QPointF( 661759.50, 1792796.50 ), QPointF( 661759.50, 1792797.50 ), QPointF( 661758.50, 1792797.50 ), QPointF( 661758.50, 1792802.50 ),
    QPointF( 661757.50, 1792802.50 ), QPointF( 661757.50, 1792803.50 ), QPointF( 661755.50, 1792803.50 ), QPointF( 661755.50, 1792807.50 ), QPointF( 661754.50, 1792807.50 ), QPointF( 661754.50, 1792808.50 ), QPointF( 661753.50, 1792808.50 ), QPointF( 661753.50, 1792809.50 ), QPointF( 661752.50, 1792809.50 ), QPointF( 661752.50, 1792810.50 ),
    QPointF( 661753.50, 1792810.50 ), QPointF( 661753.50, 1792812.50 ), QPointF( 661755.50, 1792812.50 ), QPointF( 661755.50, 1792813.50 ), QPointF( 661756.50, 1792813.50 ), QPointF( 661756.50, 1792814.50 ), QPointF( 661755.50, 1792814.50 ), QPointF( 661755.50, 1792815.50 ), QPointF( 661754.50, 1792815.50 ), QPointF( 661754.50, 1792819.50 ),
    QPointF( 661753.50, 1792819.50 ), QPointF( 661753.50, 1792823.50 ), QPointF( 661752.50, 1792823.50 ), QPointF( 661752.50, 1792822.50 ), QPointF( 661749.50, 1792822.50 ), QPointF( 661749.50, 1792823.50 ), QPointF( 661748.50, 1792823.50 ), QPointF( 661748.50, 1792822.50 ), QPointF( 661745.50, 1792822.50 ), QPointF( 661745.50, 1792821.50 ),
    QPointF( 661744.50, 1792821.50 ), QPointF( 661744.50, 1792826.50 ), QPointF( 661743.50, 1792826.50 ), QPointF( 661743.50, 1792830.50 ), QPointF( 661744.50, 1792830.50 ), QPointF( 661744.50, 1792832.50 ), QPointF( 661745.50, 1792832.50 ), QPointF( 661745.50, 1792833.50 ), QPointF( 661744.50, 1792833.50 ), QPointF( 661744.50, 1792836.50 ),
    QPointF( 661745.50, 1792836.50 ), QPointF( 661745.50, 1792838.50 ), QPointF( 661744.50, 1792838.50 ), QPointF( 661744.50, 1792839.50 ), QPointF( 661743.50, 1792839.50 ), QPointF( 661743.50, 1792840.50 ), QPointF( 661742.50, 1792840.50 ), QPointF( 661742.50, 1792841.50 ), QPointF( 661741.50, 1792841.50 ), QPointF( 661741.50, 1792842.50 ),
    QPointF( 661740.50, 1792842.50 ), QPointF( 661740.50, 1792845.50 ), QPointF( 661741.50, 1792845.50 ), QPointF( 661741.50, 1792846.50 ), QPointF( 661742.50, 1792846.50 ), QPointF( 661742.50, 1792845.50 ), QPointF( 661745.50, 1792845.50 ), QPointF( 661745.50, 1792846.50 ), QPointF( 661749.50, 1792846.50 ), QPointF( 661749.50, 1792847.50 ),
    QPointF( 661750.50, 1792847.50 ), QPointF( 661750.50, 1792848.50 ), QPointF( 661751.50, 1792848.50 ), QPointF( 661751.50, 1792849.50 ), QPointF( 661752.50, 1792849.50 ), QPointF( 661752.50, 1792852.50 ), QPointF( 661751.50, 1792852.50 ), QPointF( 661751.50, 1792853.50 ), QPointF( 661752.50, 1792853.50 ), QPointF( 661752.50, 1792854.50 ),
    QPointF( 661753.50, 1792854.50 ), QPointF( 661753.50, 1792855.50 ), QPointF( 661754.50, 1792855.50 ), QPointF( 661754.50, 1792856.50 ), QPointF( 661756.50, 1792856.50 ), QPointF( 661756.50, 1792860.50 ), QPointF( 661755.50, 1792860.50 ), QPointF( 661755.50, 1792861.50 ), QPointF( 661754.50, 1792861.50 ), QPointF( 661754.50, 1792862.50 ),
    QPointF( 661753.50, 1792862.50 ), QPointF( 661753.50, 1792863.50 ), QPointF( 661752.50, 1792863.50 ), QPointF( 661752.50, 1792864.50 ), QPointF( 661753.50, 1792864.50 ), QPointF( 661753.50, 1792867.50 ), QPointF( 661754.50, 1792867.50 ), QPointF( 661754.50, 1792868.50 ), QPointF( 661756.50, 1792868.50 ), QPointF( 661756.50, 1792870.50 ),
    QPointF( 661757.50, 1792870.50 ), QPointF( 661757.50, 1792872.50 ), QPointF( 661752.50, 1792872.50 ), QPointF( 661752.50, 1792873.50 ), QPointF( 661753.50, 1792873.50 ), QPointF( 661753.50, 1792874.50 ), QPointF( 661754.50, 1792874.50 ), QPointF( 661754.50, 1792875.50 ), QPointF( 661755.50, 1792875.50 ), QPointF( 661755.50, 1792876.50 ),
    QPointF( 661756.50, 1792876.50 ), QPointF( 661756.50, 1792877.50 ), QPointF( 661757.50, 1792877.50 ), QPointF( 661757.50, 1792878.50 ), QPointF( 661758.50, 1792878.50 ), QPointF( 661758.50, 1792879.50 ), QPointF( 661759.50, 1792879.50 ), QPointF( 661759.50, 1792884.50 ), QPointF( 661760.50, 1792884.50 ), QPointF( 661760.50, 1792885.50 ),
    QPointF( 661759.50, 1792885.50 ), QPointF( 661759.50, 1792888.50 ), QPointF( 661760.50, 1792888.50 ), QPointF( 661760.50, 1792887.50 ), QPointF( 661761.50, 1792887.50 ), QPointF( 661761.50, 1792888.50 ), QPointF( 661762.50, 1792888.50 ), QPointF( 661762.50, 1792889.50 ), QPointF( 661763.50, 1792889.50 ), QPointF( 661763.50, 1792899.50 ),
    QPointF( 661762.50, 1792899.50 ), QPointF( 661762.50, 1792905.50 ), QPointF( 661756.50, 1792905.50 ), QPointF( 661756.50, 1792906.50 ), QPointF( 661754.50, 1792906.50 ), QPointF( 661754.50, 1792907.50 ), QPointF( 661751.50, 1792907.50 ), QPointF( 661751.50, 1792908.50 ), QPointF( 661752.50, 1792908.50 ), QPointF( 661752.50, 1792909.50 ),
    QPointF( 661753.50, 1792909.50 ), QPointF( 661753.50, 1792912.50 ), QPointF( 661756.50, 1792912.50 ), QPointF( 661756.50, 1792913.50 ), QPointF( 661757.50, 1792913.50 ), QPointF( 661757.50, 1792914.50 ), QPointF( 661758.50, 1792914.50 ), QPointF( 661758.50, 1792920.50 ), QPointF( 661757.50, 1792920.50 ), QPointF( 661757.50, 1792923.50 ),
    QPointF( 661760.50, 1792923.50 ), QPointF( 661760.50, 1792922.50 ), QPointF( 661762.50, 1792922.50 ), QPointF( 661762.50, 1792921.50 ), QPointF( 661763.50, 1792921.50 ), QPointF( 661763.50, 1792920.50 ), QPointF( 661765.50, 1792920.50 ), QPointF( 661765.50, 1792924.50 ), QPointF( 661764.50, 1792924.50 ), QPointF( 661764.50, 1792925.50 ),
    QPointF( 661762.50, 1792925.50 ), QPointF( 661762.50, 1792927.50 ), QPointF( 661763.50, 1792927.50 ), QPointF( 661763.50, 1792928.50 ), QPointF( 661766.50, 1792928.50 ), QPointF( 661766.50, 1792929.50 ), QPointF( 661767.50, 1792929.50 ), QPointF( 661767.50, 1792932.50 ), QPointF( 661768.50, 1792932.50 ), QPointF( 661768.50, 1792934.50 ),
    QPointF( 661767.50, 1792934.50 ), QPointF( 661767.50, 1792935.50 ), QPointF( 661766.50, 1792935.50 ), QPointF( 661766.50, 1792936.50 ), QPointF( 661765.50, 1792936.50 ), QPointF( 661765.50, 1792937.50 ), QPointF( 661764.50, 1792937.50 ), QPointF( 661764.50, 1792938.50 ), QPointF( 661763.50, 1792938.50 ), QPointF( 661763.50, 1792939.50 ),
    QPointF( 661762.50, 1792939.50 ), QPointF( 661762.50, 1792940.50 ), QPointF( 661761.50, 1792940.50 ), QPointF( 661761.50, 1792941.50 ), QPointF( 661760.50, 1792941.50 ), QPointF( 661760.50, 1792942.50 ), QPointF( 661759.50, 1792942.50 ), QPointF( 661759.50, 1792943.50 ), QPointF( 661757.50, 1792943.50 ), QPointF( 661757.50, 1792945.50 ),
    QPointF( 661756.50, 1792945.50 ), QPointF( 661756.50, 1792947.50 ), QPointF( 661755.50, 1792947.50 ), QPointF( 661755.50, 1792948.50 ), QPointF( 661754.50, 1792948.50 ), QPointF( 661754.50, 1792949.50 ), QPointF( 661753.50, 1792949.50 ), QPointF( 661753.50, 1792950.50 ), QPointF( 661690.50, 1792950.50 ), QPointF( 661690.50, 1792951.50 ),
    QPointF( 661687.50, 1792951.50 ), QPointF( 661687.50, 1792952.50 ), QPointF( 661685.50, 1792952.50 ), QPointF( 661685.50, 1792953.50 ), QPointF( 661682.50, 1792953.50 ), QPointF( 661682.50, 1792954.50 ), QPointF( 661681.50, 1792954.50 ), QPointF( 661681.50, 1792953.50 ), QPointF( 661680.50, 1792953.50 ), QPointF( 661680.50, 1792952.50 ),
    QPointF( 661679.50, 1792952.50 ), QPointF( 661679.50, 1792953.50 ), QPointF( 661678.50, 1792953.50 ), QPointF( 661678.50, 1792954.50 ), QPointF( 661677.50, 1792954.50 ), QPointF( 661677.50, 1792953.50 ), QPointF( 661676.50, 1792953.50 ), QPointF( 661676.50, 1792952.50 ), QPointF( 661675.50, 1792952.50 ), QPointF( 661675.50, 1792951.50 ),
    QPointF( 661674.50, 1792951.50 ), QPointF( 661674.50, 1792950.50 ), QPointF( 661669.50, 1792950.50 ), QPointF( 661669.50, 1792951.50 ), QPointF( 661667.50, 1792951.50 ), QPointF( 661667.50, 1792950.50 ), QPointF( 661599.50, 1792950.50 )} );

//  std::ofstream file( "/home/vincent/watershed.txt" );
//  for ( int i = 0; i < polygonWatershed.size(); ++i )
//  {
//    if ( i % 10 == 0 && i != 0 )
//      file << std::endl;
//    QPointF pt = polygonWatershed.at( i );
//    file << QString( " QPointF(%1,%2)," ).arg( QString::number( pt.x(), 'f', 2 ) ).arg( QString::number( pt.y(), 'f', 2 ) ).toStdString();
//    //file << QString( " %1 %2," ).arg( QString::number( pt.x(), 'f', 2 ) ).arg( QString::number( pt.y(), 'f', 2 ) ).toStdString();
//  }

  QCOMPARE( polygonWatershed, polygonWatershedTest );

}

void ReosWatersehdTest::watershdDelineatingMultiWatershed()
{
  // create watershed delineating module
  ReosWatershedTree watershedStore;
  ReosWatershedDelineating watershedDelineating( &rootModule, &watershedStore, &gisEngine );
  ModuleProcessControler controler( &watershedDelineating );
  ReosWatershedItemModel itemModel( &watershedStore );

  // add raster layer and register it as DEM
  QString layerId = gisEngine.addRasterLayer( test_file( "DEM_for_multi_watershed.tif" ).c_str(), QStringLiteral( "raster_DEM" ) );
  QCOMPARE( gisEngine.layerType( layerId ), ReosGisEngine::RasterLayer );
  QVERIFY( gisEngine.registerLayerAsDigitalElevationModel( layerId ) );
  QMap<QString, QString> demList = gisEngine.digitalElevationModelRasterList();
  QVERIFY( watershedDelineating.setDigitalElevationModelDEM( layerId ) );
  QVERIFY( watershedDelineating.hasValidDigitalElevationModel() );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForDownstream );

  QPolygonF downstreamLine;
  downstreamLine << QPointF( 666701.724, 1799132.754 ) << QPointF( 666692.005, 1799212.636 );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForExtent );

  ReosMapExtent predifinedExtent( 666503.15, 1798940.06, 667100.8, 1799458.2 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( predifinedExtent ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  QPolygonF polygonWatershed = watershedDelineating.lastWatershedDelineated();

  QPolygonF polygonWatershedTest(
  {
    QPointF( 666694.50, 1799175.50 ), QPointF( 666694.50, 1799145.50 ), QPointF( 666699.50, 1799145.50 ), QPointF( 666699.50, 1799130.50 ), QPointF( 666704.50, 1799130.50 ), QPointF( 666704.50, 1799105.50 ), QPointF( 666699.50, 1799105.50 ), QPointF( 666699.50, 1799095.50 ), QPointF( 666704.50, 1799095.50 ), QPointF( 666704.50, 1799085.50 ),
    QPointF( 666709.50, 1799085.50 ), QPointF( 666709.50, 1799065.50 ), QPointF( 666714.50, 1799065.50 ), QPointF( 666714.50, 1799045.50 ), QPointF( 666719.50, 1799045.50 ), QPointF( 666719.50, 1799015.50 ), QPointF( 666724.50, 1799015.50 ), QPointF( 666724.50, 1799005.50 ), QPointF( 666744.50, 1799005.50 ), QPointF( 666744.50, 1799000.50 ),
    QPointF( 666754.50, 1799000.50 ), QPointF( 666754.50, 1798995.50 ), QPointF( 666769.50, 1798995.50 ), QPointF( 666769.50, 1798990.50 ), QPointF( 666779.50, 1798990.50 ), QPointF( 666779.50, 1798985.50 ), QPointF( 666789.50, 1798985.50 ), QPointF( 666789.50, 1798980.50 ), QPointF( 666829.50, 1798980.50 ), QPointF( 666829.50, 1798975.50 ),
    QPointF( 666854.50, 1798975.50 ), QPointF( 666854.50, 1798970.50 ), QPointF( 666859.50, 1798970.50 ), QPointF( 666859.50, 1798965.50 ), QPointF( 666864.50, 1798965.50 ), QPointF( 666864.50, 1798960.50 ), QPointF( 666869.50, 1798960.50 ), QPointF( 666869.50, 1798955.50 ), QPointF( 666874.50, 1798955.50 ), QPointF( 666874.50, 1798950.50 ),
    QPointF( 666884.50, 1798950.50 ), QPointF( 666884.50, 1798955.50 ), QPointF( 666889.50, 1798955.50 ), QPointF( 666889.50, 1798960.50 ), QPointF( 666894.50, 1798960.50 ), QPointF( 666894.50, 1798955.50 ), QPointF( 666899.50, 1798955.50 ), QPointF( 666899.50, 1798960.50 ), QPointF( 666904.50, 1798960.50 ), QPointF( 666904.50, 1798970.50 ),
    QPointF( 666909.50, 1798970.50 ), QPointF( 666909.50, 1798975.50 ), QPointF( 666914.50, 1798975.50 ), QPointF( 666914.50, 1798980.50 ), QPointF( 666919.50, 1798980.50 ), QPointF( 666919.50, 1798985.50 ), QPointF( 666924.50, 1798985.50 ), QPointF( 666924.50, 1798990.50 ), QPointF( 666929.50, 1798990.50 ), QPointF( 666929.50, 1798995.50 ),
    QPointF( 666924.50, 1798995.50 ), QPointF( 666924.50, 1799000.50 ), QPointF( 666919.50, 1799000.50 ), QPointF( 666919.50, 1799005.50 ), QPointF( 666929.50, 1799005.50 ), QPointF( 666929.50, 1799000.50 ), QPointF( 666934.50, 1799000.50 ), QPointF( 666934.50, 1798995.50 ), QPointF( 666944.50, 1798995.50 ), QPointF( 666944.50, 1799000.50 ),
    QPointF( 666954.50, 1799000.50 ), QPointF( 666954.50, 1799005.50 ), QPointF( 666959.50, 1799005.50 ), QPointF( 666959.50, 1799010.50 ), QPointF( 666964.50, 1799010.50 ), QPointF( 666964.50, 1799015.50 ), QPointF( 666979.50, 1799015.50 ), QPointF( 666979.50, 1799020.50 ), QPointF( 666984.50, 1799020.50 ), QPointF( 666984.50, 1799025.50 ),
    QPointF( 666999.50, 1799025.50 ), QPointF( 666999.50, 1799030.50 ), QPointF( 667014.50, 1799030.50 ), QPointF( 667014.50, 1799035.50 ), QPointF( 667029.50, 1799035.50 ), QPointF( 667029.50, 1799040.50 ), QPointF( 667034.50, 1799040.50 ), QPointF( 667034.50, 1799045.50 ), QPointF( 667049.50, 1799045.50 ), QPointF( 667049.50, 1799050.50 ),
    QPointF( 667054.50, 1799050.50 ), QPointF( 667054.50, 1799055.50 ), QPointF( 667059.50, 1799055.50 ), QPointF( 667059.50, 1799060.50 ), QPointF( 667064.50, 1799060.50 ), QPointF( 667064.50, 1799065.50 ), QPointF( 667074.50, 1799065.50 ), QPointF( 667074.50, 1799070.50 ), QPointF( 667079.50, 1799070.50 ), QPointF( 667079.50, 1799080.50 ),
    QPointF( 667074.50, 1799080.50 ), QPointF( 667074.50, 1799085.50 ), QPointF( 667069.50, 1799085.50 ), QPointF( 667069.50, 1799105.50 ), QPointF( 667064.50, 1799105.50 ), QPointF( 667064.50, 1799110.50 ), QPointF( 667059.50, 1799110.50 ), QPointF( 667059.50, 1799135.50 ), QPointF( 667054.50, 1799135.50 ), QPointF( 667054.50, 1799140.50 ),
    QPointF( 667049.50, 1799140.50 ), QPointF( 667049.50, 1799145.50 ), QPointF( 667054.50, 1799145.50 ), QPointF( 667054.50, 1799150.50 ), QPointF( 667049.50, 1799150.50 ), QPointF( 667049.50, 1799160.50 ), QPointF( 667044.50, 1799160.50 ), QPointF( 667044.50, 1799170.50 ), QPointF( 667039.50, 1799170.50 ), QPointF( 667039.50, 1799175.50 ),
    QPointF( 667034.50, 1799175.50 ), QPointF( 667034.50, 1799195.50 ), QPointF( 667039.50, 1799195.50 ), QPointF( 667039.50, 1799200.50 ), QPointF( 667044.50, 1799200.50 ), QPointF( 667044.50, 1799210.50 ), QPointF( 667039.50, 1799210.50 ), QPointF( 667039.50, 1799220.50 ), QPointF( 667034.50, 1799220.50 ), QPointF( 667034.50, 1799225.50 ),
    QPointF( 667029.50, 1799225.50 ), QPointF( 667029.50, 1799230.50 ), QPointF( 667024.50, 1799230.50 ), QPointF( 667024.50, 1799245.50 ), QPointF( 667019.50, 1799245.50 ), QPointF( 667019.50, 1799255.50 ), QPointF( 667014.50, 1799255.50 ), QPointF( 667014.50, 1799275.50 ), QPointF( 667009.50, 1799275.50 ), QPointF( 667009.50, 1799285.50 ),
    QPointF( 667004.50, 1799285.50 ), QPointF( 667004.50, 1799290.50 ), QPointF( 666999.50, 1799290.50 ), QPointF( 666999.50, 1799295.50 ), QPointF( 666994.50, 1799295.50 ), QPointF( 666994.50, 1799300.50 ), QPointF( 666984.50, 1799300.50 ), QPointF( 666984.50, 1799305.50 ), QPointF( 666979.50, 1799305.50 ), QPointF( 666979.50, 1799315.50 ),
    QPointF( 666984.50, 1799315.50 ), QPointF( 666984.50, 1799325.50 ), QPointF( 666974.50, 1799325.50 ), QPointF( 666974.50, 1799340.50 ), QPointF( 666969.50, 1799340.50 ), QPointF( 666969.50, 1799350.50 ), QPointF( 666964.50, 1799350.50 ), QPointF( 666964.50, 1799355.50 ), QPointF( 666954.50, 1799355.50 ), QPointF( 666954.50, 1799360.50 ),
    QPointF( 666949.50, 1799360.50 ), QPointF( 666949.50, 1799365.50 ), QPointF( 666944.50, 1799365.50 ), QPointF( 666944.50, 1799370.50 ), QPointF( 666939.50, 1799370.50 ), QPointF( 666939.50, 1799375.50 ), QPointF( 666934.50, 1799375.50 ), QPointF( 666934.50, 1799380.50 ), QPointF( 666929.50, 1799380.50 ), QPointF( 666929.50, 1799385.50 ),
    QPointF( 666924.50, 1799385.50 ), QPointF( 666924.50, 1799395.50 ), QPointF( 666919.50, 1799395.50 ), QPointF( 666919.50, 1799400.50 ), QPointF( 666914.50, 1799400.50 ), QPointF( 666914.50, 1799405.50 ), QPointF( 666909.50, 1799405.50 ), QPointF( 666909.50, 1799410.50 ), QPointF( 666904.50, 1799410.50 ), QPointF( 666904.50, 1799415.50 ),
    QPointF( 666894.50, 1799415.50 ), QPointF( 666894.50, 1799420.50 ), QPointF( 666889.50, 1799420.50 ), QPointF( 666889.50, 1799415.50 ), QPointF( 666884.50, 1799415.50 ), QPointF( 666884.50, 1799410.50 ), QPointF( 666879.50, 1799410.50 ), QPointF( 666879.50, 1799405.50 ), QPointF( 666834.50, 1799405.50 ), QPointF( 666834.50, 1799400.50 ),
    QPointF( 666824.50, 1799400.50 ), QPointF( 666824.50, 1799395.50 ), QPointF( 666814.50, 1799395.50 ), QPointF( 666814.50, 1799390.50 ), QPointF( 666809.50, 1799390.50 ), QPointF( 666809.50, 1799385.50 ), QPointF( 666784.50, 1799385.50 ), QPointF( 666784.50, 1799380.50 ), QPointF( 666774.50, 1799380.50 ), QPointF( 666774.50, 1799375.50 ),
    QPointF( 666769.50, 1799375.50 ), QPointF( 666769.50, 1799365.50 ), QPointF( 666764.50, 1799365.50 ), QPointF( 666764.50, 1799360.50 ), QPointF( 666759.50, 1799360.50 ), QPointF( 666759.50, 1799355.50 ), QPointF( 666754.50, 1799355.50 ), QPointF( 666754.50, 1799350.50 ), QPointF( 666749.50, 1799350.50 ), QPointF( 666749.50, 1799340.50 ),
    QPointF( 666744.50, 1799340.50 ), QPointF( 666744.50, 1799335.50 ), QPointF( 666734.50, 1799335.50 ), QPointF( 666734.50, 1799330.50 ), QPointF( 666729.50, 1799330.50 ), QPointF( 666729.50, 1799325.50 ), QPointF( 666724.50, 1799325.50 ), QPointF( 666724.50, 1799320.50 ), QPointF( 666719.50, 1799320.50 ), QPointF( 666719.50, 1799300.50 ),
    QPointF( 666714.50, 1799300.50 ), QPointF( 666714.50, 1799285.50 ), QPointF( 666709.50, 1799285.50 ), QPointF( 666709.50, 1799280.50 ), QPointF( 666704.50, 1799280.50 ), QPointF( 666704.50, 1799275.50 ), QPointF( 666699.50, 1799275.50 ), QPointF( 666699.50, 1799270.50 ), QPointF( 666694.50, 1799270.50 ), QPointF( 666694.50, 1799265.50 ),
    QPointF( 666689.50, 1799265.50 ), QPointF( 666689.50, 1799260.50 ), QPointF( 666684.50, 1799260.50 ), QPointF( 666684.50, 1799255.50 ), QPointF( 666679.50, 1799255.50 ), QPointF( 666679.50, 1799245.50 ), QPointF( 666674.50, 1799245.50 ), QPointF( 666674.50, 1799230.50 ), QPointF( 666679.50, 1799230.50 ), QPointF( 666679.50, 1799225.50 ),
    QPointF( 666674.50, 1799225.50 ), QPointF( 666674.50, 1799215.50 ), QPointF( 666679.50, 1799215.50 ), QPointF( 666679.50, 1799205.50 ), QPointF( 666684.50, 1799205.50 ), QPointF( 666684.50, 1799200.50 ), QPointF( 666689.50, 1799200.50 ), QPointF( 666689.50, 1799185.50 ), QPointF( 666694.50, 1799185.50 ), QPointF( 666694.50, 1799175.50 )
  } );

  QCOMPARE( polygonWatershed, polygonWatershedTest );

  bool needAdjusting;
  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  ReosWatershed *watershed1 = watershedDelineating.storeWatershed( true );
  QVERIFY( watershed1->hasDirectiondata() );
  ReosRasterExtent rasterExtent1( ReosMapExtent( 666664.5, 1798940.5, 667089.5, 1799430.5 ), 85, 98 );
  QCOMPARE( rasterExtent1, watershed1->directionExtent() );
  QVERIFY( watershedStore.masterWatershedCount() == 1 );
  QVERIFY( watershedStore.watershedCount() == 1 );
  QVERIFY( itemModel.rowCount( QModelIndex() ) == 1 );
  QVERIFY( watershed1->downstreamWatershed() == nullptr );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForDownstream );

  downstreamLine.clear();
  downstreamLine << QPointF( 666526.94, 1799174.90 ) << QPointF( 666664.30, 1799227.25 );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForExtent );

  predifinedExtent = ReosMapExtent( 666450.45, 1798920, 667143.16, 1799505.46 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( predifinedExtent ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  ReosWatershed *watershed2 = watershedDelineating.storeWatershed( true );
  QVERIFY( watershed2->hasDirectiondata() );
  QVERIFY( watershed1->hasDirectiondata() );
  QCOMPARE( watershed1->directions(), watershed2->directions() );
  QCOMPARE( watershed1->directionExtent(), watershed2->directionExtent() );
  QCOMPARE( watershedStore.masterWatershedCount(), 1 );
  QCOMPARE( watershedStore.watershedCount(), 3 ); //including residual watershed
  QVERIFY( watershed1->downstreamWatershed() == watershed2 );
  QVERIFY( watershed2->directUpstreamWatershedCount() == 2 ); //including residual watershed
  QVERIFY( watershed2->directUpstreamWatershed( 1 ) == watershed1 );
  QVERIFY( itemModel.rowCount( QModelIndex() ) == 1 );
  QVERIFY( itemModel.rowCount( itemModel.index( 0, 0, QModelIndex() ) ) == 2 ); //including residual watershed

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForDownstream );

  downstreamLine.clear();
  downstreamLine << QPointF( 666506.3, 1799336.8 ) << QPointF( 666599.38, 1799355.69 );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForExtent );
  predifinedExtent = ReosMapExtent( 666407.06, 1799254.47, 666838.9, 1799641.1 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( predifinedExtent ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );
  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  ReosWatershed *watershed3 = watershedDelineating.storeWatershed( true );
  QVERIFY( watershed3->downstreamWatershed() == nullptr );
  QVERIFY( watershed3->directUpstreamWatershedCount() == 0 );
  QCOMPARE( watershedStore.masterWatershedCount(), 2 );
  QCOMPARE( watershedStore.watershedCount(), 4 ); //including residual watershed
  QVERIFY( itemModel.rowCount( QModelIndex() ) == 2 );
  QVERIFY( itemModel.rowCount( itemModel.index( 0, 0, QModelIndex() ) ) == 2 );
  QVERIFY( itemModel.rowCount( itemModel.index( 1, 0, QModelIndex() ) ) == 0 );

  downstreamLine.clear();
  downstreamLine << QPointF( 666453.28, 1799229.84 ) << QPointF( 666495.14, 1799321.72 );
  QVERIFY( watershedDelineating.setDownstreamLine( downstreamLine ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForExtent );
  predifinedExtent = ReosMapExtent( 666407.06, 1798924.49, 667146.31, 1799678.08 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( predifinedExtent ) );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.startDelineating() );
  controler.waitForFinished();
  controler.reset();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );
  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  ReosWatershed *watershed4 = watershedDelineating.storeWatershed( true );
  QVERIFY( watershed4->downstreamWatershed() == nullptr );
  QVERIFY( watershed4->directUpstreamWatershedCount() == 3 );//including residual watershed
  QVERIFY( watershed4->directUpstreamWatershed( 1 ) == watershed2 );
  QVERIFY( watershed4->directUpstreamWatershed( 2 ) == watershed3 );

  QCOMPARE( watershedStore.masterWatershedCount(), 1 );
  QCOMPARE( watershedStore.watershedCount(), 6 ); //including residual watershed
  QCOMPARE( itemModel.rowCount( QModelIndex() ), 1 );
  QCOMPARE( itemModel.rowCount( itemModel.index( 0, 0, QModelIndex() ) ), 3 );
  QCOMPARE( itemModel.rowCount( itemModel.index( 1, 0, itemModel.index( 0, 0, QModelIndex() ) ) ), 2 );

  std::unique_ptr<ReosWatershed> removedWs( watershedStore.extractWatershed( watershed2 ) );
  QCOMPARE( removedWs->directUpstreamWatershedCount(), 0 );
  QCOMPARE( removedWs->downstreamWatershed(), nullptr );

}

QTEST_MAIN( ReosWatersehdTest )
#include "reos_watershed_test.moc"
