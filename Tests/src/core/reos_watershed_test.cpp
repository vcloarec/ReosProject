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
#include "reosrunoffmodel.h"
#include "reossyntheticrainfall.h"
#include "reostransferfunction.h"
#include "reoswatershedtree.h"
#include "reos_testutils.h"
#include "reosexporttovectorfile.h"
#include "reoshydrograph.h"
#include "reosmeteorologicmodel.h"


class ReosWatersehdTest: public QObject
{
    Q_OBJECT
  private slots:
    void inclusion();
    void watershedInteractions();
    void watershedDelineating();
    void watershedDelineatingWithBurningLine();
    void watershdDelineatingMultiWatershed();
    void concentrationTime();
    void runoffConstantCoefficient();

    void runoffhydrograph();

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
  ReosWatershedTree watershedTree( &gisEngine );

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
  ReosWatershedTree watershedStore( &gisEngine );
  ReosWatershedDelineating watershedDelineating( &rootModule, &watershedStore, &gisEngine );
  ReosWatershedItemModel itemModel( &watershedStore );
  std::unique_ptr<ModuleProcessControler> controler;

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

  QVERIFY( !watershedDelineating.prepareDelineating() );

  // Start the good delineating
  extent = ReosMapExtent( 661553.33, 1792732.0, 661780.44, 1792964.54 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

  QPolygonF polygonWatershed = watershedDelineating.lastWatershedDelineated();

  QPolygonF polygonWatershedTest = QPolygonF(
  {
    QPointF( 661598.50, 1792950.50 ), QPointF( 661598.50, 1792949.50 ), QPointF( 661597.50, 1792949.50 ), QPointF( 661597.50, 1792948.50 ), QPointF( 661596.50, 1792948.50 ), QPointF( 661596.50, 1792947.50 ), QPointF( 661595.50, 1792947.50 ), QPointF( 661595.50, 1792946.50 ), QPointF( 661594.50, 1792946.50 ), QPointF( 661594.50, 1792945.50 ),
    QPointF( 661593.50, 1792945.50 ), QPointF( 661593.50, 1792944.50 ), QPointF( 661592.50, 1792944.50 ), QPointF( 661592.50, 1792943.50 ), QPointF( 661591.50, 1792943.50 ), QPointF( 661591.50, 1792942.50 ), QPointF( 661590.50, 1792942.50 ), QPointF( 661590.50, 1792941.50 ), QPointF( 661589.50, 1792941.50 ), QPointF( 661589.50, 1792940.50 ),
    QPointF( 661588.50, 1792940.50 ), QPointF( 661588.50, 1792938.50 ), QPointF( 661587.50, 1792938.50 ), QPointF( 661587.50, 1792937.50 ), QPointF( 661586.50, 1792937.50 ), QPointF( 661586.50, 1792936.50 ), QPointF( 661584.50, 1792936.50 ), QPointF( 661584.50, 1792935.50 ), QPointF( 661583.50, 1792935.50 ), QPointF( 661583.50, 1792934.50 ),
    QPointF( 661582.50, 1792934.50 ), QPointF( 661582.50, 1792933.50 ), QPointF( 661581.50, 1792933.50 ), QPointF( 661581.50, 1792932.50 ), QPointF( 661580.50, 1792932.50 ), QPointF( 661580.50, 1792931.50 ), QPointF( 661576.50, 1792931.50 ), QPointF( 661576.50, 1792932.50 ), QPointF( 661575.50, 1792932.50 ), QPointF( 661575.50, 1792931.50 ),
    QPointF( 661574.50, 1792931.50 ), QPointF( 661574.50, 1792925.50 ), QPointF( 661573.50, 1792925.50 ), QPointF( 661573.50, 1792921.50 ), QPointF( 661572.50, 1792921.50 ), QPointF( 661572.50, 1792920.50 ), QPointF( 661571.50, 1792920.50 ), QPointF( 661571.50, 1792919.50 ), QPointF( 661570.50, 1792919.50 ), QPointF( 661570.50, 1792917.50 ),
    QPointF( 661569.50, 1792917.50 ), QPointF( 661569.50, 1792916.50 ), QPointF( 661567.50, 1792916.50 ), QPointF( 661567.50, 1792914.50 ), QPointF( 661565.50, 1792914.50 ), QPointF( 661565.50, 1792913.50 ), QPointF( 661564.50, 1792913.50 ), QPointF( 661564.50, 1792912.50 ), QPointF( 661563.50, 1792912.50 ), QPointF( 661563.50, 1792910.50 ),
    QPointF( 661564.50, 1792910.50 ), QPointF( 661564.50, 1792909.50 ), QPointF( 661567.50, 1792909.50 ), QPointF( 661567.50, 1792908.50 ), QPointF( 661572.50, 1792908.50 ), QPointF( 661572.50, 1792905.50 ), QPointF( 661571.50, 1792905.50 ), QPointF( 661571.50, 1792904.50 ), QPointF( 661574.50, 1792904.50 ), QPointF( 661574.50, 1792902.50 ),
    QPointF( 661576.50, 1792902.50 ), QPointF( 661576.50, 1792901.50 ), QPointF( 661577.50, 1792901.50 ), QPointF( 661577.50, 1792899.50 ), QPointF( 661576.50, 1792899.50 ), QPointF( 661576.50, 1792897.50 ), QPointF( 661575.50, 1792897.50 ), QPointF( 661575.50, 1792896.50 ), QPointF( 661574.50, 1792896.50 ), QPointF( 661574.50, 1792895.50 ),
    QPointF( 661575.50, 1792895.50 ), QPointF( 661575.50, 1792893.50 ), QPointF( 661576.50, 1792893.50 ), QPointF( 661576.50, 1792892.50 ), QPointF( 661577.50, 1792892.50 ), QPointF( 661577.50, 1792893.50 ), QPointF( 661578.50, 1792893.50 ), QPointF( 661578.50, 1792892.50 ), QPointF( 661581.50, 1792892.50 ), QPointF( 661581.50, 1792891.50 ),
    QPointF( 661580.50, 1792891.50 ), QPointF( 661580.50, 1792890.50 ), QPointF( 661579.50, 1792890.50 ), QPointF( 661579.50, 1792888.50 ), QPointF( 661580.50, 1792888.50 ), QPointF( 661580.50, 1792885.50 ), QPointF( 661581.50, 1792885.50 ), QPointF( 661581.50, 1792884.50 ), QPointF( 661584.50, 1792884.50 ), QPointF( 661584.50, 1792883.50 ),
    QPointF( 661585.50, 1792883.50 ), QPointF( 661585.50, 1792882.50 ), QPointF( 661584.50, 1792882.50 ), QPointF( 661584.50, 1792881.50 ), QPointF( 661583.50, 1792881.50 ), QPointF( 661583.50, 1792879.50 ), QPointF( 661584.50, 1792879.50 ), QPointF( 661584.50, 1792878.50 ), QPointF( 661585.50, 1792878.50 ), QPointF( 661585.50, 1792873.50 ),
    QPointF( 661584.50, 1792873.50 ), QPointF( 661584.50, 1792871.50 ), QPointF( 661583.50, 1792871.50 ), QPointF( 661583.50, 1792870.50 ), QPointF( 661581.50, 1792870.50 ), QPointF( 661581.50, 1792868.50 ), QPointF( 661583.50, 1792868.50 ), QPointF( 661583.50, 1792864.50 ), QPointF( 661582.50, 1792864.50 ), QPointF( 661582.50, 1792859.50 ),
    QPointF( 661583.50, 1792859.50 ), QPointF( 661583.50, 1792856.50 ), QPointF( 661582.50, 1792856.50 ), QPointF( 661582.50, 1792853.50 ), QPointF( 661583.50, 1792853.50 ), QPointF( 661583.50, 1792852.50 ), QPointF( 661584.50, 1792852.50 ), QPointF( 661584.50, 1792849.50 ), QPointF( 661585.50, 1792849.50 ), QPointF( 661585.50, 1792846.50 ),
    QPointF( 661584.50, 1792846.50 ), QPointF( 661584.50, 1792845.50 ), QPointF( 661585.50, 1792845.50 ), QPointF( 661585.50, 1792841.50 ), QPointF( 661584.50, 1792841.50 ), QPointF( 661584.50, 1792839.50 ), QPointF( 661585.50, 1792839.50 ), QPointF( 661585.50, 1792838.50 ), QPointF( 661586.50, 1792838.50 ), QPointF( 661586.50, 1792835.50 ),
    QPointF( 661584.50, 1792835.50 ), QPointF( 661584.50, 1792833.50 ), QPointF( 661583.50, 1792833.50 ), QPointF( 661583.50, 1792830.50 ), QPointF( 661586.50, 1792830.50 ), QPointF( 661586.50, 1792829.50 ), QPointF( 661585.50, 1792829.50 ), QPointF( 661585.50, 1792826.50 ), QPointF( 661586.50, 1792826.50 ), QPointF( 661586.50, 1792825.50 ),
    QPointF( 661587.50, 1792825.50 ), QPointF( 661587.50, 1792823.50 ), QPointF( 661586.50, 1792823.50 ), QPointF( 661586.50, 1792820.50 ), QPointF( 661588.50, 1792820.50 ), QPointF( 661588.50, 1792818.50 ), QPointF( 661587.50, 1792818.50 ), QPointF( 661587.50, 1792816.50 ), QPointF( 661588.50, 1792816.50 ), QPointF( 661588.50, 1792815.50 ),
    QPointF( 661589.50, 1792815.50 ), QPointF( 661589.50, 1792814.50 ), QPointF( 661592.50, 1792814.50 ), QPointF( 661592.50, 1792813.50 ), QPointF( 661597.50, 1792813.50 ), QPointF( 661597.50, 1792814.50 ), QPointF( 661601.50, 1792814.50 ), QPointF( 661601.50, 1792812.50 ), QPointF( 661602.50, 1792812.50 ), QPointF( 661602.50, 1792811.50 ),
    QPointF( 661601.50, 1792811.50 ), QPointF( 661601.50, 1792809.50 ), QPointF( 661602.50, 1792809.50 ), QPointF( 661602.50, 1792806.50 ), QPointF( 661601.50, 1792806.50 ), QPointF( 661601.50, 1792799.50 ), QPointF( 661600.50, 1792799.50 ), QPointF( 661600.50, 1792797.50 ), QPointF( 661602.50, 1792797.50 ), QPointF( 661602.50, 1792796.50 ),
    QPointF( 661601.50, 1792796.50 ), QPointF( 661601.50, 1792794.50 ), QPointF( 661602.50, 1792794.50 ), QPointF( 661602.50, 1792792.50 ), QPointF( 661603.50, 1792792.50 ), QPointF( 661603.50, 1792791.50 ), QPointF( 661604.50, 1792791.50 ), QPointF( 661604.50, 1792790.50 ), QPointF( 661606.50, 1792790.50 ), QPointF( 661606.50, 1792787.50 ),
    QPointF( 661608.50, 1792787.50 ), QPointF( 661608.50, 1792786.50 ), QPointF( 661610.50, 1792786.50 ), QPointF( 661610.50, 1792784.50 ), QPointF( 661611.50, 1792784.50 ), QPointF( 661611.50, 1792783.50 ), QPointF( 661613.50, 1792783.50 ), QPointF( 661613.50, 1792781.50 ), QPointF( 661618.50, 1792781.50 ), QPointF( 661618.50, 1792780.50 ),
    QPointF( 661619.50, 1792780.50 ), QPointF( 661619.50, 1792777.50 ), QPointF( 661622.50, 1792777.50 ), QPointF( 661622.50, 1792776.50 ), QPointF( 661623.50, 1792776.50 ), QPointF( 661623.50, 1792775.50 ), QPointF( 661624.50, 1792775.50 ), QPointF( 661624.50, 1792774.50 ), QPointF( 661627.50, 1792774.50 ), QPointF( 661627.50, 1792775.50 ),
    QPointF( 661628.50, 1792775.50 ), QPointF( 661628.50, 1792772.50 ), QPointF( 661629.50, 1792772.50 ), QPointF( 661629.50, 1792771.50 ), QPointF( 661631.50, 1792771.50 ), QPointF( 661631.50, 1792770.50 ), QPointF( 661634.50, 1792770.50 ), QPointF( 661634.50, 1792771.50 ), QPointF( 661635.50, 1792771.50 ), QPointF( 661635.50, 1792770.50 ),
    QPointF( 661636.50, 1792770.50 ), QPointF( 661636.50, 1792769.50 ), QPointF( 661642.50, 1792769.50 ), QPointF( 661642.50, 1792767.50 ), QPointF( 661644.50, 1792767.50 ), QPointF( 661644.50, 1792766.50 ), QPointF( 661646.50, 1792766.50 ), QPointF( 661646.50, 1792767.50 ), QPointF( 661647.50, 1792767.50 ), QPointF( 661647.50, 1792764.50 ),
    QPointF( 661648.50, 1792764.50 ), QPointF( 661648.50, 1792763.50 ), QPointF( 661649.50, 1792763.50 ), QPointF( 661649.50, 1792762.50 ), QPointF( 661650.50, 1792762.50 ), QPointF( 661650.50, 1792761.50 ), QPointF( 661649.50, 1792761.50 ), QPointF( 661649.50, 1792756.50 ), QPointF( 661651.50, 1792756.50 ), QPointF( 661651.50, 1792755.50 ),
    QPointF( 661652.50, 1792755.50 ), QPointF( 661652.50, 1792754.50 ), QPointF( 661653.50, 1792754.50 ), QPointF( 661653.50, 1792753.50 ), QPointF( 661654.50, 1792753.50 ), QPointF( 661654.50, 1792747.50 ), QPointF( 661655.50, 1792747.50 ), QPointF( 661655.50, 1792745.50 ), QPointF( 661656.50, 1792745.50 ), QPointF( 661656.50, 1792744.50 ),
    QPointF( 661657.50, 1792744.50 ), QPointF( 661657.50, 1792741.50 ), QPointF( 661658.50, 1792741.50 ), QPointF( 661658.50, 1792735.50 ), QPointF( 661659.50, 1792735.50 ), QPointF( 661659.50, 1792734.50 ), QPointF( 661660.50, 1792734.50 ), QPointF( 661660.50, 1792733.50 ), QPointF( 661662.50, 1792733.50 ), QPointF( 661662.50, 1792731.50 ),
    QPointF( 661669.50, 1792731.50 ), QPointF( 661669.50, 1792732.50 ), QPointF( 661672.50, 1792732.50 ), QPointF( 661672.50, 1792733.50 ), QPointF( 661674.50, 1792733.50 ), QPointF( 661674.50, 1792734.50 ), QPointF( 661676.50, 1792734.50 ), QPointF( 661676.50, 1792735.50 ), QPointF( 661677.50, 1792735.50 ), QPointF( 661677.50, 1792736.50 ),
    QPointF( 661678.50, 1792736.50 ), QPointF( 661678.50, 1792735.50 ), QPointF( 661680.50, 1792735.50 ), QPointF( 661680.50, 1792734.50 ), QPointF( 661681.50, 1792734.50 ), QPointF( 661681.50, 1792733.50 ), QPointF( 661682.50, 1792733.50 ), QPointF( 661682.50, 1792732.50 ), QPointF( 661683.50, 1792732.50 ), QPointF( 661683.50, 1792734.50 ),
    QPointF( 661684.50, 1792734.50 ), QPointF( 661684.50, 1792735.50 ), QPointF( 661685.50, 1792735.50 ), QPointF( 661685.50, 1792737.50 ), QPointF( 661687.50, 1792737.50 ), QPointF( 661687.50, 1792738.50 ), QPointF( 661689.50, 1792738.50 ), QPointF( 661689.50, 1792739.50 ), QPointF( 661690.50, 1792739.50 ), QPointF( 661690.50, 1792741.50 ),
    QPointF( 661696.50, 1792741.50 ), QPointF( 661696.50, 1792743.50 ), QPointF( 661706.50, 1792743.50 ), QPointF( 661706.50, 1792744.50 ), QPointF( 661708.50, 1792744.50 ), QPointF( 661708.50, 1792745.50 ), QPointF( 661712.50, 1792745.50 ), QPointF( 661712.50, 1792744.50 ), QPointF( 661717.50, 1792744.50 ), QPointF( 661717.50, 1792743.50 ),
    QPointF( 661719.50, 1792743.50 ), QPointF( 661719.50, 1792747.50 ), QPointF( 661718.50, 1792747.50 ), QPointF( 661718.50, 1792750.50 ), QPointF( 661721.50, 1792750.50 ), QPointF( 661721.50, 1792751.50 ), QPointF( 661722.50, 1792751.50 ), QPointF( 661722.50, 1792752.50 ), QPointF( 661723.50, 1792752.50 ), QPointF( 661723.50, 1792751.50 ),
    QPointF( 661725.50, 1792751.50 ), QPointF( 661725.50, 1792750.50 ), QPointF( 661730.50, 1792750.50 ), QPointF( 661730.50, 1792749.50 ), QPointF( 661731.50, 1792749.50 ), QPointF( 661731.50, 1792748.50 ), QPointF( 661733.50, 1792748.50 ), QPointF( 661733.50, 1792749.50 ), QPointF( 661734.50, 1792749.50 ), QPointF( 661734.50, 1792752.50 ),
    QPointF( 661736.50, 1792752.50 ), QPointF( 661736.50, 1792753.50 ), QPointF( 661737.50, 1792753.50 ), QPointF( 661737.50, 1792752.50 ), QPointF( 661741.50, 1792752.50 ), QPointF( 661741.50, 1792751.50 ), QPointF( 661742.50, 1792751.50 ), QPointF( 661742.50, 1792752.50 ), QPointF( 661746.50, 1792752.50 ), QPointF( 661746.50, 1792754.50 ),
    QPointF( 661747.50, 1792754.50 ), QPointF( 661747.50, 1792755.50 ), QPointF( 661748.50, 1792755.50 ), QPointF( 661748.50, 1792757.50 ), QPointF( 661746.50, 1792757.50 ), QPointF( 661746.50, 1792758.50 ), QPointF( 661745.50, 1792758.50 ), QPointF( 661745.50, 1792760.50 ), QPointF( 661747.50, 1792760.50 ), QPointF( 661747.50, 1792761.50 ),
    QPointF( 661748.50, 1792761.50 ), QPointF( 661748.50, 1792762.50 ), QPointF( 661749.50, 1792762.50 ), QPointF( 661749.50, 1792763.50 ), QPointF( 661752.50, 1792763.50 ), QPointF( 661752.50, 1792765.50 ), QPointF( 661753.50, 1792765.50 ), QPointF( 661753.50, 1792766.50 ), QPointF( 661754.50, 1792766.50 ), QPointF( 661754.50, 1792764.50 ),
    QPointF( 661759.50, 1792764.50 ), QPointF( 661759.50, 1792769.50 ), QPointF( 661757.50, 1792769.50 ), QPointF( 661757.50, 1792774.50 ), QPointF( 661759.50, 1792774.50 ), QPointF( 661759.50, 1792775.50 ), QPointF( 661760.50, 1792775.50 ), QPointF( 661760.50, 1792777.50 ), QPointF( 661761.50, 1792777.50 ), QPointF( 661761.50, 1792780.50 ),
    QPointF( 661760.50, 1792780.50 ), QPointF( 661760.50, 1792781.50 ), QPointF( 661761.50, 1792781.50 ), QPointF( 661761.50, 1792787.50 ), QPointF( 661762.50, 1792787.50 ), QPointF( 661762.50, 1792793.50 ), QPointF( 661761.50, 1792793.50 ), QPointF( 661761.50, 1792795.50 ), QPointF( 661760.50, 1792795.50 ), QPointF( 661760.50, 1792796.50 ),
    QPointF( 661759.50, 1792796.50 ), QPointF( 661759.50, 1792798.50 ), QPointF( 661758.50, 1792798.50 ), QPointF( 661758.50, 1792802.50 ), QPointF( 661757.50, 1792802.50 ), QPointF( 661757.50, 1792803.50 ), QPointF( 661756.50, 1792803.50 ), QPointF( 661756.50, 1792804.50 ), QPointF( 661755.50, 1792804.50 ), QPointF( 661755.50, 1792808.50 ),
    QPointF( 661753.50, 1792808.50 ), QPointF( 661753.50, 1792809.50 ), QPointF( 661752.50, 1792809.50 ), QPointF( 661752.50, 1792810.50 ), QPointF( 661753.50, 1792810.50 ), QPointF( 661753.50, 1792811.50 ), QPointF( 661754.50, 1792811.50 ), QPointF( 661754.50, 1792812.50 ), QPointF( 661755.50, 1792812.50 ), QPointF( 661755.50, 1792813.50 ),
    QPointF( 661756.50, 1792813.50 ), QPointF( 661756.50, 1792814.50 ), QPointF( 661755.50, 1792814.50 ), QPointF( 661755.50, 1792815.50 ), QPointF( 661754.50, 1792815.50 ), QPointF( 661754.50, 1792818.50 ), QPointF( 661753.50, 1792818.50 ), QPointF( 661753.50, 1792823.50 ), QPointF( 661752.50, 1792823.50 ), QPointF( 661752.50, 1792822.50 ),
    QPointF( 661744.50, 1792822.50 ), QPointF( 661744.50, 1792826.50 ), QPointF( 661743.50, 1792826.50 ), QPointF( 661743.50, 1792830.50 ), QPointF( 661744.50, 1792830.50 ), QPointF( 661744.50, 1792833.50 ), QPointF( 661743.50, 1792833.50 ), QPointF( 661743.50, 1792834.50 ), QPointF( 661744.50, 1792834.50 ), QPointF( 661744.50, 1792836.50 ),
    QPointF( 661745.50, 1792836.50 ), QPointF( 661745.50, 1792838.50 ), QPointF( 661744.50, 1792838.50 ), QPointF( 661744.50, 1792839.50 ), QPointF( 661742.50, 1792839.50 ), QPointF( 661742.50, 1792840.50 ), QPointF( 661741.50, 1792840.50 ), QPointF( 661741.50, 1792842.50 ), QPointF( 661740.50, 1792842.50 ), QPointF( 661740.50, 1792845.50 ),
    QPointF( 661744.50, 1792845.50 ), QPointF( 661744.50, 1792846.50 ), QPointF( 661747.50, 1792846.50 ), QPointF( 661747.50, 1792845.50 ), QPointF( 661748.50, 1792845.50 ), QPointF( 661748.50, 1792846.50 ), QPointF( 661749.50, 1792846.50 ), QPointF( 661749.50, 1792847.50 ), QPointF( 661750.50, 1792847.50 ), QPointF( 661750.50, 1792848.50 ),
    QPointF( 661751.50, 1792848.50 ), QPointF( 661751.50, 1792849.50 ), QPointF( 661752.50, 1792849.50 ), QPointF( 661752.50, 1792852.50 ), QPointF( 661751.50, 1792852.50 ), QPointF( 661751.50, 1792853.50 ), QPointF( 661752.50, 1792853.50 ), QPointF( 661752.50, 1792854.50 ), QPointF( 661753.50, 1792854.50 ), QPointF( 661753.50, 1792855.50 ),
    QPointF( 661754.50, 1792855.50 ), QPointF( 661754.50, 1792856.50 ), QPointF( 661755.50, 1792856.50 ), QPointF( 661755.50, 1792857.50 ), QPointF( 661756.50, 1792857.50 ), QPointF( 661756.50, 1792860.50 ), QPointF( 661755.50, 1792860.50 ), QPointF( 661755.50, 1792861.50 ), QPointF( 661754.50, 1792861.50 ), QPointF( 661754.50, 1792862.50 ),
    QPointF( 661753.50, 1792862.50 ), QPointF( 661753.50, 1792863.50 ), QPointF( 661752.50, 1792863.50 ), QPointF( 661752.50, 1792864.50 ), QPointF( 661753.50, 1792864.50 ), QPointF( 661753.50, 1792867.50 ), QPointF( 661754.50, 1792867.50 ), QPointF( 661754.50, 1792868.50 ), QPointF( 661756.50, 1792868.50 ), QPointF( 661756.50, 1792869.50 ),
    QPointF( 661757.50, 1792869.50 ), QPointF( 661757.50, 1792872.50 ), QPointF( 661752.50, 1792872.50 ), QPointF( 661752.50, 1792873.50 ), QPointF( 661750.50, 1792873.50 ), QPointF( 661750.50, 1792874.50 ), QPointF( 661751.50, 1792874.50 ), QPointF( 661751.50, 1792875.50 ), QPointF( 661753.50, 1792875.50 ), QPointF( 661753.50, 1792876.50 ),
    QPointF( 661755.50, 1792876.50 ), QPointF( 661755.50, 1792877.50 ), QPointF( 661756.50, 1792877.50 ), QPointF( 661756.50, 1792878.50 ), QPointF( 661758.50, 1792878.50 ), QPointF( 661758.50, 1792879.50 ), QPointF( 661759.50, 1792879.50 ), QPointF( 661759.50, 1792884.50 ), QPointF( 661760.50, 1792884.50 ), QPointF( 661760.50, 1792885.50 ),
    QPointF( 661759.50, 1792885.50 ), QPointF( 661759.50, 1792887.50 ), QPointF( 661762.50, 1792887.50 ), QPointF( 661762.50, 1792889.50 ), QPointF( 661763.50, 1792889.50 ), QPointF( 661763.50, 1792890.50 ), QPointF( 661764.50, 1792890.50 ), QPointF( 661764.50, 1792892.50 ), QPointF( 661763.50, 1792892.50 ), QPointF( 661763.50, 1792901.50 ),
    QPointF( 661762.50, 1792901.50 ), QPointF( 661762.50, 1792905.50 ), QPointF( 661757.50, 1792905.50 ), QPointF( 661757.50, 1792906.50 ), QPointF( 661754.50, 1792906.50 ), QPointF( 661754.50, 1792907.50 ), QPointF( 661753.50, 1792907.50 ), QPointF( 661753.50, 1792912.50 ), QPointF( 661756.50, 1792912.50 ), QPointF( 661756.50, 1792913.50 ),
    QPointF( 661758.50, 1792913.50 ), QPointF( 661758.50, 1792920.50 ), QPointF( 661757.50, 1792920.50 ), QPointF( 661757.50, 1792923.50 ), QPointF( 661760.50, 1792923.50 ), QPointF( 661760.50, 1792922.50 ), QPointF( 661762.50, 1792922.50 ), QPointF( 661762.50, 1792921.50 ), QPointF( 661763.50, 1792921.50 ), QPointF( 661763.50, 1792920.50 ),
    QPointF( 661765.50, 1792920.50 ), QPointF( 661765.50, 1792923.50 ), QPointF( 661764.50, 1792923.50 ), QPointF( 661764.50, 1792925.50 ), QPointF( 661762.50, 1792925.50 ), QPointF( 661762.50, 1792927.50 ), QPointF( 661763.50, 1792927.50 ), QPointF( 661763.50, 1792928.50 ), QPointF( 661766.50, 1792928.50 ), QPointF( 661766.50, 1792929.50 ),
    QPointF( 661767.50, 1792929.50 ), QPointF( 661767.50, 1792932.50 ), QPointF( 661768.50, 1792932.50 ), QPointF( 661768.50, 1792934.50 ), QPointF( 661767.50, 1792934.50 ), QPointF( 661767.50, 1792935.50 ), QPointF( 661766.50, 1792935.50 ), QPointF( 661766.50, 1792936.50 ), QPointF( 661765.50, 1792936.50 ), QPointF( 661765.50, 1792937.50 ),
    QPointF( 661764.50, 1792937.50 ), QPointF( 661764.50, 1792938.50 ), QPointF( 661763.50, 1792938.50 ), QPointF( 661763.50, 1792939.50 ), QPointF( 661762.50, 1792939.50 ), QPointF( 661762.50, 1792940.50 ), QPointF( 661761.50, 1792940.50 ), QPointF( 661761.50, 1792941.50 ), QPointF( 661760.50, 1792941.50 ), QPointF( 661760.50, 1792942.50 ),
    QPointF( 661759.50, 1792942.50 ), QPointF( 661759.50, 1792943.50 ), QPointF( 661757.50, 1792943.50 ), QPointF( 661757.50, 1792944.50 ), QPointF( 661756.50, 1792944.50 ), QPointF( 661756.50, 1792947.50 ), QPointF( 661755.50, 1792947.50 ), QPointF( 661755.50, 1792948.50 ), QPointF( 661754.50, 1792948.50 ), QPointF( 661754.50, 1792949.50 ),
    QPointF( 661753.50, 1792949.50 ), QPointF( 661753.50, 1792950.50 ), QPointF( 661690.50, 1792950.50 ), QPointF( 661690.50, 1792951.50 ), QPointF( 661687.50, 1792951.50 ), QPointF( 661687.50, 1792952.50 ), QPointF( 661685.50, 1792952.50 ), QPointF( 661685.50, 1792953.50 ), QPointF( 661683.50, 1792953.50 ), QPointF( 661683.50, 1792954.50 ),
    QPointF( 661681.50, 1792954.50 ), QPointF( 661681.50, 1792953.50 ), QPointF( 661680.50, 1792953.50 ), QPointF( 661680.50, 1792952.50 ), QPointF( 661678.50, 1792952.50 ), QPointF( 661678.50, 1792953.50 ), QPointF( 661677.50, 1792953.50 ), QPointF( 661677.50, 1792952.50 ), QPointF( 661676.50, 1792952.50 ), QPointF( 661676.50, 1792951.50 ),
    QPointF( 661675.50, 1792951.50 ), QPointF( 661675.50, 1792950.50 ), QPointF( 661599.50, 1792950.50 )
  } );

  QCOMPARE( polygonWatershed, polygonWatershedTest );

  QPolygonF streamLine = watershedDelineating.lastStreamLine();

  QPolygonF streamLinetest(
  {
    QPointF( 661674.00, 1792734.00 ), QPointF( 661676.00, 1792736.00 ), QPointF( 661676.00, 1792738.00 ), QPointF( 661678.00, 1792740.00 ), QPointF( 661678.00, 1792741.00 ), QPointF( 661679.00, 1792742.00 ), QPointF( 661679.00, 1792743.00 ), QPointF( 661678.00, 1792744.00 ), QPointF( 661678.00, 1792745.00 ), QPointF( 661679.00, 1792746.00 ),
    QPointF( 661679.00, 1792747.00 ), QPointF( 661681.00, 1792749.00 ), QPointF( 661681.00, 1792750.00 ), QPointF( 661682.00, 1792751.00 ), QPointF( 661682.00, 1792754.00 ), QPointF( 661683.00, 1792755.00 ), QPointF( 661683.00, 1792756.00 ), QPointF( 661682.00, 1792756.00 ), QPointF( 661679.00, 1792759.00 ), QPointF( 661678.00, 1792759.00 ),
    QPointF( 661678.00, 1792758.00 ), QPointF( 661677.00, 1792757.00 ), QPointF( 661676.00, 1792758.00 ), QPointF( 661670.00, 1792758.00 ), QPointF( 661669.00, 1792759.00 ), QPointF( 661668.00, 1792759.00 ), QPointF( 661667.00, 1792760.00 ), QPointF( 661664.00, 1792760.00 ), QPointF( 661663.00, 1792761.00 ), QPointF( 661662.00, 1792760.00 ),
    QPointF( 661661.00, 1792761.00 ), QPointF( 661659.00, 1792761.00 ), QPointF( 661658.00, 1792762.00 ), QPointF( 661654.00, 1792762.00 ), QPointF( 661653.00, 1792763.00 ), QPointF( 661653.00, 1792765.00 ), QPointF( 661651.00, 1792767.00 ), QPointF( 661651.00, 1792768.00 ), QPointF( 661650.00, 1792768.00 ), QPointF( 661650.00, 1792769.00 ),
    QPointF( 661649.00, 1792770.00 ), QPointF( 661649.00, 1792773.00 ), QPointF( 661646.00, 1792776.00 ), QPointF( 661646.00, 1792782.00 ), QPointF( 661650.00, 1792786.00 ), QPointF( 661650.00, 1792787.00 ), QPointF( 661652.00, 1792789.00 ), QPointF( 661652.00, 1792790.00 ), QPointF( 661654.00, 1792792.00 ), QPointF( 661654.00, 1792794.00 ),
    QPointF( 661655.00, 1792795.00 ), QPointF( 661656.00, 1792795.00 ), QPointF( 661658.00, 1792797.00 ), QPointF( 661658.00, 1792806.00 ), QPointF( 661659.00, 1792807.00 ), QPointF( 661659.00, 1792810.00 ), QPointF( 661658.00, 1792811.00 ), QPointF( 661658.00, 1792812.00 ), QPointF( 661659.00, 1792813.00 ), QPointF( 661659.00, 1792814.00 ),
    QPointF( 661660.00, 1792814.00 ), QPointF( 661661.00, 1792815.00 ), QPointF( 661661.00, 1792816.00 ), QPointF( 661662.00, 1792817.00 ), QPointF( 661665.00, 1792817.00 ), QPointF( 661667.00, 1792819.00 ), QPointF( 661667.00, 1792820.00 ), QPointF( 661668.00, 1792821.00 ), QPointF( 661668.00, 1792824.00 ), QPointF( 661667.00, 1792825.00 ),
    QPointF( 661667.00, 1792829.00 ), QPointF( 661670.00, 1792832.00 ), QPointF( 661670.00, 1792834.00 ), QPointF( 661671.00, 1792835.00 ), QPointF( 661672.00, 1792835.00 ), QPointF( 661672.00, 1792839.00 ), QPointF( 661671.00, 1792840.00 ), QPointF( 661671.00, 1792841.00 ), QPointF( 661670.00, 1792842.00 ), QPointF( 661670.00, 1792843.00 ),
    QPointF( 661671.00, 1792844.00 ), QPointF( 661671.00, 1792846.00 ), QPointF( 661670.00, 1792847.00 ), QPointF( 661670.00, 1792849.00 ), QPointF( 661671.00, 1792850.00 ), QPointF( 661671.00, 1792853.00 ), QPointF( 661672.00, 1792853.00 ), QPointF( 661674.00, 1792855.00 ), QPointF( 661674.00, 1792857.00 ), QPointF( 661673.00, 1792858.00 ),
    QPointF( 661672.00, 1792858.00 ), QPointF( 661671.00, 1792859.00 ), QPointF( 661671.00, 1792860.00 ), QPointF( 661667.00, 1792864.00 ), QPointF( 661667.00, 1792866.00 ), QPointF( 661666.00, 1792867.00 ), QPointF( 661666.00, 1792868.00 ), QPointF( 661665.00, 1792869.00 ), QPointF( 661665.00, 1792870.00 ), QPointF( 661664.00, 1792871.00 ),
    QPointF( 661664.00, 1792875.00 ), QPointF( 661665.00, 1792876.00 ), QPointF( 661665.00, 1792879.00 ), QPointF( 661666.00, 1792880.00 ), QPointF( 661666.00, 1792882.00 ), QPointF( 661668.00, 1792884.00 ), QPointF( 661669.00, 1792884.00 ), QPointF( 661670.00, 1792885.00 ), QPointF( 661670.00, 1792886.00 ), QPointF( 661669.00, 1792887.00 ),
    QPointF( 661669.00, 1792891.00 ), QPointF( 661671.00, 1792893.00 ), QPointF( 661671.00, 1792904.00 ), QPointF( 661668.00, 1792907.00 ), QPointF( 661668.00, 1792909.00 ), QPointF( 661672.00, 1792913.00 ), QPointF( 661672.00, 1792914.00 ), QPointF( 661673.00, 1792915.00 ), QPointF( 661673.00, 1792917.00 ), QPointF( 661674.00, 1792918.00 ),
    QPointF( 661673.00, 1792919.00 ), QPointF( 661673.00, 1792924.00 ), QPointF( 661669.00, 1792928.00 ), QPointF( 661668.00, 1792928.00 ), QPointF( 661668.00, 1792929.00 ), QPointF( 661670.00, 1792931.00 ), QPointF( 661670.00, 1792934.00 ), QPointF( 661668.00, 1792936.00 ), QPointF( 661668.00, 1792937.00 ), QPointF( 661667.00, 1792938.00 ),
    QPointF( 661667.00, 1792939.00 ), QPointF( 661666.00, 1792940.00 ), QPointF( 661666.00, 1792941.00 ), QPointF( 661665.00, 1792942.00 ), QPointF( 661664.00, 1792941.00 ), QPointF( 661663.00, 1792942.00 ), QPointF( 661663.00, 1792944.00 ), QPointF( 661661.00, 1792946.00 ), QPointF( 661660.00, 1792946.00 ), QPointF( 661660.00, 1792948.00 ),
    QPointF( 661659.00, 1792949.00 ), QPointF( 661659.00, 1792950.00 )} );

  QCOMPARE( streamLine, streamLinetest );

  //! Watershed exceed predfined extent--> predefined extent not valid, need to set another one
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingWithBroughtBackExtent );

  //! Restart with a bigger extent
  extent = ReosMapExtent( 661553.33, 1792690, 661780.44, 1792964.54 );
  QVERIFY( watershedDelineating.setPreDefinedExtent( extent ) );
  QCOMPARE( watershedDelineating.currentState(), ReosWatershedDelineating::WaitingforProceed );
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  bool needAdjusting;
  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  watershedDelineating.storeWatershed( true );
  QVERIFY( watershedStore.watershedCount() == 1 );
  QVERIFY( watershedStore.masterWatershedCount() == 1 );
  QVERIFY( itemModel.rowCount( QModelIndex() ) == 1 );
  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForDownstream );
  ReosWatershed *ws = watershedStore.allWatersheds().at( 0 );
  QVERIFY( equal( ws->averageElevation()->value(), 23.2079186831, 0.00000001 ) );

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

  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

  polygonWatershed = watershedDelineating.lastWatershedDelineated();

  polygonWatershedTest = QPolygonF(
  {
    QPointF( 661645.50, 1792842.50 ), QPointF( 661645.50, 1792841.50 ), QPointF( 661644.50, 1792841.50 ), QPointF( 661644.50, 1792840.50 ), QPointF( 661630.50, 1792840.50 ), QPointF( 661630.50, 1792839.50 ), QPointF( 661629.50, 1792839.50 ), QPointF( 661629.50, 1792838.50 ), QPointF( 661627.50, 1792838.50 ), QPointF( 661627.50, 1792837.50 ),
    QPointF( 661617.50, 1792837.50 ), QPointF( 661617.50, 1792836.50 ), QPointF( 661616.50, 1792836.50 ), QPointF( 661616.50, 1792835.50 ), QPointF( 661615.50, 1792835.50 ), QPointF( 661615.50, 1792834.50 ), QPointF( 661614.50, 1792834.50 ), QPointF( 661614.50, 1792833.50 ), QPointF( 661613.50, 1792833.50 ), QPointF( 661613.50, 1792832.50 ),
    QPointF( 661612.50, 1792832.50 ), QPointF( 661612.50, 1792831.50 ), QPointF( 661611.50, 1792831.50 ), QPointF( 661611.50, 1792827.50 ), QPointF( 661612.50, 1792827.50 ), QPointF( 661612.50, 1792826.50 ), QPointF( 661611.50, 1792826.50 ), QPointF( 661611.50, 1792824.50 ), QPointF( 661605.50, 1792824.50 ), QPointF( 661605.50, 1792827.50 ),
    QPointF( 661601.50, 1792827.50 ), QPointF( 661601.50, 1792826.50 ), QPointF( 661600.50, 1792826.50 ), QPointF( 661600.50, 1792825.50 ), QPointF( 661599.50, 1792825.50 ), QPointF( 661599.50, 1792824.50 ), QPointF( 661598.50, 1792824.50 ), QPointF( 661598.50, 1792823.50 ), QPointF( 661599.50, 1792823.50 ), QPointF( 661599.50, 1792822.50 ),
    QPointF( 661600.50, 1792822.50 ), QPointF( 661600.50, 1792820.50 ), QPointF( 661599.50, 1792820.50 ), QPointF( 661599.50, 1792818.50 ), QPointF( 661601.50, 1792818.50 ), QPointF( 661601.50, 1792812.50 ), QPointF( 661602.50, 1792812.50 ), QPointF( 661602.50, 1792811.50 ), QPointF( 661601.50, 1792811.50 ), QPointF( 661601.50, 1792809.50 ),
    QPointF( 661602.50, 1792809.50 ), QPointF( 661602.50, 1792806.50 ), QPointF( 661601.50, 1792806.50 ), QPointF( 661601.50, 1792799.50 ), QPointF( 661600.50, 1792799.50 ), QPointF( 661600.50, 1792797.50 ), QPointF( 661602.50, 1792797.50 ), QPointF( 661602.50, 1792796.50 ), QPointF( 661601.50, 1792796.50 ), QPointF( 661601.50, 1792794.50 ),
    QPointF( 661602.50, 1792794.50 ), QPointF( 661602.50, 1792792.50 ), QPointF( 661603.50, 1792792.50 ), QPointF( 661603.50, 1792791.50 ), QPointF( 661604.50, 1792791.50 ), QPointF( 661604.50, 1792790.50 ), QPointF( 661606.50, 1792790.50 ), QPointF( 661606.50, 1792787.50 ), QPointF( 661608.50, 1792787.50 ), QPointF( 661608.50, 1792786.50 ),
    QPointF( 661610.50, 1792786.50 ), QPointF( 661610.50, 1792784.50 ), QPointF( 661611.50, 1792784.50 ), QPointF( 661611.50, 1792783.50 ), QPointF( 661613.50, 1792783.50 ), QPointF( 661613.50, 1792781.50 ), QPointF( 661618.50, 1792781.50 ), QPointF( 661618.50, 1792780.50 ), QPointF( 661619.50, 1792780.50 ), QPointF( 661619.50, 1792777.50 ),
    QPointF( 661622.50, 1792777.50 ), QPointF( 661622.50, 1792776.50 ), QPointF( 661623.50, 1792776.50 ), QPointF( 661623.50, 1792775.50 ), QPointF( 661624.50, 1792775.50 ), QPointF( 661624.50, 1792774.50 ), QPointF( 661627.50, 1792774.50 ), QPointF( 661627.50, 1792775.50 ), QPointF( 661628.50, 1792775.50 ), QPointF( 661628.50, 1792772.50 ),
    QPointF( 661629.50, 1792772.50 ), QPointF( 661629.50, 1792771.50 ), QPointF( 661631.50, 1792771.50 ), QPointF( 661631.50, 1792770.50 ), QPointF( 661634.50, 1792770.50 ), QPointF( 661634.50, 1792771.50 ), QPointF( 661635.50, 1792771.50 ), QPointF( 661635.50, 1792770.50 ), QPointF( 661636.50, 1792770.50 ), QPointF( 661636.50, 1792769.50 ),
    QPointF( 661642.50, 1792769.50 ), QPointF( 661642.50, 1792767.50 ), QPointF( 661644.50, 1792767.50 ), QPointF( 661644.50, 1792766.50 ), QPointF( 661646.50, 1792766.50 ), QPointF( 661646.50, 1792767.50 ), QPointF( 661647.50, 1792767.50 ), QPointF( 661647.50, 1792764.50 ), QPointF( 661648.50, 1792764.50 ), QPointF( 661648.50, 1792763.50 ),
    QPointF( 661649.50, 1792763.50 ), QPointF( 661649.50, 1792762.50 ), QPointF( 661650.50, 1792762.50 ), QPointF( 661650.50, 1792761.50 ), QPointF( 661649.50, 1792761.50 ), QPointF( 661649.50, 1792756.50 ), QPointF( 661651.50, 1792756.50 ), QPointF( 661651.50, 1792755.50 ), QPointF( 661652.50, 1792755.50 ), QPointF( 661652.50, 1792754.50 ),
    QPointF( 661653.50, 1792754.50 ), QPointF( 661653.50, 1792753.50 ), QPointF( 661654.50, 1792753.50 ), QPointF( 661654.50, 1792747.50 ), QPointF( 661655.50, 1792747.50 ), QPointF( 661655.50, 1792745.50 ), QPointF( 661656.50, 1792745.50 ), QPointF( 661656.50, 1792744.50 ), QPointF( 661657.50, 1792744.50 ), QPointF( 661657.50, 1792741.50 ),
    QPointF( 661658.50, 1792741.50 ), QPointF( 661658.50, 1792735.50 ), QPointF( 661659.50, 1792735.50 ), QPointF( 661659.50, 1792734.50 ), QPointF( 661660.50, 1792734.50 ), QPointF( 661660.50, 1792733.50 ), QPointF( 661661.50, 1792733.50 ), QPointF( 661661.50, 1792732.50 ), QPointF( 661660.50, 1792732.50 ), QPointF( 661660.50, 1792731.50 ),
    QPointF( 661661.50, 1792731.50 ), QPointF( 661661.50, 1792729.50 ), QPointF( 661663.50, 1792729.50 ), QPointF( 661663.50, 1792728.50 ), QPointF( 661664.50, 1792728.50 ), QPointF( 661664.50, 1792725.50 ), QPointF( 661665.50, 1792725.50 ), QPointF( 661665.50, 1792724.50 ), QPointF( 661666.50, 1792724.50 ), QPointF( 661666.50, 1792723.50 ),
    QPointF( 661668.50, 1792723.50 ), QPointF( 661668.50, 1792722.50 ), QPointF( 661670.50, 1792722.50 ), QPointF( 661670.50, 1792723.50 ), QPointF( 661671.50, 1792723.50 ), QPointF( 661671.50, 1792727.50 ), QPointF( 661674.50, 1792727.50 ), QPointF( 661674.50, 1792728.50 ), QPointF( 661676.50, 1792728.50 ), QPointF( 661676.50, 1792729.50 ),
    QPointF( 661680.50, 1792729.50 ), QPointF( 661680.50, 1792730.50 ), QPointF( 661681.50, 1792730.50 ), QPointF( 661681.50, 1792731.50 ), QPointF( 661682.50, 1792731.50 ), QPointF( 661682.50, 1792732.50 ), QPointF( 661683.50, 1792732.50 ), QPointF( 661683.50, 1792734.50 ), QPointF( 661684.50, 1792734.50 ), QPointF( 661684.50, 1792735.50 ),
    QPointF( 661685.50, 1792735.50 ), QPointF( 661685.50, 1792737.50 ), QPointF( 661687.50, 1792737.50 ), QPointF( 661687.50, 1792738.50 ), QPointF( 661689.50, 1792738.50 ), QPointF( 661689.50, 1792739.50 ), QPointF( 661690.50, 1792739.50 ), QPointF( 661690.50, 1792741.50 ), QPointF( 661696.50, 1792741.50 ), QPointF( 661696.50, 1792743.50 ),
    QPointF( 661706.50, 1792743.50 ), QPointF( 661706.50, 1792744.50 ), QPointF( 661708.50, 1792744.50 ), QPointF( 661708.50, 1792745.50 ), QPointF( 661712.50, 1792745.50 ), QPointF( 661712.50, 1792744.50 ), QPointF( 661717.50, 1792744.50 ), QPointF( 661717.50, 1792743.50 ), QPointF( 661719.50, 1792743.50 ), QPointF( 661719.50, 1792747.50 ),
    QPointF( 661718.50, 1792747.50 ), QPointF( 661718.50, 1792750.50 ), QPointF( 661721.50, 1792750.50 ), QPointF( 661721.50, 1792751.50 ), QPointF( 661722.50, 1792751.50 ), QPointF( 661722.50, 1792752.50 ), QPointF( 661723.50, 1792752.50 ), QPointF( 661723.50, 1792751.50 ), QPointF( 661725.50, 1792751.50 ), QPointF( 661725.50, 1792750.50 ),
    QPointF( 661730.50, 1792750.50 ), QPointF( 661730.50, 1792749.50 ), QPointF( 661731.50, 1792749.50 ), QPointF( 661731.50, 1792748.50 ), QPointF( 661733.50, 1792748.50 ), QPointF( 661733.50, 1792749.50 ), QPointF( 661734.50, 1792749.50 ), QPointF( 661734.50, 1792752.50 ), QPointF( 661736.50, 1792752.50 ), QPointF( 661736.50, 1792753.50 ),
    QPointF( 661737.50, 1792753.50 ), QPointF( 661737.50, 1792752.50 ), QPointF( 661741.50, 1792752.50 ), QPointF( 661741.50, 1792751.50 ), QPointF( 661742.50, 1792751.50 ), QPointF( 661742.50, 1792752.50 ), QPointF( 661746.50, 1792752.50 ), QPointF( 661746.50, 1792754.50 ), QPointF( 661747.50, 1792754.50 ), QPointF( 661747.50, 1792755.50 ),
    QPointF( 661748.50, 1792755.50 ), QPointF( 661748.50, 1792757.50 ), QPointF( 661746.50, 1792757.50 ), QPointF( 661746.50, 1792758.50 ), QPointF( 661745.50, 1792758.50 ), QPointF( 661745.50, 1792760.50 ), QPointF( 661747.50, 1792760.50 ), QPointF( 661747.50, 1792761.50 ), QPointF( 661748.50, 1792761.50 ), QPointF( 661748.50, 1792762.50 ),
    QPointF( 661749.50, 1792762.50 ), QPointF( 661749.50, 1792763.50 ), QPointF( 661752.50, 1792763.50 ), QPointF( 661752.50, 1792765.50 ), QPointF( 661753.50, 1792765.50 ), QPointF( 661753.50, 1792766.50 ), QPointF( 661754.50, 1792766.50 ), QPointF( 661754.50, 1792764.50 ), QPointF( 661759.50, 1792764.50 ), QPointF( 661759.50, 1792769.50 ),
    QPointF( 661757.50, 1792769.50 ), QPointF( 661757.50, 1792774.50 ), QPointF( 661759.50, 1792774.50 ), QPointF( 661759.50, 1792775.50 ), QPointF( 661760.50, 1792775.50 ), QPointF( 661760.50, 1792777.50 ), QPointF( 661761.50, 1792777.50 ), QPointF( 661761.50, 1792780.50 ), QPointF( 661760.50, 1792780.50 ), QPointF( 661760.50, 1792781.50 ),
    QPointF( 661761.50, 1792781.50 ), QPointF( 661761.50, 1792787.50 ), QPointF( 661762.50, 1792787.50 ), QPointF( 661762.50, 1792793.50 ), QPointF( 661761.50, 1792793.50 ), QPointF( 661761.50, 1792795.50 ), QPointF( 661760.50, 1792795.50 ), QPointF( 661760.50, 1792796.50 ), QPointF( 661759.50, 1792796.50 ), QPointF( 661759.50, 1792798.50 ),
    QPointF( 661758.50, 1792798.50 ), QPointF( 661758.50, 1792802.50 ), QPointF( 661757.50, 1792802.50 ), QPointF( 661757.50, 1792803.50 ), QPointF( 661756.50, 1792803.50 ), QPointF( 661756.50, 1792804.50 ), QPointF( 661755.50, 1792804.50 ), QPointF( 661755.50, 1792808.50 ), QPointF( 661753.50, 1792808.50 ), QPointF( 661753.50, 1792809.50 ),
    QPointF( 661752.50, 1792809.50 ), QPointF( 661752.50, 1792810.50 ), QPointF( 661753.50, 1792810.50 ), QPointF( 661753.50, 1792811.50 ), QPointF( 661754.50, 1792811.50 ), QPointF( 661754.50, 1792812.50 ), QPointF( 661755.50, 1792812.50 ), QPointF( 661755.50, 1792813.50 ), QPointF( 661756.50, 1792813.50 ), QPointF( 661756.50, 1792814.50 ),
    QPointF( 661755.50, 1792814.50 ), QPointF( 661755.50, 1792815.50 ), QPointF( 661754.50, 1792815.50 ), QPointF( 661754.50, 1792818.50 ), QPointF( 661753.50, 1792818.50 ), QPointF( 661753.50, 1792823.50 ), QPointF( 661752.50, 1792823.50 ), QPointF( 661752.50, 1792822.50 ), QPointF( 661744.50, 1792822.50 ), QPointF( 661744.50, 1792826.50 ),
    QPointF( 661743.50, 1792826.50 ), QPointF( 661743.50, 1792830.50 ), QPointF( 661744.50, 1792830.50 ), QPointF( 661744.50, 1792833.50 ), QPointF( 661743.50, 1792833.50 ), QPointF( 661743.50, 1792834.50 ), QPointF( 661744.50, 1792834.50 ), QPointF( 661744.50, 1792836.50 ), QPointF( 661745.50, 1792836.50 ), QPointF( 661745.50, 1792838.50 ),
    QPointF( 661744.50, 1792838.50 ), QPointF( 661744.50, 1792839.50 ), QPointF( 661742.50, 1792839.50 ), QPointF( 661742.50, 1792840.50 ), QPointF( 661741.50, 1792840.50 ), QPointF( 661741.50, 1792842.50 ), QPointF( 661722.50, 1792842.50 ), QPointF( 661722.50, 1792843.50 ), QPointF( 661721.50, 1792843.50 ), QPointF( 661721.50, 1792844.50 ),
    QPointF( 661695.50, 1792844.50 ), QPointF( 661695.50, 1792843.50 ), QPointF( 661678.50, 1792843.50 ), QPointF( 661678.50, 1792842.50 ), QPointF( 661668.50, 1792842.50 ), QPointF( 661668.50, 1792843.50 ), QPointF( 661664.50, 1792843.50 ), QPointF( 661664.50, 1792844.50 ), QPointF( 661662.50, 1792844.50 ), QPointF( 661662.50, 1792843.50 ),
    QPointF( 661652.50, 1792843.50 ), QPointF( 661652.50, 1792844.50 ), QPointF( 661651.50, 1792844.50 ), QPointF( 661651.50, 1792843.50 ), QPointF( 661646.50, 1792843.50 ), QPointF( 661646.50, 1792842.50 )} );

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
  ReosWatershedTree watershedStore( &gisEngine );;
  ReosWatershedDelineating watershedDelineating( &rootModule, &watershedStore, &gisEngine );
  std::unique_ptr<ModuleProcessControler> controler;


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
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();


  QPolygonF polygonWatershed = watershedDelineating.lastWatershedDelineated();

  QPolygonF polygonWatershedTest = QPolygonF(
  {
    QPointF( 661598.50, 1792950.50 ), QPointF( 661598.50, 1792949.50 ), QPointF( 661597.50, 1792949.50 ), QPointF( 661597.50, 1792948.50 ), QPointF( 661596.50, 1792948.50 ), QPointF( 661596.50, 1792947.50 ), QPointF( 661595.50, 1792947.50 ), QPointF( 661595.50, 1792946.50 ), QPointF( 661594.50, 1792946.50 ), QPointF( 661594.50, 1792945.50 ),
    QPointF( 661593.50, 1792945.50 ), QPointF( 661593.50, 1792944.50 ), QPointF( 661592.50, 1792944.50 ), QPointF( 661592.50, 1792943.50 ), QPointF( 661591.50, 1792943.50 ), QPointF( 661591.50, 1792942.50 ), QPointF( 661590.50, 1792942.50 ), QPointF( 661590.50, 1792941.50 ), QPointF( 661589.50, 1792941.50 ), QPointF( 661589.50, 1792940.50 ),
    QPointF( 661588.50, 1792940.50 ), QPointF( 661588.50, 1792938.50 ), QPointF( 661587.50, 1792938.50 ), QPointF( 661587.50, 1792937.50 ), QPointF( 661586.50, 1792937.50 ), QPointF( 661586.50, 1792936.50 ), QPointF( 661584.50, 1792936.50 ), QPointF( 661584.50, 1792935.50 ), QPointF( 661583.50, 1792935.50 ), QPointF( 661583.50, 1792934.50 ),
    QPointF( 661582.50, 1792934.50 ), QPointF( 661582.50, 1792933.50 ), QPointF( 661581.50, 1792933.50 ), QPointF( 661581.50, 1792932.50 ), QPointF( 661580.50, 1792932.50 ), QPointF( 661580.50, 1792931.50 ), QPointF( 661576.50, 1792931.50 ), QPointF( 661576.50, 1792932.50 ), QPointF( 661575.50, 1792932.50 ), QPointF( 661575.50, 1792931.50 ),
    QPointF( 661574.50, 1792931.50 ), QPointF( 661574.50, 1792925.50 ), QPointF( 661573.50, 1792925.50 ), QPointF( 661573.50, 1792921.50 ), QPointF( 661572.50, 1792921.50 ), QPointF( 661572.50, 1792920.50 ), QPointF( 661571.50, 1792920.50 ), QPointF( 661571.50, 1792919.50 ), QPointF( 661570.50, 1792919.50 ), QPointF( 661570.50, 1792917.50 ),
    QPointF( 661569.50, 1792917.50 ), QPointF( 661569.50, 1792916.50 ), QPointF( 661567.50, 1792916.50 ), QPointF( 661567.50, 1792914.50 ), QPointF( 661565.50, 1792914.50 ), QPointF( 661565.50, 1792913.50 ), QPointF( 661564.50, 1792913.50 ), QPointF( 661564.50, 1792912.50 ), QPointF( 661563.50, 1792912.50 ), QPointF( 661563.50, 1792910.50 ),
    QPointF( 661564.50, 1792910.50 ), QPointF( 661564.50, 1792909.50 ), QPointF( 661567.50, 1792909.50 ), QPointF( 661567.50, 1792908.50 ), QPointF( 661572.50, 1792908.50 ), QPointF( 661572.50, 1792905.50 ), QPointF( 661571.50, 1792905.50 ), QPointF( 661571.50, 1792904.50 ), QPointF( 661574.50, 1792904.50 ), QPointF( 661574.50, 1792902.50 ),
    QPointF( 661576.50, 1792902.50 ), QPointF( 661576.50, 1792901.50 ), QPointF( 661577.50, 1792901.50 ), QPointF( 661577.50, 1792899.50 ), QPointF( 661576.50, 1792899.50 ), QPointF( 661576.50, 1792897.50 ), QPointF( 661575.50, 1792897.50 ), QPointF( 661575.50, 1792896.50 ), QPointF( 661574.50, 1792896.50 ), QPointF( 661574.50, 1792895.50 ),
    QPointF( 661575.50, 1792895.50 ), QPointF( 661575.50, 1792893.50 ), QPointF( 661576.50, 1792893.50 ), QPointF( 661576.50, 1792892.50 ), QPointF( 661577.50, 1792892.50 ), QPointF( 661577.50, 1792893.50 ), QPointF( 661578.50, 1792893.50 ), QPointF( 661578.50, 1792892.50 ), QPointF( 661581.50, 1792892.50 ), QPointF( 661581.50, 1792891.50 ),
    QPointF( 661580.50, 1792891.50 ), QPointF( 661580.50, 1792890.50 ), QPointF( 661579.50, 1792890.50 ), QPointF( 661579.50, 1792888.50 ), QPointF( 661578.50, 1792888.50 ), QPointF( 661578.50, 1792887.50 ), QPointF( 661577.50, 1792887.50 ), QPointF( 661577.50, 1792886.50 ), QPointF( 661576.50, 1792886.50 ), QPointF( 661576.50, 1792884.50 ),
    QPointF( 661575.50, 1792884.50 ), QPointF( 661575.50, 1792883.50 ), QPointF( 661574.50, 1792883.50 ), QPointF( 661574.50, 1792882.50 ), QPointF( 661573.50, 1792882.50 ), QPointF( 661573.50, 1792881.50 ), QPointF( 661572.50, 1792881.50 ), QPointF( 661572.50, 1792880.50 ), QPointF( 661571.50, 1792880.50 ), QPointF( 661571.50, 1792879.50 ),
    QPointF( 661570.50, 1792879.50 ), QPointF( 661570.50, 1792878.50 ), QPointF( 661569.50, 1792878.50 ), QPointF( 661569.50, 1792877.50 ), QPointF( 661568.50, 1792877.50 ), QPointF( 661568.50, 1792876.50 ), QPointF( 661567.50, 1792876.50 ), QPointF( 661567.50, 1792875.50 ), QPointF( 661565.50, 1792875.50 ), QPointF( 661565.50, 1792874.50 ),
    QPointF( 661563.50, 1792874.50 ), QPointF( 661563.50, 1792873.50 ), QPointF( 661561.50, 1792873.50 ), QPointF( 661561.50, 1792872.50 ), QPointF( 661560.50, 1792872.50 ), QPointF( 661560.50, 1792869.50 ), QPointF( 661571.50, 1792869.50 ), QPointF( 661571.50, 1792870.50 ), QPointF( 661577.50, 1792870.50 ), QPointF( 661577.50, 1792869.50 ),
    QPointF( 661581.50, 1792869.50 ), QPointF( 661581.50, 1792868.50 ), QPointF( 661583.50, 1792868.50 ), QPointF( 661583.50, 1792864.50 ), QPointF( 661582.50, 1792864.50 ), QPointF( 661582.50, 1792859.50 ), QPointF( 661583.50, 1792859.50 ), QPointF( 661583.50, 1792856.50 ), QPointF( 661582.50, 1792856.50 ), QPointF( 661582.50, 1792853.50 ),
    QPointF( 661583.50, 1792853.50 ), QPointF( 661583.50, 1792852.50 ), QPointF( 661584.50, 1792852.50 ), QPointF( 661584.50, 1792849.50 ), QPointF( 661585.50, 1792849.50 ), QPointF( 661585.50, 1792846.50 ), QPointF( 661584.50, 1792846.50 ), QPointF( 661584.50, 1792845.50 ), QPointF( 661585.50, 1792845.50 ), QPointF( 661585.50, 1792841.50 ),
    QPointF( 661584.50, 1792841.50 ), QPointF( 661584.50, 1792839.50 ), QPointF( 661585.50, 1792839.50 ), QPointF( 661585.50, 1792838.50 ), QPointF( 661586.50, 1792838.50 ), QPointF( 661586.50, 1792835.50 ), QPointF( 661584.50, 1792835.50 ), QPointF( 661584.50, 1792833.50 ), QPointF( 661583.50, 1792833.50 ), QPointF( 661583.50, 1792830.50 ),
    QPointF( 661586.50, 1792830.50 ), QPointF( 661586.50, 1792829.50 ), QPointF( 661585.50, 1792829.50 ), QPointF( 661585.50, 1792826.50 ), QPointF( 661586.50, 1792826.50 ), QPointF( 661586.50, 1792825.50 ), QPointF( 661587.50, 1792825.50 ), QPointF( 661587.50, 1792823.50 ), QPointF( 661586.50, 1792823.50 ), QPointF( 661586.50, 1792820.50 ),
    QPointF( 661588.50, 1792820.50 ), QPointF( 661588.50, 1792818.50 ), QPointF( 661587.50, 1792818.50 ), QPointF( 661587.50, 1792816.50 ), QPointF( 661588.50, 1792816.50 ), QPointF( 661588.50, 1792815.50 ), QPointF( 661589.50, 1792815.50 ), QPointF( 661589.50, 1792814.50 ), QPointF( 661592.50, 1792814.50 ), QPointF( 661592.50, 1792813.50 ),
    QPointF( 661597.50, 1792813.50 ), QPointF( 661597.50, 1792814.50 ), QPointF( 661601.50, 1792814.50 ), QPointF( 661601.50, 1792812.50 ), QPointF( 661602.50, 1792812.50 ), QPointF( 661602.50, 1792811.50 ), QPointF( 661601.50, 1792811.50 ), QPointF( 661601.50, 1792809.50 ), QPointF( 661602.50, 1792809.50 ), QPointF( 661602.50, 1792806.50 ),
    QPointF( 661601.50, 1792806.50 ), QPointF( 661601.50, 1792799.50 ), QPointF( 661600.50, 1792799.50 ), QPointF( 661600.50, 1792797.50 ), QPointF( 661602.50, 1792797.50 ), QPointF( 661602.50, 1792796.50 ), QPointF( 661601.50, 1792796.50 ), QPointF( 661601.50, 1792794.50 ), QPointF( 661602.50, 1792794.50 ), QPointF( 661602.50, 1792792.50 ),
    QPointF( 661603.50, 1792792.50 ), QPointF( 661603.50, 1792791.50 ), QPointF( 661604.50, 1792791.50 ), QPointF( 661604.50, 1792790.50 ), QPointF( 661606.50, 1792790.50 ), QPointF( 661606.50, 1792787.50 ), QPointF( 661608.50, 1792787.50 ), QPointF( 661608.50, 1792786.50 ), QPointF( 661610.50, 1792786.50 ), QPointF( 661610.50, 1792784.50 ),
    QPointF( 661611.50, 1792784.50 ), QPointF( 661611.50, 1792783.50 ), QPointF( 661613.50, 1792783.50 ), QPointF( 661613.50, 1792781.50 ), QPointF( 661618.50, 1792781.50 ), QPointF( 661618.50, 1792780.50 ), QPointF( 661619.50, 1792780.50 ), QPointF( 661619.50, 1792777.50 ), QPointF( 661622.50, 1792777.50 ), QPointF( 661622.50, 1792776.50 ),
    QPointF( 661623.50, 1792776.50 ), QPointF( 661623.50, 1792775.50 ), QPointF( 661624.50, 1792775.50 ), QPointF( 661624.50, 1792774.50 ), QPointF( 661627.50, 1792774.50 ), QPointF( 661627.50, 1792775.50 ), QPointF( 661628.50, 1792775.50 ), QPointF( 661628.50, 1792772.50 ), QPointF( 661629.50, 1792772.50 ), QPointF( 661629.50, 1792771.50 ),
    QPointF( 661631.50, 1792771.50 ), QPointF( 661631.50, 1792770.50 ), QPointF( 661634.50, 1792770.50 ), QPointF( 661634.50, 1792771.50 ), QPointF( 661635.50, 1792771.50 ), QPointF( 661635.50, 1792770.50 ), QPointF( 661636.50, 1792770.50 ), QPointF( 661636.50, 1792769.50 ), QPointF( 661642.50, 1792769.50 ), QPointF( 661642.50, 1792767.50 ),
    QPointF( 661644.50, 1792767.50 ), QPointF( 661644.50, 1792766.50 ), QPointF( 661646.50, 1792766.50 ), QPointF( 661646.50, 1792767.50 ), QPointF( 661647.50, 1792767.50 ), QPointF( 661647.50, 1792764.50 ), QPointF( 661648.50, 1792764.50 ), QPointF( 661648.50, 1792763.50 ), QPointF( 661649.50, 1792763.50 ), QPointF( 661649.50, 1792762.50 ),
    QPointF( 661650.50, 1792762.50 ), QPointF( 661650.50, 1792761.50 ), QPointF( 661649.50, 1792761.50 ), QPointF( 661649.50, 1792756.50 ), QPointF( 661651.50, 1792756.50 ), QPointF( 661651.50, 1792755.50 ), QPointF( 661652.50, 1792755.50 ), QPointF( 661652.50, 1792754.50 ), QPointF( 661653.50, 1792754.50 ), QPointF( 661653.50, 1792753.50 ),
    QPointF( 661654.50, 1792753.50 ), QPointF( 661654.50, 1792747.50 ), QPointF( 661655.50, 1792747.50 ), QPointF( 661655.50, 1792745.50 ), QPointF( 661656.50, 1792745.50 ), QPointF( 661656.50, 1792744.50 ), QPointF( 661657.50, 1792744.50 ), QPointF( 661657.50, 1792741.50 ), QPointF( 661658.50, 1792741.50 ), QPointF( 661658.50, 1792735.50 ),
    QPointF( 661659.50, 1792735.50 ), QPointF( 661659.50, 1792734.50 ), QPointF( 661660.50, 1792734.50 ), QPointF( 661660.50, 1792733.50 ), QPointF( 661674.50, 1792733.50 ), QPointF( 661674.50, 1792734.50 ), QPointF( 661676.50, 1792734.50 ), QPointF( 661676.50, 1792735.50 ), QPointF( 661677.50, 1792735.50 ), QPointF( 661677.50, 1792736.50 ),
    QPointF( 661678.50, 1792736.50 ), QPointF( 661678.50, 1792735.50 ), QPointF( 661680.50, 1792735.50 ), QPointF( 661680.50, 1792733.50 ), QPointF( 661684.50, 1792733.50 ), QPointF( 661684.50, 1792735.50 ), QPointF( 661685.50, 1792735.50 ), QPointF( 661685.50, 1792737.50 ), QPointF( 661687.50, 1792737.50 ), QPointF( 661687.50, 1792738.50 ),
    QPointF( 661689.50, 1792738.50 ), QPointF( 661689.50, 1792739.50 ), QPointF( 661690.50, 1792739.50 ), QPointF( 661690.50, 1792741.50 ), QPointF( 661696.50, 1792741.50 ), QPointF( 661696.50, 1792743.50 ), QPointF( 661706.50, 1792743.50 ), QPointF( 661706.50, 1792744.50 ), QPointF( 661708.50, 1792744.50 ), QPointF( 661708.50, 1792745.50 ),
    QPointF( 661712.50, 1792745.50 ), QPointF( 661712.50, 1792744.50 ), QPointF( 661717.50, 1792744.50 ), QPointF( 661717.50, 1792743.50 ), QPointF( 661719.50, 1792743.50 ), QPointF( 661719.50, 1792747.50 ), QPointF( 661718.50, 1792747.50 ), QPointF( 661718.50, 1792750.50 ), QPointF( 661721.50, 1792750.50 ), QPointF( 661721.50, 1792751.50 ),
    QPointF( 661722.50, 1792751.50 ), QPointF( 661722.50, 1792752.50 ), QPointF( 661723.50, 1792752.50 ), QPointF( 661723.50, 1792751.50 ), QPointF( 661725.50, 1792751.50 ), QPointF( 661725.50, 1792750.50 ), QPointF( 661730.50, 1792750.50 ), QPointF( 661730.50, 1792749.50 ), QPointF( 661731.50, 1792749.50 ), QPointF( 661731.50, 1792748.50 ),
    QPointF( 661733.50, 1792748.50 ), QPointF( 661733.50, 1792749.50 ), QPointF( 661734.50, 1792749.50 ), QPointF( 661734.50, 1792752.50 ), QPointF( 661736.50, 1792752.50 ), QPointF( 661736.50, 1792753.50 ), QPointF( 661737.50, 1792753.50 ), QPointF( 661737.50, 1792752.50 ), QPointF( 661741.50, 1792752.50 ), QPointF( 661741.50, 1792751.50 ),
    QPointF( 661742.50, 1792751.50 ), QPointF( 661742.50, 1792752.50 ), QPointF( 661746.50, 1792752.50 ), QPointF( 661746.50, 1792754.50 ), QPointF( 661747.50, 1792754.50 ), QPointF( 661747.50, 1792755.50 ), QPointF( 661748.50, 1792755.50 ), QPointF( 661748.50, 1792757.50 ), QPointF( 661746.50, 1792757.50 ), QPointF( 661746.50, 1792758.50 ),
    QPointF( 661745.50, 1792758.50 ), QPointF( 661745.50, 1792760.50 ), QPointF( 661747.50, 1792760.50 ), QPointF( 661747.50, 1792761.50 ), QPointF( 661748.50, 1792761.50 ), QPointF( 661748.50, 1792762.50 ), QPointF( 661749.50, 1792762.50 ), QPointF( 661749.50, 1792763.50 ), QPointF( 661752.50, 1792763.50 ), QPointF( 661752.50, 1792765.50 ),
    QPointF( 661753.50, 1792765.50 ), QPointF( 661753.50, 1792766.50 ), QPointF( 661754.50, 1792766.50 ), QPointF( 661754.50, 1792764.50 ), QPointF( 661759.50, 1792764.50 ), QPointF( 661759.50, 1792769.50 ), QPointF( 661757.50, 1792769.50 ), QPointF( 661757.50, 1792774.50 ), QPointF( 661759.50, 1792774.50 ), QPointF( 661759.50, 1792775.50 ),
    QPointF( 661760.50, 1792775.50 ), QPointF( 661760.50, 1792777.50 ), QPointF( 661761.50, 1792777.50 ), QPointF( 661761.50, 1792780.50 ), QPointF( 661760.50, 1792780.50 ), QPointF( 661760.50, 1792781.50 ), QPointF( 661761.50, 1792781.50 ), QPointF( 661761.50, 1792787.50 ), QPointF( 661762.50, 1792787.50 ), QPointF( 661762.50, 1792793.50 ),
    QPointF( 661761.50, 1792793.50 ), QPointF( 661761.50, 1792795.50 ), QPointF( 661760.50, 1792795.50 ), QPointF( 661760.50, 1792796.50 ), QPointF( 661759.50, 1792796.50 ), QPointF( 661759.50, 1792798.50 ), QPointF( 661758.50, 1792798.50 ), QPointF( 661758.50, 1792802.50 ), QPointF( 661757.50, 1792802.50 ), QPointF( 661757.50, 1792803.50 ),
    QPointF( 661756.50, 1792803.50 ), QPointF( 661756.50, 1792804.50 ), QPointF( 661755.50, 1792804.50 ), QPointF( 661755.50, 1792808.50 ), QPointF( 661753.50, 1792808.50 ), QPointF( 661753.50, 1792809.50 ), QPointF( 661752.50, 1792809.50 ), QPointF( 661752.50, 1792810.50 ), QPointF( 661753.50, 1792810.50 ), QPointF( 661753.50, 1792811.50 ),
    QPointF( 661754.50, 1792811.50 ), QPointF( 661754.50, 1792812.50 ), QPointF( 661755.50, 1792812.50 ), QPointF( 661755.50, 1792813.50 ), QPointF( 661756.50, 1792813.50 ), QPointF( 661756.50, 1792814.50 ), QPointF( 661755.50, 1792814.50 ), QPointF( 661755.50, 1792815.50 ), QPointF( 661754.50, 1792815.50 ), QPointF( 661754.50, 1792818.50 ),
    QPointF( 661753.50, 1792818.50 ), QPointF( 661753.50, 1792823.50 ), QPointF( 661752.50, 1792823.50 ), QPointF( 661752.50, 1792822.50 ), QPointF( 661744.50, 1792822.50 ), QPointF( 661744.50, 1792826.50 ), QPointF( 661743.50, 1792826.50 ), QPointF( 661743.50, 1792830.50 ), QPointF( 661744.50, 1792830.50 ), QPointF( 661744.50, 1792833.50 ),
    QPointF( 661743.50, 1792833.50 ), QPointF( 661743.50, 1792834.50 ), QPointF( 661744.50, 1792834.50 ), QPointF( 661744.50, 1792836.50 ), QPointF( 661745.50, 1792836.50 ), QPointF( 661745.50, 1792838.50 ), QPointF( 661744.50, 1792838.50 ), QPointF( 661744.50, 1792839.50 ), QPointF( 661742.50, 1792839.50 ), QPointF( 661742.50, 1792840.50 ),
    QPointF( 661741.50, 1792840.50 ), QPointF( 661741.50, 1792842.50 ), QPointF( 661740.50, 1792842.50 ), QPointF( 661740.50, 1792845.50 ), QPointF( 661744.50, 1792845.50 ), QPointF( 661744.50, 1792846.50 ), QPointF( 661747.50, 1792846.50 ), QPointF( 661747.50, 1792845.50 ), QPointF( 661748.50, 1792845.50 ), QPointF( 661748.50, 1792846.50 ),
    QPointF( 661749.50, 1792846.50 ), QPointF( 661749.50, 1792847.50 ), QPointF( 661750.50, 1792847.50 ), QPointF( 661750.50, 1792848.50 ), QPointF( 661751.50, 1792848.50 ), QPointF( 661751.50, 1792849.50 ), QPointF( 661752.50, 1792849.50 ), QPointF( 661752.50, 1792852.50 ), QPointF( 661751.50, 1792852.50 ), QPointF( 661751.50, 1792853.50 ),
    QPointF( 661752.50, 1792853.50 ), QPointF( 661752.50, 1792854.50 ), QPointF( 661753.50, 1792854.50 ), QPointF( 661753.50, 1792855.50 ), QPointF( 661754.50, 1792855.50 ), QPointF( 661754.50, 1792856.50 ), QPointF( 661755.50, 1792856.50 ), QPointF( 661755.50, 1792857.50 ), QPointF( 661756.50, 1792857.50 ), QPointF( 661756.50, 1792860.50 ),
    QPointF( 661755.50, 1792860.50 ), QPointF( 661755.50, 1792861.50 ), QPointF( 661754.50, 1792861.50 ), QPointF( 661754.50, 1792862.50 ), QPointF( 661753.50, 1792862.50 ), QPointF( 661753.50, 1792863.50 ), QPointF( 661752.50, 1792863.50 ), QPointF( 661752.50, 1792864.50 ), QPointF( 661753.50, 1792864.50 ), QPointF( 661753.50, 1792867.50 ),
    QPointF( 661754.50, 1792867.50 ), QPointF( 661754.50, 1792868.50 ), QPointF( 661756.50, 1792868.50 ), QPointF( 661756.50, 1792869.50 ), QPointF( 661757.50, 1792869.50 ), QPointF( 661757.50, 1792872.50 ), QPointF( 661752.50, 1792872.50 ), QPointF( 661752.50, 1792873.50 ), QPointF( 661750.50, 1792873.50 ), QPointF( 661750.50, 1792874.50 ),
    QPointF( 661751.50, 1792874.50 ), QPointF( 661751.50, 1792875.50 ), QPointF( 661753.50, 1792875.50 ), QPointF( 661753.50, 1792876.50 ), QPointF( 661755.50, 1792876.50 ), QPointF( 661755.50, 1792877.50 ), QPointF( 661756.50, 1792877.50 ), QPointF( 661756.50, 1792878.50 ), QPointF( 661758.50, 1792878.50 ), QPointF( 661758.50, 1792879.50 ),
    QPointF( 661759.50, 1792879.50 ), QPointF( 661759.50, 1792884.50 ), QPointF( 661760.50, 1792884.50 ), QPointF( 661760.50, 1792885.50 ), QPointF( 661759.50, 1792885.50 ), QPointF( 661759.50, 1792887.50 ), QPointF( 661762.50, 1792887.50 ), QPointF( 661762.50, 1792889.50 ), QPointF( 661763.50, 1792889.50 ), QPointF( 661763.50, 1792890.50 ),
    QPointF( 661764.50, 1792890.50 ), QPointF( 661764.50, 1792892.50 ), QPointF( 661763.50, 1792892.50 ), QPointF( 661763.50, 1792901.50 ), QPointF( 661762.50, 1792901.50 ), QPointF( 661762.50, 1792905.50 ), QPointF( 661757.50, 1792905.50 ), QPointF( 661757.50, 1792906.50 ), QPointF( 661754.50, 1792906.50 ), QPointF( 661754.50, 1792907.50 ),
    QPointF( 661753.50, 1792907.50 ), QPointF( 661753.50, 1792912.50 ), QPointF( 661756.50, 1792912.50 ), QPointF( 661756.50, 1792913.50 ), QPointF( 661758.50, 1792913.50 ), QPointF( 661758.50, 1792920.50 ), QPointF( 661757.50, 1792920.50 ), QPointF( 661757.50, 1792923.50 ), QPointF( 661760.50, 1792923.50 ), QPointF( 661760.50, 1792922.50 ),
    QPointF( 661762.50, 1792922.50 ), QPointF( 661762.50, 1792921.50 ), QPointF( 661763.50, 1792921.50 ), QPointF( 661763.50, 1792920.50 ), QPointF( 661765.50, 1792920.50 ), QPointF( 661765.50, 1792923.50 ), QPointF( 661764.50, 1792923.50 ), QPointF( 661764.50, 1792925.50 ), QPointF( 661762.50, 1792925.50 ), QPointF( 661762.50, 1792927.50 ),
    QPointF( 661763.50, 1792927.50 ), QPointF( 661763.50, 1792928.50 ), QPointF( 661766.50, 1792928.50 ), QPointF( 661766.50, 1792929.50 ), QPointF( 661767.50, 1792929.50 ), QPointF( 661767.50, 1792932.50 ), QPointF( 661768.50, 1792932.50 ), QPointF( 661768.50, 1792934.50 ), QPointF( 661767.50, 1792934.50 ), QPointF( 661767.50, 1792935.50 ),
    QPointF( 661766.50, 1792935.50 ), QPointF( 661766.50, 1792936.50 ), QPointF( 661765.50, 1792936.50 ), QPointF( 661765.50, 1792937.50 ), QPointF( 661764.50, 1792937.50 ), QPointF( 661764.50, 1792938.50 ), QPointF( 661763.50, 1792938.50 ), QPointF( 661763.50, 1792939.50 ), QPointF( 661762.50, 1792939.50 ), QPointF( 661762.50, 1792940.50 ),
    QPointF( 661761.50, 1792940.50 ), QPointF( 661761.50, 1792941.50 ), QPointF( 661760.50, 1792941.50 ), QPointF( 661760.50, 1792942.50 ), QPointF( 661759.50, 1792942.50 ), QPointF( 661759.50, 1792943.50 ), QPointF( 661757.50, 1792943.50 ), QPointF( 661757.50, 1792944.50 ), QPointF( 661756.50, 1792944.50 ), QPointF( 661756.50, 1792947.50 ),
    QPointF( 661755.50, 1792947.50 ), QPointF( 661755.50, 1792948.50 ), QPointF( 661754.50, 1792948.50 ), QPointF( 661754.50, 1792949.50 ), QPointF( 661753.50, 1792949.50 ), QPointF( 661753.50, 1792950.50 ), QPointF( 661690.50, 1792950.50 ), QPointF( 661690.50, 1792951.50 ), QPointF( 661687.50, 1792951.50 ), QPointF( 661687.50, 1792952.50 ),
    QPointF( 661685.50, 1792952.50 ), QPointF( 661685.50, 1792953.50 ), QPointF( 661683.50, 1792953.50 ), QPointF( 661683.50, 1792954.50 ), QPointF( 661681.50, 1792954.50 ), QPointF( 661681.50, 1792953.50 ), QPointF( 661680.50, 1792953.50 ), QPointF( 661680.50, 1792952.50 ), QPointF( 661678.50, 1792952.50 ), QPointF( 661678.50, 1792953.50 ),
    QPointF( 661677.50, 1792953.50 ), QPointF( 661677.50, 1792952.50 ), QPointF( 661676.50, 1792952.50 ), QPointF( 661676.50, 1792951.50 ), QPointF( 661675.50, 1792951.50 ), QPointF( 661675.50, 1792950.50 ), QPointF( 661599.50, 1792950.50 )} );

  QCOMPARE( polygonWatershed, polygonWatershedTest );

}

void ReosWatersehdTest::watershdDelineatingMultiWatershed()
{
  // create watershed delineating module
  ReosWatershedTree watershedStore( &gisEngine );
  ReosWatershedDelineating watershedDelineating( &rootModule, &watershedStore, &gisEngine );
  ReosWatershedItemModel itemModel( &watershedStore );
  std::unique_ptr<ModuleProcessControler> controler;

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
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  QPolygonF polygonWatershed = watershedDelineating.lastWatershedDelineated();

//  ReosExportToVectorFile exportPolygon( "/home/vincent/bv_poly.shp", QList<ReosExportToVectorFile::Field>(), ReosExportToVectorFile::Polygon, QString() );
//  exportPolygon.addPolygon( polygonWatershed, QVariantMap() );

//  std::ofstream file( "/home/vincent/watershed.txt" );
//  for ( int i = 0; i < polygonWatershed.size(); ++i )
//  {
//    if ( i % 10 == 0 && i != 0 )
//      file << std::endl;
//    QPointF pt = polygonWatershed.at( i );
//    file << QString( " QPointF(%1,%2)," ).arg( QString::number( pt.x(), 'f', 2 ) ).arg( QString::number( pt.y(), 'f', 2 ) ).toStdString();
//    //file << QString( " %1 %2," ).arg( QString::number( pt.x(), 'f', 2 ) ).arg( QString::number( pt.y(), 'f', 2 ) ).toStdString();
//  }

  QPolygonF polygonWatershedTest(
  {
    QPointF( 666694.50, 1799175.50 ), QPointF( 666694.50, 1799100.50 ), QPointF( 666699.50, 1799100.50 ), QPointF( 666699.50, 1799095.50 ), QPointF( 666704.50, 1799095.50 ), QPointF( 666704.50, 1799085.50 ), QPointF( 666709.50, 1799085.50 ), QPointF( 666709.50, 1799065.50 ), QPointF( 666714.50, 1799065.50 ), QPointF( 666714.50, 1799045.50 ),
    QPointF( 666719.50, 1799045.50 ), QPointF( 666719.50, 1799015.50 ), QPointF( 666724.50, 1799015.50 ), QPointF( 666724.50, 1799005.50 ), QPointF( 666744.50, 1799005.50 ), QPointF( 666744.50, 1799000.50 ), QPointF( 666754.50, 1799000.50 ), QPointF( 666754.50, 1798995.50 ), QPointF( 666769.50, 1798995.50 ), QPointF( 666769.50, 1798990.50 ),
    QPointF( 666779.50, 1798990.50 ), QPointF( 666779.50, 1798985.50 ), QPointF( 666789.50, 1798985.50 ), QPointF( 666789.50, 1798980.50 ), QPointF( 666824.50, 1798980.50 ), QPointF( 666824.50, 1798975.50 ), QPointF( 666854.50, 1798975.50 ), QPointF( 666854.50, 1798970.50 ), QPointF( 666859.50, 1798970.50 ), QPointF( 666859.50, 1798965.50 ),
    QPointF( 666864.50, 1798965.50 ), QPointF( 666864.50, 1798960.50 ), QPointF( 666874.50, 1798960.50 ), QPointF( 666874.50, 1798965.50 ), QPointF( 666889.50, 1798965.50 ), QPointF( 666889.50, 1798960.50 ), QPointF( 666894.50, 1798960.50 ), QPointF( 666894.50, 1798955.50 ), QPointF( 666899.50, 1798955.50 ), QPointF( 666899.50, 1798960.50 ),
    QPointF( 666904.50, 1798960.50 ), QPointF( 666904.50, 1798970.50 ), QPointF( 666909.50, 1798970.50 ), QPointF( 666909.50, 1798975.50 ), QPointF( 666914.50, 1798975.50 ), QPointF( 666914.50, 1798980.50 ), QPointF( 666919.50, 1798980.50 ), QPointF( 666919.50, 1798985.50 ), QPointF( 666924.50, 1798985.50 ), QPointF( 666924.50, 1798995.50 ),
    QPointF( 666939.50, 1798995.50 ), QPointF( 666939.50, 1799000.50 ), QPointF( 666954.50, 1799000.50 ), QPointF( 666954.50, 1799005.50 ), QPointF( 666959.50, 1799005.50 ), QPointF( 666959.50, 1799010.50 ), QPointF( 666964.50, 1799010.50 ), QPointF( 666964.50, 1799015.50 ), QPointF( 666979.50, 1799015.50 ), QPointF( 666979.50, 1799020.50 ),
    QPointF( 666989.50, 1799020.50 ), QPointF( 666989.50, 1799025.50 ), QPointF( 667004.50, 1799025.50 ), QPointF( 667004.50, 1799030.50 ), QPointF( 667014.50, 1799030.50 ), QPointF( 667014.50, 1799035.50 ), QPointF( 667029.50, 1799035.50 ), QPointF( 667029.50, 1799040.50 ), QPointF( 667034.50, 1799040.50 ), QPointF( 667034.50, 1799045.50 ),
    QPointF( 667049.50, 1799045.50 ), QPointF( 667049.50, 1799050.50 ), QPointF( 667054.50, 1799050.50 ), QPointF( 667054.50, 1799055.50 ), QPointF( 667064.50, 1799055.50 ), QPointF( 667064.50, 1799060.50 ), QPointF( 667069.50, 1799060.50 ), QPointF( 667069.50, 1799065.50 ), QPointF( 667074.50, 1799065.50 ), QPointF( 667074.50, 1799070.50 ),
    QPointF( 667079.50, 1799070.50 ), QPointF( 667079.50, 1799065.50 ), QPointF( 667084.50, 1799065.50 ), QPointF( 667084.50, 1799075.50 ), QPointF( 667079.50, 1799075.50 ), QPointF( 667079.50, 1799080.50 ), QPointF( 667074.50, 1799080.50 ), QPointF( 667074.50, 1799085.50 ), QPointF( 667069.50, 1799085.50 ), QPointF( 667069.50, 1799100.50 ),
    QPointF( 667064.50, 1799100.50 ), QPointF( 667064.50, 1799115.50 ), QPointF( 667059.50, 1799115.50 ), QPointF( 667059.50, 1799125.50 ), QPointF( 667054.50, 1799125.50 ), QPointF( 667054.50, 1799140.50 ), QPointF( 667049.50, 1799140.50 ), QPointF( 667049.50, 1799160.50 ), QPointF( 667044.50, 1799160.50 ), QPointF( 667044.50, 1799165.50 ),
    QPointF( 667039.50, 1799165.50 ), QPointF( 667039.50, 1799170.50 ), QPointF( 667034.50, 1799170.50 ), QPointF( 667034.50, 1799185.50 ), QPointF( 667024.50, 1799185.50 ), QPointF( 667024.50, 1799195.50 ), QPointF( 667029.50, 1799195.50 ), QPointF( 667029.50, 1799200.50 ), QPointF( 667034.50, 1799200.50 ), QPointF( 667034.50, 1799205.50 ),
    QPointF( 667039.50, 1799205.50 ), QPointF( 667039.50, 1799220.50 ), QPointF( 667034.50, 1799220.50 ), QPointF( 667034.50, 1799225.50 ), QPointF( 667024.50, 1799225.50 ), QPointF( 667024.50, 1799245.50 ), QPointF( 667019.50, 1799245.50 ), QPointF( 667019.50, 1799255.50 ), QPointF( 667014.50, 1799255.50 ), QPointF( 667014.50, 1799275.50 ),
    QPointF( 667009.50, 1799275.50 ), QPointF( 667009.50, 1799285.50 ), QPointF( 667004.50, 1799285.50 ), QPointF( 667004.50, 1799305.50 ), QPointF( 666999.50, 1799305.50 ), QPointF( 666999.50, 1799320.50 ), QPointF( 666984.50, 1799320.50 ), QPointF( 666984.50, 1799325.50 ), QPointF( 666974.50, 1799325.50 ), QPointF( 666974.50, 1799340.50 ),
    QPointF( 666969.50, 1799340.50 ), QPointF( 666969.50, 1799350.50 ), QPointF( 666964.50, 1799350.50 ), QPointF( 666964.50, 1799355.50 ), QPointF( 666954.50, 1799355.50 ), QPointF( 666954.50, 1799360.50 ), QPointF( 666949.50, 1799360.50 ), QPointF( 666949.50, 1799365.50 ), QPointF( 666944.50, 1799365.50 ), QPointF( 666944.50, 1799370.50 ),
    QPointF( 666939.50, 1799370.50 ), QPointF( 666939.50, 1799375.50 ), QPointF( 666934.50, 1799375.50 ), QPointF( 666934.50, 1799380.50 ), QPointF( 666929.50, 1799380.50 ), QPointF( 666929.50, 1799385.50 ), QPointF( 666924.50, 1799385.50 ), QPointF( 666924.50, 1799395.50 ), QPointF( 666919.50, 1799395.50 ), QPointF( 666919.50, 1799400.50 ),
    QPointF( 666914.50, 1799400.50 ), QPointF( 666914.50, 1799405.50 ), QPointF( 666909.50, 1799405.50 ), QPointF( 666909.50, 1799410.50 ), QPointF( 666904.50, 1799410.50 ), QPointF( 666904.50, 1799415.50 ), QPointF( 666894.50, 1799415.50 ), QPointF( 666894.50, 1799420.50 ), QPointF( 666889.50, 1799420.50 ), QPointF( 666889.50, 1799415.50 ),
    QPointF( 666884.50, 1799415.50 ), QPointF( 666884.50, 1799410.50 ), QPointF( 666879.50, 1799410.50 ), QPointF( 666879.50, 1799405.50 ), QPointF( 666834.50, 1799405.50 ), QPointF( 666834.50, 1799400.50 ), QPointF( 666824.50, 1799400.50 ), QPointF( 666824.50, 1799395.50 ), QPointF( 666814.50, 1799395.50 ), QPointF( 666814.50, 1799390.50 ),
    QPointF( 666809.50, 1799390.50 ), QPointF( 666809.50, 1799385.50 ), QPointF( 666784.50, 1799385.50 ), QPointF( 666784.50, 1799380.50 ), QPointF( 666774.50, 1799380.50 ), QPointF( 666774.50, 1799375.50 ), QPointF( 666769.50, 1799375.50 ), QPointF( 666769.50, 1799365.50 ), QPointF( 666764.50, 1799365.50 ), QPointF( 666764.50, 1799360.50 ),
    QPointF( 666759.50, 1799360.50 ), QPointF( 666759.50, 1799355.50 ), QPointF( 666754.50, 1799355.50 ), QPointF( 666754.50, 1799350.50 ), QPointF( 666749.50, 1799350.50 ), QPointF( 666749.50, 1799340.50 ), QPointF( 666744.50, 1799340.50 ), QPointF( 666744.50, 1799335.50 ), QPointF( 666734.50, 1799335.50 ), QPointF( 666734.50, 1799330.50 ),
    QPointF( 666729.50, 1799330.50 ), QPointF( 666729.50, 1799325.50 ), QPointF( 666724.50, 1799325.50 ), QPointF( 666724.50, 1799320.50 ), QPointF( 666719.50, 1799320.50 ), QPointF( 666719.50, 1799300.50 ), QPointF( 666714.50, 1799300.50 ), QPointF( 666714.50, 1799285.50 ), QPointF( 666709.50, 1799285.50 ), QPointF( 666709.50, 1799280.50 ),
    QPointF( 666694.50, 1799280.50 ), QPointF( 666694.50, 1799275.50 ), QPointF( 666689.50, 1799275.50 ), QPointF( 666689.50, 1799265.50 ), QPointF( 666684.50, 1799265.50 ), QPointF( 666684.50, 1799225.50 ), QPointF( 666689.50, 1799225.50 ), QPointF( 666689.50, 1799185.50 ), QPointF( 666694.50, 1799185.50 ), QPointF( 666694.50, 1799175.50 )
  } );

  QCOMPARE( polygonWatershed, polygonWatershedTest );

  ReosExportToVectorFile exportPolygon_1( "/home/vincent/bv_poly_test_1.shp", QList<ReosExportToVectorFile::Field>(), ReosExportToVectorFile::Polygon, QString() );
  exportPolygon_1.addPolygon( watershedDelineating.lastWatershedDelineated(), QVariantMap() );

  bool needAdjusting;
  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  ReosWatershed *watershed1 = watershedDelineating.storeWatershed( true );
  QVERIFY( watershed1->hasDirectiondata( layerId ) );
  ReosRasterExtent rasterExtent1( ReosMapExtent( 666674.5, 1798945.5, 667094.5, 1799430.5 ), 84, 97 );
  QCOMPARE( rasterExtent1, watershed1->directionExtent( layerId ) );
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
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

  QVERIFY( watershedDelineating.currentState() == ReosWatershedDelineating::WaitingForValidate );

  QVERIFY( watershedDelineating.validateWatershed( needAdjusting ) );
  QVERIFY( !needAdjusting );
  ReosWatershed *watershed2 = watershedDelineating.storeWatershed( true );
  QVERIFY( watershed2->hasDirectiondata( layerId ) );
  QVERIFY( watershed1->hasDirectiondata( layerId ) );
  QCOMPARE( watershed1->directions( layerId ), watershed2->directions( layerId ) );
  QCOMPARE( watershed1->directionExtent( layerId ), watershed2->directionExtent( layerId ) );
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
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

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
  QVERIFY( watershedDelineating.prepareDelineating() );
  controler.reset( new ModuleProcessControler( watershedDelineating.delineatingProcess() ) );
  controler->waitForFinished();

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

  // Test encoding
  ReosEncodedElement elem = watershedStore.encode();
  ReosWatershedTree newTree( &gisEngine );
  newTree.decode( elem );
  QCOMPARE( newTree.masterWatershedCount(), 1 );
  QCOMPARE( newTree.watershedCount(), 6 );
  QVERIFY( *newTree.masterWatershed( 0 ) == *watershedStore.masterWatershed( 0 ) );
  QVERIFY( newTree.masterWatershed( 0 )->hasDirectiondata( layerId ) );

  // Test extraction
  std::unique_ptr<ReosWatershed> removedWs( watershedStore.extractWatershed( watershed2 ) );
  QCOMPARE( removedWs->directUpstreamWatershedCount(), 0 );
  QCOMPARE( removedWs->downstreamWatershed(), nullptr );
}



void ReosWatersehdTest::concentrationTime()
{
  ReosConcentrationTimeCalculation calculation;
  ReosConcentrationTimeFormula::Parameters params;

  QStringList activeFormulas;
  activeFormulas << QStringLiteral( "Kirpich" )
                 << QStringLiteral( "Johnstone" )
                 << QStringLiteral( "Ven te Chow" )
                 << QStringLiteral( "Turazza" )
                 << QStringLiteral( "Ventura" )
                 << QStringLiteral( "Passini" );

  calculation.setActiveFormula( activeFormulas );

  params.area = ReosArea( 10, ReosArea::ha );
  params.slope = 0.03;
  params.drop = 100;
  params.length = 3000;

  QCOMPARE( ReosDuration(), calculation.concentrationTime( params ) );

  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaKirpich );

  QCOMPARE( ReosDuration( 2147224, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaJohnstone );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaPassini );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaVentura );
  ReosConcentrationTimeFormulasRegistery::instance()->registerFormulas( new ReosConcentrationTimeFormulaVenTeChow );

  QVERIFY( ReosConcentrationTimeFormulasRegistery::instance()->formula( "Kirpich" )->isInValidityDomain( params ) );
  QVERIFY( !ReosConcentrationTimeFormulasRegistery::instance()->formula( "Johnstone" )->isInValidityDomain( params ) );
  QVERIFY( ReosConcentrationTimeFormulasRegistery::instance()->formula( "Ven te Chow" )->isInValidityDomain( params ) );
  QVERIFY( ReosConcentrationTimeFormulasRegistery::instance()->formula( "Ventura" )->isInValidityDomain( params ) );
  QVERIFY( ReosConcentrationTimeFormulasRegistery::instance()->formula( "Passini" )->isInValidityDomain( params ) );

  calculation.setUsedMethod( ReosConcentrationTimeCalculation::UserChoosenFormula );
  calculation.setUserChoosenFormula( QStringLiteral( "Kirpich" ) );
  QCOMPARE( ReosDuration( 2147224, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUserChoosenFormula( QStringLiteral( "Johnstone" ) );
  QCOMPARE( ReosDuration( 6926386, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUserChoosenFormula( QStringLiteral( "Ven te Chow" ) );
  QCOMPARE( ReosDuration( 3578028, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUserChoosenFormula( QStringLiteral( "Ventura" ) );
  QCOMPARE( ReosDuration( 791893, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUserChoosenFormula( QStringLiteral( "Passini" ) );
  QCOMPARE( ReosDuration( 1503304, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUsedMethod( ReosConcentrationTimeCalculation::Average );
  QCOMPARE( ReosDuration( 2989367, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUsedMethod( ReosConcentrationTimeCalculation::Maximum );
  QCOMPARE( ReosDuration( 6926386, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  calculation.setUsedMethod( ReosConcentrationTimeCalculation::Minimum );
  QCOMPARE( ReosDuration( 791893, ReosDuration::millisecond ), calculation.concentrationTime( params ) );

  if ( ReosConcentrationTimeFormulasRegistery::isInstantiate() )
    delete ReosConcentrationTimeFormulasRegistery::instance();
}

void ReosWatersehdTest::runoffConstantCoefficient()
{
// build a rainfall
  ReosModule root;
  ReosIdfFormulaRegistery::instantiate( &root );
  ReosIdfFormulaRegistery *idfRegistery = ReosIdfFormulaRegistery::instance();
  idfRegistery->registerFormula( new ReosIdfFormulaMontana );

  ReosIntensityDurationCurve idCurve;
  idCurve.addInterval( ReosDuration( 5, ReosDuration::minute ), ReosDuration( 1, ReosDuration::hour ) );
  idCurve.createParameters( 0, idfRegistery->formula( QStringLiteral( "Montana" ) ), ReosDuration::minute, ReosDuration::minute );
  idCurve.setCurrentFormula( QStringLiteral( "Montana" ) );
  idCurve.setupFormula( idfRegistery );
  ReosParameterDouble *a = idCurve.currentParameters( 0 )->parameter( 0 );
  ReosParameterDouble *b = idCurve.currentParameters( 0 )->parameter( 1 );
  a->setValue( 4.78 );
  b->setValue( 0.322 );
  ReosChicagoRainfall chicagoRainfall;
  chicagoRainfall.timeStepParameter()->setValue( ReosDuration( 5, ReosDuration::minute ) );
  chicagoRainfall.totalDuration()->setValue( ReosDuration( 1, ReosDuration::hour ) );
  chicagoRainfall.setIntensityDurationCurve( &idCurve );

  QCOMPARE( chicagoRainfall.valueCount(), 12 );
  QVERIFY( equal( chicagoRainfall.valueAt( 5 ), 14.234, 0.001 ) );

  // apply runoff

  ReosRunoffConstantCoefficientModel runoffConstantCoefficientModel_1( "test_1" );
  runoffConstantCoefficientModel_1.coefficient()->setValue( 0.5 );

  ReosRunoffModelsGroup modelsGroup;
  modelsGroup.addRunoffModel( &runoffConstantCoefficientModel_1 );

  ReosRunoff runoff( &modelsGroup, &chicagoRainfall );
  QVERIFY( runoff.isObsolete() );
  runoff.updateValues();
  QVERIFY( !runoff.isObsolete() );

  QCOMPARE( runoff.valueCount(), 12 );
  for ( int i = 0; i < runoff.valueCount(); ++i )
    QVERIFY( equal( runoff.value( 5 ),
                    chicagoRainfall.valueAt( 5 ) * runoffConstantCoefficientModel_1.coefficient()->value(),
                    0.001 ) );

  //change rainfall
  chicagoRainfall.setValueAt( 5, 10 );
  QVERIFY( equal( chicagoRainfall.valueAt( 5 ), 10, 0.001 ) );
  QVERIFY( runoff.isObsolete() );

  QVERIFY( equal( runoff.value( 5 ), 5, 0.001 ) );
  QVERIFY( !runoff.isObsolete() );

  for ( int i = 0; i < runoff.valueCount(); ++i )
    QVERIFY( equal( runoff.value( 5 ),
                    chicagoRainfall.valueAt( 5 ) * runoffConstantCoefficientModel_1.coefficient()->value(),
                    0.001 ) );

  runoffConstantCoefficientModel_1.coefficient()->setValue( 0.1 );
  QVERIFY( runoff.isObsolete() );
  QVERIFY( equal( runoff.value( 5 ), 1, 0.001 ) );
  QVERIFY( !runoff.isObsolete() );

  for ( int i = 0; i < runoff.valueCount(); ++i )
    QVERIFY( equal( runoff.value( 5 ),
                    chicagoRainfall.valueAt( 5 ) * runoffConstantCoefficientModel_1.coefficient()->value(),
                    0.001 ) );


  ReosRunoffConstantCoefficientModel runoffConstantCoefficientModel_2( "test_2" );
  runoffConstantCoefficientModel_2.coefficient()->setValue( 1 );

  modelsGroup.addRunoffModel( &runoffConstantCoefficientModel_2 );
  QVERIFY( runoff.isObsolete() );
  modelsGroup.coefficient( 0 )->setValue( 0.25 );
  modelsGroup.coefficient( 1 )->setValue( 0.75 );

  QCOMPARE( runoff.valueCount(), 12 );
  QVERIFY( !runoff.isObsolete() );

  for ( int i = 0; i < runoff.valueCount(); ++i )
  {
    QVERIFY( equal( runoff.value( 5 ),
                    chicagoRainfall.valueAt( 5 ) *
                    ( runoffConstantCoefficientModel_1.coefficient()->value() * 0.25 +
                      runoffConstantCoefficientModel_2.coefficient()->value() * 0.75 ),
                    0.001 ) );
  }

  ReosWatershed watershed;
  watershed.area()->setValue( ReosArea( 5.2, ReosArea::km2 ) );
  watershed.concentrationTime()->setValue( ReosDuration( 45, ReosDuration::minute ) );

  ReosTransferFunctionLinearReservoir *linearReservoir = new ReosTransferFunctionLinearReservoir( &watershed );
  linearReservoir->useConcentrationTime()->setValue( true );
  linearReservoir->factorToLagTime()->setValue( 0.6 );

  std::unique_ptr<ReosHydrograph> hydrograph( linearReservoir->applyFunction( &runoff ) );
  QList<QPair<int, double>> values;
  values <<
         QPair<int, double>( {0, 0.0} ) <<
         QPair<int, double>( {300000,  10.4488001684914} ) <<
         QPair<int, double>( {600000,  19.9141563524573} ) <<
         QPair<int, double>( {900000,  28.8803387638808} ) <<
         QPair<int, double>( {1200000, 38.0865801467911} ) <<
         QPair<int, double>( {1500000, 49.525669548095} ) <<
         QPair<int, double>( {1800000, 63.8623720443718} ) <<
         QPair<int, double>( {2100000,  70.9440738754042} ) <<
         QPair<int, double>( {2400000, 73.0394572384162} ) <<
         QPair<int, double>( {2700000, 73.0248282486918} ) <<
         QPair<int, double>( {3000000, 71.911731284174} ) <<
         QPair<int, double>( {3300000, 70.2038813175527} ) <<
         QPair<int, double>( {3600000, 68.3191661471113} ) <<
         QPair<int, double>( {3900000, 56.7698377476733} ) <<
         QPair<int, double>( {4200000, 47.1729188110622} ) <<
         QPair<int, double>( {4500000, 39.1983552788341} ) <<
         QPair<int, double>( {4800000, 32.5718886024366} ) <<
         QPair<int, double>( {5100000, 27.0656235340161} ) <<
         QPair<int, double>( {5400000, 22.4901904285123} ) <<
         QPair<int, double>( {5700000, 18.6882325055266} ) <<
         QPair<int, double>( {6000000, 15.5289940870333} ) <<
         QPair<int, double>( {6300000, 12.9038236913952} ) <<
         QPair<int, double>( {6600000, 10.7224373275824} ) <<
         QPair<int, double>( {6900000, 8.90981347804673} ) <<
         QPair<int, double>( {7200000, 7.40361298353072} ) <<
         QPair<int, double>( {7500000, 6.15203509534312} ) <<
         QPair<int, double>( {7800000, 5.11203596116179} ) <<
         QPair<int, double>( {8100000, 4.24784827511682} ) <<
         QPair<int, double>( {8400000, 3.52975118044985} ) <<
         QPair<int, double>( {8700000, 2.93304811964935} ) <<
         QPair<int, double>( {9000000, 2.43721747862188} ) <<
         QPair<int, double>( {9300000, 2.02520681413509} ) <<
         QPair<int, double>( {9600000, 1.68284639183631} ) <<
         QPair<int, double>( {9900000, 1.39836186544036} ) <<
         QPair<int, double>( {10200000,  1.16196933731077} ) <<
         QPair<int, double>( {10500000,  0.965538873891728} ) <<
         QPair<int, double>( {10800000,  0.802314903725186} ) <<
         QPair<int, double>( {11100000,  0.666683882074061} );

  Q_ASSERT( hydrograph->valueCount() == values.count() );
  for ( int i = 0; i < hydrograph->valueCount(); ++i )
  {
    QCOMPARE( values.at( i ).first,  int( hydrograph->relativeTimeAt( i ).valueMilliSecond() ) );
    QCOMPARE( values.at( i ).second,  hydrograph->valueAt( i ) );
  }

  watershed.concentrationTime()->setValue( ReosDuration( 32, ReosDuration::minute ) );
  ReosTransferFunctionGeneralizedRationalMethod *rationalUH = new ReosTransferFunctionGeneralizedRationalMethod( &watershed );
  hydrograph.reset( rationalUH->applyFunction( &runoff ) );

  values.clear();
  values << QPair<int, double>( {0, 0} ) <<
         QPair<int, double>( {300000,  9.65766809739158} ) <<
         QPair<int, double>( {600000,  20.0389788076493} ) <<
         QPair<int, double>( {900000,  31.4378726470174} ) <<
         QPair<int, double>( {1200000, 44.4596153678346} ) <<
         QPair<int, double>( {1500000, 60.9836209861718} ) <<
         QPair<int, double>( {1800000, 81.9732043195051} ) <<
         QPair<int, double>( {1920000, 88.58280656684} ) <<
         QPair<int, double>( {2100000, 92.7026090794073} ) <<
         QPair<int, double>( {2220000, 94.0482389287776} ) <<
         QPair<int, double>( {2400000, 95.6324981351133} ) <<
         QPair<int, double>( {2520000, 96.0395313867574} ) <<
         QPair<int, double>( {2700000, 96.0395313867574} ) <<
         QPair<int, double>( {2820000, 95.6324981351133} ) <<
         QPair<int, double>( {3000000, 94.0482389287776} ) <<
         QPair<int, double>( {3120000, 92.7026090794073} ) <<
         QPair<int, double>( {3300000, 88.58280656684} ) <<
         QPair<int, double>( {3420000, 85.6641414014797} ) <<
         QPair<int, double>( {3600000, 78.6067970244417} ) <<
         QPair<int, double>( {3720000, 70.2109636911084} ) <<
         QPair<int, double>( {4020000, 53.6869580727712} ) <<
         QPair<int, double>( {4320000, 40.665215351954} ) <<
         QPair<int, double>( {4620000, 29.2663215125859} ) <<
         QPair<int, double>( {4920000, 18.8850108023282} ) <<
         QPair<int, double>( {5220000, 9.22734270493658} ) <<
         QPair<int, double>( {5520000, 0} );

  QCOMPARE( hydrograph->valueCount(), values.count() );
  for ( int i = 0; i < hydrograph->valueCount(); ++i )
  {
    QCOMPARE( values.at( i ).first,  int( hydrograph->relativeTimeAt( i ).valueMilliSecond() ) );
    QCOMPARE( values.at( i ).second,  hydrograph->valueAt( i ) );
  }

  ReosTransferFunctionSCSUnitHydrograph *scsUH = new ReosTransferFunctionSCSUnitHydrograph( &watershed );
  scsUH->useConcentrationTime()->setValue( false );
  scsUH->peakRateFactor()->setValue( 484.0 );
  scsUH->factorToLagTime()->setValue( 0.6 );
  hydrograph.reset( scsUH->applyFunction( &runoff ) );

  values.clear();
  values << QPair<int, double>( {0, 0} ) <<
         QPair<int, double>( {130200,  0.319926055137039} ) <<
         QPair<int, double>( {260400,  1.06642018379013} ) <<
         QPair<int, double>( {390600,  2.26550057491395} ) <<
         QPair<int, double>( {520800,  4.20817233027324} ) <<
         QPair<int, double>( {651000,  7.02431845458297} ) <<
         QPair<int, double>( {781200,  10.8963338421879} ) <<
         QPair<int, double>( {911400,  15.3147268596325} ) <<
         QPair<int, double>( {1041600, 20.3236707781381} ) <<
         QPair<int, double>( {1171800, 25.6415843472443} ) <<
         QPair<int, double>( {1302000, 31.1060218206529} ) <<
         QPair<int, double>( {1432200, 37.0000812405754} ) <<
         QPair<int, double>( {1562400, 42.6513188920306} ) <<
         QPair<int, double>( {1692600, 48.9027953353517} ) <<
         QPair<int, double>( {1822800, 54.984280391843} ) <<
         QPair<int, double>( {1953000, 61.1388720401515} ) <<
         QPair<int, double>( {2083200, 67.0090218394755} ) <<
         QPair<int, double>( {2213400, 72.540068314109} ) <<
         QPair<int, double>( {  2343600, 77.9630280130392} ) <<
         QPair<int, double>( {2473800, 82.1455099278323} ) <<
         QPair<int, double>( {2604000, 85.9232406016311} ) <<
         QPair<int, double>( {2734200, 88.2622236031578} ) <<
         QPair<int, double>( {2864400, 89.7978491906397} ) <<
         QPair<int, double>( {2994600, 90.2227890881551} ) <<
         QPair<int, double>( {3124800, 89.5807645375469} ) <<
         QPair<int, double>( {3255000, 88.6158159488254} ) <<
         QPair<int, double>( {3385200, 86.6115087005479} ) <<
         QPair<int, double>( {3515400, 84.6608918268563} ) <<
         QPair<int, double>( {3645600, 81.9994960778087} ) <<
         QPair<int, double>( {3775800, 79.2586173949161} ) <<
         QPair<int, double>( {3906000, 76.0617789004734} ) <<
         QPair<int, double>( {4036200, 72.3741138246314} ) <<
         QPair<int, double>( {4166400, 68.3562871127761} ) <<
         QPair<int, double>( {4296600, 63.376007017574} ) <<
         QPair<int, double>( {4426800, 58.2233822770845} ) <<
         QPair<int, double>( {4557000, 52.4865587235491} ) <<
         QPair<int, double>( {4687200, 46.8431506867218} ) <<
         QPair<int, double>( {4817400, 41.2781916489604} ) <<
         QPair<int, double>( {4947600, 35.8830422786034} ) <<
         QPair<int, double>( {5077800, 30.9438555299734} ) <<
         QPair<int, double>( {5208000, 26.3836442394093} ) <<
         QPair<int, double>( {5338200, 22.3926271082785} ) <<
         QPair<int, double>( {5468400, 18.8591844830841} ) <<
         QPair<int, double>( {5598600, 15.9792947744262} ) <<
         QPair<int, double>( {5728800, 13.6112908557051} ) <<
         QPair<int, double>( {5859000, 11.5549737768769} ) <<
         QPair<int, double>( {5989200, 9.86825507416119} ) <<
         QPair<int, double>( {6119400, 8.37254301421143} ) <<
         QPair<int, double>( {6249600, 7.13523894666032} ) <<
         QPair<int, double>( {6379800, 6.02660504843071} ) <<
         QPair<int, double>( {6510000, 5.11908274748348} ) <<
         QPair<int, double>( {6549600, 4.87114175434491} ) <<
         QPair<int, double>( {6679800, 4.12096400845863} ) <<
         QPair<int, double>( {6810000, 3.49861339699591} ) <<
         QPair<int, double>( {6849600, 3.32501324254422} ) <<
         QPair<int, double>( {6979800, 2.8140770102472} ) <<
         QPair<int, double>( {7110000, 2.37427815557269} ) <<
         QPair<int, double>( {7149600, 2.25442576158561} ) <<
         QPair<int, double>( {7279800, 1.90645048894117} ) <<
         QPair<int, double>( {7410000, 1.59554234372908} ) <<
         QPair<int, double>( {7449600, 1.51463979805148} ) <<
         QPair<int, double>( {7579800, 1.27317697289775} ) <<
         QPair<int, double>( {7710000, 1.05102293897201} ) <<
         QPair<int, double>( {7749600, 0.997510805043472} ) <<
         QPair<int, double>( {7879800, 0.828053321544088} ) <<
         QPair<int, double>( {8010000, 0.673801851328885} ) <<
         QPair<int, double>( {8049600, 0.639317391544571} ) <<
         QPair<int, double>( {8179800, 0.525936667708267} ) <<
         QPair<int, double>( {8310000, 0.425108789977906} ) <<
         QPair<int, double>( {8349600, 0.402633266276684} ) <<
         QPair<int, double>( {8479800, 0.32873646865297} ) <<
         QPair<int, double>( {8610000, 0.262123464604087} ) <<
         QPair<int, double>( {8649600, 0.247197720123718} ) <<
         QPair<int, double>( {8779800, 0.198123681453414} ) <<
         QPair<int, double>( {8910000, 0.149885087166725} ) <<
         QPair<int, double>( {8949600, 0.139436314475832} ) <<
         QPair<int, double>( {9079800, 0.105082016386076} ) <<
         QPair<int, double>( {9210000, 0.071525937034092} ) <<
         QPair<int, double>( {9249600, 0.065183480209885} ) <<
         QPair<int, double>( {9379800, 0.044330250954536} ) <<
         QPair<int, double>( {9510000, 0.023477021699188} ) <<
         QPair<int, double>( {9549600, 0.020378054834895} ) <<
         QPair<int, double>( {9679800, 0.010189027417447} ) <<
         QPair<int, double>( {9810000, 0} );

  QCOMPARE( hydrograph->valueCount(), values.count() );
  for ( int i = 0; i < hydrograph->valueCount(); ++i )
  {
    QCOMPARE( values.at( i ).first,  int( hydrograph->relativeTimeAt( i ).valueMilliSecond() ) );
    QCOMPARE( values.at( i ).second,  hydrograph->valueAt( i ) );
  }

  ReosTransferFunctionNashUnitHydrograph *nashUH = new ReosTransferFunctionNashUnitHydrograph( &watershed );
  nashUH->useConcentrationTime()->setValue( false );
  nashUH->nParam()->setValue( 3 );
  nashUH->useConcentrationTime()->setValue( true );
  hydrograph.reset( nashUH->applyFunction( &runoff ) );

  values.clear();
  values << QPair<int, double>( {0,  0} ) <<
         QPair<int, double>( {150000, 0.314751885480496} ) <<
         QPair<int, double>( {300000, 1.31070882619237} ) <<
         QPair<int, double>( {450000, 3.10699324684955} ) <<
         QPair<int, double>( {600000, 5.67463534547189} ) <<
         QPair<int, double>( {750000, 8.9220917534873} ) <<
         QPair<int, double>( {900000, 12.7240321361553} ) <<
         QPair<int, double>( {1050000, 16.9741255105152} ) <<
         QPair<int, double>( {1200000, 21.5722937636719} ) <<
         QPair<int, double>( {1350000, 26.5019625725543} ) <<
         QPair<int, double>( {1500000, 31.7628914820306} ) <<
         QPair<int, double>( {1650000, 37.4151751203476} ) <<
         QPair<int, double>( {1800000,  43.5080250497316} ) <<
         QPair<int, double>( {1950000,  49.8150334698382} ) <<
         QPair<int, double>( {2100000,  56.0273262172112} ) <<
         QPair<int, double>( {2250000,  61.8318729222973} ) <<
         QPair<int, double>( {2400000,  66.9764376331747} ) <<
         QPair<int, double>( {2550000,  71.3107349016906} ) <<
         QPair<int, double>( {2700000, 74.770329446913} ) <<
         QPair<int, double>( {2850000,  77.3572015816612} ) <<
         QPair<int, double>( {3000000,  79.1208755289647} ) <<
         QPair<int, double>( {3150000,  80.1389879987729} ) <<
         QPair<int, double>( {3300000,  80.5060688675524} ) <<
         QPair<int, double>( {3450000,  80.3249251931539} ) <<
         QPair<int, double>( {3600000, 79.6988036394642} ) <<
         QPair<int, double>( {3750000, 78.4339563394058} ) <<
         QPair<int, double>( {3900000, 76.2801097801219} ) <<
         QPair<int, double>( {4050000,  73.2332221785876} ) <<
         QPair<int, double>( {4200000,  69.419746133985} ) <<
         QPair<int, double>( {4350000,  65.023471841131} ) <<
         QPair<int, double>( {4500000, 60.2411724612252} ) <<
         QPair<int, double>( {4650000, 55.2573888227363} ) <<
         QPair<int, double>( {4800000, 50.2316426089956} ) <<
         QPair<int, double>( {4950000,  45.2934716139456} ) <<
         QPair<int, double>( {5100000, 40.5421703042586} ) <<
         QPair<int, double>( {5250000, 36.0491647641101} ) <<
         QPair<int, double>( {5400000, 31.8616782326417} ) <<
         QPair<int, double>( {5550000, 28.0068431019027} ) <<
         QPair<int, double>( {5700000, 24.4957536699005} ) <<
         QPair<int, double>( {5850000, 21.3271790201678} ) <<
         QPair<int, double>( {6000000, 18.4908015526511} ) <<
         QPair<int, double>( {6150000, 15.9699385263522} ) <<
         QPair<int, double>( {6300000, 13.7437589577554} ) <<
         QPair<int, double>( {6450000, 11.7890386492722} ) <<
         QPair<int, double>( {6600000, 10.0815105770942} ) <<
         QPair<int, double>( {6750000, 8.59687226343714} ) <<
         QPair<int, double>( {6900000, 7.31151011634996} ) <<
         QPair<int, double>( {7050000, 6.18854377029007} ) <<
         QPair<int, double>( {7200000, 5.22402647101286} ) <<
         QPair<int, double>( {7350000, 4.39719533219186} ) <<
         QPair<int, double>( {7500000,  3.69146979916491} ) <<
         QPair<int, double>( {7650000, 3.08925583591464} ) <<
         QPair<int, double>( {7800000,  2.5780259966692} ) <<
         QPair<int, double>( {7950000,  2.14264460781499} ) <<
         QPair<int, double>( {8100000, 1.77479991019488} ) <<
         QPair<int, double>( {8250000, 1.45942301856704} ) <<
         QPair<int, double>( {8400000,  1.19413006181468} ) <<
         QPair<int, double>( {8550000,  0.96469120321767} ) <<
         QPair<int, double>( {8700000,  0.772458441776088} ) <<
         QPair<int, double>( {8850000, 0.618337231087849} ) <<
         QPair<int, double>( {9000000,  0.489540508062577} ) <<
         QPair<int, double>( {9150000, 0.387304925145542} ) <<
         QPair<int, double>( {9300000, 0.302067737029717} ) <<
         QPair<int, double>( {9450000, 0.233526462102227} ) <<
         QPair<int, double>( {9600000, 0.176515017079972} ) <<
         QPair<int, double>( {9750000, 0.130674807176691} ) <<
         QPair<int, double>( {9900000, 0.09262983472297} ) <<
         QPair<int, double>( {10050000, 0.062172843856876} ) <<
         QPair<int, double>( {10200000, 0.036947301940866} ) <<
         QPair<int, double>( {10350000, 0.016720095919204} );


  QCOMPARE( hydrograph->valueCount(), values.count() );
  for ( int i = 0; i < hydrograph->valueCount(); ++i )
  {
    QCOMPARE( values.at( i ).first,  int( hydrograph->relativeTimeAt( i ).valueMilliSecond() ) );
    QCOMPARE( values.at( i ).second,  hydrograph->valueAt( i ) );
  }

}

void ReosWatersehdTest::runoffhydrograph()
{
  // build rainfalls
  ReosModule root;
  ReosIdfFormulaRegistery::instantiate( &root );
  ReosIdfFormulaRegistery *idfRegistery = ReosIdfFormulaRegistery::instance();
  idfRegistery->registerFormula( new ReosIdfFormulaMontana );
  ReosTransferFunctionFactories::instantiate( &root );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionNashUnitHydrographFactory );
  ReosTransferFunctionFactories::instance()->addFactory( new ReosTransferFunctionLinearReservoirFactory );

  ReosIntensityDurationCurve idCurve;
  idCurve.addInterval( ReosDuration( 5, ReosDuration::minute ), ReosDuration( 1, ReosDuration::hour ) );
  idCurve.createParameters( 0, idfRegistery->formula( QStringLiteral( "Montana" ) ), ReosDuration::minute, ReosDuration::minute );
  idCurve.setCurrentFormula( QStringLiteral( "Montana" ) );
  idCurve.setupFormula( idfRegistery );
  ReosParameterDouble *a = idCurve.currentParameters( 0 )->parameter( 0 );
  ReosParameterDouble *b = idCurve.currentParameters( 0 )->parameter( 1 );
  a->setValue( 4.78 );
  b->setValue( 0.322 );
  ReosRainfallChicagoItem chicagoRainfallItem( "chicago", QString() );
  ReosRainfallAlternatingBlockItem alternateRainfallItem( "alternate", QString() );

  chicagoRainfallItem.data()->timeStepParameter()->setValue( ReosDuration( 5, ReosDuration::minute ) );
  chicagoRainfallItem.data()->totalDuration()->setValue( ReosDuration( 1, ReosDuration::hour ) );
  chicagoRainfallItem.data()->setIntensityDurationCurve( &idCurve );

  alternateRainfallItem.data()->timeStepParameter()->setValue( ReosDuration( 5, ReosDuration::minute ) );
  alternateRainfallItem.data()->totalDuration()->setValue( ReosDuration( 30, ReosDuration::minute ) );
  alternateRainfallItem.data()->setIntensityDurationCurve( &idCurve );

  QCOMPARE( chicagoRainfallItem.data()->valueCount(), 12 );
  QCOMPARE( alternateRainfallItem.data()->valueCount(), 6 );
  QVERIFY( equal( chicagoRainfallItem.data()->valueAt( 5 ), 14.234, 0.001 ) );
  QVERIFY( equal( alternateRainfallItem.data()->valueAt( 2 ), 8.539, 0.001 ) );

  ReosWatershedTree watershedTree( &gisEngine );

  ReosWatershed *watershed = watershedTree.addWatershed( new ReosWatershed() );
  watershed->area()->setValue( ReosArea( 10, ReosArea::km2 ) );
  watershed->concentrationTime()->setValue( ReosDuration( 2, ReosDuration::hour ) );
  ReosRunoffConstantCoefficientModel runoffConstantCoefficientModel_1( "test_1" );
  runoffConstantCoefficientModel_1.coefficient()->setValue( 0.5 );
  watershed->runoffModels()->addRunoffModel( &runoffConstantCoefficientModel_1 );
  watershed->setCurrentTransferFunction( ReosTransferFunctionNashUnitHydrograph::staticType() );

  ReosMeteorologicModelsCollection meteoCollection;
  QCOMPARE( meteoCollection.modelCount(), 1 ); //by default one model is present
  meteoCollection.meteorologicModel( 0 )->associate( watershed, &chicagoRainfallItem );
  ReosMeteorologicModel *meteoModel = meteoCollection.meteorologicModel( 0 );
  ReosSerieRainfall *rainfall = meteoModel->associatedRainfall( watershed );
  QVERIFY( rainfall );
  QCOMPARE( rainfall->valueCount(), 12 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 72.34254782834526 );

  ReosRunoffHydrographsStore runoffHydrographStore( &meteoCollection );
  runoffHydrographStore.setWatershed( watershed );
  QCOMPARE( runoffHydrographStore.updateCount, 0 );

  QPointer<ReosRunoff> runoff = runoffHydrographStore.runoff( meteoModel );
  QVERIFY( runoff->isObsolete() );
  QCOMPARE( runoff->valueCount(), 12 );
  QVERIFY( !runoff->isObsolete() );
  QCOMPARE( runoff->data()->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 36.17127391417263 );

  QEventLoop loop;
  QTimer timer;
  connect( &timer, &QTimer::timeout, &loop, &QEventLoop::quit );
  QCOMPARE( runoffHydrographStore.updateCount, 0 );
  QPointer<ReosHydrograph> hydrograph = runoffHydrographStore.hydrograph( meteoModel ); //call calculation on parallel thread
  QVERIFY( hydrograph->isObsolete() ); //calculation not done
  QVERIFY( hydrograph->valueCount() == 0 );
  QCOMPARE( hydrograph->valueCount(), 0 );
  QCOMPARE( runoffHydrographStore.updateCount, 0 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 1 ); //update just launch
  QVERIFY( hydrograph->isObsolete() );
  QVERIFY( hydrograph->valueCount() == 0 );
  QCOMPARE( hydrograph->valueCount(), 0 );

  timer.start( 200 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 1 ); //update done
  QVERIFY( ! hydrograph->isObsolete() );
  QCOMPARE( hydrograph->valueCount(), 98 );
  QCOMPARE( hydrograph->valueAt( 70 ), 1.6764100895121765 );
  QVERIFY( hydrograph->referenceTime().isValid() );

  watershed->area()->setValue( ReosArea( 9, ReosArea::km2 ) );
  QCOMPARE( runoffHydrographStore.updateCount, 1 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 2 ); //update just launch
  QVERIFY( hydrograph->isObsolete() ); //but not done
  QCOMPARE( hydrograph->valueCount(), 0 );

  timer.start( 200 );
  loop.exec();

  QVERIFY( !hydrograph->isObsolete() ); //update done
  QCOMPARE( hydrograph->valueCount(), 98 );
  QCOMPARE( hydrograph->valueAt( 70 ), 1.508769080560959 );
  QCOMPARE( runoff->data()->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 36.17127391417263 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 72.34254782834526 );

  rainfall->setValueAt( 5, 0 );
  QCOMPARE( runoffHydrographStore.updateCount, 2 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 3 ); //update just launch
  QVERIFY( hydrograph->isObsolete() ); //but not done
  QCOMPARE( hydrograph->valueCount(), 0 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 58.10846267665004 ); //just the rainfall and runoff updated (done a least in the main thread before launching)
  QCOMPARE( runoff->data()->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 29.05423133832502 );

  timer.start( 200 );
  loop.exec();

  QVERIFY( !hydrograph->isObsolete() ); //update done
  QCOMPARE( hydrograph->valueCount(), 98 );
  QCOMPARE( hydrograph->valueAt( 70 ), 1.2479706882948054 );

  runoffConstantCoefficientModel_1.coefficient()->setValue( 0.4 );
  QCOMPARE( runoffHydrographStore.updateCount, 3 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 4 ); //update just launch
  QVERIFY( hydrograph->isObsolete() ); //but not done
  QCOMPARE( hydrograph->valueCount(), 0 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 58.10846267665004 );
  QCOMPARE( runoff->data()->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 23.243385070660015 ); // just the runoff updated (done a least in the main thread before launching)

  timer.start( 200 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 4 );
  QVERIFY( !hydrograph->isObsolete() );
  QCOMPARE( hydrograph->valueCount(), 98 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.9983765506358444 );

  meteoCollection.meteorologicModel( 0 )->associate( watershed, &alternateRainfallItem );
  QCOMPARE( runoffHydrographStore.updateCount, 4 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 5 ); //update just launch
  QVERIFY( hydrograph->isObsolete() );
  QCOMPARE( hydrograph->valueCount(), 0 );

  timer.start( 200 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 5 );
  QVERIFY( !hydrograph->isObsolete() ); //update done
  QCOMPARE( hydrograph->valueCount(), 92 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.56555092734 );

  meteoCollection.addMeteorologicModel( "model 2" );
  QCOMPARE( meteoCollection.modelCount(), 2 );
  QCOMPARE( runoffHydrographStore.updateCount, 5 );

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 5 );
  QVERIFY( !hydrograph->isObsolete() ); //adding a model do not change anything
  QCOMPARE( hydrograph->valueCount(), 92 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.56555092734 );

  ReosMeteorologicModel *meteoModel_2 = meteoCollection.meteorologicModel( 1 );
  QVERIFY( meteoModel_2 );
  QVERIFY( !runoffHydrographStore.hydrograph( meteoModel_2 ) ); //no rainfall associated yet

  meteoModel_2->associate( watershed, &chicagoRainfallItem );

  hydrograph = runoffHydrographStore.hydrograph( meteoModel_2 );
  QCOMPARE( runoffHydrographStore.updateCount, 5 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 6 ); //update just launch
  QVERIFY( hydrograph->isObsolete() ); //but not done
  QCOMPARE( hydrograph->valueCount(), 0 );

  timer.start( 200 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 6 );
  QVERIFY( !hydrograph->isObsolete() ); //update done
  QCOMPARE( hydrograph->valueCount(), 98 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.9983765506358444 );

  meteoCollection.removeMeteorologicModel( 1 );

  hydrograph = runoffHydrographStore.hydrograph( meteoModel_2 );
  QVERIFY( !hydrograph );

  timer.start( 1 );
  loop.exec();

  hydrograph = runoffHydrographStore.hydrograph( meteoModel_2 );

  QVERIFY( !hydrograph );

  hydrograph = runoffHydrographStore.hydrograph( meteoModel );

  QVERIFY( hydrograph );
  QVERIFY( !hydrograph->isObsolete() );
  QCOMPARE( runoffHydrographStore.updateCount, 6 ); //update will be launch when come back to the event loop if needed (actually no)

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 6 ); //update not launch because hydrograph was no obsolete
  QVERIFY( !hydrograph->isObsolete() ); //still not obsolete

  QVERIFY( !hydrograph->isObsolete() ); //adding a model do not change anything
  QCOMPARE( hydrograph->valueCount(), 92 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.56555092734 );

  runoffConstantCoefficientModel_1.coefficient()->setValue( 0.4 );
  meteoModel->disassociate( watershed );
  //here hydrograph and runoff are deleted
  QVERIFY( hydrograph.isNull() );
  QVERIFY( runoff.isNull() );
  meteoModel->associate( watershed, &chicagoRainfallItem );
  // need to recall hydrograph/runoff to get them again
  hydrograph = runoffHydrographStore.hydrograph( meteoModel );
  runoff = runoffHydrographStore.runoff( meteoModel );
  QVERIFY( hydrograph->isObsolete() );
  QCOMPARE( runoffHydrographStore.updateCount, 6 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 7 ); //update just launch
  QVERIFY( hydrograph->isObsolete() ); //but not done
  QCOMPARE( hydrograph->valueCount(), 0 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 58.10846267665004 );
  QCOMPARE( runoff->data()->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 23.243385070660015 ); //just the runoff updated (done a least in the main thread before launching)

  timer.start( 200 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 7 );
  QVERIFY( !hydrograph->isObsolete() );
  QCOMPARE( hydrograph->valueCount(), 98 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.9983765506358444 );

  watershed->setCurrentTransferFunction( ReosTransferFunctionLinearReservoir::staticType() );
  QCOMPARE( runoffHydrographStore.updateCount, 7 ); //update will be launch when come back to the event loop

  timer.start( 1 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 8 ); //update just launch
  QVERIFY( hydrograph->isObsolete() ); //but not done
  QCOMPARE( hydrograph->valueCount(), 0 );
  QCOMPARE( rainfall->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 58.10846267665004 );
  QCOMPARE( runoff->data()->valueWithMode( 11, ReosTimeSerieConstantInterval::Cumulative ), 23.243385070660015 ); //just the runoff updated (done a least in the main thread before launching)

  timer.start( 200 );
  loop.exec();

  QCOMPARE( runoffHydrographStore.updateCount, 8 );
  QVERIFY( !hydrograph->isObsolete() );
  QCOMPARE( hydrograph->valueCount(), 80 );
  QCOMPARE( hydrograph->valueAt( 70 ), 0.625077585479 );
}

QTEST_MAIN( ReosWatersehdTest )
#include "reos_watershed_test.moc"
