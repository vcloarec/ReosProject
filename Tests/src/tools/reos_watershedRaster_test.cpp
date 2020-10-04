/***************************************************************************
                      reos_watershedRaster_test.cpp
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

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "reosmemoryraster.h"
#include "reosrasterfilling.h"
#include "reosrasterwatershed.h"
#include "reos_testutils.h"
#include <fstream>

using namespace testing;

class ReosRasterWatershedTest: public Test
{
  public:

  protected:
    void SetUp() override
    {}
};

void init_test()
{
}

void finalize_test()
{
}

TEST_F( ReosRasterWatershedTest, RasterFilling )
{
  std::shared_ptr<ReosRasterMemory<float>> dem;

  std::unique_ptr<ReosRasterFillingWangLiu> rasterFilling;
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0 ) );

  rasterFilling->start();
  EXPECT_FALSE( rasterFilling->isSuccessful() );

  dem = std::make_shared<ReosRasterMemory<float>>( 11, 11 );
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0 ) );
  rasterFilling->start();
  EXPECT_FALSE( rasterFilling->isSuccessful() );

  dem->reserveMemory();
  dem->fill( 5 );
  dem->setValue( 0, 5, 3.0 );
  rasterFilling->start();
  EXPECT_TRUE( rasterFilling->isSuccessful() );

  GDALAllRegister();

  ReosRasterMemory<float> testFilledDem;
  testFilledDem.loadDataFromTiffFile( test_file( "filledDem.tiff" ).c_str(), GDALDataType::GDT_Float32 );
  EXPECT_TRUE( testFilledDem == *dem.get() );

  ReosRasterMemory<unsigned char> testFilledDemDir;
  testFilledDemDir.loadDataFromTiffFile( test_file( "filledDemDir.tiff" ).c_str(), GDALDataType::GDT_Byte );
  EXPECT_TRUE( testFilledDemDir == *rasterFilling->direction().get() );
}

TEST_F( ReosRasterWatershedTest, Delineate )
{
  ReosRasterWatershed::Directions directions = std::make_shared<ReosRasterMemory<unsigned char>>();
  GDALAllRegister();
  directions->loadDataFromTiffFile( test_file( "filledDemDir.tiff" ).c_str(), GDALDataType::GDT_Byte );

  ReosRasterLine downStreamLine;
  downStreamLine.addPoint( 2, 4 );
  downStreamLine.addPoint( 2, 9 );

  ReosRasterWatershedFromDirectionAndDownStreamLine watershedDelineate( directions, downStreamLine );

  watershedDelineate.start();

  ReosRasterWatershed::Watershed watershed = watershedDelineate.watershed();
  ReosRasterWatershed::Watershed testWatershed = std::make_shared<ReosRasterMemory<unsigned char>>();;
  testWatershed->loadDataFromTiffFile( test_file( "watershed.tiff" ).c_str(), GDALDataType::GDT_Byte );

  EXPECT_TRUE( *testWatershed.get() == *watershed.get() );

  ReosRasterExtent extent( ReosMapExtent( 10, 10, 32, 32 ), 11, 11, true, true );
  ReosRasterWatershedToVector rasterToVector( watershed, extent, watershedDelineate.fisrtCell() );
  rasterToVector.start();
  QPolygonF watershedPolygon = rasterToVector.watershed();

  QPolygonF testWatershedPolygon( {QPointF( 14, 18 ), QPointF( 16, 18 ), QPointF( 16, 16 ), QPointF( 20, 16 ),
                                   QPointF( 20, 18 ), QPointF( 24, 18 ), QPointF( 24, 22 ), QPointF( 22, 22 ),
                                   QPointF( 22, 24 ), QPointF( 20, 24 ), QPointF( 20, 26 ), QPointF( 18, 26 ),
                                   QPointF( 18, 28 ), QPointF( 16, 28 ), QPointF( 16, 30 ), QPointF( 14, 30 ),
                                   QPointF( 14, 20 )} );

  EXPECT_TRUE( watershedPolygon == testWatershedPolygon );

  ReosRasterCellPos startCell( 6, 6 );
  ReosRasterWatershedTraceDownstream traceDownstream( directions, watershedPolygon, extent, startCell );
  traceDownstream.start();

  QPolygonF traceDownstreamPolyline = traceDownstream.resultPolyline();

  QPolygonF testTraceDownstream;
  testTraceDownstream << QPointF( 23.0, 23.0 ) << QPointF( 25.0, 21.0 );

  EXPECT_TRUE( traceDownstreamPolyline == testTraceDownstream );
}

int main( int argc, char **argv )
{
  testing::InitGoogleTest( &argc, argv );
  init_test();
  int ret =  RUN_ALL_TESTS();
  finalize_test();
  return ret;
}
