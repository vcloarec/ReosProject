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
  GDALAllRegister();
}

void finalize_test()
{
}

TEST_F( ReosRasterWatershedTest, RasterFilling )
{
  ReosRasterMemory<float> dem;

  std::unique_ptr<ReosRasterFillingWangLiu> rasterFilling;
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0, 100 ) );

  rasterFilling->start();
  EXPECT_FALSE( rasterFilling->isSuccessful() );

  dem = ReosRasterMemory<float>( 11, 11 );
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0, 100 ) );
  rasterFilling->start();
  EXPECT_FALSE( rasterFilling->isSuccessful() );

  dem.reserveMemory();
  dem.fill( 5 );
  dem.setValue( 0, 5, 3.0 );
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0, 100 ) );
  rasterFilling->start();
  EXPECT_TRUE( rasterFilling->isSuccessful() );

  ReosRasterMemory<float> testFilledDem;
  testFilledDem.loadDataFromTiffFile( test_file( "filledDem.tiff" ).c_str(), GDALDataType::GDT_Float32 );
  ReosRasterMemory<float> returnDem;
  returnDem = rasterFilling->filledDEM();
  EXPECT_TRUE( testFilledDem == returnDem );

  std::unique_ptr<ReosRasterWatershedDirectionCalculation> directionCal = std::make_unique<ReosRasterWatershedDirectionCalculation>( rasterFilling->filledDEM() );
  directionCal->start();

  ReosRasterMemory<unsigned char> testFilledDemDir;
  testFilledDemDir.loadDataFromTiffFile( test_file( "filledDemDir_expected.tiff" ).c_str(), GDALDataType::GDT_Byte );

  ReosRasterExtent extent( 0, 0, 11, 11, 1, -1 );
  ReosRasterMemory<unsigned char> filledDemDir = directionCal->directions();
  EXPECT_TRUE( testFilledDemDir == filledDemDir );
}

TEST_F( ReosRasterWatershedTest, PlanDEM_1 )
{
  ReosRasterMemory<float> dem( 10, 10 );
  dem.reserveMemory();

  for ( int i = 0; i < 10; ++i )
    for ( int j = 0; j < 10; ++j )
      dem.setValue( i, j, 10.0 - i / 10.0 );

  std::unique_ptr<ReosRasterFillingWangLiu> rasterFilling;
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0, 100 ) );

  rasterFilling->start();
  EXPECT_TRUE( rasterFilling->isSuccessful() );

  std::unique_ptr<ReosRasterWatershedDirectionCalculation> directionCalculation = std::make_unique<ReosRasterWatershedDirectionCalculation>( rasterFilling->filledDEM() );
  directionCalculation->start();
  ReosRasterMemory<unsigned char> direction = directionCalculation->directions();

  for ( int r = 0; r < 9; r++ )
    for ( int c = 0; c < 10; c++ )
      EXPECT_EQ( direction.value( r, c ), 5 );

  for ( int c = 0; c < 10; c++ )
    EXPECT_EQ( direction.value( 9, c ), 4 );
}

TEST_F( ReosRasterWatershedTest, Delineate )
{
  ReosRasterWatershed::Directions directions;
  directions.loadDataFromTiffFile( test_file( "filledDemDir.tiff" ).c_str(), GDALDataType::GDT_Byte );

  ReosRasterLine downStreamLine;
  downStreamLine.addPoint( 2, 4 );
  downStreamLine.addPoint( 2, 9 );

  ReosRasterWatershedFromDirectionAndDownStreamLine watershedDelineate( directions, downStreamLine );

  watershedDelineate.start();

  ReosRasterWatershed::Watershed watershed = watershedDelineate.watershed();
  ReosRasterWatershed::Watershed testWatershed;
  testWatershed.loadDataFromTiffFile( test_file( "watershed.tiff" ).c_str(), GDALDataType::GDT_Byte );

  EXPECT_TRUE( testWatershed == watershed );

  ReosRasterExtent extent( ReosMapExtent( 0, 0, 11, 11 ), 11, 11 );
  ReosRasterWatershedToVector rasterToVector( watershed, extent, watershedDelineate.firstCell() );
  rasterToVector.start();
  QPolygonF watershedPolygon = rasterToVector.watershed();

  QPolygonF testWatershedPolygon( {QPointF( 4, 9 ), QPointF( 4, 8 ), QPointF( 3, 8 ), QPointF( 3, 6 ),
                                   QPointF( 4, 6 ), QPointF( 4, 4 ), QPointF( 6, 4 ), QPointF( 6, 5 ),
                                   QPointF( 7, 5 ), QPointF( 7, 6 ), QPointF( 8, 6 ), QPointF( 8, 7 ),
                                   QPointF( 9, 7 ), QPointF( 9, 8 ), QPointF( 10, 8 ), QPointF( 10, 9 ),
                                   QPointF( 5, 9 )} );

  EXPECT_TRUE( watershedPolygon == testWatershedPolygon );

  ReosRasterCellPos startCell( 6, 5 );
  ReosRasterWatershedTraceDownstream traceDownstream( directions, watershedPolygon, extent, startCell );
  traceDownstream.start();

  QPolygonF traceDownstreamPolyline = traceDownstream.resultPolyline();

  QPolygonF testTraceDownstream;
  testTraceDownstream << QPointF( 5.5, 4.5 ) << QPointF( 4.5, 5.5 ) << QPointF( 4.5, 9.5 );

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
