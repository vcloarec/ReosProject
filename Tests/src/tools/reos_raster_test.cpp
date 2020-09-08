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

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "reosmemoryraster.h"
#include "reosrasterline.h"
#include "reosrastertrace.h"
#include "reos_testutils.h"

using namespace testing;

class ReosRasterTesting: public Test
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

TEST_F( ReosRasterTesting, ReosRasterExtent )
{
  ReosRasterExtent extent;
  ASSERT_FALSE( extent.isValid() );
  extent = ReosRasterExtent( QRectF( 20, 10, 110, 100 ), 0.5, -0.5 );
  ASSERT_TRUE( extent.isValid() );

  EXPECT_EQ( extent.xOrigin(), 20.0 );
  EXPECT_EQ( extent.yOrigin(), 110.0 ); //as y size < 0, the origin is Ymax
  EXPECT_EQ( extent.cellSurface(), 0.25 );
  EXPECT_EQ( extent.xCellCount(), 220 );
  EXPECT_EQ( extent.yCellCount(), 200 );

  EXPECT_EQ( extent.interCellToMap( QPoint( 0, 0 ) ), QPointF( 20, 110.0 ) );
  EXPECT_EQ( extent.interCellToMap( QPoint( 1, 0 ) ), QPointF( 20.5, 110.0 ) );
  EXPECT_EQ( extent.interCellToMap( QPoint( 1, 1 ) ), QPointF( 20.5, 109.5 ) );
  EXPECT_EQ( extent.cellCenterToMap( QPoint( 10, 10 ) ), QPointF( 25.25, 104.75 ) );

  extent = ReosRasterExtent( QRectF( 0, 0, 10, 10 ), 1, -1 );
  EXPECT_EQ( extent.mapToCell( QPointF( 1.6, 2.6 ) ), QPoint( 1, 7 ) );
  EXPECT_EQ( extent.cellXBeforeToMap( 4 ), 4.0 );
  EXPECT_EQ( extent.cellXAfterToMap( 7 ), 8.0 );
  EXPECT_EQ( extent.cellYBeforeToMap( 7 ), 3.0 );
  EXPECT_EQ( extent.cellYAfterToMap( 7 ), 2.0 );
  EXPECT_EQ( extent.cellMinMinCornerToMap( QPoint( 1, 1 ) ), QPointF( 1.0, 9.0 ) );
  EXPECT_EQ( extent.cellMaxMaxCornerToMap( QPoint( 1, 1 ) ), QPointF( 2.0, 8.0 ) );

  QRect rect = extent.mapRectToCellRect( QRectF( 3.6, 5.2, 4, 3 ) );
  EXPECT_EQ( rect, QRect( QPoint( 3, 1 ), QPoint( 7, 4 ) ) );

  EXPECT_EQ( extent.cellRectToMapRect( rect ), QRectF( 3.5, 5.5, 4, 3 ) );
  EXPECT_EQ( extent.cellRectToMapRect( rect, ReosRasterExtent::Interior ), QRectF( 4.0, 6.0, 3.0, 2.0 ) );
  EXPECT_EQ( extent.cellRectToMapRect( rect, ReosRasterExtent::Exterior ), QRectF( 3.0, 5.0, 5.0, 4.0 ) );

  extent = ReosRasterExtent( QRectF( 0, 0, 10, 10 ), 1, 1 );
  EXPECT_EQ( extent.mapToCell( QPointF( 1.6, 2.6 ) ), QPoint( 1, 2 ) );
  EXPECT_EQ( extent.cellXBeforeToMap( 4 ), 4.0 );
  EXPECT_EQ( extent.cellXAfterToMap( 7 ), 8.0 );
  EXPECT_EQ( extent.cellYBeforeToMap( 7 ), 7.0 );
  EXPECT_EQ( extent.cellYAfterToMap( 7 ), 8.0 );
  EXPECT_EQ( extent.cellMinMinCornerToMap( QPoint( 1, 1 ) ), QPointF( 1.0, 1.0 ) );
  EXPECT_EQ( extent.cellMaxMaxCornerToMap( QPoint( 1, 1 ) ), QPointF( 2.0, 2.0 ) );

  rect = extent.mapRectToCellRect( QRectF( 3.6, 5.2, 4, 3 ) );
  EXPECT_EQ( rect, QRect( QPoint( 3, 5 ), QPoint( 7, 8 ) ) );

  EXPECT_EQ( extent.cellRectToMapRect( rect ), QRectF( 3.5, 5.5, 4, 3 ) );
  EXPECT_EQ( extent.cellRectToMapRect( rect, ReosRasterExtent::Interior ), QRectF( 4.0, 6.0, 3.0, 2.0 ) );
  EXPECT_EQ( extent.cellRectToMapRect( rect, ReosRasterExtent::Exterior ), QRectF( 3.0, 5.0, 5.0, 4.0 ) );

  ReosRasterExtent extent_2( QRectF( 5, 5, 15, 4 ), 0.5, -0.5 );

  ReosRasterExtent extent_3 = extent * extent_2;
  EXPECT_EQ( extent_3.cellSurface(), 1 );
  EXPECT_EQ( extent_3.xOrigin(), 5 );
  EXPECT_EQ( extent_3.yOrigin(), 5 );
  EXPECT_EQ( extent_3.xCellCount(), 5 );
  EXPECT_EQ( extent_3.yCellCount(), 4 );
}

TEST_F( ReosRasterTesting, ReosRasterMemory )
{
  ReosRasterMemory<double> memoryRaster( 100, 100 );
  ASSERT_FALSE( memoryRaster.isValid() );
  ASSERT_TRUE( memoryRaster.reserveMemory() );
  ASSERT_TRUE( memoryRaster.isValid() );

  memoryRaster.fill( 12.345 );
  EXPECT_EQ( memoryRaster.value( 50, 50 ), 12.345 );

  ReosRasterMemory<double> memoryRaster_2;
  ASSERT_FALSE( memoryRaster_2.isValid() );

  ASSERT_TRUE( memoryRaster_2.copy( &memoryRaster ) );
  ASSERT_TRUE( memoryRaster_2.isValid() );
  EXPECT_EQ( memoryRaster_2.columnCount(), 100 );
  EXPECT_EQ( memoryRaster_2.rowCount(), 100 );
  EXPECT_EQ( memoryRaster_2.value( 50, 50 ), 12.345 );

  memoryRaster_2.freeMemory();
  ASSERT_FALSE( memoryRaster_2.isValid() );

  GDALAllRegister();
  ReosRasterExtent extent( QRectF( 0, 0, 100, 100 ), 1, -1 );
  ASSERT_TRUE( memoryRaster.createTiffFile( tmp_file( "/rasterMemory.tif" ).c_str(), GDALDataType::GDT_Float64, extent ) );

  ReosRasterMemory<double> memoryRaster_3;
  ASSERT_TRUE( memoryRaster_3.loadDataFromTiffFile( tmp_file( "/rasterMemory.tif" ).c_str(), GDALDataType::GDT_Float64 ) );

  ASSERT_TRUE( memoryRaster_3.isValid() );
  EXPECT_EQ( memoryRaster_3.columnCount(), 100 );
  EXPECT_EQ( memoryRaster_3.rowCount(), 100 );
  EXPECT_EQ( memoryRaster_3.value( 50, 50 ), 12.345 );
}


TEST_F( ReosRasterTesting, ReosRasterCellValue )
{
  ReosRasterMemory<double> memoryRaster( 100, 100 );
  ASSERT_FALSE( memoryRaster.isValid() );
  ReosRasterCellValue<double> rasterValue( &memoryRaster );
  ASSERT_FALSE( rasterValue.isValid() );
  ASSERT_TRUE( memoryRaster.reserveMemory() );
  ASSERT_TRUE( rasterValue.isValid() );

  memoryRaster.fill( 0.1 );

  EXPECT_EQ( rasterValue.row(), 0 );
  EXPECT_EQ( rasterValue.column(), 0 );
  EXPECT_EQ( rasterValue.value(), 0.1 );

  unsigned char direction = 7;
  int counter = 0;
  while ( rasterValue.isValid() )
  {
    int column = rasterValue.column();
    rasterValue.setValue( column );
    rasterValue.goInDirection( direction );
    EXPECT_EQ( rasterValue.column(), column + 1 );
    if ( !rasterValue.isValid() )
    {
      rasterValue.setRow( rasterValue.row() + 1 );
      rasterValue.setColumn( 0 );
    }
    else
      EXPECT_EQ( rasterValue.value(), 0.1 );
    ++counter;
  }

  EXPECT_EQ( counter, 10000 );

  rasterValue.setRow( 0 );
  rasterValue.setColumn( 0 );
  while ( rasterValue.isValid() )
  {
    rasterValue.goInDirection( direction );
    if ( !rasterValue.isValid() )
    {
      rasterValue.setRow( rasterValue.row() + 1 );
      rasterValue.setColumn( 0 );
    }
    else
      EXPECT_EQ( rasterValue.value(), rasterValue.column() );
    ++counter;
  }
}

TEST_F( ReosRasterTesting, ReosRasterLine )
{
  ReosRasterLine slimLine( false );
  ReosRasterLine thickLine( true );
  ASSERT_EQ( slimLine.cellCount(), 0 );
  ASSERT_EQ( thickLine.cellCount(), 0 );

  slimLine.addPoint( 0, 0 );
  thickLine.addPoint( 0, 0 );
  EXPECT_EQ( slimLine.cellCount(), 1 );
  EXPECT_EQ( thickLine.cellCount(), 1 );

  slimLine.addPoint( ReosRasterCellPos( 2, 5 ) );
  thickLine.addPoint( 2, 5 );
  EXPECT_EQ( slimLine.cellCount(), 6 );
  EXPECT_EQ( thickLine.cellCount(), 8 );

  slimLine.addPoint( 0, 7 );
  thickLine.addPoint( 0, 7 );
  EXPECT_EQ( slimLine.cellCount(), 8 );
  EXPECT_EQ( thickLine.cellCount(), 12 );

  slimLine.addPoint( 3, 9 );
  thickLine.addPoint( 3, 9 );
  EXPECT_EQ( slimLine.cellCount(), 11 );
  EXPECT_EQ( thickLine.cellCount(), 17 );

  ReosRasterCellPos cellPos( 1, 3 );
  EXPECT_TRUE( slimLine.contains( cellPos ) );
  EXPECT_TRUE( thickLine.contains( cellPos ) );

  cellPos = ReosRasterCellPos( 1, 4 );
  EXPECT_FALSE( slimLine.contains( cellPos ) );
  EXPECT_TRUE( thickLine.contains( cellPos ) );

  cellPos = ReosRasterCellPos( 2, 9 );
  EXPECT_FALSE( slimLine.contains( cellPos ) );
  EXPECT_TRUE( thickLine.contains( cellPos ) );
}

TEST_F( ReosRasterTesting, ReosRasterTrace )
{
  ReosRasterMemory<int> memoryRaster( 100, 100 );
  QVector<QPoint> stopLine;
  stopLine << QPoint( 2, 47 ) << QPoint( 2, 48 ) << QPoint( 2, 49 ) << QPoint( 2, 50 ) << QPoint( 2, 51 );
  QList<QPoint> elimination;
  ReosRasterTraceBetweenCellsUniqueValue<int> invalidTrace( &memoryRaster, 5, QPoint( 0, 20 ), QPoint( -1, 20 ), stopLine, elimination );

  ASSERT_FALSE( invalidTrace.startTracing() );

  GDALAllRegister();
  memoryRaster.loadDataFromTiffFile( test_file( "rasterForTrace.tiff" ).c_str(), GDALDataType::GDT_Int32 );

  ReosRasterTraceBetweenCellsUniqueValue<int> trace( &memoryRaster, 5, QPoint( 2, 50 ), QPoint( 1, 0 ), stopLine, elimination );

  ASSERT_TRUE( trace.startTracing() );
  ASSERT_EQ( trace.error(), 0 );
  QPolygon tr = trace.trace();

  QPolygon testTrace( {QPoint( {2, 50} ), QPoint( {2, 49} ), QPoint( {4, 49} ), QPoint( {4, 48} ), QPoint( {6, 48} ), QPoint( {6, 47} ), QPoint( {8, 47} ), QPoint( {8, 46} ), QPoint( {10, 46} ), QPoint( {10, 45} ),
                       QPoint( {12, 45} ), QPoint( {12, 44} ), QPoint( {14, 44} ), QPoint( {14, 43} ), QPoint( {16, 43} ), QPoint( {16, 42} ), QPoint( {18, 42} ), QPoint( {18, 41} ), QPoint( {20, 41} ), QPoint( {20, 40} ),
                       QPoint( {22, 40} ), QPoint( {22, 39} ), QPoint( {24, 39} ), QPoint( {24, 38} ), QPoint( {26, 38} ), QPoint( {26, 37} ), QPoint( {28, 37} ), QPoint( {28, 36} ), QPoint( {30, 36} ), QPoint( {30, 35} ),
                       QPoint( {32, 35} ), QPoint( {32, 34} ), QPoint( {34, 34} ), QPoint( {34, 33} ), QPoint( {36, 33} ), QPoint( {36, 32} ), QPoint( {38, 32} ), QPoint( {38, 31} ), QPoint( {40, 31} ), QPoint( {40, 30} ),
                       QPoint( {42, 30} ), QPoint( {42, 29} ), QPoint( {44, 29} ), QPoint( {44, 28} ), QPoint( {46, 28} ), QPoint( {46, 27} ), QPoint( {48, 27} ), QPoint( {48, 26} ), QPoint( {50, 26} ), QPoint( {50, 25} ),
                       QPoint( {52, 25} ), QPoint( {52, 24} ), QPoint( {54, 24} ), QPoint( {54, 23} ), QPoint( {56, 23} ), QPoint( {56, 22} ), QPoint( {58, 22} ), QPoint( {58, 21} ), QPoint( {60, 21} ), QPoint( {60, 20} ),
                       QPoint( {62, 20} ), QPoint( {62, 19} ), QPoint( {64, 19} ), QPoint( {64, 18} ), QPoint( {66, 18} ), QPoint( {66, 17} ), QPoint( {68, 17} ), QPoint( {68, 16} ), QPoint( {70, 16} ), QPoint( {70, 85} ),
                       QPoint( {68, 85} ), QPoint( {68, 84} ), QPoint( {66, 84} ), QPoint( {66, 83} ), QPoint( {64, 83} ), QPoint( {64, 82} ), QPoint( {62, 82} ), QPoint( {62, 81} ), QPoint( {60, 81} ), QPoint( {60, 80} ),
                       QPoint( {58, 80} ), QPoint( {58, 79} ), QPoint( {56, 79} ), QPoint( {56, 78} ), QPoint( {54, 78} ), QPoint( {54, 77} ), QPoint( {52, 77} ), QPoint( {52, 76} ), QPoint( {50, 76} ), QPoint( {50, 75} ),
                       QPoint( {48, 75} ), QPoint( {48, 74} ), QPoint( {46, 74} ), QPoint( {46, 73} ), QPoint( {44, 73} ), QPoint( {44, 72} ), QPoint( {42, 72} ), QPoint( {42, 71} ), QPoint( {40, 71} ), QPoint( {40, 70} ),
                       QPoint( {38, 70} ), QPoint( {38, 69} ), QPoint( {36, 69} ), QPoint( {36, 68} ), QPoint( {34, 68} ), QPoint( {34, 67} ), QPoint( {32, 67} ), QPoint( {32, 66} ), QPoint( {30, 66} ), QPoint( {30, 65} ),
                       QPoint( {28, 65} ), QPoint( {28, 64} ), QPoint( {26, 64} ), QPoint( {26, 63} ), QPoint( {24, 63} ), QPoint( {24, 62} ), QPoint( {22, 62} ), QPoint( {22, 61} ), QPoint( {20, 61} ), QPoint( {20, 60} ),
                       QPoint( {18, 60} ), QPoint( {18, 59} ), QPoint( {16, 59} ), QPoint( {16, 58} ), QPoint( {14, 58} ), QPoint( {14, 57} ), QPoint( {12, 57} ), QPoint( {12, 56} ), QPoint( {10, 56} ), QPoint( {10, 55} ),
                       QPoint( {8, 55} ), QPoint( {8, 54} ), QPoint( {6, 54} ), QPoint( {6, 53} ), QPoint( {4, 53} ), QPoint( {4, 52} ), QPoint( {3, 52} )} );

  EXPECT_EQ( testTrace, tr );
}

int main( int argc, char **argv )
{
  testing::InitGoogleTest( &argc, argv );
  init_test();
  int ret =  RUN_ALL_TESTS();
  finalize_test();
  return ret;
}
