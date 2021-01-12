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
#include "reosrastercompressed.h"
#include "reos_testutils.h"

#include "fstream"

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
  extent = ReosRasterExtent( 20, 110, 220, 200, 0.5, -0.5 );
  ASSERT_TRUE( extent.isValid() );

  EXPECT_EQ( extent.xMapOrigin(), 20.0 );
  EXPECT_EQ( extent.yMapOrigin(), 110.0 ); //as y size < 0, the origin is Ymax
  EXPECT_EQ( extent.cellSurface(), 0.25 );
  EXPECT_EQ( extent.xCellCount(), 220 );
  EXPECT_EQ( extent.yCellCount(), 200 );

  EXPECT_EQ( extent.interCellToMap( QPoint( 0, 0 ) ), QPointF( 20, 110.0 ) );
  EXPECT_EQ( extent.interCellToMap( QPoint( 1, 0 ) ), QPointF( 20.5, 110.0 ) );
  EXPECT_EQ( extent.interCellToMap( QPoint( 1, 1 ) ), QPointF( 20.5, 109.5 ) );
  EXPECT_EQ( extent.cellCenterToMap( QPoint( 10, 10 ) ), QPointF( 25.25, 104.75 ) );

  extent = ReosRasterExtent( 0, 10, 10, 10, 1, -1 );
  EXPECT_EQ( extent.mapToCell( QPointF( 1.6, 2.6 ) ), QPoint( 1, 7 ) );
  EXPECT_EQ( extent.cellXBeforeToMap( 4 ), 4.0 );
  EXPECT_EQ( extent.cellXAfterToMap( 7 ), 8.0 );
  EXPECT_EQ( extent.cellYBeforeToMap( 7 ), 3.0 );
  EXPECT_EQ( extent.cellYAfterToMap( 7 ), 2.0 );
  EXPECT_EQ( extent.cellMinMinCornerToMap( QPoint( 1, 1 ) ), QPointF( 1.0, 9.0 ) );
  EXPECT_EQ( extent.cellMaxMaxCornerToMap( QPoint( 1, 1 ) ), QPointF( 2.0, 8.0 ) );

  QRect rect = extent.mapExtentToCellRect( ReosMapExtent( 3.6, 5.2, 7.6, 8.2 ) );
  EXPECT_EQ( rect, QRect( QPoint( 3, 1 ), QPoint( 7, 4 ) ) );

  EXPECT_EQ( extent.cellRectToMapExtent( rect ), ReosMapExtent( 3.5, 5.5, 7.5, 8.5 ) );
  EXPECT_EQ( extent.cellRectToMapExtent( rect, ReosRasterExtent::Interior ), ReosMapExtent( 4.0, 6.0, 7.0, 8.0 ) );
  EXPECT_EQ( extent.cellRectToMapExtent( rect, ReosRasterExtent::Exterior ), ReosMapExtent( 3.0, 5.0, 8.0, 9.0 ) );

  extent = ReosRasterExtent( 0, 0, 10, 10, 1, 1 );
  EXPECT_EQ( extent.mapToCell( QPointF( 1.6, 2.6 ) ), QPoint( 1, 2 ) );
  EXPECT_EQ( extent.cellXBeforeToMap( 4 ), 4.0 );
  EXPECT_EQ( extent.cellXAfterToMap( 7 ), 8.0 );
  EXPECT_EQ( extent.cellYBeforeToMap( 7 ), 7.0 );
  EXPECT_EQ( extent.cellYAfterToMap( 7 ), 8.0 );
  EXPECT_EQ( extent.cellMinMinCornerToMap( QPoint( 1, 1 ) ), QPointF( 1.0, 1.0 ) );
  EXPECT_EQ( extent.cellMaxMaxCornerToMap( QPoint( 1, 1 ) ), QPointF( 2.0, 2.0 ) );

  rect = extent.mapExtentToCellRect( ReosMapExtent( 3.6, 5.2, 7.6, 8.2 ) );
  EXPECT_EQ( rect, QRect( QPoint( 3, 5 ), QPoint( 7, 8 ) ) );

  EXPECT_EQ( extent.cellRectToMapExtent( rect ), ReosMapExtent( 3.5, 5.5, 7.5, 8.5 ) );
  EXPECT_EQ( extent.cellRectToMapExtent( rect, ReosRasterExtent::Interior ), ReosMapExtent( 4.0, 6.0, 7.0, 8.0 ) );
  EXPECT_EQ( extent.cellRectToMapExtent( rect, ReosRasterExtent::Exterior ), ReosMapExtent( 3.0, 5.0, 8.0, 9.0 ) );

  ReosRasterExtent extent_2( 5, 5, 20, 9, 0.5, -0.5 );

  ReosRasterExtent extent_3 = extent * extent_2;
  EXPECT_EQ( extent_3.cellSurface(), 1 );
  EXPECT_EQ( extent_3.xMapOrigin(), 5 );
  EXPECT_EQ( extent_3.yMapOrigin(), 0 );
  EXPECT_EQ( extent_3.xCellCount(), 5 );
  EXPECT_EQ( extent_3.yCellCount(), 5 );

  extent = ReosRasterExtent( 0, 10, 10, 10, 1, -1 );
  extent_2 = ReosRasterExtent( 5, 5, 20, 9, 0.5, -0.5 );
  extent_3 = extent * extent_2;
  EXPECT_EQ( extent_3.cellSurface(), 1 );
  EXPECT_EQ( extent_3.xMapOrigin(), 5 );
  EXPECT_EQ( extent_3.yMapOrigin(), 5 );
  EXPECT_EQ( extent_3.xCellCount(), 5 );
  EXPECT_EQ( extent_3.yCellCount(), 5 );
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
  ReosRasterCellValue<double> rasterValue( memoryRaster );
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
  stopLine << QPoint( 47, 2 ) << QPoint( 48, 2 ) << QPoint( 49, 2 ) << QPoint( 50, 2 ) << QPoint( 51, 2 );
  QList<QPoint> elimination;
  ReosRasterTraceBetweenCellsUniqueValue<int> invalidTrace( memoryRaster, 5, QPoint( 0, 20 ), QPoint( -1, 20 ), stopLine, elimination );

  ASSERT_FALSE( invalidTrace.startTracing() );

  GDALAllRegister();
  memoryRaster.loadDataFromTiffFile( test_file( "rasterForTrace.tiff" ).c_str(), GDALDataType::GDT_Int32 );

  ReosRasterTraceBetweenCellsUniqueValue<int> trace( memoryRaster, 5, QPoint( 50, 2 ), QPoint( 0, 1 ), stopLine, elimination );

  ASSERT_TRUE( trace.startTracing() );
  ASSERT_EQ( trace.error(), 0 );
  QPolygon tr = trace.trace();

  QPolygon testTrace( {QPoint( {50, 2} ), QPoint( {49, 2} ), QPoint( {49, 4} ), QPoint( {48, 4} ), QPoint( {48, 6} ), QPoint( {47, 6} ), QPoint( {47, 8} ),
                       QPoint( {46, 8} ), QPoint( {46, 10} ), QPoint( {45, 10} ), QPoint( {45, 12} ), QPoint( {44, 12} ), QPoint( {44, 14} ),
                       QPoint( {43, 14} ), QPoint( {43, 16} ), QPoint( {42, 16} ), QPoint( {42, 18} ), QPoint( {41, 18} ), QPoint( {41, 20} ),
                       QPoint( {40, 20} ), QPoint( {40, 22} ), QPoint( {39, 22} ), QPoint( {39, 24} ), QPoint( {38, 24} ), QPoint( {38, 26} ),
                       QPoint( {37, 26} ), QPoint( {37, 28} ), QPoint( {36, 28} ), QPoint( {36, 30} ), QPoint( {35, 30} ), QPoint( {35, 32} ),
                       QPoint( {34, 32} ), QPoint( {34, 34} ), QPoint( {33, 34} ), QPoint( {33, 36} ), QPoint( {32, 36} ), QPoint( {32, 38} ),
                       QPoint( {31, 38} ), QPoint( {31, 40} ), QPoint( {30, 40} ), QPoint( {30, 42} ), QPoint( {29, 42} ), QPoint( {29, 44} ),
                       QPoint( {28, 44} ), QPoint( {28, 46} ), QPoint( {27, 46} ), QPoint( {27, 48} ), QPoint( {26, 48} ), QPoint( {26, 50} ),
                       QPoint( {25, 50} ), QPoint( {25, 52} ), QPoint( {24, 52} ), QPoint( {24, 54} ), QPoint( {23, 54} ), QPoint( {23, 56} ),
                       QPoint( {22, 56} ), QPoint( {22, 58} ), QPoint( {21, 58} ), QPoint( {21, 60} ), QPoint( {20, 60} ), QPoint( {20, 62} ),
                       QPoint( {19, 62} ), QPoint( {19, 64} ), QPoint( {18, 64} ), QPoint( {18, 66} ), QPoint( {17, 66} ), QPoint( {17, 68} ),
                       QPoint( {16, 68} ), QPoint( {16, 70} ), QPoint( {85, 70} ), QPoint( {85, 68} ), QPoint( {84, 68} ), QPoint( {84, 66} ),
                       QPoint( {83, 66} ), QPoint( {83, 64} ), QPoint( {82, 64} ), QPoint( {82, 62} ), QPoint( {81, 62} ), QPoint( {81, 60} ),
                       QPoint( {80, 60} ), QPoint( {80, 58} ), QPoint( {79, 58} ), QPoint( {79, 56} ), QPoint( {78, 56} ), QPoint( {78, 54} ),
                       QPoint( {77, 54} ), QPoint( {77, 52} ), QPoint( {76, 52} ), QPoint( {76, 50} ), QPoint( {75, 50} ), QPoint( {75, 48} ),
                       QPoint( {74, 48} ), QPoint( {74, 46} ), QPoint( {73, 46} ), QPoint( {73, 44} ), QPoint( {72, 44} ), QPoint( {72, 42} ),
                       QPoint( {71, 42} ), QPoint( {71, 40} ), QPoint( {70, 40} ), QPoint( {70, 38} ), QPoint( {69, 38} ), QPoint( {69, 36} ),
                       QPoint( {68, 36} ), QPoint( {68, 34} ), QPoint( {67, 34} ), QPoint( {67, 32} ), QPoint( {66, 32} ), QPoint( {66, 30} ),
                       QPoint( {65, 30} ), QPoint( {65, 28} ), QPoint( {64, 28} ), QPoint( {64, 26} ), QPoint( {63, 26} ), QPoint( {63, 24} ),
                       QPoint( {62, 24} ), QPoint( {62, 22} ), QPoint( {61, 22} ), QPoint( {61, 20} ), QPoint( {60, 20} ), QPoint( {60, 18} ),
                       QPoint( {59, 18} ), QPoint( {59, 16} ), QPoint( {58, 16} ), QPoint( {58, 14} ), QPoint( {57, 14} ), QPoint( {57, 12} ),
                       QPoint( {56, 12} ), QPoint( {56, 10} ), QPoint( {55, 10} ), QPoint( {55, 8} ), QPoint( {54, 8} ), QPoint( {54, 6} ),
                       QPoint( {53, 6} ), QPoint( {53, 4} ), QPoint( {52, 4} ), QPoint( {52, 3} )} );


  EXPECT_EQ( testTrace, tr );
}

TEST_F( ReosRasterTesting, ReosRasterByteCompressed )
{
  GDALAllRegister();
  ReosRasterMemory<unsigned char> memoryRaster;
  memoryRaster.loadDataFromTiffFile( test_file( "filledDemDir.tiff" ).c_str(), GDALDataType::GDT_Byte );

  ReosRasterByteCompressed compressed( memoryRaster );

  ReosRasterMemory<unsigned char> uncompressed = compressed.uncompressRaster();

  ASSERT_TRUE( uncompressed == memoryRaster );
}

int main( int argc, char **argv )
{
  testing::InitGoogleTest( &argc, argv );
  init_test();
  int ret =  RUN_ALL_TESTS();
  finalize_test();
  return ret;
}
