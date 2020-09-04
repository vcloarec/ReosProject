#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "raster/reosmemoryraster.h"

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
  ReosRasterExtent extent( QRectF( 0, 0, 100, 100 ), 1, 1 );
  ASSERT_TRUE( memoryRaster.createTiffFile( "/home/vincent/es.tif", GDALDataType::GDT_Float64, extent ) );

  ReosRasterMemory<double> memoryRaster_3;
  ASSERT_TRUE( memoryRaster_3.loadDataFromTiffFile( "/home/vincent/es.tif", GDALDataType::GDT_Float64 ) );

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

int main( int argc, char **argv )
{
  testing::InitGoogleTest( &argc, argv );
  init_test();
  int ret =  RUN_ALL_TESTS();
  finalize_test();
  return ret;
}
