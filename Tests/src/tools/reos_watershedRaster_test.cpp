#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "raster/reosmemoryraster.h"
#include "raster/reosrasterfilling.h"
#include "reos_testutils.h"

using namespace testing;

class ReosRasterWatershed: public Test
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

TEST_F( ReosRasterWatershed, RasterFilling )
{
  std::shared_ptr<ReosRasterMemory<float>> dem;

  std::unique_ptr<ReosRasterFillingWangLiu> rasterFilling;
  rasterFilling.reset( new ReosRasterFillingWangLiu( dem, 1.0, 1.0 ) );

  rasterFilling->start();
  EXPECT_FALSE( rasterFilling->isSuccessful() );

  dem = std::make_shared<ReosRasterMemory<float>>( 10, 10 );
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
  ReosRasterExtent extent( QRectF( 0, 0, 10, 10 ), 1, -1 );
  testFilledDemDir.loadDataFromTiffFile( test_file( "filledDemDir.tiff" ).c_str(), GDALDataType::GDT_Byte );
  EXPECT_TRUE( testFilledDemDir == *rasterFilling->direction().get() );
}

int main( int argc, char **argv )
{
  testing::InitGoogleTest( &argc, argv );
  init_test();
  int ret =  RUN_ALL_TESTS();
  finalize_test();
  return ret;
}
