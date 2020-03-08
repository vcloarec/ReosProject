//#include "tst_001_meshgenerator.cpp" //Contains test related to Triangle lib
#include "tst_002_00_mesheseditortest.cpp"
#include "tst_003_meshergeneratorprovidertest.cpp"
#include "tst_004_editablemeshlayertest.cpp"
#include "tst_005_tineditorgraphictest.cpp"
#include "tst_006_mapmeshitemtest.cpp"

#include <gtest/gtest.h>

int main( int argc, char *argv[] )
{
  QApplication app( argc, argv );
  ::testing::InitGoogleTest( &argc, argv );
  return RUN_ALL_TESTS();
}
