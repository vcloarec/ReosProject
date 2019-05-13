#include "tst_001_hdmeshgenerator.h"
#include "tst_002_hdmesheseditortest.h"
#include "tst_003_meshergeneratorprovidertest.h"
#include "tst_004_hdeditablemeshlayertest.h"
#include "tst_005_hdtineditorgraphictest.h"
#include "tst_006_hdmapmeshitemtest.h"

#include <gtest/gtest.h>

int main(int argc, char *argv[])
{
    QApplication app(argc,argv);
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
