include(../config.pri)

######################### Google Test
include(gtest_dependency.pri)

######################### GDAL
include(../configGDAL.pri);

QT += core gui widgets network xml sql
CONFIG += crypto

TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG += thread
CONFIG +=C++14

HEADERS += \
    ../src/Tools/raster/reosmemoryraster.h


SOURCES += \
    src/main.cpp \
    src/tools/reos_raster_test.cpp \
#Tools
    ../src/Tools/raster/reosmemoryraster.cpp



