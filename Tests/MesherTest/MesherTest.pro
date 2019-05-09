include(gtest_dependency.pri)

win32 {
    include(../../../../QGIS_3_MSVC.pri);
    include(../../../GDAL_OSGEO.pri);
    include(../../../QWT613_MSVC2017_x64.pri);
}

linux {
    include(../../../QGIS_LINUX.pri);
    include(../../../GDAL_LINUX.pri);
    include(../../../QWT_LINUX.pri);
}

QT += xml
QT += widgets

TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG += thread
#CONFIG -= qt

HEADERS += \
    ../../Mesher/meshdataprovider.h \
    ../../Mesher/hdmeshgenerator.h \
    ../../Mesher/hdmesheditor.h \
    ../../Mesher/hdmapmeshitem.h \
    ../../Mesher/hdtineditorgraphic.h \
    ../../Reos/reosmodule.h \
    tst_001_hdmeshgenerator.h \
    tst_002_hdmesheseditortest.h \
    tst_003_meshergeneratorprovidertest.h \
    tst_004_hdeditablemeshlayertest.h \
    tst_005_hdmapmeshitemtest.h \
    tst_006_hdtineditorgraphictest.h

SOURCES += \
        main.cpp \
    ../../Mesher/meshdataprovider.cpp \
    ../../Mesher/hdmeshgenerator.cpp \
    ../../Mesher/hdmesheditor.cpp \
    ../../Mesher/hdmapmeshitem.cpp \
    ../../Mesher/hdtineditorgraphic.cpp \
    ../../Reos/reosmodule.cpp \

DEFINES +=ANSI_DECLARATORS
DEFINES +=REAL=double

include(../../GIS/ConfigQgis.pri);
