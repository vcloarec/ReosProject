include(gtest_dependency.pri)

win32 {
    include(../../../../QGIS_3_MSVC.pri);
    include(../../../../GDAL_OSGEO.pri);
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
    ../../Mesher/provider/meshdataprovider.h \
    ../../Mesher/provider/hdmeshgenerator.h \
    ../../Mesher/provider/hdmesheditor.h \
    ../../Mesher/tinEditorUi/hdmapmeshitem.h \
    ../../Mesher/tinEditorUi/hdtineditorgraphic.h \
    ../../Reos/reosmodule.h \
    tst_001_hdmeshgenerator.h \
    tst_002_hdmesheseditortest.h \
    tst_003_meshergeneratorprovidertest.h \
    tst_004_hdeditablemeshlayertest.h \
    ../../GIS/hdgismanager.h \
    ../../GIS/hdmap.h \
    ../../Reos/reossettings.h \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.h \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.h \
    ../../Reos/reosdialogbox.h \
    ../../Reos/reosencodedelement.h \
    ../../GIS/hdcrsdialogselection.h \
    ../../GIS/hdmaptool.h \
    ../../GIS/hdvectorlayerpropertiesdialog.h \
    ../../UtilsGeometry/utilsgeometry2d.h \
    tst_005_hdtineditorgraphictest.h \
    tst_006_hdmapmeshitemtest.h

SOURCES += \
        main.cpp \
    ../../Mesher/provider/meshdataprovider.cpp \
    ../../Mesher/provider/hdmeshgenerator.cpp \
    ../../Mesher/provider/hdmesheditor.cpp \
    ../../Mesher/tinEditorUi/hdmapmeshitem.cpp \
    ../../Mesher/tinEditorUi/hdtineditorgraphic.cpp \
    ../../Reos/reosmodule.cpp \
    ../../GIS/hdgismanager.cpp \
    ../../GIS/hdmap.cpp \
    ../../Reos/reossettings.cpp \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.cpp \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.cpp \
    ../../Reos/reosdialogbox.cpp \
    ../../Reos/reosencodedelement.cpp \
    ../../GIS/hdcrsdialogselection.cpp \
    ../../GIS/hdmaptool.cpp \
    ../../GIS/hdvectorlayerpropertiesdialog.cpp \
    ../../UtilsGeometry/utilsgeometry2d.cpp

DEFINES +=ANSI_DECLARATORS
DEFINES +=REAL=double

include(../../GIS/ConfigQgis.pri);
INCLUDEPATH +=../../GIS/QGis_app/

FORMS += \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.ui \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.ui \
    ../../GIS/hdcrsdialogselection.ui \
    ../../GIS/hdvectorlayerpropertiesdialog.ui

