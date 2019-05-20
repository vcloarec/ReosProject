include(../../config.pri)

######################### Google Test
include(gtest_dependency.pri)

######################### QGIG
INCLUDEPATH +=../../GIS/QGis_app/
include(../../GIS/ConfigQgis.pri); #comment to generate the linguist file

######################### CGAL
include(../../Mesher/configCGAL.pri);


######################### GDAL
include(../../GIS/configGDAL.pri);


QT += core gui widgets network xml sql
CONFIG += crypto

TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG += thread
CONFIG +=C++11
#CONFIG -= qt

HEADERS += \
    ../../Mesher/provider/meshdataprovider.h \
    ../../Mesher/HdMesh/hdmeshgenerator.h \
    ../../Mesher/HdMesh/hdmesheditor.h \
    ../../Mesher/HdMesh/hdmesh.h \
    ../../Mesher/HdTin/hdtin.h \
    ../../Mesher/HdTin/hdtineditor.h \
    ../../Mesher/tinEditorUi/hdmapmeshitem.h \
    ../../Mesher/tinEditorUi/hdtineditorgraphic.h \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.h \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.h \
    ../../Reos/reosmodule.h \
    ../../Reos/reosdialogbox.h \
    ../../Reos/reosencodedelement.h \
    ../../Reos/reossettings.h \
    ../../GIS/hdgismanager.h \
    ../../GIS/hdmap.h \
    ../../GIS/hdcrsdialogselection.h \
    ../../GIS/hdmaptool.h \
    ../../GIS/hdvectorlayerpropertiesdialog.h \
    ../../UtilsGeometry/utilsgeometry2d.h \
    tst_001_hdmeshgenerator.h \
    tst_002_hdmesheseditortest.h \
    tst_003_meshergeneratorprovidertest.h \
    tst_004_hdeditablemeshlayertest.h \
    tst_005_hdtineditorgraphictest.h \
    tst_006_hdmapmeshitemtest.h


SOURCES += \
        main.cpp \
    ../../Mesher/provider/meshdataprovider.cpp \
    ../../Mesher/HdMesh/hdmeshgenerator.cpp \
    ../../Mesher/HdMesh/hdmesheditor.cpp \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.cpp \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.cpp \
    ../../Mesher/HdMesh/hdmesh.cpp \
    ../../Mesher/HdTin/hdtin.cpp \
    ../../Mesher/HdTin/hdtineditor.cpp \
    ../../Mesher/tinEditorUi/hdmapmeshitem.cpp \
    ../../Mesher/tinEditorUi/hdtineditorgraphic.cpp \
    ../../Reos/reosmodule.cpp \
    ../../Reos/reossettings.cpp \
    ../../Reos/reosdialogbox.cpp \
    ../../Reos/reosencodedelement.cpp \
    ../../GIS/hdgismanager.cpp \
    ../../GIS/hdmap.cpp \
    ../../GIS/hdcrsdialogselection.cpp \
    ../../GIS/hdmaptool.cpp \
    ../../GIS/hdvectorlayerpropertiesdialog.cpp \
    ../../UtilsGeometry/utilsgeometry2d.cpp


FORMS += \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.ui \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.ui \
    ../../GIS/hdcrsdialogselection.ui \
    ../../GIS/hdvectorlayerpropertiesdialog.ui



