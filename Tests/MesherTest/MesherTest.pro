include(../../config.pri)

######################### Google Test
include(gtest_dependency.pri)


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
CONFIG +=C++14
#CONFIG -= qt

HEADERS += \
    ../../GIS/reosmapitem.h \
    ../../GIS/reosmap.h \
    ../../Reos/reosmodule.h \
    ../../Reos/reosdialogbox.h \
    ../../Reos/reosencodedelement.h \
    ../../Reos/reossettings.h \
    ../../GIS/hdgismanager.h \
    ../../GIS/hdcrsdialogselection.h \
    ../../GIS/reosmaptool.h \
    ../../GIS/hdvectorlayerpropertiesdialog.h \
    ../../UtilsGeometry/utilsgeometry2d.h \
    ../../Mesher/HdTin/test.h \
    ../../Mesher/HdMesh/reosmesh.h \
    ../../Mesher/HdTin/reostin.h \
    ../../Mesher/HdTin/reostineditor.h \
    ../../Mesher/HdMesh/reosmeshgenerator.h \
    ../../Mesher/HdMesh/reosmesheditor.h \
    ../../Mesher/provider/meshdataprovider.h \
    ../../Mesher/tinEditorUi/reosmapmeshitem.h \
    ../../Mesher/tinEditorUi/reostineditorgraphic.h \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.h \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.h\
    tst_001_hdmeshgenerator.h \
    tst_002_hdmesheseditortest.h \
    tst_003_meshergeneratorprovidertest.h \
    tst_004_hdeditablemeshlayertest.h \
    tst_005_hdtineditorgraphictest.h \
    tst_006_hdmapmeshitemtest.h


SOURCES += \
    ../../GIS/reosmapitem.cpp \
    ../../GIS/reosmap.cpp \
        main.cpp \
    ../../Mesher/provider/meshdataprovider.cpp \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.cpp \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.cpp \
    ../../Mesher/tinEditorUi/reosmapmeshitem.cpp \
    ../../Mesher/tinEditorUi/reostineditorgraphic.cpp \
    ../../Reos/reosmodule.cpp \
    ../../Reos/reossettings.cpp \
    ../../Reos/reosdialogbox.cpp \
    ../../Reos/reosencodedelement.cpp \
    ../../GIS/hdgismanager.cpp \
    ../../GIS/hdcrsdialogselection.cpp \
    ../../GIS/reosmaptool.cpp \
    ../../GIS/hdvectorlayerpropertiesdialog.cpp \
    ../../UtilsGeometry/utilsgeometry2d.cpp \
    ../../Mesher/HdMesh/reosmesh.cpp \
    ../../Mesher/HdMesh/reosmeshgenerator.cpp \
    ../../Mesher/HdMesh/reosmesheditor.cpp \
    ../../Mesher/HdTin/reostin.cpp \
    ../../Mesher/HdTin/reostineditor.cpp


FORMS += \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.ui \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.ui \
    ../../GIS/hdcrsdialogselection.ui \
    ../../GIS/hdvectorlayerpropertiesdialog.ui


######################### QGIG
INCLUDEPATH +=../../GIS/QGis_app/
include(../../GIS/ConfigQgis.pri); #comment to generate the linguist file
