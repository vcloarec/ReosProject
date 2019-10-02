include(../../config.pri)

######################### Google Test
include(gtest_dependency.pri)


######################### CGAL
include(../../Mesher/configCGAL.pri);

######################### GDAL
include(../../GIS/configGDAL.pri);

######################### NetCDF
include(../../Mesher/configNetCDF.pri);


QT += core gui widgets network xml sql
CONFIG += crypto

TEMPLATE = app
CONFIG += console
CONFIG -= app_bundle
CONFIG += thread
CONFIG +=C++14

HEADERS += \
    ../../GIS/reosmapitem.h \
    ../../GIS/reosmap.h \
    ../../Reos/reosmodule.h \
    ../../Reos/reosdialogbox.h \
    ../../Reos/reosencodedelement.h \
    ../../Reos/reossettings.h \
    ../../Reos/reosutils.h \
    ../../GIS/hdgismanager.h \
    ../../GIS/hdcrsdialogselection.h \
    ../../GIS/reosmaptool.h \
    ../../GIS/hdvectorlayerpropertiesdialog.h \
    ../../UtilsGeometry/utilsgeometry2d.h \
    ../../Mesher/ReosTin/test.h \
    ../../Mesher/ReosMesh/reosmesh.h \
    ../../Mesher/ReosTin/reostin.h \
    ../../Mesher/ReosMesh/reosmeshgenerator.h \
    ../../Mesher/ReosMesh/reosmesheditor.h \
    ../../Mesher/provider/meshdataprovider.h \
    ../../Mesher/tinEditorUi/reosmapmeshitem.h \
    ../../Mesher/tinEditorUi/reostineditorgraphic.h \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.h \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.h \
    ../../Mesher/ReosMesh/reosvertexzspecifier.h \
    ../../Reos/Form/reosform.h \
    ../../Reos/quantity/reosarea.h \
    ../../Reos/quantity/reosduration.h \
    ../../Reos/quantity/reostime.h \
    ../../Mesher/ReosMesh/vertex.h



SOURCES += \
    ../../GIS/reosmapitem.cpp \
    ../../GIS/reosmap.cpp \
    ../../Reos/reosutils.cpp \
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
    ../../Mesher/ReosMesh/reosmesh.cpp \
    ../../Mesher/ReosMesh/reosmeshgenerator.cpp \
    ../../Mesher/ReosMesh/reosmesheditor.cpp \
    ../../Mesher/ReosTin/reostin.cpp \
    tst_003_meshergeneratorprovidertest.cpp \
    tst_002_01_vertexzvalueediting.cpp \
    tst_002_00_mesheseditortest.cpp \
    tst_001_meshgenerator.cpp \
    tst_004_editablemeshlayertest.cpp \
    tst_005_tineditorgraphictest.cpp \
    tst_006_mapmeshitemtest.cpp \
    ../../Mesher/ReosMesh/reosvertexzspecifier.cpp \
    ../../Reos/Form/reosform.cpp \
    ../../Reos/quantity/reosarea.cpp \
    ../../Reos/quantity/reosduration.cpp \
    ../../Reos/quantity/reostime.cpp \
    ../../Mesher/ReosMesh/vertex.cpp


FORMS += \
    ../../Mesher/tinEditorUi/hdtineditornewdialog.ui \
    ../../Mesher/tinEditorUi/hdtineditoruidialog.ui \
    ../../GIS/hdcrsdialogselection.ui \
    ../../GIS/hdvectorlayerpropertiesdialog.ui


######################### QGIG
INCLUDEPATH +=../../GIS/QGis_app/
include(../../GIS/ConfigQgis.pri); #comment to generate the linguist file
