#-------------------------------------------------
#
# Project created by QtCreator 2019-03-05T14:59:33
#
#-------------------------------------------------

include(../config.pri)

######################### GDAL
include(../GIS/configGDAL.pri);

######################### CGAL
include(configCGAL.pri);

######################### NetCDF
include(../Mesher/configNetCDF.pri);


QT += core gui widgets network xml sql
CONFIG += crypto

TARGET = Mesher
TEMPLATE = app

DEFINES += QT_DEPRECATED_WARNINGS

CONFIG += c++14

SOURCES += \
        main.cpp \
        mainwindow.cpp \
    provider/meshdataprovider.cpp \
    ReosMesh/reosmeshgenerator.cpp \
    tinEditorUi/reosmapmeshitem.cpp \
    ReosTin/reostin.cpp \
    tinEditorUi/hdtineditoruidialog.cpp \
    tinEditorUi/hdtineditornewdialog.cpp \
    ../GIS/hdcrsdialogselection.cpp \
    ../GIS/hdgismanager.cpp \
    ../GIS/reosmaptool.cpp \
    ../GIS/reosmap.cpp \
    ../GIS/reosmapitem.cpp \
    ../GIS/hdvectorlayerpropertiesdialog.cpp \
    ../Reos/reosmessagebox.cpp \
    ../Reos/reosmodule.cpp \
    ../Reos/reosencodedelement.cpp \
    ../Reos/reossettings.cpp \
    ../Reos/reosdialogbox.cpp \
    ../Reos/reosutils.cpp \
    ../UtilsGeometry/utilsgeometry2d.cpp \
    tinEditorUi/reostineditorgraphic.cpp \
    ../Reos/Form/reosform.cpp \
    ReosMesh/reosmesh.cpp \
    ReosMesh/reosmesheditor.cpp \
    ReosMesh/reosvertexzspecifier.cpp \
    ReosMesh/vertex.cpp \
    ../Reos/quantity/reosarea.cpp \
    ../Reos/quantity/reosduration.cpp \
    ../Reos/quantity/reostime.cpp \
    tinEditorUi/reosvertexzspecifierwidget.cpp



HEADERS += \
        mainwindow.h \
    provider/meshdataprovider.h \
    tinEditorUi/hdtineditoruidialog.h \
    tinEditorUi/hdtineditornewdialog.h \
    ReosTin/reostin.h \
    tinEditorUi/reosmapmeshitem.h\
    ../GIS/reosmapitem.h \
    ../GIS/hdcrsdialogselection.h \
    ../GIS/hdgismanager.h \
    ../GIS/reosmap.h \
    ../GIS/reosmaptool.h \
    ../GIS/hdvectorlayerpropertiesdialog.h \
    ../Reos/reosencodedelement.h \
    ../Reos/reosmodule.h \
    ../Reos/reossettings.h \
    ../Reos/reosmessagebox.h \
    ../Reos/reosdialogbox.h \
    ../Reos/reosutils.h \
    ../UtilsGeometry/utilsgeometry2d.h \
    tinEditorUi/reostineditorgraphic.h \
    HdTin/test.h \
    ../Reos/Form/reosform.h\
    ReosMesh/reosmesh.h \
    ReosMesh/reosmesheditor.h \
    ReosMesh/reosmeshgenerator.h \
    ReosMesh/reosvertexzspecifier.h \
    ReosMesh/vertex.h \
    ../Reos/quantity/reosarea.h \
    ../Reos/quantity/reosduration.h \
    ../Reos/quantity/reostime.h \
    tinEditorUi/reosvertexzspecifierwidget.h \
    ../icones/curseur.h


FORMS += \
        mainwindow.ui \
    ../GIS/hdcrsdialogselection.ui \
    ../GIS/hdrasterlayerpropertiesdialog.ui \
    ../GIS/hdvectorlayerpropertiesdialog.ui \
    ../Reos/reosmessagebox.ui \
    tinEditorUi/hdtineditoruidialog.ui \
    tinEditorUi/hdtineditornewdialog.ui \
    tinEditorUi/reosvertexzspecifierwidget.ui


TRANSLATIONS +=../i18n/hydro_en.ts
TRANSLATIONS +=../i18n/hydro_fr.ts


RESOURCES += \
    ../icones/cursor/curseur.qrc \
    ../icones/hdicone.qrc \
    ../icones/symbology/symbology.qrc

######################### QGIG
INCLUDEPATH +=../GIS/QGis_app/
include(../GIS/ConfigQgis.pri); #comment to generate the linguist file

#RC_FILE = ../cmn/icone/mesher.rc






