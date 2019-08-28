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
    ReosMesh/reosmesh.cpp \
    ReosMesh/reosmesheditor.cpp \
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
    tinEditorUi/reostineditorgraphic.cpp



HEADERS += \
        mainwindow.h \
    provider/meshdataprovider.h \
    tinEditorUi/hdtineditoruidialog.h \
    tinEditorUi/hdtineditornewdialog.h \
    ReosTin/reostin.h \
    ReosMesh/reosmesh.h \
    ReosMesh/reosmesheditor.h \
    ReosMesh/reosmeshgenerator.h \
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
    HdTin/test.h


FORMS += \
        mainwindow.ui \
    ../GIS/hdcrsdialogselection.ui \
    ../GIS/hdrasterlayerpropertiesdialog.ui \
    ../GIS/hdvectorlayerpropertiesdialog.ui \
    ../Reos/reosmessagebox.ui \
    tinEditorUi/hdtineditoruidialog.ui \
    tinEditorUi/hdtineditornewdialog.ui


TRANSLATIONS +=../i18n/hydro_en.ts
TRANSLATIONS +=../i18n/hydro_fr.ts


RESOURCES += \
    ../icones/cursor/curseur.qrc \
    ../icones/hdicone.qrc

######################### QGIG
INCLUDEPATH +=../GIS/QGis_app/
include(../GIS/ConfigQgis.pri); #comment to generate the linguist file

#RC_FILE = ../cmn/icone/mesher.rc





