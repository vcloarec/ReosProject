#-------------------------------------------------
#
# Project created by QtCreator 2019-03-05T14:59:33
#
#-------------------------------------------------

include(../config.pri)

######################### QGIG
INCLUDEPATH +=../GIS/QGis_app/
include(../GIS/ConfigQgis.pri); #comment to generate the linguist file


######################### GDAL
include(../GIS/configGDAL.pri);

######################### CGAL
include(configCGAL.pri);


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
    HdMesh/hdmesh.cpp \
    HdMesh/hdmesheditor.cpp \
    HdMesh/hdmeshgenerator.cpp \
    HdTin/hdtin.cpp \
    HdTin/hdtineditor.cpp \
    tinEditorUi/hdmapmeshitem.cpp \
    tinEditorUi/hdtineditorgraphic.cpp \
    tinEditorUi/hdtineditoruidialog.cpp \
    tinEditorUi/hdtineditornewdialog.cpp \
    ../GIS/hdcrsdialogselection.cpp \
    ../GIS/hdgismanager.cpp \
    ../GIS/hdmap.cpp \
    ../GIS/hdmaptool.cpp \
    ../GIS/hdvectorlayerpropertiesdialog.cpp \
    ../Reos/reosmessagebox.cpp \
    ../Reos/reosmodule.cpp \
    ../Reos/reosencodedelement.cpp \
    ../Reos/reossettings.cpp \
    ../Reos/reosdialogbox.cpp \
    ../UtilsGeometry/utilsgeometry2d.cpp



HEADERS += \
        mainwindow.h \
    provider/meshdataprovider.h \
    tinEditorUi/hdmapmeshitem.h \
    tinEditorUi/hdtineditorgraphic.h \
    tinEditorUi/hdtineditoruidialog.h \
    tinEditorUi/hdtineditornewdialog.h \
    HdTin/hdtin.h \
    HdTin/hdtineditor.h \
    HdMesh/hdmesh.h \
    HdMesh/hdmesheditor.h \
    HdMesh/hdmeshgenerator.h \
    ../GIS/hdcrsdialogselection.h \
    ../GIS/hdgismanager.h \
    ../GIS/hdmap.h \
    ../GIS/hdmaptool.h \
    ../GIS/hdvectorlayerpropertiesdialog.h \
    ../Reos/reosencodedelement.h \
    ../Reos/reosmodule.h \
    ../Reos/reossettings.h \
    ../Reos/reosmessagebox.h \
    ../Reos/reosdialogbox.h \
    ../UtilsGeometry/utilsgeometry2d.h


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
    ../icones/hdicone.qrc

#RC_FILE = ../cmn/icone/mesher.rc


SOURCES_DIR=$$PWD
SOURCES_FILES +=Lekan.pro
SOURCES_FILES +=../cmn/icone/toolbar/*.*
SOURCES_FILES +=../cmn/icone/cursor/*.*
SOURCES_FILES +=../cmn/icone/Qgis/*.*



