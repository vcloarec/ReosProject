#-------------------------------------------------
#
# Project created by QtCreator 2019-03-05T14:59:33
#
#-------------------------------------------------
win32 {
    include(../../../QGIS_3_MSVC.pri);
    include(../../../GDAL_OSGEO.pri);
    include(../../QWT613_MSVC2017_x64.pri)
}

linux {
    include(../../../QGIS_LINUX.pri);
    include(../../../GDAL_LINUX.pri);
    include(../../QWT_LINUX.pri);
}

QT       += xml

QT       += core gui
QT += network
QT += sql

CONFIG += crypto

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets

TARGET = Mesher
TEMPLATE = app

# The following define makes your compiler emit warnings if you use
# any feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

CONFIG += c++11

SOURCES += \
        main.cpp \
        mainwindow.cpp \
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
    ../UtilsGeometry/utilsgeometry2d.cpp \
    provider/meshdataprovider.cpp \
    provider/hdmeshgenerator.cpp \
    provider/hdmesheditor.cpp \
    tinEditorUi/hdmapmeshitem.cpp \
    tinEditorUi/hdtineditorgraphic.cpp \
    tinEditorUi/hdtineditoruidialog.cpp \
    tinEditorUi/hdtineditornewdialog.cpp

HEADERS += \
        mainwindow.h \
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
    ../UtilsGeometry/utilsgeometry2d.h \
    provider/meshdataprovider.h \
    provider/hdmeshgenerator.h \
    proider/hdmesheditor.h \
    tinEditorUi/hdmapmeshitem.h \
    tinEditorUi/hdtineditorgraphic.h \
    tinEditorUi/hdtineditoruidialog.h \
    tinEditorUi/hdtineditornewdialog.h

FORMS += \
        mainwindow.ui \
    ../GIS/hdcrsdialogselection.ui \
    ../GIS/hdrasterlayerpropertiesdialog.ui \
    ../GIS/hdvectorlayerpropertiesdialog.ui \
    ../Reos/reosmessagebox.ui \
    tinEditorUi/hdtineditoruidialog.ui \
    tinEditorUi/hdtineditornewdialog.ui




TRANSLATIONS +=../cmn/hydro_en.ts
TRANSLATIONS +=../cmn/hydro_fr.ts


RESOURCES += \
    ../icones/hdicone.qrc

#RC_FILE = ../cmn/icone/mesher.rc


SOURCES_DIR=$$PWD
SOURCES_FILES +=Lekan.pro
SOURCES_FILES +=../cmn/icone/toolbar/*.*
SOURCES_FILES +=../cmn/icone/cursor/*.*
SOURCES_FILES +=../cmn/icone/Qgis/*.*

#uncomment to copy the source file
#include(../PRI/exportSource.pri)

INCLUDEPATH +=../GIS/QGis_app/
#comment to generate the linguist file
include(../GIS/ConfigQgis.pri);

