*#-------------------------------------------------
#
# Project created by QtCreator 2018-10-31T16:19:56
#
#-------------------------------------------------

include(config.pri)

######################### GDAL
include(configGDAL.pri);

######################### QWT
include(QWT.pri);

QT += core gui widgets network xml sql

TARGET = Lekan
TEMPLATE = app
CONFIG += c++14
CONFIG += thread

#CONFIG += console

SOURCES += \
        src/LEKAN/main.cpp \
        src/LEKAN/lekanmainwindow.cpp \
    src/Reos/reoscontrollerthread.cpp \
    ../Reos/reoscore.cpp \
    ../Reos/reosmodule.cpp \
    ../Reos/reosmessagebox.cpp \
    ../GIS/reosmap.cpp \
    ../GIS/reosmaptool.cpp \
    ../GIS/hdcrsdialogselection.cpp \
    ../GIS/hdgismanager.cpp \
    ../GIS/hdvectorlayerpropertiesdialog.cpp \
    ../Reos/reosencodedelement.cpp \
    ../Reos/reossettings.cpp \
    ../GIS/reosmapitem.cpp \
    ../cmn/Mainwindow/demarrage.cpp \
    ../cmn/geometrie2D/FonctionsDiverses.cpp \
    ../ReosProject/UtilsGeometry/utilsgeometry2d.cpp \
    ../cmn/DEM/hddemmanager.cpp \
    ../cmn/Outils/raster/hdfillraster.cpp \
    ../cmn/Outils/raster/hdrasterline.cpp \
    ../cmn/TreeItem/treeitem.cpp \
    ../cmn/classes_diverses/Widget/hdeditionaffichage.cpp \
    src/Tools/raster/reosmemoryraster.cpp \
    ../cmn/Outils/raster/hdwatershedfromraster.cpp \
    ../cmn/Outils/raster/hdrastertools.cpp \
    ../cmn/classes_diverses/Widget/dialogtext.cpp \
    ../cmn/DEM/hddemmanagercontrolpannel.cpp \
    ../cmn/Noyau/apropos.cpp \
    ../hydrologie/Runoff/hlgrunofftransferfunction.cpp \
    ../hydrologie/Watershed/hlgwatershedmanager.cpp \
    ../hydrologie/Watershed/hlgdelineatewatershed.cpp \
    ../hydrologie/Watershed/hlgwatershedtreeitem.cpp \
    ../hydrologie/Watershed/hlgdelineatewatersheddialog.cpp \
    ../hydrologie/Watershed/hlgwatershed.cpp \
    ../hydrologie/Watershed/hlgwatershededitor.cpp \
    ../hydrologie/Watershed/hlgwatershedviewer.cpp \
    ../cmn/classes_diverses/dialogchoixlangue.cpp \
    ../hydrologie/Watershed/hlgwatershedstreamprofile.cpp \
    ../cmn/graphique/hdgraphique.cpp \
    ../hydrologie/Runoff/hlgrunoff.cpp \
    ../hydrologie/Runoff/hlgrunoffmanager.cpp \
    ../hydrologie/Runoff/hlgrunoffmanagerwidget.cpp \
    ../hydrologie/Runoff/hlgrunoffeditordialog.cpp \
    ../hydrologie/Pluie/HlgPluie.cpp \
    ../hydrologie/Pluie/HlgSerie.cpp \
    ../hydrologie/Pluie/HdPeriodeRetour.cpp \
    ../hydrologie/Pluie/hlgidf.cpp \
    ../hydrologie/Pluie/HlgCoefMontana.cpp \
    ../hydrologie/Pluie/GestionnairePluie.cpp \
    ../hydrologie/InterfacePluies/dialogcaracterisation.cpp \
    ../hydrologie/InterfacePluies/editeuridf.cpp \
    ../hydrologie/InterfacePluies/hlgediteurpluiechicago.cpp \
    ../hydrologie/InterfacePluies/hlgediteurpluiedoubletriangle.cpp \
    ../hydrologie/InterfacePluies/hlgediteursaisiepluie.cpp \
    ../hydrologie/InterfacePluies/hlginterfacepluiefactory.cpp \
    ../hydrologie/InterfacePluies/interfacedonneepluie.cpp \
    ../hydrologie/hlgpromotewidget.cpp \
    ../hydrologie/hlgrainfallitem.cpp \
    ../cmn/Fichier/hdenregistrement.cpp \
    ../cmn/Fichier/dialogimporttxt.cpp \
    ../hydrologie/InterfacePluies/hlgrainfallmanager.cpp \
    ../hydrologie/InterfacePluies/hlgrainfallwatersheddialog.cpp \
    ../hydrologie/Runoff/hlgrunoffviewer.cpp \
    ../hydrologie/Watershed/hlgconcentrationtime.cpp \
    ../hydrologie/Watershed/hlgconcentrationtimedialog.cpp \
    ../hydrologie/Hydrograph/hlghydrograph.cpp \
    ../cmn/TimeFunction/hdtimefunction.cpp \
    ../hydrologie/Hydrograph/hlghydrographmanagerdialog.cpp \
    ../hydrologie/Hydrograph/hlghydrographmanager.cpp \
    ../Reos/reosversion.cpp \
    ../Reos/reosdocumentation.cpp \
    ../SIG/hdpolylinemaptool.cpp \
    ../ReosProject/Reos/reosdialogbox.cpp \
    ../ReosProject/Reos/Form/reosform.cpp \
    ../ReosProject/Reos/quantity/reostime.cpp \
    ../ReosProject/Reos/quantity/reosduration.cpp \
    ../ReosProject/Reos/quantity/reosarea.cpp

HEADERS += \
        src/LEKAN/lekanmainwindow.h \
        ../ReosProject/GIS/reosmapitem.h \
        ../ReosProject/GIS/reosmaptool.h \
        ../hydrologie/Runoff/hlgrunofftransferfunction.h \
        ../ReosProject/Reos/reosmodule.h \
        ../ReosProject/Reos/reosmessagebox.h \
        ../ReosProject/GIS/hdcrsdialogselection.h \
        ../ReosProject/GIS/hdgismanager.h \
        ../ReosProject/GIS/hdvectorlayerpropertiesdialog.h \
        ../ReosProject/GIS/reosmap.h \
        ../ReosProject/Reos/reosencodedelement.h \
        ../ReosProject/Reos/reossettings.h \
        ../Reos/reoscontrollerthread.h \
        ../Reos/reoscore.h \
        ../cmn/Mainwindow/demarrage.h \
        ../cmn/TreeItem/treeitem.h \
        ../cmn/classes_diverses/Widget/hdeditionaffichage.h \
        src/Tools/raster/reosmemoryraster.h \
        ../cmn/geometrie2D/FonctionsDiverses.h \
        ../ReosProject/UtilsGeometry/utilsgeometry2d.h \
        ../cmn/DEM/hddemmanager.h \
        ../cmn/Outils/raster/hdfillraster.h \
        ../cmn/Outils/raster/hdrasterline.h \
        ../cmn/classes_diverses/Widget/dialogtext.h \
        ../cmn/Outils/raster/hdwatershedfromraster.h \
        ../cmn/Outils/raster/hdrastertools.h \
        ../hydrologie/Watershed/hlgwatershedmanager.h \
        ../hydrologie/Watershed/hlgwatershedtreeitem.h \
        ../hydrologie/Watershed/hlgdelineatewatersheddialog.h \
        ../hydrologie/Watershed/hlgwatershed.h \
        ../hydrologie/Watershed/hlgdelineatewatershed.h \
        ../hydrologie/Watershed/hlgwatershededitor.h \
        ../hydrologie/Watershed/hlgwatershedviewer.h \
        ../cmn/DEM/hddemmanagercontrolpannel.h \
        ../cmn/Noyau/apropos.h \
        ../cmn/classes_diverses/dialogchoixlangue.h \
        ../hydrologie/Watershed/hlgwatershedstreamprofile.h \
        ../cmn/graphique/hdgraphique.h \
    ../hydrologie/Runoff/hlgrunoff.h \
    ../hydrologie/Runoff/hlgrunoffmanager.h \
    ../hydrologie/Runoff/hlgrunoffmanagerwidget.h \
    ../hydrologie/Runoff/hlgrunoffeditordialog.h \
    ../hydrologie/Pluie/GestionnairePluie.h \
    ../hydrologie/Pluie/HdPeriodeRetour.h \
    ../hydrologie/Pluie/HlgCoefMontana.h \
    ../hydrologie/Pluie/hlgidf.h \
    ../hydrologie/Pluie/HlgPluie.h \
    ../hydrologie/Pluie/HlgSerie.h \
    ../hydrologie/InterfacePluies/dialogcaracterisation.h \
    ../hydrologie/InterfacePluies/editeuridf.h \
    ../hydrologie/InterfacePluies/hlgediteurpluiechicago.h \
    ../hydrologie/InterfacePluies/hlgediteurpluiedoubletriangle.h \
    ../hydrologie/InterfacePluies/hlgediteursaisiepluie.h \
    ../hydrologie/InterfacePluies/hlginterfacepluiefactory.h \
    ../hydrologie/InterfacePluies/interfacedonneepluie.h \
    ../hydrologie/hlgitem.h \
    ../hydrologie/hlgpromotewidget.h \
    ../hydrologie/hlgrainfallitem.h \
    ../cmn/Fichier/hdenregistrement.h \
    ../cmn/Fichier/dialogimporttxt.h \
    ../hydrologie/InterfacePluies/hlgrainfallmanager.h \
    ../hydrologie/InterfacePluies/hlgrainfallwatersheddialog.h \
    ../hydrologie/Runoff/hlgrunoffviewer.h \
    ../hydrologie/Watershed/hlgconcentrationtime.h \
    ../hydrologie/Watershed/hlgconcentrationtimedialog.h \
    ../hydrologie/Hydrograph/hlghydrograph.h \
    ../cmn/TimeFunction/hdtimefunction.h \
    ../hydrologie/Hydrograph/hlghydrographmanagerdialog.h \
    ../hydrologie/Hydrograph/hlghydrographmanager.h \
    ../Reos/reosversion.h \
    ../Reos/reosdocumentation.h \
    ../SIG/hdpolylinemaptool.h \
    ../ReosProject/Reos/reosdialogbox.h \
    ../ReosProject/Reos/Form/reosform.h \
    ../ReosProject/Reos/quantity/reostime.h \
    ../ReosProject/Reos/quantity/reosduration.h \
    ../ReosProject/Reos/quantity/reosarea.h

FORMS += \
        lekanmainwindow.ui \
    ../ReosProject/Reos/reosmessagebox.ui \
    ../Reos/reoscontrollerthread.ui \
    ../cmn/Mainwindow/demarrage.ui \
    ../cmn/classes_diverses/Widget/dialogtext.ui \
    ../ReosProject/GIS/hdcrsdialogselection.ui \
    ../ReosProject/GIS/hdvectorlayerpropertiesdialog.ui \
    ../hydrologie/Watershed/hlgdelineatewatersheddialog.ui \
    ../hydrologie/Watershed/hlgwatershededitor.ui \
    ../hydrologie/Watershed/hlgwatershedviewer.ui \
    ../cmn/DEM/hddemmanagercontrolpannel.ui \
    ../cmn/Noyau/apropos.ui \
    ../cmn/classes_diverses/dialogchoixlangue.ui \
    ../hydrologie/Watershed/hlgwatershedstreamprofile.ui \
    ../hydrologie/Runoff/hlgrunoffmanagerwidget.ui \
    ../hydrologie/Runoff/hlgrunoffeditordialog.ui \
    ../hydrologie/InterfacePluies/dialogcaracterisation.ui \
    ../hydrologie/InterfacePluies/editeuridf.ui \
    ../hydrologie/InterfacePluies/hlgediteurpluiechicago.ui \
    ../hydrologie/InterfacePluies/hlgediteurpluieDoubleTriangle.ui \
    ../hydrologie/InterfacePluies/hlgediteursaisiepluie.ui \
    ../hydrologie/InterfacePluies/interfacedonneepluie.ui \
    ../hydrologie/InterfacePluies/interfacepluies.ui \
    ../cmn/Fichier/dialogimporttxt.ui \
    ../hydrologie/InterfacePluies/hlgrainfallwatersheddialog.ui \
    ../hydrologie/Runoff/hlgrunoffviewer.ui \
    ../hydrologie/Watershed/hlgconcentrationtimedialog.ui \
    ../hydrologie/Hydrograph/hlghydrographmanagerdialog.ui

#comment to generate the linguist file
INCLUDEPATH +=src/GIS/QGis_app/
include(ConfigQgis.pri);

TRANSLATIONS +=i18n/hydro_en.ts
TRANSLATIONS +=i18n/hydro_fr.ts

RESOURCES += \
    icones/hdicone.qrc \
    icones/icone.qrc

RC_FILE = src/cmn/icone/lekan.rc

SOURCES_DIR=$$PWD
SOURCES_FILES +=Lekan.pro
SOURCES_FILES +=src/cmn/icone/toolbar/*.*
SOURCES_FILES +=src/cmn/icone/cursor/*.*
SOURCES_FILES +=src/cmn/icone/Qgis/*.*

#uncomment to copy the source file
include(PRI/exportSource.pri)


DISTFILES += \
    allLicences.txt \
    src/hydrologie/InterfacePluies/FormInterfacePluies.pri
