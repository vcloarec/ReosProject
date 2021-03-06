######################################################################
# Automatically generated by qmake (3.1) Fri May 28 21:32:18 2021
######################################################################

TEMPLATE = app
TARGET = reos_ts

# The following define makes your compiler warn you if you use any
# feature of Qt which has been marked as deprecated (the exact warnings
# depend on your compiler). Please consult the documentation of the
# deprecated API in order to know how to port your code away from it.
DEFINES += QT_DEPRECATED_WARNINGS

# You can also make your code fail to compile if you use deprecated APIs.
# In order to do so, uncomment the following line.
# You can also select to disable deprecated APIs only up to a certain version of Qt.
#DEFINES += QT_DISABLE_DEPRECATED_BEFORE=0x060000    # disables all the APIs deprecated before Qt 6.0.0

# Input
HEADERS += src/core/reosapplication.h \
           src/core/reoscore.h \
           src/core/reosdocumentation.h \
           src/core/reosencodedelement.h \
           src/core/reosexception.h \
           src/core/reosmemoryraster.h \
           src/core/reosmodule.h \
           src/core/reosparameter.h \
           src/core/reossettings.h \
           src/core/reosversion.h \
           src/gui/reosaboutwidget.h \
           src/gui/reosactionwidget.h \
           src/gui/reosgui.h \
           src/gui/reoslanguageselectionwidget.h \
           src/gui/reosmainwindow.h \
           src/gui/reosmenupopulator.h \
           src/gui/reosmessagebox.h \
           src/gui/reosprocesscontroler.h \
           src/gui/reosstartingwidget.h \
           src/gui/reosversionmessagebox.h \
           src/lekan/lekanmainwindow.h \
           src/core/data/reosdataobject.h \
           src/core/data/reostextfiledata.h \
           src/core/data/reostimeserie.h \
           src/core/GIS/reosdigitalelevationmodel.h \
           src/core/GIS/reosexporttovectorfile.h \
           src/core/GIS/reosgisengine.h \
           src/core/GIS/reosmapextent.h \
           src/core/process/reosprocess.h \
           src/core/quantity/reosarea.h \
           src/core/quantity/reosduration.h \
           src/core/quantity/reostime.h \
           src/core/rainfall/reosidfcurves.h \
           src/core/rainfall/reosrainfallintensityduration.h \
           src/core/rainfall/reosrainfallitem.h \
           src/core/rainfall/reosrainfallmodel.h \
           src/core/rainfall/reosrainfallregistery.h \
           src/core/rainfall/reossyntheticrainfall.h \
           src/core/raster/reosrastercompressed.h \
           src/core/raster/reosrasterfilling.h \
           src/core/raster/reosrasterline.h \
           src/core/raster/reosrastertrace.h \
           src/core/raster/reosrasterwatershed.h \
           src/core/utils/reosgeometryutils.h \
           src/core/watershed/reosconcentrationtimecalculation.h \
           src/core/watershed/reosmeteorologicmodel.h \
           src/core/watershed/reosrunoffmodel.h \
           src/core/watershed/reostransferfunction.h \
           src/core/watershed/reoswatershed.h \
           src/core/watershed/reoswatersheddelineating.h \
           src/core/watershed/reoswatershedmodule.h \
           src/core/watershed/reoswatershedtree.h \
           src/gui/chart/reoschart.h \
           src/gui/chart/reoschartview.h \
           src/gui/chart/reoschartwidget.h \
           src/gui/chart/reoseditableprofile.h \
           src/gui/chart/reosplotidfcurve.h \
           src/gui/chart/reosplottimeconstantinterval.h \
           src/gui/chart/reosplotwidget.h \
           src/gui/data/reosimportfromtextfile.h \
           src/gui/form/reosformwidget.h \
           src/gui/form/reosparameterwidget.h \
           src/gui/GIS/reosdigitalelevationmodelcombobox.h \
           src/gui/GIS/reosgislayerswidget.h \
           src/gui/GIS/reosmap.h \
           src/gui/GIS/reosmapitem.h \
           src/gui/GIS/reosmaptool.h \
           src/gui/rainfall/reosintensitydurationselectedcurvewidget.h \
           src/gui/rainfall/reosrainfalldataform.h \
           src/gui/rainfall/reosrainfallintensitydurationwidget.h \
           src/gui/rainfall/reosrainfallmanager.h \
           src/gui/watershed/reosconcentrationtimewidget.h \
           src/gui/watershed/reosdelineatingwatershedwidget.h \
           src/gui/watershed/reosexportwatershedtovectordialog.h \
           src/gui/watershed/reoslongitudinalprofilewidget.h \
           src/gui/watershed/reosmeteorologicmodelwidget.h \
           src/gui/watershed/reosrunoffhydrographwidget.h \
           src/gui/watershed/reosrunoffmanager.h \
           src/gui/watershed/reoswatershedwidget.h \
           src/core/GIS/private/reosdigitalelevationmodel_p.h \
           src/gui/chart/private/reosidfplot_p.h \
           src/gui/chart/private/reosplot_p.h \
           src/gui/chart/private/reosplotpicker_p.h \
           src/gui/chart/private/reosprofileplot_p.h \
           src/gui/GIS/private/reoslayertreecontextmenuprovider_p.h \
           src/gui/GIS/private/reoslayertreeviewdemindicator.h \
           src/gui/GIS/private/reosmappolygon_p.h \
           src/gui/GIS/private/reosmaptool_p.h
FORMS += src/gui/reosmessagebox.ui \
         src/lekan/lekanmainwindow.ui \
         src/ui/reosaboutwidget.ui \
         src/ui/reoschartwidget.ui \
         src/ui/reosconcentrationtimewidget.ui \
         src/ui/reosdelineatingwatershedwidget.ui \
         src/ui/reosexportwatershedtovectordialog.ui \
         src/ui/reosimportfromtextfile.ui \
         src/ui/reosintensitydurationselectedcurvewidget.ui \
         src/ui/reoslanguageselectionwidget.ui \
         src/ui/reoslongitudinalprofilewidget.ui \
         src/ui/reosmainwindow.ui \
         src/ui/reosmessagebox.ui \
         src/ui/reosmeteorologicmodelwidget.ui \
         src/ui/reosprocesscontroler.ui \
         src/ui/reosrainfallmanager.ui \
         src/ui/reosrunoffhydrographwidget.ui \
         src/ui/reosrunoffmanager.ui \
         src/ui/reosstartingwidget.ui \
         src/ui/reoswatershedwidget.ui
SOURCES += src/core/reosapplication.cpp \
           src/core/reosdocumentation.cpp \
           src/core/reosencodedelement.cpp \
           src/core/reosexception.cpp \
           src/core/reosmemoryraster.cpp \
           src/core/reosmodule.cpp \
           src/core/reosparameter.cpp \
           src/core/reossettings.cpp \
           src/core/reosversion.cpp \
           src/gui/reosaboutwidget.cpp \
           src/gui/reosactionwidget.cpp \
           src/gui/reoslanguageselectionwidget.cpp \
           src/gui/reosmainwindow.cpp \
           src/gui/reosmenupopulator.cpp \
           src/gui/reosmessagebox.cpp \
           src/gui/reosprocesscontroler.cpp \
           src/gui/reosstartingwidget.cpp \
           src/gui/reosversionmessagebox.cpp \
           src/lekan/lekanmainwindow.cpp \
           src/lekan/main.cpp \
           src/core/data/reosdataobject.cpp \
           src/core/data/reostextfiledata.cpp \
           src/core/data/reostimeserie.cpp \
           src/core/GIS/reosdigitalelevationmodel.cpp \
           src/core/GIS/reosexporttovectorfile.cpp \
           src/core/GIS/reosgisengine.cpp \
           src/core/GIS/reosmapextent.cpp \
           src/core/process/reosprocess.cpp \
           src/core/quantity/reosarea.cpp \
           src/core/quantity/reosduration.cpp \
           src/core/quantity/reosrainfalldata.cpp \
           src/core/quantity/reostime.cpp \
           src/core/rainfall/reosidfcurves.cpp \
           src/core/rainfall/reosrainfallintensityduration.cpp \
           src/core/rainfall/reosrainfallitem.cpp \
           src/core/rainfall/reosrainfallmodel.cpp \
           src/core/rainfall/reosrainfallregistery.cpp \
           src/core/rainfall/reossyntheticrainfall.cpp \
           src/core/raster/reosrastercompressed.cpp \
           src/core/raster/reosrasterfilling.cpp \
           src/core/raster/reosrasterline.cpp \
           src/core/raster/reosrastertrace.cpp \
           src/core/raster/reosrasterwatershed.cpp \
           src/core/utils/reosgeometryutils.cpp \
           src/core/watershed/reosconcentrationtimecalculation.cpp \
           src/core/watershed/reosmeteorologicmodel.cpp \
           src/core/watershed/reosrunoffmodel.cpp \
           src/core/watershed/reostransferfunction.cpp \
           src/core/watershed/reoswatershed.cpp \
           src/core/watershed/reoswatersheddelineating.cpp \
           src/core/watershed/reoswatershedmodule.cpp \
           src/core/watershed/reoswatershedtree.cpp \
           src/gui/chart/reoschart.cpp \
           src/gui/chart/reoschartview.cpp \
           src/gui/chart/reoschartwidget.cpp \
           src/gui/chart/reoseditableprofile.cpp \
           src/gui/chart/reosplotidfcurve.cpp \
           src/gui/chart/reosplottimeconstantinterval.cpp \
           src/gui/chart/reosplotwidget.cpp \
           src/gui/data/reosimportfromtextfile.cpp \
           src/gui/form/reosformwidget.cpp \
           src/gui/form/reosparameterwidget.cpp \
           src/gui/GIS/reosdigitalelevationmodelcombobox.cpp \
           src/gui/GIS/reosgislayerswidget.cpp \
           src/gui/GIS/reosmap.cpp \
           src/gui/GIS/reosmapitem.cpp \
           src/gui/GIS/reosmaptool.cpp \
           src/gui/rainfall/reosintensitydurationselectedcurvewidget.cpp \
           src/gui/rainfall/reosrainfalldataform.cpp \
           src/gui/rainfall/reosrainfallintensitydurationwidget.cpp \
           src/gui/rainfall/reosrainfallmanager.cpp \
           src/gui/watershed/reosconcentrationtimewidget.cpp \
           src/gui/watershed/reosdelineatingwatershedwidget.cpp \
           src/gui/watershed/reosexportwatershedtovectordialog.cpp \
           src/gui/watershed/reoslongitudinalprofilewidget.cpp \
           src/gui/watershed/reosmeteorologicmodelwidget.cpp \
           src/gui/watershed/reosrunoffhydrographwidget.cpp \
           src/gui/watershed/reosrunoffmanager.cpp \
           src/gui/watershed/reoswatershedwidget.cpp \
           src/core/GIS/private/reosdigitalelevationmodel_p.cpp \
           src/gui/chart/private/reosidfplot_p.cpp \
           src/gui/chart/private/reosplot_p.cpp \
           src/gui/chart/private/reosplotpicker_p.cpp \
           src/gui/chart/private/reosprofileplot_p.cpp \
           src/gui/GIS/private/reoslayertreecontextmenuprovider_p.cpp \
           src/gui/GIS/private/reoslayertreeviewdemindicator.cpp \
           src/gui/GIS/private/reosmappolygon_p.cpp \
           src/gui/GIS/private/reosmaptool_p.cpp
TRANSLATIONS += i18n/reos_en.ts \
                i18n/reos_es.ts \
                i18n/reos_fr.ts \
                i18n/reos_ne.ts \
                i18n/reos_pt.ts \
                i18n/reos_zh_TW.ts
TR_EXCLUDE = /usr/include/x86_64-linux-gnu/qt5/*
