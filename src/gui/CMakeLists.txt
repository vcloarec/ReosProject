# Reos GPL v2.0
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_GUI_SOURCES
    reosaboutwidget.cpp
    reoslanguageselectionwidget.cpp
    reosmainwindow.cpp
    reosmessagebox.cpp
    reosstartingwidget.cpp
    reosversionmessagebox.cpp
    reosprocesscontroler.cpp
    reosmenupopulator.cpp
    reosactionwidget.cpp


    GIS/reosdigitalelevationmodelcombobox.cpp
    GIS/reosmap.cpp
    GIS/reosmapitem.cpp
    GIS/reosmaptool.cpp
    GIS/reosgislayerswidget.cpp
    GIS/private/reosmappolygon_p.cpp
    GIS/private/reosmaptool_p.cpp
    GIS/private/reoslayertreecontextmenuprovider_p.cpp
    GIS/private/reoslayertreeviewdemindicator.cpp

    watershed/reosdelineatingwatershedwidget.cpp
    watershed/reoswatershedwidget.cpp
    watershed/reoslongitudinalprofilewidget.cpp
    watershed/reosconcentrationtimewidget.cpp
    watershed/reosmeteorologicmodelwidget.cpp

    rainfall/reosrainfallmanager.cpp
    rainfall/reosrainfallintensitydurationwidget.cpp
    rainfall/reosintensitydurationselectedcurvewidget.cpp

    chart/reoschartview.cpp
    chart/reoschart.cpp
    chart/reoschartwidget.cpp
    chart/reosplotwidget.cpp
    chart/reoseditableprofile.cpp
    chart/reosplottimeconstantinterval.cpp
    chart/reosplotidfcurve.cpp
    chart/private/reosprofileplot_p.cpp
    chart/private/reosplot_p.cpp
    chart/private/reosplotpicker_p.cpp
    chart/private/reosidfplot_p.cpp

    form/reosparameterwidget.cpp
    form/reosformwidget.cpp

    data/reosimportfromtextfile.cpp
)

SET(REOS_GUI_HEADERS
    reosgui.h
    reosaboutwidget.h
    reoslanguageselectionwidget.h
    reosmainwindow.h
    reosmessagebox.h
    reosstartingwidget.h
    reosversionmessagebox.h
    reosprocesscontroler.h
    reosmenupopulator.h
    reosactionwidget.h

    GIS/reosdigitalelevationmodelcombobox.h
    GIS/reosmap.h
    GIS/reosmapitem.h
    GIS/reosmaptool.h
    GIS/reosgislayerswidget.h

    watershed/reosdelineatingwatershedwidget.h
    watershed/reoswatershedwidget.h
    watershed/reoslongitudinalprofilewidget.h
    watershed/reosconcentrationtimewidget.h
    watershed/reosmeteorologicmodelwidget.h

    rainfall/reosrainfallmanager.h
    rainfall/reosrainfallintensitydurationwidget.h
    rainfall/reosintensitydurationselectedcurvewidget.h

    chart/reoschartview.h
    chart/reoschart.h
    chart/reoschartwidget.h
    chart/reosplotwidget.h
    chart/reoseditableprofile.h
    chart/reosplottimeconstantinterval.h
    chart/reosplotidfcurve.h

    form/reosparameterwidget.h
    form/reosformwidget.h

    data/reosimportfromtextfile.h
)

SET(REOS_GUI_HEADERS_PRIVATE
    GIS/private/reosmappolygon_p.h
    GIS/private/reosmaptool_p.h
    GIS/private/reoslayertreecontextmenuprovider_p.h
    GIS/private/reoslayertreeviewdemindicator.h

    chart/private/reosplot_p.h
    chart/private/reosidfplot_p.h
    )

ADD_LIBRARY(reosGui SHARED
    ${REOS_GUI_HEADERS}
    #${REOS_GUI_HEADERS_PRIVATE}
    ${REOS_GUI_SOURCES}
  )

TARGET_LINK_LIBRARIES(reosGui PUBLIC
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        ${Qt5Charts_LIBRARIES}
        ${Qt5WebKitWidgets_LIBRARIES}
        ${QGIS_LIBS}
        ${QWT_LIB}
        reosCore
)

IF (MSVC)
  add_compile_definitions(_USE_MATH_DEFINES)
  add_compile_definitions(QWT_DLL)
ENDIF (MSVC)

IF(WITH_QTWEBKIT)
  TARGET_LINK_LIBRARIES(reosGui PUBLIC
        ${Qt5WebKitWidgets_LIBRARIES}
  )
  add_compile_definitions(WITH_QTWEBKIT)
ENDIF(WITH_QTWEBKIT)

IF(WIN32)
  INSTALL(TARGETS reosGui RUNTIME DESTINATION lib)
ELSE(WIN32)
  INSTALL(TARGETS reosGui LIBRARY DESTINATION lib)
ENDIF(WIN32)

INCLUDE_DIRECTORIES(
    ${CMAKE_BINARY_DIR}/src/ui

    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/data

    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/data
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/GIS/private
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/chart/private
    ${CMAKE_SOURCE_DIR}/src/gui/form
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${QGIS_INCLUDE_DIR}
    ${QSCISCINTILLA_INCLUDE_DIR}
    ${QWT_INCLUDE}
)


#TARGET_INCLUDE_DIRECTORIES(reosGui
#    PUBLIC
#    $<INSTALL_INTERFACE:include>
#    PRIVATE
#    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
#    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>
#  )



# INSTALL HEADER
INSTALL(FILES ${REOS_GUI_HEADERS} DESTINATION include)
