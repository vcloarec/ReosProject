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

    chart/reoschartview.cpp
    chart/reoschart.cpp
    chart/reoschartwidget.cpp
    chart/reosplotwidget.cpp
    chart/reoseditableprofile.cpp
    chart/private/reosprofileplot_p.cpp
    chart/private/reosplot_p.cpp
    chart/private/reosplotpicker_p.cpp
)

SET(REOS_GUI_HEADERS
    reosaboutwidget.h
    reoslanguageselectionwidget.h
    reosmainwindow.h
    reosmessagebox.h
    reosstartingwidget.h
    reosversionmessagebox.h
    reosprocesscontroler.h

    GIS/reosdigitalelevationmodelcombobox.h
    GIS/reosmap.h
    GIS/reosmapitem.h
    GIS/reosmaptool.h
    GIS/reosgislayerswidget.h

    watershed/reosdelineatingwatershedwidget.h
    watershed/reoswatershedwidget.h
    watershed/reoslongitudinalprofilewidget.h

    chart/reoschartview.h
    chart/reoschart.h
    chart/reoschartwidget.h
    chart/reosplotwidget.h
    chart/reoseditableprofile.h
)

SET(REOS_GUI_HEADERS_PRIVATE
    GIS/private/reosmappolygon_p.h
    GIS/private/reosmaptool_p.h
    GIS/private/reoslayertreecontextmenuprovider_p.h
    GIS/private/reoslayertreeviewdemindicator.h

    chart/private/reosplot_p.h
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
        ${QGIS_LIBS}
        ${QWT_LIB}
        reosCore
)

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
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed

    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/GIS/private
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/chart/private
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
