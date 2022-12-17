# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_GRIB_SOURCES
    reosgribprovider.cpp
    reosgribprecipitationwidget.cpp
)

SET(REOS_GRIB_HEADERS
    reosgribprovider.h
    reosgribprecipitationwidget.h
)


ADD_LIBRARY(provider_grib MODULE
    ${REOS_GRIB_SOURCES}
    ${REOS_GRIB_HEADERS}
)


TARGET_LINK_LIBRARIES(provider_grib
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        reosGui
        reosCore
)


set_target_properties(provider_grib
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    )

INCLUDE_DIRECTORIES(
    ${CMAKE_BINARY_DIR}/src/ui
    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/form
    ${CMAKE_SOURCE_DIR}/src/gui/data
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/watershed
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    )



INSTALL(TARGETS provider_grib
  LIBRARY DESTINATION bin/${REOS_PROVIDERS_DIR})



