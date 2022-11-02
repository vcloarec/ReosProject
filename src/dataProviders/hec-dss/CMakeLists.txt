# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_HEC_DSS_SOURCES
    reosdssutils.cpp
    reosdssfile.cpp
)

SET(REOS_HEC_DSS_HEADERS
    reosdssutils.h
    reosdssfile.h
)

SET(REOS_PROVIDER_HEC_DSS_SOURCES
    reosdssprovider.cpp
)

SET(REOS_PROVIDER_HEC_DSS_HEADERS
    reosdssprovider.h
    reosdsswidget.h
)

#SET(LOGO_RCCS ${CMAKE_SOURCE_DIR}/images/providers/delft-fews/delft-fews-images.qrc)

ADD_LIBRARY(reosHecDss SHARED
    ${REOS_HEC_DSS_SOURCES}
    ${REOS_HEC_DSS_HEADERS}
)

TARGET_LINK_LIBRARIES(reosHecDss
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
        ${HEC_DSS_ALL_STATIC_LIB}
)

ADD_LIBRARY(provider_HEC_DSS MODULE
    ${REOS_PROVIDER_HEC_DSS_SOURCES}
    ${REOS_PROVIDER_HEC_DSS_HEADERS}
)

TARGET_LINK_LIBRARIES(provider_HEC_DSS
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
        reosHecDss
)

set_target_properties(reosHecDss
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    )

set_target_properties(provider_HEC_DSS
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
    ${HEC_DSS_INCLUDE}
    ${REOS_HEC_DSS_HEADERS}
    )

INSTALL(TARGETS reosHecDss
  RUNTIME DESTINATION ${REOS_PROVIDERS_DIR}
  LIBRARY DESTINATION ${REOS_PROVIDERS_DIR})

INSTALL(TARGETS provider_HEC_DSS 
  RUNTIME DESTINATION ${REOS_PROVIDERS_DIR}
  LIBRARY DESTINATION ${REOS_PROVIDERS_DIR})



