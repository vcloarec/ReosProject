# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_DELFT_FEWS_SOURCES
  reosdelftfewswidget.cpp
  reosdelftfewsxmlprovider.cpp
)

SET(REOS_DELFT_FEWS_HEADERS
    reosdelftfewswidget.h
    reosdelftfewsxmlprovider.h
)

SET(LOGO_RCCS ${CMAKE_SOURCE_DIR}/images/providers/delft-fews/delft-fews-images.qrc)

ADD_LIBRARY(provider_delft_FEWS MODULE
    ${REOS_DELFT_FEWS_SOURCES}
    ${REOS_DELFT_FEWS_HEADERS}
    ${LOGO_RCCS}
)


TARGET_LINK_LIBRARIES(provider_delft_FEWS
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


set_target_properties(provider_delft_FEWS
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/${REOS_PROVIDERS_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/${REOS_PROVIDERS_DIR}
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


IF(WIN32)
  INSTALL(TARGETS provider_delft_FEWS RUNTIME DESTINATION ${REOS_PROVIDERS_DIR})
ELSE(WIN32)
  INSTALL(TARGETS provider_delft_FEWS LIBRARY DESTINATION ${REOS_PROVIDERS_DIR})
ENDIF(WIN32)


