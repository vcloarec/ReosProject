# Reos licence GPL version 2
# Copyright (C) 2023 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_MF_API_SOURCES
    reosmeteofranceapi.cpp
    reosmeteofrancearomeprovider.cpp
    reosmeteofrancearomeapiwidget.cpp
)

SET(REOS_MF_API_HEADERS
 reosmeteofranceapi.h
 reosmeteofrancearomeprovider.h
 reosmeteofrancearomeapiwidget.h
)

#SET(LOGO_RCCS ${CMAKE_SOURCE_DIR}/images/providers/hub-eau/hub-eau-images.qrc)


ADD_LIBRARY(provider_mf_api MODULE
    ${REOS_MF_API_SOURCES}
    ${REOS_MF_API_HEADERS})

TARGET_LINK_LIBRARIES(provider_mf_api
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

set_target_properties(provider_mf_api
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
    ${CMAKE_SOURCE_DIR}/src/core/network
    )


INSTALL(TARGETS provider_mf_api
 LIBRARY DESTINATION bin/${REOS_PROVIDERS_DIR})



