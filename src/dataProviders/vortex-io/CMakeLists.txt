# Reos licence GPL version 2
# Copyright (C) 2020 vorteX-io (v.cloarec at vortex-io dot fr)

SET(REOS_VORTEXIO_SOURCES
  reosvortexioprovider.cpp
)

SET(REOS_VORTEXIO_HEADERS
    reosvortexioprovider.cpp
)

ADD_LIBRARY(provider_vortexio MODULE
    ${REOS_VORTEXIO_SOURCES}
    ${REOS_VORTEXIO_HEADERS}
    ${LOGO_RCCS})

TARGET_LINK_LIBRARIES(provider_vortexio
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        reosCore
)

set_target_properties(provider_vortexio
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    )

INCLUDE_DIRECTORIES(
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


INSTALL(TARGETS provider_vortexio
 LIBRARY DESTINATION bin/${REOS_PROVIDERS_DIR})



