# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_SYSYEM_SOURCES
main.cpp
 ${CMAKE_SOURCE_DIR}/src/lekan/lekanmainwindow.cpp
)

SET(REOS_SYSTEM_HEADERS
    ${CMAKE_SOURCE_DIR}/src/lekan/lekanmainwindow.h
)

ADD_EXECUTABLE(${SYSTEM_APP_NAME}
    ${REOS_SYSYEM_SOURCES}
    ${REOS_SYSTEM_HEADERS})

set_target_properties(${SYSTEM_APP_NAME}
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}$<0:>
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}$<0:>
    RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}$<0:>
        )


TARGET_LINK_LIBRARIES(${SYSTEM_APP_NAME} PUBLIC
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

INCLUDE_DIRECTORIES(
    ${CMAKE_BINARY_DIR}/src/ui
    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/form
    ${CMAKE_SOURCE_DIR}/src/gui/data
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/watershed
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork/structure2d
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
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/python
    ${CMAKE_SOURCE_DIR}/src/lekan
    )

INSTALL(TARGETS ${SYSTEM_APP_NAME} RUNTIME DESTINATION bin)

