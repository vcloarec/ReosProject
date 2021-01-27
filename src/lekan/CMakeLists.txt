# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_LEKAN_SOURCES
  main.cpp
  lekanmainwindow.cpp
)

SET(REOS_LEKAN_HEADERS
    lekanmainwindow.h
)

ADD_EXECUTABLE(lekan
    ${REOS_LEKAN_SOURCES}
    ${REOS_LEKAN_HEADERS})

TARGET_LINK_LIBRARIES(lekan PUBLIC
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
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/watershed
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    )

INSTALL(TARGETS lekan RUNTIME DESTINATION bin)

