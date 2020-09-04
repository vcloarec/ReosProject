# MDAL - Mesh Data Abstraction Library (MIT License)
# Copyright (C) 2018 Peter Petrik (zilolv at gmail dot com)

FIND_PACKAGE(GDAL REQUIRED)

SET(REOS_TOOLS_SOURCES
  raster/reosmemoryraster.cpp
  raster/reosrasterfilling.cpp
)

SET(REOS_TOOLS_HEADERS
  reos_tools.h
  raster/reosmemoryraster.h
  raster/reosrasterfilling.h
)

SET(REOS_TOOLS_LIBS ${REOS_TOOLS_LIBS} reosTools)
  ADD_LIBRARY(reosTools SHARED
    ${REOS_TOOLS_SOURCES}
    ${REOS_TOOLS_HEADERS}
  )
IF(WIN32)
  INSTALL(TARGETS reosTools RUNTIME DESTINATION lib)
ELSE(WIN32)
  INSTALL(TARGETS reosTools LIBRARY DESTINATION lib)
ENDIF(WIN32)


TARGET_INCLUDE_DIRECTORIES(reosTools
    PUBLIC
    $<INSTALL_INTERFACE:include>
    PRIVATE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>
)

TARGET_LINK_LIBRARIES(reosTools PUBLIC
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

INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}
  PUBLIC
  ${CMAKE_SOURCE_DIR}/src/core
  )


IF(GDAL_FOUND)
    message(STATUS "************************  GDAL found")
    TARGET_INCLUDE_DIRECTORIES(reosTools PUBLIC ${GDAL_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES(reosTools PUBLIC ${GDAL_LIBRARIES} )
ELSE(GDAL_FOUND)
    message(STATUS "************************  GDAL not found")
    ENDIF(GDAL_FOUND)


# INSTALL HEADER
INSTALL(FILES raster/reosmemoryraster.h DESTINATION include)
