# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

SET(TESTDATA_DIR "${CMAKE_SOURCE_DIR}/Tests/testData")
ADD_DEFINITIONS(-DTESTDATA="${TESTDATA_DIR}")

ADD_LIBRARY(reostestutils OBJECT ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp )
TARGET_LINK_LIBRARIES(reostestutils
                      ${Qt5Core_LIBRARIES}
                      reosCore)

INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/Tests)

INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/src/tools
  ${CMAKE_SOURCE_DIR}/src/core
  ${CMAKE_SOURCE_DIR}/src/core/data
  ${CMAKE_SOURCE_DIR}/src/core/GIS
  ${CMAKE_SOURCE_DIR}/src/core/raster
  ${CMAKE_SOURCE_DIR}/src/core/quantity
  ${CMAKE_SOURCE_DIR}/src/core/process
  ${CMAKE_SOURCE_DIR}/src/core/watershed
  ${CMAKE_SOURCE_DIR}/src/core/rainfall
  ${CMAKE_SOURCE_DIR}/src/core/hydrograph
  ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork
  ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork/simulation
  ${CMAKE_SOURCE_DIR}/src/core/utils
  ${CMAKE_SOURCE_DIR}/src/core/mesh
  ${CMAKE_SOURCE_DIR}/src/core/network

  ${CMAKE_SOURCE_DIR}/src/gui
  ${CMAKE_SOURCE_DIR}/src/gui/GIS
)


ADD_SUBDIRECTORY(core)
ADD_SUBDIRECTORY(gui)
ADD_SUBDIRECTORY(dataProviders)
ADD_SUBDIRECTORY(simulationEngines)
