# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)

SET(REOS_HECRAS_SOURCES
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras/reoshecrascontroller.cpp
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras/reoshecrassimulation.cpp
     ${CMAKE_SOURCE_DIR}/src/dataProviders/hec-dss/reosdssfile.cpp
)

SET(REOS_HECRAS_HEADERS
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras/reoshecrascontroller.h
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras/reoshecrassimulation.h
     ${CMAKE_SOURCE_DIR}/src/dataProviders/hec-dss/reosdssfile.h
)

SET(TESTHECDATA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/testData/")
ADD_DEFINITIONS(-DTESTHECDATA="${TESTHECDATA_DIR}")

ADD_EXECUTABLE(test_hecras ${REOS_HECRAS_HEADERS} ${REOS_HECRAS_SOURCES} test_hecras.cpp)
TARGET_LINK_LIBRARIES(test_hecras
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
                      reosGui
                      ${HEC_DSS_ALL_STATIC_LIB}
)

IF(GDAL_FOUND)
  TARGET_INCLUDE_DIRECTORIES(${TESTNAME} PRIVATE ${GDAL_INCLUDE_DIRS})
ENDIF(GDAL_FOUND)
ADD_TEST(test_hecras ${CMAKE_CURRENT_BINARY_DIR}/test_hecras)

  set_target_properties(${TESTNAME}
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      )


INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras
    ${CMAKE_SOURCE_DIR}/src/dataProviders/hec-dss

    ${CMAKE_BINARY_DIR}/src/ui
    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/form
    ${CMAKE_SOURCE_DIR}/src/gui/data
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/watershed
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork/structure2d
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/mesh
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork/simulation
    ${HEC_DSS_INCLUDE}
    )


