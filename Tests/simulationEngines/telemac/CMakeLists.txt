# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)

ADD_DEFINITIONS(-DTELEMAC_PATH="${TELEMAC_PATH}")
ADD_DEFINITIONS(-DTELEMAC_CONFIG_NAME="${TELEMAC_CONFIG_NAME}")
ADD_DEFINITIONS(-DTELEMAC_CONFIG_FILE="${TELEMAC_CONFIG_FILE}")

SET(REOS_TELEMAC_SOURCES
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/telemac/reostelemac2dsimulation.cpp
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/telemac/reostelemac2dinitialcondition.cpp
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/telemac/reostelemac2dsimulationresults.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
 )

SET(REOS_TELEMAC_HEADERS
)

ADD_EXECUTABLE(test_telemac ${REOS_TELEMAC_HEADERS} ${REOS_TELEMAC_SOURCES} test_telemac.cpp)
TARGET_LINK_LIBRARIES(test_telemac
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
                      reosGui
                      ${MDAL_LIB}
)

IF(GDAL_FOUND)
  TARGET_INCLUDE_DIRECTORIES(${TESTNAME} PRIVATE ${GDAL_INCLUDE_DIRS})
ENDIF(GDAL_FOUND)

ADD_TEST(NAME test_telemac
         COMMAND "test_telemac"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_telemac
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )


INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}/src/simulationEngines/telemac

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
    ${QGIS_INCLUDE_DIR}
    ${MDAL_INCLUDE_DIR}
    )


