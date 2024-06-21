# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

set(TESTDATA_DIR "${CMAKE_SOURCE_DIR}/Tests/testData")
add_definitions(-DTESTDATA="${TESTDATA_DIR}")

add_library(reostestutils OBJECT ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp )
target_link_libraries(reostestutils
                      ${Qt5Core_LIBRARIES}
                      reosCore)

include_directories(${CMAKE_SOURCE_DIR}/Tests)

include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/src/tools

  ${CMAKE_SOURCE_DIR}/src/gui
  ${CMAKE_SOURCE_DIR}/src/gui/GIS
)

IF(WITH_GUI)
    include_directories(
      ${CMAKE_SOURCE_DIR}/src/gui
      ${CMAKE_SOURCE_DIR}/src/gui/GIS
    )
ENDIF(WITH_GUI)


add_subdirectory(core)
IF(WITH_GUI)
    add_subdirectory(gui)
ENDIF(WITH_GUI)

add_subdirectory(dataProviders)
add_subdirectory(simulationEngines)

if(INTERN_PYTHON_TESTS_ENABLE)
    add_subdirectory(python)
endif(INTERN_PYTHON_TESTS_ENABLE)
