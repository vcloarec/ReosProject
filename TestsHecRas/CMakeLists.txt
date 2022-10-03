# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)


#########################################################################
#########################################################################

SET(REOS_HECRAS_SOURCES
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras/reoshecrascontroller.cpp
)

SET(REOS_HECRAS_HEADERS
     ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras/reoshecrascontroller.h
)


ADD_EXECUTABLE(test_hecras ${REOS_HECRAS_HEADERS} ${REOS_HECRAS_SOURCES} test_hecras.cpp)
TARGET_LINK_LIBRARIES(test_hecras
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
                      reosGui)

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
    )


