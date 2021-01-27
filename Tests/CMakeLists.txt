# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)
FIND_PACKAGE(GTest)

IF (NOT GTEST_FOUND)
    # Download and unpack googletest at configure time
    FILE(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download")
    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/GTestCMakeLists.in" "${CMAKE_BINARY_DIR}/googletest-download/CMakeLists.txt")
    EXECUTE_PROCESS(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download" )
    EXECUTE_PROCESS(COMMAND "${CMAKE_COMMAND}" --build .
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download" )

    # Prevent GoogleTest from overriding our compiler/linker options
    # when building with Visual Studio
    SET(gtest_force_shared_crt ON CACHE BOOL "" FORCE)

    # Add googletest directly to our build. This adds
    # the following targets: gtest, gtest_main, gmock
    # and gmock_main
    ADD_SUBDIRECTORY("${CMAKE_BINARY_DIR}/googletest-src"
                     "${CMAKE_BINARY_DIR}/googletest-build"
                     EXCLUDE_FROM_ALL)

    INCLUDE_DIRECTORIES("${gtest_SOURCE_DIR}/include"
                        "${gmock_SOURCE_DIR}/include")
ELSE(NOT GTEST_FOUND)
    INCLUDE_DIRECTORIES(${GTEST_INCLUDE_DIR})
ENDIF(NOT GTEST_FOUND)

#########################################################################
#########################################################################
SET(TESTDATA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/testData/")
ADD_DEFINITIONS(-DTESTDATA="${TESTDATA_DIR}")

#########################################################################
#########################################################################
SET(TESTDATA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/data")
ADD_DEFINITIONS(-DTESTDATA="${TESTDATA_DIR}")
ADD_LIBRARY(reostestutils OBJECT ${CMAKE_CURRENT_SOURCE_DIR}/src/reos_testutils.h ${CMAKE_CURRENT_SOURCE_DIR}/src/reos_testutils.cpp )
TARGET_LINK_LIBRARIES(reostestutils
                      ${Qt5Core_LIBRARIES}
                      reosCore)
INCLUDE_DIRECTORIES(${CMAKE_SOURCE_DIR}/Tests/src/tools)

#########################################################################
#########################################################################

INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}
  ${CMAKE_CURRENT_SOURCE_DIR}/src
  ${CMAKE_CURRENT_BINARY_DIR}
  ${CMAKE_BINARY_DIR}
  ${CMAKE_SOURCE_DIR}/src/tools
  ${CMAKE_SOURCE_DIR}/src/core
  ${CMAKE_SOURCE_DIR}/src/core/GIS
  ${CMAKE_SOURCE_DIR}/src/core/raster
  ${CMAKE_SOURCE_DIR}/src/core/quantity
  ${CMAKE_SOURCE_DIR}/src/core/process
  ${CMAKE_SOURCE_DIR}/src/core/watershed
  ${CMAKE_SOURCE_DIR}/src/core/rainfall
  ${CMAKE_SOURCE_DIR}/src/core/utils

  ${CMAKE_SOURCE_DIR}/src/gui/GIS
  ${CMAKE_SOURCE_DIR}/src/gui
)

MACRO (ADD_REOS_TEST TESTSRC)
  GET_FILENAME_COMPONENT(TESTNAME ${TESTSRC} NAME)
  STRING(REPLACE ".cpp" "" TESTNAME ${TESTNAME})
  ADD_EXECUTABLE(${TESTNAME} ${TESTSRC} $<TARGET_OBJECTS:reostestutils>)
  TARGET_LINK_LIBRARIES(${TESTNAME}
                        ${Qt5Core_LIBRARIES}
                        ${Qt5Test_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT}
                        gtest
                        gmock
                        reosTools
                        reosCore
                        reosGui)

  IF(GDAL_FOUND)
    TARGET_INCLUDE_DIRECTORIES(${TESTNAME} PRIVATE ${GDAL_INCLUDE_DIRS})
  ENDIF(GDAL_FOUND)
  ADD_TEST(${TESTNAME} ${CMAKE_CURRENT_BINARY_DIR}/${TESTNAME})
ENDMACRO (ADD_REOS_TEST)

SET(TESTS
    src/core/reos_dem_test.cpp
    src/core/reos_watershed_test.cpp
    src/core/reos_rainfall_test.cpp
    src/core/reos_raster_test.cpp
    src/core/reos_watershedRaster_test.cpp
    src/core/reos_geometry_test.cpp

    src/gui/reos_gisGui_test.cpp
    )

FOREACH(TESTSRC ${TESTS})
    ADD_REOS_TEST(${TESTSRC})
ENDFOREACH(TESTSRC)


