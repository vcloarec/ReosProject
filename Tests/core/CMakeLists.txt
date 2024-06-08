# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)

# Determine if GTest/GMock pre-built libraries are available
IF(TARGET GTest::gmock)
    SET(GTEST_TARGET GTest::gtest)
    SET(GMOCK_TARGET GTest::gmock)
ELSEIF(EXISTS "/usr/src/googletest/CMakeLists.txt")
    # Source-only install (e.g. libgtest-dev on Ubuntu): build from system sources
    SET(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    ADD_SUBDIRECTORY("/usr/src/googletest"
                     "${CMAKE_BINARY_DIR}/googletest-build"
                     EXCLUDE_FROM_ALL)
    SET(GTEST_TARGET gtest)
    SET(GMOCK_TARGET gmock)
ELSE()
    # Download and unpack googletest at configure time
    FILE(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download")
    CONFIGURE_FILE("${CMAKE_SOURCE_DIR}/Tests/GTestCMakeLists.in" "${CMAKE_BINARY_DIR}/googletest-download/CMakeLists.txt")
    EXECUTE_PROCESS(COMMAND "${CMAKE_COMMAND}" -G "${CMAKE_GENERATOR}" .
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download")
    EXECUTE_PROCESS(COMMAND "${CMAKE_COMMAND}" --build .
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download")

    SET(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    ADD_SUBDIRECTORY("${CMAKE_BINARY_DIR}/googletest-src"
                     "${CMAKE_BINARY_DIR}/googletest-build"
                     EXCLUDE_FROM_ALL)
    SET(GTEST_TARGET gtest)
    SET(GMOCK_TARGET gmock)
ENDIF()

MACRO (ADD_REOS_TEST TESTSRC)
  GET_FILENAME_COMPONENT(TESTNAME ${TESTSRC} NAME)
  STRING(REPLACE ".cpp" "" TESTNAME ${TESTNAME})
  ADD_EXECUTABLE(${TESTNAME} ${TESTSRC} $<TARGET_OBJECTS:reostestutils>)
  TARGET_LINK_LIBRARIES(${TESTNAME}
                        ${Qt5Core_LIBRARIES}
                        ${Qt5Test_LIBRARIES}
                        ${CMAKE_THREAD_LIBS_INIT}
                        ${GTEST_TARGET}
                        ${GMOCK_TARGET}
                        reosCore)

  IF(GDAL_FOUND)
    TARGET_INCLUDE_DIRECTORIES(${TESTNAME} PRIVATE ${GDAL_INCLUDE_DIR})
    TARGET_LINK_LIBRARIES(${TESTNAME} ${GDAL_LIBRARIES} )
  ENDIF(GDAL_FOUND)
  ADD_TEST(NAME ${TESTNAME}
           COMMAND "${TESTNAME}"
           WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

  #set_target_properties(${TESTNAME}
      #PROPERTIES
      #ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      #LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      #RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
   #   )
ENDMACRO (ADD_REOS_TEST)

SET(TESTS
    reos_dem_test.cpp
    reos_watershed_test.cpp
    reos_rainfall_test.cpp
    reos_raster_test.cpp
    reos_watershedRaster_test.cpp
    reos_geometry_test.cpp
    reos_data_test.cpp
    reos_hubeau_test.cpp
    reos_hydrograph_transfer_test.cpp
    reos_hydraulic_structure_2D_test.cpp
    reos_hydraulic_network_test.cpp
    )


IF(WITH_HYDRAULIC_MODEL_SUPPORT)
    SET(TESTS
        ${TESTS}
        reos_mesh_test.cpp
        )
ENDIF(WITH_HYDRAULIC_MODEL_SUPPORT)

FOREACH(TESTSRC ${TESTS})
    ADD_REOS_TEST(${TESTSRC})
ENDFOREACH(TESTSRC)


