# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)
FIND_PACKAGE(GTest)

IF (NOT GTEST_FOUND)
    # Download and unpack googletest at configure time
    FILE(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/googletest-download")
    CONFIGURE_FILE("${CMAKE_SOURCE_DIR}/Tests/GTestCMakeLists.in" "${CMAKE_BINARY_DIR}/googletest-download/CMakeLists.txt")
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
                        reosCore)

  IF(GDAL_FOUND)
    TARGET_INCLUDE_DIRECTORIES(${TESTNAME} PRIVATE ${GDAL_INCLUDE_DIRS})
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
    reos_mesh_test.cpp
    )

FOREACH(TESTSRC ${TESTS})
    ADD_REOS_TEST(${TESTSRC})
ENDFOREACH(TESTSRC)


