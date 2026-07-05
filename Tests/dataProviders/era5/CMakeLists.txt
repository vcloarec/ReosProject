# Reos licence GPL version 2
# Copyright (C) 2023 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

SET(ERA5_TESTS_FILES_PATH CACHE PATH "Path to the era5 test files.")
IF (ERA5_TESTS_ENABLE)
    ADD_DEFINITIONS(-DERA5_FILES_PATH="${ERA5_TESTS_FILES_PATH}")
ENDIF (ERA5_TESTS_ENABLE)

SET(REOS_ERA5_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/era5/reosera5provider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_ERA5_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/era5/reosera5provider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_era5 ${REOS_ERA5_HEADERS} ${REOS_ERA5_SOURCES} reos_era5_test.cpp)
TARGET_LINK_LIBRARIES(test_era5
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_comephore
         COMMAND "test_comephore"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_era5
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/era5/
  )
