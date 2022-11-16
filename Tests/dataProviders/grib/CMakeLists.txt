# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

SET(REOS_GRIB_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/grib/reosgribprovider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_GRIB_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/grib/reosgribprovider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_grib ${REOS_GRIB_HEADERS} ${REOS_GRIB_SOURCES} reos_grib_test.cpp)
TARGET_LINK_LIBRARIES(test_grib
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(test_grib ${CMAKE_CURRENT_BINARY_DIR}/test_grib)

set_target_properties(test_grib
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/grib/
  )
