# Reos licence GPL version 2
# Copyright (C) 2025 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

SET(REOS_METEOHDF5_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/meteo-hdf5/reosmeteohdf5.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_METEOHDF5_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/meteo-hdf5/reosmeteohdf5.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_meteo_hdf5 ${REOS_METEOHDF5_HEADERS} ${REOS_METEOHDF5_SOURCES} reos_meteo_hdf5_test.cpp)
TARGET_LINK_LIBRARIES(test_meteo_hdf5
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_meteo_hdf5
         COMMAND "test_meteo_hdf5"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_grib
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/meteo-hdf5/
  )
