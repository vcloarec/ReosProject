# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################


SET(REOS_VORTEXIO_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/vortex-io/reosvortexioprovider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_VORTEXIO_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/vortex-io/reosvortexioprovider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_vortexio ${REOS_VORTEXIO_HEADERS} ${REOS_VORTEXIO_SOURCES} reos_vortexio_test.cpp)
TARGET_LINK_LIBRARIES(test_vortexio
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_vortexio
         COMMAND "test_vortexio"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_vortexio
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/vortex-io/
  )
