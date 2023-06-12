# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################


SET(REOS_HYDROPORTAIL_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/hydroportail/reoshydroportailprovider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_HYDROPORTAIL_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/hydroportail/reoshydroportailprovider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_hydroportail ${REOS_HYDROPORTAIL_HEADERS} ${REOS_HYDROPORTAIL_SOURCES} reos_hydroportail_test.cpp)
TARGET_LINK_LIBRARIES(test_hydroportail
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_hydroportail
         COMMAND "test_hydroportail"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_hydroportail
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/hydroportail/
  )
