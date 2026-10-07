# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################


SET(REOS_CDSLCC_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/cdslcc/reoscdslccprovider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_CDSLCC_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/cdslcc/reoscdslccprovider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_cdslcc ${REOS_CDSLCC_HEADERS} ${REOS_CDSLCC_SOURCES} reos_cdslcc_test.cpp)
TARGET_LINK_LIBRARIES(test_cdslcc
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_cdslcc
         COMMAND "test_cdslcc"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_cdslcc
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/cds_lcc/
  )
