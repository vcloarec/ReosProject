# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

SET(REOS_COMEPHORES_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/comephores/reoscomephoresprovider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_COMEPHORES_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/comephores/reoscomephoresprovider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_comephores ${REOS_COMEPHORES_HEADERS} ${REOS_COMEPHORES_SOURCES} reos_comephores_test.cpp)
TARGET_LINK_LIBRARIES(test_comephores
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_comephores
         COMMAND "test_comephores"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_comephores
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/comephores/
  )
