# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

SET(COMEPHORE_TESTS_FILES_PATH CACHE PATH "Path to the Comephore test files.")
IF (COMEPHORE_TESTS_ENABLE)
    ADD_DEFINITIONS(-DCOMEPHORE_FILES_PATH="${COMEPHORE_TESTS_FILES_PATH}")
ENDIF (COMEPHORE_TESTS_ENABLE)

SET(REOS_COMEPHORE_SOURCES
     ${CMAKE_SOURCE_DIR}/src/dataProviders/comephore/reoscomephoreprovider.cpp
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_COMEPHORE_HEADERS
     ${CMAKE_SOURCE_DIR}/src/dataProviders/comephore/reoscomephoreprovider.h
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_comephore ${REOS_COMEPHORE_HEADERS} ${REOS_COMEPHORE_SOURCES} reos_comephores_test.cpp)
TARGET_LINK_LIBRARIES(test_comephore
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_comephore
         COMMAND "test_comephore"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_comephore
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/comephore/
  )
