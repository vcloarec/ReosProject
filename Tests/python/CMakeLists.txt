# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(Threads REQUIRED)


SET(REOS_PYTHON_TEST_SOURCES
     ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
 )

SET(REOS_PYTHON_TEST_HEADERS

)

ADD_EXECUTABLE(test_python ${REOS_PYTHON_TEST_HEADERS} ${REOS_PYTHON_TEST_SOURCES} reos_test_python.cpp)

TARGET_LINK_LIBRARIES(test_python
                      ${Qt5Test_LIBRARIES}
                      reosPython
                      reosCore
)

ADD_TEST(NAME test_python
         COMMAND "test_python"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_python
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )


INCLUDE_DIRECTORIES(
${CMAKE_SOURCE_DIR}/src/python/
    )


