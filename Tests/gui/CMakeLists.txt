# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################
SET(TESTDATA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/testData/")
ADD_DEFINITIONS(-DTESTDATA="${TESTDATA_DIR}")

#########################################################################
#########################################################################
SET(TESTDATA_DIR "${CMAKE_CURRENT_SOURCE_DIR}/data")
ADD_DEFINITIONS(-DTESTDATA="${TESTDATA_DIR}")

#########################################################################
#########################################################################

MACRO (ADD_REOS_TEST TESTSRC)
  GET_FILENAME_COMPONENT(TESTNAME ${TESTSRC} NAME)
  STRING(REPLACE ".cpp" "" TESTNAME ${TESTNAME})
  ADD_EXECUTABLE(${TESTNAME} ${TESTSRC} $<TARGET_OBJECTS:reostestutils>)
  TARGET_LINK_LIBRARIES(${TESTNAME}
                        ${Qt5Core_LIBRARIES}
                        ${Qt5Test_LIBRARIES}
                        reosCore
                        reosGui)

  ADD_TEST(${TESTNAME} ${CMAKE_CURRENT_BINARY_DIR}/${TESTNAME})

  set_target_properties(${TESTNAME}
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
      )
ENDMACRO (ADD_REOS_TEST)

SET(TESTS
    reos_gisGui_test.cpp
    )

FOREACH(TESTSRC ${TESTS})
    ADD_REOS_TEST(${TESTSRC})
ENDFOREACH(TESTSRC)


