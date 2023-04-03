# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

#########################################################################
#########################################################################

set(METEOFRANCEAPI_TESTS_KEY_FILE CACHE FILEPATH "File containing the API key of Meteo France API.")
add_definitions(-DMETEOFRANCE_API_KEY_FILE="${METEOFRANCEAPI_TESTS_KEY_FILE}")


SET(REOS_MFAPI_SOURCES
      ${CMAKE_SOURCE_DIR}/src/dataProviders/meteofrance-api/reosmeteofranceapi.cpp
      ${CMAKE_SOURCE_DIR}/src/dataProviders/meteofrance-api/reosmeteofrancearomeprovider.cpp
      ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.cpp
)

SET(REOS_MFAPI_HEADERS
      ${CMAKE_SOURCE_DIR}/src/dataProviders/meteofrance-api/reosmeteofranceapi.h
      ${CMAKE_SOURCE_DIR}/src/dataProviders/meteofrance-api/reosmeteofrancearomeprovider.h
      ${CMAKE_SOURCE_DIR}/Tests/reos_testutils.h
 )

ADD_EXECUTABLE(test_mf_api ${REOS_MFAPI_HEADERS} ${REOS_MFAPI_SOURCES} reos_mfapi_test.cpp)
TARGET_LINK_LIBRARIES(test_mf_api
                      ${Qt5Core_LIBRARIES}
                      ${Qt5Test_LIBRARIES}
                      ${CMAKE_THREAD_LIBS_INIT}
                      reosCore
)

ADD_TEST(NAME test_mf_api
         COMMAND "test_mf_api"
         WORKING_DIRECTORY ${REOS_OUTPUT_DIRECTORY})

set_target_properties(test_mf_api
      PROPERTIES
      ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>
      )

INCLUDE_DIRECTORIES(
      ${CMAKE_SOURCE_DIR}/src/dataProviders/meteofrance-api/
  )
