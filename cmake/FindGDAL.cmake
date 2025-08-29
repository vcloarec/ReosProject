# Find GDAL
# ~~~~~~~~~
# Copyright (c) 2007, Magnus Homann <magnus at homann dot se>
# Redistribution and use is allowed according to the terms of the BSD license.
# For details see the accompanying COPYING-CMAKE-SCRIPTS file.
#
#
# Once run this will define: 
# 
# GDAL_FOUND       = system has GDAL lib
#
# GDAL_LIBRARY     = full path to the library
#
# GDAL_INCLUDE_DIR      = where to find headers 

find_package(GDAL CONFIG)
if(NOT GDAL_FOUND)
  # Fallback logic for GDAL < 3.5, as soon as we switch to GDAL>=3.5 this file (Find_GDAL.cmake) can be deleted
  INCLUDE (${CMAKE_SOURCE_DIR}/cmake/MacPlistMacros.cmake)

  IF(WIN32)
  
    IF (MINGW)
      FIND_PATH(GDAL_INCLUDE_DIR gdal.h /usr/local/include /usr/include c:/msys/local/include PATH_SUFFIXES gdal)
      FIND_LIBRARY(GDAL_LIBRARY NAMES gdal PATHS /usr/local/lib /usr/lib c:/msys/local/lib)
    ENDIF (MINGW)
  
    IF (MSVC)
      FIND_PATH(GDAL_INCLUDE_DIR gdal.h "$ENV{LIB_DIR}/include/gdal" $ENV{INCLUDE})
      FIND_LIBRARY(GDAL_LIBRARY NAMES gdal gdal_i PATHS 
  	    "$ENV{LIB_DIR}/lib" $ENV{LIB} /usr/lib c:/msys/local/lib)
      IF (GDAL_LIBRARY)
        SET (
           GDAL_LIBRARY;odbc32;odbccp32 
           CACHE STRING INTERNAL)
      ENDIF (GDAL_LIBRARY)
    ENDIF (MSVC)
  
  ELSEIF(APPLE AND QGIS_MAC_DEPS_DIR)
  
      FIND_PATH(GDAL_INCLUDE_DIR gdal.h "$ENV{LIB_DIR}/include")
      FIND_LIBRARY(GDAL_LIBRARY NAMES gdal PATHS "$ENV{LIB_DIR}/lib")
  
  ELSE(WIN32)
  
    IF(UNIX) 
  
      # try to use framework on mac
      # want clean framework path, not unix compatibility path
      IF (APPLE)
        IF (CMAKE_FIND_FRAMEWORK MATCHES "FIRST"
            OR CMAKE_FRAMEWORK_PATH MATCHES "ONLY"
            OR NOT CMAKE_FIND_FRAMEWORK)
          SET (CMAKE_FIND_FRAMEWORK_save ${CMAKE_FIND_FRAMEWORK} CACHE STRING "" FORCE)
          SET (CMAKE_FIND_FRAMEWORK "ONLY" CACHE STRING "" FORCE)
          FIND_LIBRARY(GDAL_LIBRARY GDAL)
          IF (GDAL_LIBRARY)
            # they're all the same in a framework
            SET (GDAL_INCLUDE_DIR ${GDAL_LIBRARY}/Headers CACHE PATH "Path to a file.")
            # set GDAL_CONFIG to make later test happy, not used here, may not exist
            SET (GDAL_CONFIG ${GDAL_LIBRARY}/unix/bin/gdal-config CACHE FILEPATH "Path to a program.")
            # version in info.plist
            GET_VERSION_PLIST (${GDAL_LIBRARY}/Resources/Info.plist GDAL_VERSION)
            IF (NOT GDAL_VERSION)
              MESSAGE (FATAL_ERROR "Could not determine GDAL version from framework.")
            ENDIF (NOT GDAL_VERSION)
            STRING(REGEX REPLACE "([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\1" GDAL_VERSION_MAJOR "${GDAL_VERSION}")
            STRING(REGEX REPLACE "([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\2" GDAL_VERSION_MINOR "${GDAL_VERSION}")
  	  IF (GDAL_VERSION_MAJOR LESS 3)
              MESSAGE (FATAL_ERROR "GDAL version is too old (${GDAL_VERSION}). Use 3.2 or higher.")
  	  ENDIF (GDAL_VERSION_MAJOR LESS 3)
            IF ( (GDAL_VERSION_MAJOR EQUAL 3) AND (GDAL_VERSION_MINOR LESS 2) )
              MESSAGE (FATAL_ERROR "GDAL version is too old (${GDAL_VERSION}). Use 3.2 or higher.")
            ENDIF( (GDAL_VERSION_MAJOR EQUAL 3) AND (GDAL_VERSION_MINOR LESS 2) )
  
          ENDIF (GDAL_LIBRARY)
          SET (CMAKE_FIND_FRAMEWORK ${CMAKE_FIND_FRAMEWORK_save} CACHE STRING "" FORCE)
        ENDIF ()
      ENDIF (APPLE)
  
      IF(CYGWIN)
        FIND_LIBRARY(GDAL_LIBRARY NAMES gdal PATHS /usr/lib /usr/local/lib)
      ENDIF(CYGWIN)
  
      IF (NOT GDAL_INCLUDE_DIR OR NOT GDAL_LIBRARY OR NOT GDAL_CONFIG)
        # didn't find OS X framework, and was not set by user
        SET(GDAL_CONFIG_PREFER_PATH "$ENV{GDAL_HOME}/bin" CACHE STRING "preferred path to GDAL (gdal-config)")
        SET(GDAL_CONFIG_PREFER_FWTOOLS_PATH "$ENV{FWTOOLS_HOME}/bin_safe" CACHE STRING "preferred path to GDAL (gdal-config) from FWTools")
        FIND_PROGRAM(GDAL_CONFIG gdal-config
            ${GDAL_CONFIG_PREFER_PATH}
            ${GDAL_CONFIG_PREFER_FWTOOLS_PATH}
            $ENV{LIB_DIR}/bin
            /usr/local/bin/
            /usr/bin/
            )
        # MESSAGE("DBG GDAL_CONFIG ${GDAL_CONFIG}")
      
        IF (GDAL_CONFIG) 
  
          ## extract gdal version 
          execute_process(COMMAND ${GDAL_CONFIG} --version
              OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE GDAL_VERSION )
          STRING(REGEX REPLACE "([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\1" GDAL_VERSION_MAJOR "${GDAL_VERSION}")
          STRING(REGEX REPLACE "([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\2" GDAL_VERSION_MINOR "${GDAL_VERSION}")
          STRING(REGEX REPLACE "([0-9]+)\\.([0-9]+)\\.([0-9]+)" "\\3" GDAL_VERSION_MICRO "${GDAL_VERSION}")
    
          # MESSAGE("DBG GDAL_VERSION ${GDAL_VERSION}")
          # MESSAGE("DBG GDAL_VERSION_MAJOR ${GDAL_VERSION_MAJOR}")
          # MESSAGE("DBG GDAL_VERSION_MINOR ${GDAL_VERSION_MINOR}")
    
          # check for gdal version
          # version 1.2.5 is known NOT to be supported (missing CPL_STDCALL macro)
          # According to INSTALL, 2.1+ is required
  	IF (GDAL_VERSION_MAJOR LESS 3)
  	  MESSAGE (FATAL_ERROR "GDAL version is too old (${GDAL_VERSION}). Use 3.0 or higher.")
  	ENDIF (GDAL_VERSION_MAJOR LESS 3)
  	#IF ( (GDAL_VERSION_MAJOR EQUAL 2) AND (GDAL_VERSION_MINOR LESS 1) )
  	#  MESSAGE (FATAL_ERROR "GDAL version is too old (${GDAL_VERSION}). Use 2.1 or higher.")
  	#ENDIF( (GDAL_VERSION_MAJOR EQUAL 2) AND (GDAL_VERSION_MINOR LESS 1) )
          IF ( (GDAL_VERSION_MAJOR EQUAL 3) AND (GDAL_VERSION_MINOR EQUAL 0) AND (GDAL_VERSION_MICRO LESS 3) )
            MESSAGE (FATAL_ERROR "GDAL version is too old (${GDAL_VERSION}). Use 3.0.3 or higher.")
          ENDIF( (GDAL_VERSION_MAJOR EQUAL 3) AND (GDAL_VERSION_MINOR EQUAL 0) AND (GDAL_VERSION_MICRO LESS 3) )
  
          # set INCLUDE_DIR to prefix+include
          execute_process(COMMAND ${GDAL_CONFIG} --prefix
              OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE GDAL_PREFIX)
          #SET(GDAL_INCLUDE_DIR ${GDAL_PREFIX}/include CACHE STRING INTERNAL)
          FIND_PATH(GDAL_INCLUDE_DIR 
              gdal.h 
              ${GDAL_PREFIX}/include/gdal
              ${GDAL_PREFIX}/include
              /usr/local/include 
              /usr/include 
              )
  
          ## extract link dirs for rpath  
          execute_process(COMMAND ${GDAL_CONFIG} --libs
              OUTPUT_STRIP_TRAILING_WHITESPACE OUTPUT_VARIABLE GDAL_CONFIG_LIBS )
  
          ## split off the link dirs (for rpath)
          ## use regular expression to match wildcard equivalent "-L*<endchar>"
          ## with <endchar> is a space or a semicolon
          STRING(REGEX MATCHALL "[-][L]([^ ;])+" 
              GDAL_LINK_DIRECTORIES_WITH_PREFIX 
              "${GDAL_CONFIG_LIBS}" )
          #      MESSAGE("DBG  GDAL_LINK_DIRECTORIES_WITH_PREFIX=${GDAL_LINK_DIRECTORIES_WITH_PREFIX}")
  
          ## remove prefix -L because we need the pure directory for LINK_DIRECTORIES
        
          IF (GDAL_LINK_DIRECTORIES_WITH_PREFIX)
            STRING(REGEX REPLACE "[-][L]" "" GDAL_LINK_DIRECTORIES ${GDAL_LINK_DIRECTORIES_WITH_PREFIX} )
          ENDIF (GDAL_LINK_DIRECTORIES_WITH_PREFIX)
  
          ## split off the name
          ## use regular expression to match wildcard equivalent "-l*<endchar>"
          ## with <endchar> is a space or a semicolon
          STRING(REGEX MATCHALL "[-][l]([^ ;])+" 
              GDAL_LIB_NAME_WITH_PREFIX 
              "${GDAL_CONFIG_LIBS}" )
          #      MESSAGE("DBG  GDAL_LIB_NAME_WITH_PREFIX=${GDAL_LIB_NAME_WITH_PREFIX}")
  
  
          ## remove prefix -l because we need the pure name
        
          IF (GDAL_LIB_NAME_WITH_PREFIX)
            STRING(REGEX REPLACE "[-][l]" "" GDAL_LIB_NAME ${GDAL_LIB_NAME_WITH_PREFIX} )
          ENDIF (GDAL_LIB_NAME_WITH_PREFIX)
  
          IF (APPLE)
            IF (NOT GDAL_LIBRARY)
              # work around empty GDAL_LIBRARY left by framework check
              # while still preserving user setting if given
              # ***FIXME*** need to improve framework check so below not needed
              SET(GDAL_LIBRARY ${GDAL_LINK_DIRECTORIES}/lib${GDAL_LIB_NAME}.dylib CACHE STRING INTERNAL FORCE)
            ENDIF (NOT GDAL_LIBRARY)
          ELSE (APPLE)
            FIND_LIBRARY(GDAL_LIBRARY NAMES ${GDAL_LIB_NAME} gdal PATHS ${GDAL_LINK_DIRECTORIES}/lib ${GDAL_LINK_DIRECTORIES})
          ENDIF (APPLE)
        
        ELSE(GDAL_CONFIG)
          MESSAGE("FindGDAL.cmake: gdal-config not found. Please set it manually. GDAL_CONFIG=${GDAL_CONFIG}")
        ENDIF(GDAL_CONFIG)
      ENDIF (NOT GDAL_INCLUDE_DIR OR NOT GDAL_LIBRARY OR NOT GDAL_CONFIG)
    ENDIF(UNIX)
  ENDIF(WIN32)
  
  
  IF (GDAL_INCLUDE_DIR AND GDAL_LIBRARY)
     SET(GDAL_FOUND TRUE)
  ENDIF (GDAL_INCLUDE_DIR AND GDAL_LIBRARY)
  
  IF (GDAL_FOUND)
     IF (NOT GDAL_FIND_QUIETLY)
        FILE(READ ${GDAL_INCLUDE_DIR}/gdal_version.h gdal_version)
        STRING(REGEX REPLACE "^.*GDAL_RELEASE_NAME +\"([^\"]+)\".*$" "\\1" GDAL_RELEASE_NAME "${gdal_version}")
  
        MESSAGE(STATUS "Found GDAL: ${GDAL_LIBRARY} (${GDAL_RELEASE_NAME})")
     ENDIF (NOT GDAL_FIND_QUIETLY)
     add_library(GDAL::GDAL UNKNOWN IMPORTED)
     target_link_libraries(GDAL::GDAL INTERFACE ${GDAL_LIBRARY})
     target_include_directories(GDAL::GDAL INTERFACE ${GDAL_INCLUDE_DIR})
     set_target_properties(GDAL::GDAL PROPERTIES IMPORTED_LOCATION ${GDAL_LIBRARY})
  ELSE (GDAL_FOUND)
  
     MESSAGE(GDAL_INCLUDE_DIR=${GDAL_INCLUDE_DIR})
     MESSAGE(GDAL_LIBRARY=${GDAL_LIBRARY})
     MESSAGE(FATAL_ERROR "Could not find GDAL")
  
  ENDIF (GDAL_FOUND)
endif()
