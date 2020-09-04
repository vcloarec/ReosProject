# Reos
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

CMAKE_MINIMUM_REQUIRED(VERSION 3.9)
PROJECT(Reos)

SET (CMAKE_CXX_STANDARD 11)
# set path to additional CMake modules
SET(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

# global vars
SET (REOS_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/output CACHE PATH "Output base directory")
SET (ENABLE_TESTS TRUE CACHE BOOL "Build tests?")

FIND_PACKAGE(Qt5 COMPONENTS Core Gui Widgets Network Xml Svg Concurrent Test UiTools Sql REQUIRED)

ADD_SUBDIRECTORY(src/core)
ADD_SUBDIRECTORY(src/tools)

# testing
IF (ENABLE_TESTS)
  INCLUDE (CTest)
  ADD_DEFINITIONS(-DENABLE_TESTS)
  ENABLE_TESTING()
  ADD_SUBDIRECTORY(Tests)
ENDIF(ENABLE_TESTS)
