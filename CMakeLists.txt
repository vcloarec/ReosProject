# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

CMAKE_MINIMUM_REQUIRED(VERSION 3.9)

SET(CMAKE_AUTORCC ON)

if(DEFINED ENV{LEKAN_EXPERIMENTAL_VERSION})
    set(CPACK_PACKAGE_VERSION_MAJOR "2")
    set(CPACK_PACKAGE_VERSION_MINOR "3")
    set(CPACK_PACKAGE_VERSION_PATCH $ENV{LEKAN_EXPERIMENTAL_VERSION})
    set(CPACK_PACKAGE_API_VERSION_PATCH "9999")
    ADD_DEFINITIONS(-DLEKAN_EXP)
else(DEFINED ENV{LEKAN_EXPERIMENTAL_VERSION})
    set(CPACK_PACKAGE_VERSION_MAJOR "2")
    set(CPACK_PACKAGE_VERSION_MINOR "3")
    set(CPACK_PACKAGE_VERSION_PATCH "90")
    set(CPACK_PACKAGE_API_VERSION_PATCH "90")
endif()

ADD_DEFINITIONS(-DMAJ_VER_LEKAN="${CPACK_PACKAGE_VERSION_MAJOR}")
ADD_DEFINITIONS(-DMIN_VER_LEKAN="${CPACK_PACKAGE_VERSION_MINOR}")
ADD_DEFINITIONS(-DPAT_VER_LEKAN="${CPACK_PACKAGE_VERSION_PATCH}")

set(COMPLETE_VERSION ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_VERSION_PATCH})

set(COMPLETE_API_VERSION ${CPACK_PACKAGE_VERSION_MAJOR}.${CPACK_PACKAGE_VERSION_MINOR}.${CPACK_PACKAGE_API_VERSION_PATCH})

PROJECT(Reos)

set (LEKAN_APP_NAME "lekan")
set (SYSTEM_APP_NAME "reossystem")

SET(CMAKE_CXX_STANDARD 17)
IF(MSVC)
  SET(CMAKE_GENERATOR_PLATFORM x64)
ENDIF(MSVC)
SET(CMAKE_AUTOMOC ON)

# set path to additional CMake modules
SET(CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake ${CMAKE_MODULE_PATH})

# global vars
SET (REOS_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/output CACHE PATH "Output base directory")
SET (ENABLE_TESTS TRUE CACHE BOOL "Build tests?")
SET (ENABLE_HECRAS FALSE CACHE BOOL "Build tests?")

SET (REOS_PROVIDERS_DIR providers)
SET (REOS_ENGINES_DIR engines)
ADD_DEFINITIONS(-DREOS_PROVIDERS="${REOS_PROVIDERS_DIR}")
ADD_DEFINITIONS(-DREOS_SIMULATION_ENGINES="${REOS_ENGINES_DIR}")
ADD_DEFINITIONS(-DREOS_BUILDING_OUTPUT="${REOS_OUTPUT_DIRECTORY}")

SET(WITH_GUI TRUE CACHE BOOL "With Graphic User Interface (LEKAN)")
SET(WITH_3D TRUE CACHE BOOL "With Graphic User Interface (LEKAN)")

SET(WITH_QTWEBKIT FALSE CACHE BOOL "With QtWebKit")
SET(QGIS_PROVIDERS_PATH CACHE PATH "Path to the folder containing the data providers of QGIS. Only necessary for dev and tests.")
SET(QGIS_INCLUDE_DIR CACHE PATH "Path to the include dir of QGIS")
SET(QGIS_CORE_LIB CACHE FILEPATH "Path to QGIS core lib")
SET(QGIS_GUI_LIB CACHE FILEPATH "Path to QGIS gui lib")
SET(QGIS_ANALYSIS_LIB CACHE FILEPATH "Path to QGIS analysis lib")
SET(QGIS_3D_LIB CACHE FILEPATH "Path to QGIS 3D lib")
SET(QGIS_APP_LIB CACHE FILEPATH "Path to QGIS app lib")
SET(QGIS_APP_INCLUDE CACHE PATH "Path to QGIS app header files (in source)")
SET(QGIS_LIBS ${QGIS_CORE_LIB} ${QGIS_GUI_LIB} ${QGIS_ANALYSIS_LIB} ${QGIS_3D_LIB})
SET(QSCISCINTILLA_INCLUDE_DIR CACHE PATH "Path to Qsciscintilla include dir")


SET(ENABLE_HEC_DSS TRUE CACHE BOOL "Enable hec-dss")

IF(WITH_3D)
    add_definitions(-DENABLE_3D)
ENDIF(WITH_3D)

IF(WITH_GUI)
    add_definitions(-DENABLE_GUI)
ENDIF(WITH_GUI)

IF(ENABLE_HEC_DSS)
    SET (HEC_DSS_LIBS_PATH CACHE PATH "Path to the hec-dss libraries")
    SET (HEC_DSS_INCLUDE CACHE PATH "Path to the hec-dss include dir")
    IF(MSVC)
        SET(INTEL_FORTRAN_COMPILER_LIB_PATH CACHE PATH "C:\\Program Files (x86)\\Intel\\oneAPI\\compiler\\2022.2.0\\windows\\compiler\\lib\\intel64_win\\")
        SET(HEC_DSS_ALL_STATIC_LIB
        ${HEC_DSS_LIBS_PATH}/heclib_c.lib
        ${HEC_DSS_LIBS_PATH}/heclib_f.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/ifconsol.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/libifcoremt.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/libifport.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/libmmt.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/libirc.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/svml_dispmt.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/ifmodintr.lib
        ${INTEL_FORTRAN_COMPILER_LIB_PATH}/ifwin.lib
    )
    ELSE()
        SET(HEC_DSS_ALL_STATIC_LIB
        ${HEC_DSS_LIBS_PATH}/heclib.a
        gfortran
        z
    )
    ENDIF(MSVC)
ENDIF(ENABLE_HEC_DSS)

SET(WITH_HYDRAULIC_MODEL_SUPPORT TRUE CACHE BOOL "Enable Hydraulic Model support")
IF(WITH_HYDRAULIC_MODEL_SUPPORT)
    add_definitions(-DWITH_GMSH)
    SET(GMSH_INCLUDE_DIR CACHE PATH "Path to the include dir of gmsh")
    SET(GMSH_LIB CACHE FILEPATH "Path to the lib file of gmsh")

    SET(WITH_TELEMAC_SUPPORT TRUE CACHE BOOL "Enable TELEMAC Model support")
    IF(WITH_TELEMAC_SUPPORT)
        SET(TELEMAC_PATH CACHE PATH "Path to TELEMAC.")
        SET(TELEMAC_CONFIG_NAME CACHE STRING "Configuration name of TELEMAC.")
        SET(TELEMAC_CONFIG_FILE CACHE FILEPATH "Configuration file of TELEMAC.")
        SET(TELEMAC_PYTHON_PATH CACHE FILEPATH "Python interpreter path to use for TELEMAC.")
    ENDIF(WITH_TELEMAC_SUPPORT)

    SET(MDAL_INCLUDE_DIR CACHE PATH "Path to the include dir of MDAL")
    SET(MDAL_LIB CACHE FILEPATH "Path to the lib file of MDAL")

ENDIF(WITH_HYDRAULIC_MODEL_SUPPORT)

SET(ENABLE_ECCODES_READER TRUE CACHE BOOL "Enable ecCodes reader for grib format")

IF(ENABLE_ECCODES_READER)
    SET(HAS_ECCODES TRUE)
    SET(ECCODES_INCLUDE_DIR CACHE PATH "Path to ecCodes headers.")
    SET(ECCODES_LIB CACHE FILEPATH "Path to ecCodes lib.")
ENDIF(ENABLE_ECCODES_READER)


set(QT_MIN_VERSION 5.14.0)
set(QT_VERSION_BASE "Qt5")
set(QT_VERSION_BASE_LOWER "qt5")
set(HAS_KDE_QT5_PDF_TRANSFORM_FIX FALSE CACHE BOOL "Using KDE's Qt 5.15 fork with the PDF brush transform fix")
set(HAS_KDE_QT5_SMALL_CAPS_FIX FALSE CACHE BOOL "Using KDE's Qt 5.15 fork with the QFont::SmallCaps fix")
set(HAS_KDE_QT5_FONT_STRETCH_FIX FALSE CACHE BOOL "Using KDE's Qt 5.15 fork with the QFont stretch fix")

FIND_PACKAGE(NetCDF REQUIRED)

FIND_PACKAGE(Qt5 COMPONENTS Core Gui Widgets Network Xml Svg Concurrent Test UiTools Sql REQUIRED)

get_target_property(QMAKE_EXECUTABLE ${QT_VERSION_BASE}::qmake LOCATION)

# Password helper
find_package(QtKeychain REQUIRED)
# Master password hash and authentication encryption
FIND_PACKAGE(QCA REQUIRED)
# Check for runtime dependency of qca-ossl plugin
if(NOT MSVC)
  include(QCAMacros)
  FIND_QCAOSSL_PLUGIN_CPP(ENABLE_TESTS)
endif()

IF(WITH_GUI)
    IF (WITH_QTWEBKIT)
      FIND_PACKAGE(Qt5 COMPONENTS WebKit WebKitWidgets REQUIRED)
    ENDIF(WITH_QTWEBKIT)

    SET(QWT_LIB CACHE FILEPATH "Path to qwt LIB")
    SET(QWT_INCLUDE CACHE PATH "Path to qwt headers")
ENDIF(WITH_GUI)


#############################################################
# Python

SET(WITH_BINDINGS TRUE CACHE BOOL "Enable python binding.")

set(MIN_PYTHON_VERSION "3.7")
set(Python_FIND_FRAMEWORK "LAST")

if(PYTHON_DIR)
    set(Python_FIND_STRATEGY "LOCATION")
        set(Python_ROOT_DIR "${PYTHON_DIR}")
endif()

if (WITH_BINDINGS)
  find_package(Python ${MIN_PYTHON_VERSION} REQUIRED COMPONENTS Interpreter Development)
else()
  find_package(Python ${MIN_PYTHON_VERSION} REQUIRED COMPONENTS Interpreter)
endif()

message("-- Found Python executable: ${Python_EXECUTABLE} (version ${Python_VERSION})")
message("-- Python library: ${Python_LIBRARIES}")
message("-- Python site-packages: ${Python_SITEARCH}")

if (WITH_BINDINGS)
  set(PYTHON_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/python)
  set (REOS_PYTHON_OUTPUT_DIRECTORY ${PYTHON_OUTPUT_DIRECTORY}/reos)

  # python support: check for interpreter, sip, pyqt5
  find_package(SIP REQUIRED)
  if(BUILD_WITH_QT6)
    find_package(PyQt6 REQUIRED)
    set(PYQT_FOUND ${PYQT6_FOUND})
    set(PYQT_SIP_FLAGS ${PYQT6_SIP_FLAGS})
    set(PYQT_SIP_DIR ${PYQT6_SIP_DIR})
    set(PYQT_SIP_IMPORT ${PYQT6_SIP_IMPORT})
    set(PYQT_VERSION_STR ${PYQT6_VERSION_STR})
    set(PYQT_MOD_DIR ${PYQT6_MOD_DIR})
  else()
    find_package(PyQt5 REQUIRED)
    set(PYQT_FOUND ${PYQT5_FOUND})
    set(PYQT_SIP_FLAGS ${PYQT5_SIP_FLAGS})
    set(PYQT_SIP_DIR ${PYQT5_SIP_DIR})
    set(PYQT_SIP_IMPORT ${PYQT5_SIP_IMPORT})
    set(PYQT_VERSION_STR ${PYQT5_VERSION_STR})
    set(PYQT_MOD_DIR ${PYQT5_MOD_DIR})
  endif()
  separate_arguments(PYQT_SIP_FLAGS) # convert space separated values to a list

  #find_package(Qsci REQUIRED)
  include(PythonMacros)
  include(PyQtMacros)
  include(SIPMacros)

  set(SIP_INCLUDES ${PYQT_SIP_DIR} ${CMAKE_SOURCE_DIR}/python)
  set(SIP_CONCAT_PARTS 10)

  set(Python_SITEARCH ${CMAKE_INSTALL_PREFIX}/python)

  include_directories(SYSTEM ${QT_INCLUDE_DIR})

endif()

ADD_SUBDIRECTORY(images)
ADD_SUBDIRECTORY(src)
IF(WITH_GUI)
    ADD_SUBDIRECTORY(i18n)
ENDIF(WITH_GUI)

if (WITH_BINDINGS)
    ADD_SUBDIRECTORY(python)
endif()

# testing
IF (ENABLE_TESTS)

  set(COMEPHORE_TESTS_ENABLE FALSE CACHE BOOL "Enable tests for Comephore gridded rainfall, some specific files are necessary.")
  set(ERA5_TESTS_ENABLE FALSE CACHE BOOL "Enable tests for ERA5 gridded data, some specific files are necessary.")
  set(METEOFRANCEAPI_TESTS_ENABLE FALSE CACHE BOOL "Enable tests for Meteo France, a file containing a API key is necessary")
  set(INTERN_PYTHON_TESTS_ENABLE FALSE CACHE BOOL "Enable tests related calling python from Reos")

  include (CTest)
  add_definitions(-DENABLE_TESTS)
  enable_testing()
  add_subdirectory(Tests)

ENDIF(ENABLE_TESTS)

