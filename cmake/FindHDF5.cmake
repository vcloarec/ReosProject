# - Find HDF5
# Find the native HDF5 includes and library
#
# This module returns these variables for the rest of the project to use.
#
#  HDF5_FOUND          - True if HDF5 found including required interfaces (see below)
#  HDF5_LIBRARIES      - All HDF5 related libraries
#  HDF5_C_LIBRARIES    - HDF5 C libraries
#  HDF5_INCLUDE_DIRS   - All directories to include
#  HDF5_DEFINITIONS    - Compile definitions

IF (HDF5_INCLUDE_DIRS AND HDF5_C_LIBRARY)
  # Already in cache, be silent
  SET (HDF5_FIND_QUIETLY TRUE)
ENDIF ()

FIND_PATH (HDF5_INCLUDE_DIRS hdf5.h
           HINTS $ENV{HDF5_ROOT}/include
                 $ENV{OSGEO4W_ROOT}/include
                 $ENV{OSGEO4W_ROOT}/apps/gdal-dev/include
                 ${HDF5_ROOT}/include)

FIND_LIBRARY (HDF5_C_LIBRARY
              NAMES hdf5_C hdf5 libhdf5
              HINTS $ENV{HDF5_ROOT}/lib
                    $ENV{OSGEO4W_ROOT}/lib
                    $ENV{OSGEO4W_ROOT}/apps/gdal-dev/lib
                    ${HDF5_ROOT}/lib)

SET(HDF5_LIBRARIES ${HDF5_C_LIBRARY})
SET(HDF5_C_LIBRARIES ${HDF5_C_LIBRARY})
SET(HDF5_DEFINITIONS "")

INCLUDE (FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS (HDF5
  DEFAULT_MSG HDF5_C_LIBRARY HDF5_INCLUDE_DIRS)

MARK_AS_ADVANCED(HDF5_INCLUDE_DIRS HDF5_C_LIBRARY)
