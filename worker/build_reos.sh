#!/bin/sh

set -e

export DEBIAN_FRONTEND=noninteractive

if [ -z "$1" ]; then
    REOS_SRC=$PWD
else
    REOS_SRC=$1
fi

REOS_INSTALL=$REOS_DIR

QGIS_INSTALL=/qgis_built

ECCODE_INSTALL=/eccodes_built

HDF5_INCLUDE_DIR=/usr/include/hdf5/serial
HDF5_C_LIBRARY=/usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.so

test -f "$HDF5_INCLUDE_DIR/hdf5.h"
test -f "$HDF5_C_LIBRARY"

echo "Using HDF5 include dir: $HDF5_INCLUDE_DIR"
echo "Using HDF5 library: $HDF5_C_LIBRARY"

mkdir reos_building
cd reos_building
cmake   -S $REOS_SRC \
		-B . \
        -D BUILD_GMOCK=ON \
        -D BUILD_TESTING=ON \
        -D CMAKE_INSTALL_PREFIX=/app/reos_built \
        -D ENABLE_TESTS=TRUE \
        -D QGIS_INCLUDE_DIR=$QGIS_INSTALL/include/qgis \
        -D QGIS_3D_LIB=$QGIS_INSTALL/lib/libqgis_3d.so \
        -D QGIS_ANALYSIS_LIB=$QGIS_INSTALL/lib/libqgis_analysis.so \
        -D QGIS_APP_LIB=$QGIS_INSTALL/lib/libqgis_app.so \
        -D QGIS_CORE_LIB=$QGIS_INSTALL/lib/libqgis_core.so \
        -D QGIS_GUI_LIB=$QGIS_INSTALL/lib/libqgis_gui.so \
        -D QGIS_PROVIDERS_PATH=$QGIS_INSTALL/plugins \
        -D QGIS_APP_INCLUDE=$QGIS_INSTALL/app_src \
        -D ENABLE_ECCODES_READER=TRUE \
        -D ECCODES_INCLUDE_DIR=$ECCODE_INSTALL/include \
        -D ECCODES_LIB=$ECCODE_INSTALL/lib/libeccodes.so \
        -D HDF5_INCLUDE_DIRS=$HDF5_INCLUDE_DIR \
        -D HDF5_C_LIBRARY=$HDF5_C_LIBRARY \
        -D INSTALL_GTEST=FALSE \
        -D WITH_QTWEBKIT=FALSE \
		-D ENABLE_HECRAS=FALSE \
        -D ENABLE_HEC_DSS=FALSE \
        -D ENABLE_TESTS=FALSE \
        -D WITH_3D=FALSE \
        -D WITH_BINDINGS=TRUE \
        -D WITH_GUI=FALSE \
        -D WITH_HYDRAULIC_MODEL_SUPPORT=FALSE \
        -D WITH_TELEMAC_SUPPORT=FALSE \
        -D WITH_QTWEBKIT=FALSE \
        -D HEC_DSS_INCLUDE=$HEC_DSS_INSTALL/headers \
        -D HEC_DSS_LIBS_PATH=$HEC_DSS_INSTALL/ \
        -D QWT_INCLUDE=/usr/include/qwt \
        -D QWT_LIB=/usr/lib/libqwt-qt5.so

cmake --build .  --config Release -j20
cmake --install .

cd ..
rm -r -f  reos_building





