#!/usr/bin/env bash
# Local build helper mirroring worker/build_reos.sh but targeting the
# workspace mounted inside the dev container.
set -euo pipefail

SRC_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BUILD_DIR="${SRC_DIR}/build"
INSTALL_DIR="${INSTALL_DIR:-${SRC_DIR}/install}"

: "${QGIS_INSTALL:=/qgis_built}"
: "${ECCODES_INSTALL:=/eccodes_built}"

HDF5_INCLUDE_DIR=/usr/include/hdf5/serial
HDF5_C_LIBRARY=/usr/lib/x86_64-linux-gnu/hdf5/serial/libhdf5.so

mkdir -p "${BUILD_DIR}"

cmake -S "${SRC_DIR}" -B "${BUILD_DIR}" -G Ninja \
    -D CMAKE_BUILD_TYPE=RelWithDebInfo \
    -D CMAKE_EXPORT_COMPILE_COMMANDS=ON \
    -D BUILD_GMOCK=ON \
    -D BUILD_TESTING=ON \
    -D CMAKE_INSTALL_PREFIX="${INSTALL_DIR}" \
    -D ENABLE_TESTS=TRUE \
    -D INSTALL_GTEST=ON \
    -D QGIS_INCLUDE_DIR="${QGIS_INSTALL}/include/qgis" \
    -D QGIS_3D_LIB="${QGIS_INSTALL}/lib/libqgis_3d.so" \
    -D QGIS_ANALYSIS_LIB="${QGIS_INSTALL}/lib/libqgis_analysis.so" \
    -D QGIS_APP_LIB="${QGIS_INSTALL}/lib/libqgis_app.so" \
    -D QGIS_CORE_LIB="${QGIS_INSTALL}/lib/libqgis_core.so" \
    -D QGIS_GUI_LIB="${QGIS_INSTALL}/lib/libqgis_gui.so" \
    -D QGIS_PROVIDERS_PATH="${QGIS_INSTALL}/plugins" \
    -D QGIS_APP_INCLUDE="${QGIS_INSTALL}/app_src" \
    -D ENABLE_ECCODES_READER=TRUE \
    -D ECCODES_INCLUDE_DIR="${ECCODES_INSTALL}/include" \
    -D ECCODES_LIB="${ECCODES_INSTALL}/lib/libeccodes.so" \
    -D HDF5_INCLUDE_DIRS="${HDF5_INCLUDE_DIR}" \
    -D HDF5_C_LIBRARY="${HDF5_C_LIBRARY}" \
    -D WITH_QTWEBKIT=FALSE \
    -D WITH_3D=FALSE \
    -D WITH_BINDINGS=TRUE \
    -D WITH_GUI=FALSE \
    -D WITH_HYDRAULIC_MODEL_SUPPORT=FALSE \
    -D WITH_TELEMAC_SUPPORT=FALSE \
    -D ENABLE_HECRAS=FALSE \
    -D ENABLE_HEC_DSS=FALSE \
    -D QWT_INCLUDE=/usr/include/qwt \
    -D QWT_LIB=/usr/lib/libqwt-qt5.so

cmake --build "${BUILD_DIR}" --config RelWithDebInfo -j "$(nproc)"

if [[ "${1:-}" == "--install" ]]; then
    cmake --install "${BUILD_DIR}"
fi
