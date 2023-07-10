git clone https://github.com/vcloarec/ReosProject.git

cd ReosProject

REOS_INSTALL=$PWD

mkdir build;
cd build
cmake   -S .. \
		-B . \
        -D BUILD_GMOCK=ON \
        -D BUILD_TESTING=ON \
        -D CMAKE_INSTALL_PREFIX=$REOS_INSTALL \
        -D ENABLE_TESTS=TRUE \
        -D QGIS_INCLUDE_DIR=$QGIS_INSTALL/include/qgis \
        -D QGIS_3D_LIB=$QGIS_INSTALL/lib/libqgis_3d.so \
        -D QGIS_ANALYSIS_LIB=$QGIS_INSTALL/lib/libqgis_analysis.so \
        -D QGIS_APP_LIB=$QGIS_INSTALL/lib/libqgis_app.so \
        -D QGIS_CORE_LIB=$QGIS_INSTALL/lib/libqgis_core.so \
        -D QGIS_GUI_LIB=$QGIS_INSTALL/lib/libqgis_gui.so \
        -D QGIS_PROVIDERS_PATH=$QGIS_INSTALL/plugins \
        -D QGIS_APP_INCLUDE=$QGIS_INSTALL/app_src \
        -D GMSH_INCLUDE_DIR=$GMSH_INSTALL/include \
        -D GMSH_LIB=$GMSH_INSTALL/lib/libgmsh.so \
        -D INSTALL_GTEST=ON \
        -D MDAL_INCLUDE_DIR=$MDAL_INSTALL/include \
        -D MDAL_LIB=$MDAL_INSTALL/lib/libmdal.so \
        -D WITH_QTWEBKIT=FALSE \
		-D ENABLE_HECRAS=TRUE \
        -D ENABLE_HEC_DSS=TRUE \
        -D HEC_DSS_INCLUDE=$HEC_DSS_INSTALL/headers \
        -D HEC_DSS_LIBS_PATH=$HEC_DSS_INSTALL/ \
        -D QWT_INCLUDE=/usr/include/qwt \
        -D QWT_LIB=/usr/lib/libqwt-qt5.so

cmake --build .  --config Release -j20
cmake --install .

cd ..
rm -r build
cd ..
rm -r ReosProject




