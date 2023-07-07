export DEBIAN_FRONTEND=noninteractive

apt-get -y install bison build-essential ca-certificates ccache cmake cmake-curses-gui dh-python doxygen expect flex flip gdal-bin git graphviz grass-dev libexiv2-dev libexpat1-dev libfcgi-dev libgdal-dev libgeos-dev libgsl-dev libpdal-dev libpq-dev libproj-dev libprotobuf-dev libqca-qt5-2-dev libqca-qt5-2-plugins libqscintilla2-qt5-dev libqt5opengl5-dev libqt5serialport5-dev libqt5sql5-sqlite libqt5svg5-dev libqt5webkit5-dev libqt5xmlpatterns5-dev libqwt-qt5-dev libspatialindex-dev libspatialite-dev libsqlite3-dev libsqlite3-mod-spatialite libyaml-tiny-perl libzip-dev libzstd-dev lighttpd locales ninja-build ocl-icd-opencl-dev opencl-headers pandoc pdal pkg-config poppler-utils protobuf-compiler pyqt5-dev pyqt5-dev-tools pyqt5.qsci-dev python3-all-dev python3-autopep8 python3-dev python3-gdal python3-jinja2 python3-lxml python3-mock python3-nose2 python3-owslib python3-plotly python3-psycopg2 python3-pygments python3-pyproj python3-pyqt5 python3-pyqt5.qsci python3-pyqt5.qtmultimedia python3-pyqt5.qtpositioning python3-pyqt5.qtsql python3-pyqt5.qtsvg python3-pyqt5.qtwebkit python3-pyqtbuild python3-sip python3-termcolor python3-yaml qt3d-assimpsceneimport-plugin qt3d-defaultgeometryloader-plugin qt3d-gltfsceneio-plugin qt3d-scene2d-plugin qt3d5-dev qtbase5-dev qtbase5-private-dev qtkeychain-qt5-dev qtmultimedia5-dev qtpositioning5-dev qttools5-dev qttools5-dev-tools sip-tools spawn-fcgi xauth xfonts-100dpi xfonts-75dpi xfonts-base xfonts-scalable xvfb

git clone https://github.com/vcloarec/QGIS.git
cd QGIS
git checkout forLekanMaster
cd ..

mkdir QGIS_building
mkdir QGIS_built

cd QGIS_building

cmake   -S $env:QGIS_SRC \
		-B . \
		-D WITH_QSPATIALITE=TRUE \
		-D WITH_SERVER=FALSE \
		-D SERVER_SKIP_ECW=TRUE \
		-D WITH_3D=TRUE \
		-D WITH_PDAL=FALSE \
		-D WITH_HANA=FALSE \
		-D WITH_GRASS=FALSE \
		-D WITH_GRASS7=FALSE \
		-D WITH_BINDINGS=TRUE \
		-D WITH_ORACLE=FALSE \
		-D WITH_CUSTOM_WIDGETS=FALSE \
		-D WITH_QTWEBKIT=FALSE \
		-D WITH_PY_COMPILE=FALSE \
		-D ENABLE_TESTS=FALSE \
		-D CMAKE_BUILD_TYPE=$BUILDCONF \
		-D CMAKE_CONFIGURATION_TYPES=$BUILDCONF \
		-D Qt5_DIR=$OSGEO_DIR/apps/qt5/lib/cmake/Qt5 \
		-D CMAKE_INSTALL_PREFIX=$env:QGIS_BUILT \
		-D CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS=TRUE \

cmake --build .  --config Release -j
cmake --install .

cd ..
rm -r QGIS_building
