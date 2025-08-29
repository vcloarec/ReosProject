export DEBIAN_FRONTEND=noninteractive

if [ -z "$1" ]; then
    QGIS_SRC=$PWD
else
    QGIS_SRC=$1
fi

mkdir qgis_building
cd qgis_building

rm -f $QGIS_SRC/resources/cpt-city-qgis-min/*

cmake   -S $QGIS_SRC \
		-B . \
		-D WITH_QSPATIALITE=TRUE \
		-D WITH_SERVER=FALSE \
		-D WITH_GUI=FALSE \
		-D SERVER_SKIP_ECW=TRUE \
        -D WITH_DESKTOP=FALSE \
        -D WITH_PDAL=FALSE \
		-D WITH_EPT=FALSE \
		-D WITH_COPC=TRUE \
		-D WITH_DRACO=FALSE \
		-D WITH_3D=FALSE \
		-D WITH_PDAL=FALSE \
		-D WITH_HANA=FALSE \
		-D WITH_GRASS=FALSE \
		-D WITH_GRASS7=FALSE \
		-D WITH_BINDINGS=TRUE \
		-D WITH_ORACLE=FALSE \
		-D WITH_CUSTOM_WIDGETS=FALSE \
		-D WITH_QTWEBKIT=FALSE \
		-D WITH_PY_COMPILE=FALSE \
		-D WITH_QTSERIALPORT=FALSE \
		-D WITH_QT5SERIALPORT=FALSE \
		-D ENABLE_TESTS=FALSE \
		-D CMAKE_BUILD_TYPE=Release \
		-D CMAKE_CONFIGURATION_TYPES=Release \
		-D CMAKE_INSTALL_PREFIX=/app/qgis_built \
		-D CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS=TRUE \

cmake --build .  --config Release -j20
cmake --install .

cd ..
rm -r qgis_building

