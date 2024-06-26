$starter_path = Get-Location

cd QGIS
$env:QGIS_SRC=Get-Location

$OSGEO_DIR=$env:OSGEO4W_ROOT
$BUILDCONF="Release"
$PYTHONHOME=Join-Path $OSGEO_DIR "apps\Python312"
Write-Host "=== Python HOME"
ls $PYTHONHOME

Write-Host "=== Start building QGIS ..."
Write-Host "=== Osgeo directory:"
$OSGEO_DIR
ls $OSGEO_DIR

Write-Host "=== Cygwin directory:"
$env:CYGWIN_ROOT
ls $env:CYGWIN_ROOT

Write-Host "===================================== Current PATH:"
$env:Path

$SDK_PATH=$(Get-Item "hklm:\SOFTWARE\Microsoft\Windows Kits\Installed Roots").GetValue("KitsRoot10")
$SDK_VERSION=$(Get-childItem -Name "hklm:\SOFTWARE\Microsoft\Windows Kits\Installed Roots" | Select-Object -last 1)

md $env:QGIS_BUILDING
md $env:QGIS_BUILT
cd $env:QGIS_BUILDING

cmake   -S $env:QGIS_SRC `
		-B . `
		-D CMAKE_CXX_FLAGS_${BUILDCONF^^}="/MD /Z7 /MP /Od /D NDEBUG" `
		-D CMAKE_EXE_LINKER_FLAGS=/machine:x64 `
		-D WITH_QSPATIALITE=TRUE `
		-D WITH_SERVER=FALSE `
		-D SERVER_SKIP_ECW=TRUE `
		-D WITH_3D=TRUE `
		-D WITH_PDAL=FALSE `
		-D WITH_HANA=FALSE `
		-D WITH_GRASS=FALSE `
		-D WITH_GRASS7=FALSE `
		-D WITH_BINDINGS=FALSE `
		-D WITH_ORACLE=FALSE `
		-D WITH_CUSTOM_WIDGETS=FALSE `
		-D WITH_QTWEBKIT=FALSE `
		-D WITH_PY_COMPILE=FALSE `
		-D WITH_DRACO=FALSE `
		-D ENABLE_TESTS=FALSE `
		-D SETUPAPI_LIBRARY=$SDK_PATH/Lib/$SDK_VERSION/um/x64/setupAPI.Lib `
		-D VERSION_LIBRARY=$SDK_PATH/Lib/$SDK_VERSION/um/x64/Version.Lib `
		-D FLEX_EXECUTABLE=$env:CYGWIN_ROOT/bin/flex.exe `
		-D BISON_EXECUTABLE=$env:CYGWIN_ROOT/bin/bison.exe `
		-D CMAKE_BUILD_TYPE=$BUILDCONF `
		-D CMAKE_CONFIGURATION_TYPES=$BUILDCONF `
		-D GDAL_LIBRARY=$env:GDAL_ROOT/lib/gdal_i.lib `
		-D GDAL_INCLUDE_DIR=$env:GDAL_ROOT/include `
		-D PROJ_INCLUDE_DIR=$OSGEO_DIR/include `
		-D GEOS_LIBRARY=$OSGEO_DIR/lib/geos_c.lib `
		-D SQLITE3_LIBRARY=$OSGEO_DIR/lib/sqlite3_i.lib `
		-D SPATIALITE_LIBRARY=$OSGEO_DIR/lib/spatialite_i.lib `
		-D SPATIALINDEX_LIBRARY=$OSGEO_DIR/lib/spatialindex-64.lib `
		-D Python_EXECUTABLE=$OSGEO_DIR/apps/python312/python3.exe `
		-D SIP_MODULE_EXECUTABLE=$PYTHONHOME/Scripts/sip-module.exe `
		-D PYUIC_PROGRAM=$PYTHONHOME/Scripts/pyuic5.exe `
		-D PYRCC_PROGRAM=$PYTHONHOME/Scripts/pyrcc5.exe `
		-D PYTHON_INCLUDE_PATH=$PYTHONHOME/include `
		-D QT_LIBRARY_DIR=$OSGEO_DIR/lib `
		-D QT_HEADERS_DIR=$OSGEO_DIR/apps/qt5/include `
		-D Qt5_DIR=$OSGEO_DIR/apps/qt5/lib/cmake/Qt5 `
		-D CMAKE_INSTALL_PREFIX=$env:QGIS_BUILT `
		-D CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS=TRUE `
		-D FCGI_INCLUDE_DIR=$OSGEO_DIR/include `
		-D FCGI_LIBRARY=$OSGEO_DIR/lib/libfcgi.lib `
		-D QCA_INCLUDE_DIR=$OSGEO_DIR/apps/Qt5/include/QtCrypto `
		-D QCA_LIBRARY=$OSGEO_DIR/apps/Qt5/lib/qca-qt5.lib `
		-D QWT_LIBRARY=$OSGEO_DIR/apps/Qt5/lib/qwt.lib `
		-D QSCINTILLA_LIBRARY=$OSGEO_DIR/apps/Qt5/lib/qscintilla2.lib
		
cmake --build .  --config Release
cmake --install .

cd ..

Remove-Item $env:QGIS_BUILDING -Recurse

Set-Location $starter_path
