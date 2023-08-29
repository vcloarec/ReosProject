$starter_path = Get-Location

$GMSH_INSTALL=$env:GMSH_BUILT
$QGIS_SRC=$env:QGIS_SRC
$REOS_INSTALL=$env:REOS_INSTALL
$OSGEO_DIR=$env:OSGEO4W_ROOT
$QGIS_INSTALL=$env:QGIS_BUILT
$REOS_BUILD=$env:REOS_BUILDING

Write-Host "============================= dependencies directory:"
Write-Host "=== OSGEO:"
$OSGEO_DIR
ls $OSGEO_DIR
ls $OSGEO_DIR\apps
ls $OSGEO_DIR\apps\Python39

Write-Host "=== MDAL"
$env:MDAL_ROOT
ls $env:MDAL_ROOT\lib

Write-Host "=== Qwt folder:"
$QWT_INCLUDE=Join-Path $OSGEO_DIR "apps/Qt5/include/qwt6"
$QWT_INCLUDE
ls $QWT_INCLUDE

Write-Host "=== QGIS:"
$QGIS_INSTALL
ls $QGIS_INSTALL

Write-Host "=== QGIS sources:"
$QGIS_SRC
ls $QGIS_SRC

Write-Host "=== GMSH:"
$GMSH_INSTALL
ls $GMSH_INSTALL
ls $GMSH_INSTALL\lib

Write-Host "=== FORTRAN COMPILER:"
$env:FORTRAN_COMPILER_PATH
ls $env:FORTRAN_COMPILER_PATH
ls $env:FORTRAN_COMPILER_PATH\windows
ls $env:FORTRAN_COMPILER_PATH\windows\compiler
ls $env:FORTRAN_COMPILER_PATH\windows\compiler\lib
ls $env:FORTRAN_COMPILER_PATH\windows\compiler\lib\intel64_win

if (Test-Path -Path $REOS_BUILD)
{
    "Path $REOS_BUILD exists"
}else
{
    md $REOS_BUILD
}

cd $REOS_BUILD

Write-Host "===================================== Current PATH:"
$env:Path


cmake   -S $env:REOS_SOURCE `
		-B . `
        -D BUILD_GMOCK=ON `
        -D BUILD_TESTING=ON `
        -D CMAKE_INSTALL_PREFIX=$REOS_INSTALL `
        -D ENABLE_TESTS=TRUE `
        -D GDAL_INCLUDE_DIR=$env:GDAL_ROOT/include `
        -D GDAL_LIBRARY=$env:GDAL_ROOT/lib/gdal_i.lib `
        -D QGIS_INCLUDE_DIR=$QGIS_INSTALL/include `
        -D QGIS_3D_LIB=$QGIS_INSTALL/lib/qgis_3d.lib `
        -D QGIS_ANALYSIS_LIB=$QGIS_INSTALL/lib/qgis_analysis.lib `
        -D QGIS_APP_LIB=$QGIS_INSTALL//lib/qgis_app.lib `
        -D QGIS_CORE_LIB=$QGIS_INSTALL/lib/qgis_core.lib `
        -D QGIS_GUI_LIB=$QGIS_INSTALL/lib/qgis_gui.lib `
        -D QGIS_PROVIDERS_PATH=$QGIS_INSTALL/plugins `
        -D GMSH_INCLUDE_DIR=$GMSH_INSTALL/include `
        -D QGIS_APP_INCLUDE=$QGIS_SRC/src/app `
        -D GMSH_LIB=$GMSH_INSTALL/lib/gmsh.lib `
		-D PYTHON_DIR=$OSGEO_DIR/apps/Python39 `
        -D GTest_DIR=GTest_DIR-NOTFOUND `
        -D INSTALL_GTEST=ON `
        -D MDAL_INCLUDE_DIR=$env:MDAL_ROOT/include `
        -D MDAL_LIB=$env:MDAL_ROOT/lib/mdal.lib `
        -D Qt5_DIR=$OSGEO_DIR/apps/Qt5/lib/cmake/Qt5 `
        -D QT_QMAKE_EXECUTABLE=$OSGEO_DIR/apps/Qt5/bin/qmake `
        -D QCA_INCLUDE_DIR=$OSGEO_DIR/apps/Qt5/include/QtCrypto `
        -D QCA_LIBRARY=$OSGEO_DIR/apps/Qt5/qca-qt5.lib `
        -D QSCISCINTILLA_INCLUDE_DIR:PATH= `
        -D QTKEYCHAIN_INCLUDE_DIR=$OSGEO_DIR/apps/Qt5/include/qt5keychain `
        -D QTKEYCHAIN_LIBRARY=$OSGEO_DIR/apps/Qt5/lib/qt5keychain.lib `
        -D QWT_INCLUDE=$OSGEO_DIR/apps/Qt5/include/qwt6 `
        -D QWT_LIB=$OSGEO_DIR/apps/Qt5/lib/qwt.lib `
        -D WITH_QTWEBKIT:BOOL=FALSE `
		-D ENABLE_HECRAS=TRUE `
        -D ENABLE_HEC_DSS=TRUE `
        -D HEC_DSS_INCLUDE=$env:REOS_SOURCE/external/heclib_win/heclib-7-IR-0-win-x86_64/headers `
        -D HEC_DSS_LIBS_PATH=$env:REOS_SOURCE/external/heclib_win/heclib-7-IR-0-win-x86_64/Release64 `
        -D INTEL_FORTRAN_COMPILER_LIB_PATH=$env:FORTRAN_COMPILER_PATH\windows\compiler\lib\intel64_win `
        -D TELEMAC_PATH=$REOS_INSTALL\apps\telemac `
        -D TELEMAC_CONFIG_FILE=$REOS_INSTALL\apps\telemac\configs\systel.lekan.cfg `
        -D TELEMAC_CONFIG_NAME=win `
        -D TELEMAC_PYTHON_PATH=$REOS_INSTALL\apps\python

cmake --build .  --config $env:BUILD_TYPE --parallel

#cmake -G Ninja -DCMAKE_BUILD_TYPE=$env:BUILD_TYPE

if ($LastExitCode -ne 0) {
Write-Host "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to build Reos"
	exit $LastExitCode
	}

rm -r $REOS_INSTALL
cmake --install .

if ($LastExitCode -ne 0) {
Write-Host "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to Install Reos"
	exit $LastExitCode
	}

& "$env:REOS_SOURCE\windows\copyDepFiles.bat"

exit 0
