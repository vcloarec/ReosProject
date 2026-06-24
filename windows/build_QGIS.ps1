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
ls $OSGEO_DIR\bin
ls $OSGEO_DIR\include

Write-Host "=== Cygwin directory:"
$env:CYGWIN_ROOT
ls $env:CYGWIN_ROOT

Write-Host "===================================== Current PATH:"
$env:Path

$SDK_PATH=$(Get-Item "hklm:\SOFTWARE\Microsoft\Windows Kits\Installed Roots").GetValue("KitsRoot10")
$SDK_VERSION=$(Get-childItem -Name "hklm:\SOFTWARE\Microsoft\Windows Kits\Installed Roots" | Select-Object -last 1)

md $env:QGIS_BUILDING -Force | Out-Null
md $env:QGIS_BUILT -Force | Out-Null
cd $env:QGIS_BUILDING

cmake -S $env:QGIS_SRC `
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
      -D WITH_BINDINGS=TRUE `
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
      -D Python_EXECUTABLE=$OSGEO_DIR/apps/python312/python3.exe `
      -D CMAKE_PREFIX_PATH="$OSGEO_DIR;$OSGEO_DIR/apps/Qt5" `
      -D CMAKE_INSTALL_PREFIX=$env:QGIS_BUILT `
      -D CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS_NO_WARNINGS=TRUE

if ($LASTEXITCODE -ne 0) {
    Write-Error "CMake configure failed with exit code $LASTEXITCODE."
    exit 1
}
		
cmake --build .  --config Release
if ($LASTEXITCODE -ne 0) {
	Write-Error "QGIS build failed with exit code $LASTEXITCODE."
	exit 1
}
cmake --install .

cd ..

Remove-Item $env:QGIS_BUILDING -Recurse

Set-Location $starter_path
