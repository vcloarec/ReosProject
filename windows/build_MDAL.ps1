$starter_path = Get-Location

$OSGEO_DIR=$env:OSGEO4W_ROOT
$MDAL_DIR=$env:MDAL_ROOT

Write-Host "============================= dependencies directory:"
Write-Host "=== OSGEO:"
$OSGEO_DIR
ls $OSGEO_DIR

$mdal_source=Join-Path $starter_path MDAL

md MDAL_building
cd MDAL_building

Write-Host "============================= MDAL will be installed in the following folder:"
$MDAL_DIR
md $MDAL_DIR

Write-Host "===================================== Current PATH:"
$env:Path

cmake   -S $mdal_source `
		-B . `
        -D BUILD_EXTERNAL_DRIVERS=FALSE `
        -D BUILD_PLY=TRUE `
        -D BUILD_SHARED=TRUE `
        -D BUILD_STATIC=FALSE `
        -D BUILD_TESTING=FALSE `
        -D BUILD_TOOLS=FALSE `
        -D CMAKE_BUILD_TYPE:STRING=Release `
        -D CMAKE_INSTALL_PREFIX=$MDAL_DIR `
        -D ENABLE_COVERAGE:BOOL=FALSE `
        -D ENABLE_TESTS=FALSE `
        -D INSTALL_GTEST=FALSE `
        -D WITH_GDAL=TRUE `
        -D WITH_HDF5=TRUE `
        -D WITH_NETCDF=TRUE `
        -D WITH_SQLITE3=TRUE `
        -D WITH_XML=TRUE `

cmake --build .  --config Release

cmake --install .

Copy-Item '.\mdal\Release\mdal.lib' $MDAL_DIR\lib\mdal.lib

cd ..

Remove-Item MDAL_building -Recurse

