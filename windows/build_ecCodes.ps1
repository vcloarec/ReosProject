$starter_path = Get-Location

$OSGEO_DIR=$env:OSGEO4W_ROOT
$ECCODES_INSTALL=$env:ECCODES_ROOT
$ECCODES_CMAKE_INSTALL=$ECCODES_INSTALL.Replace('\', '/')
$OSGEO_CMAKE_PREFIX=$OSGEO_DIR.Replace('\', '/')
$LIBAEC_CMAKE_DIR=(Join-Path $OSGEO_DIR "lib/cmake/libaec").Replace('\', '/')

Write-Host "============================= Download ecCodes source:"
$url="https://confluence.ecmwf.int/download/attachments/45757960/eccodes-2.47.0-Source.tar.gz"

$eccodes_archive = Join-Path $starter_path "eccodes-2.47.0-Source.tar.gz"
$eccodes_source_dir = Join-Path $starter_path "eccodes-2.47.0-Source"

Invoke-WebRequest -Uri $url -OutFile $eccodes_archive

if (Test-Path $eccodes_source_dir) {
    Remove-Item $eccodes_source_dir -Recurse -Force
}

tar -xzf $eccodes_archive -C $starter_path

mkdir -p $eccodes_source_dir/build
mkdir -p $ECCODES_INSTALL
ls $OSGEO_DIR\lib\cmake
Write-Host "=== libaec CMake directory:"
$LIBAEC_CMAKE_DIR
ls $LIBAEC_CMAKE_DIR

cd $eccodes_source_dir/build 
cmake -S .. `
  -B $eccodes_source_dir/build `
  -G "Visual Studio 17 2022" -A x64 `
  -DBUILD_SHARED_LIBS=ON `
  -DENABLE_PKGCONFIG=OFF
  "-DCMAKE_INSTALL_PREFIX:PATH=$ECCODES_CMAKE_INSTALL" `
  -DCMAKE_BUILD_TYPE=Release `
  "-DCMAKE_PREFIX_PATH:PATH=$OSGEO_CMAKE_PREFIX" `
  -DENABLE_NETCDF=OFF `
  -DENABLE_FORTRAN=OFF `
  -DPRODUCT_BUFR=OFF `
  -DEXAMPLES=OFF `
  "-Dlibaec_DIR:PATH=$LIBAEC_CMAKE_DIR"

cmake --build .  
cmake --install .

ls $ECCODES_INSTALL

cd ..

Remove-Item $eccodes_source_dir/build -Recurse
