$starter_path = Get-Location

$OSGEO_DIR=$env:OSGEO4W_ROOT
$ECCODES_INSTALL=$env:ECCODES_ROOT

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

cd $eccodes_source_dir/build 
cmake -S .. `
  -B $eccodes_source_dir/build `
  -DCMAKE_PREFIX_PATH=$OSGEO_DIR `
  -DCMAKE_INSTALL_PREFIX=$ECCODES_INSTALL `
  -DCMAKE_BUILD_TYPE=Release `
  -DENABLE_NETCDF=OFF `
  -DENABLE_FORTRAN=OFF `
  -DPRODUCT_BUFR=OFF `
  -DEXAMPLES=OFF `
  -Dlibaec_DIR=$OSGEO_DIR\lib\cmake\libaec

cmake --build .  
cmake --install .

ls $ECCODES_INSTALL

cd ..

Remove-Item MDAL_building -Recurse
