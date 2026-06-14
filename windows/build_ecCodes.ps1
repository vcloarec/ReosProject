$starter_path = Get-Location

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


mkdir -p $eccodes_source_dir/eccodes_src/build 
ls -la 
cd $eccodes_source_dir/eccodes_src/build 
cmake -DCMAKE_INSTALL_PREFIX=$ECCODES_INSTALL \
-DCMAKE_BUILD_TYPE=Release \
-DENABLE_NETCDF=OFF \ 
-DENABLE_FORTRAN=OFF \
-DPRODUCT_BUFR=OFF \
-DEXAMPLES=OFF \
.$eccodes_source_dir/. && \

cmake --build .  
cmake --install .

ls $ECCODES_INSTALL

cd ..

Remove-Item MDAL_building -Recurse

