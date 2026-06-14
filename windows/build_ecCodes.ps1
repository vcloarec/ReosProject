$starter_path = Get-Location

$ECCODES_INSTALL=$env:ECCODES_ROOT

Write-Host "============================= Download ecCodes source:"
$url="https://confluence.ecmwf.int/download/attachments/45757960/eccodes-2.47.0-Source.tar.gz"

$eccodes_archive = Join-Path $starter_path "eccodes-2.47.0-Source.tar.gz"
$eccodes_source_dir = Join-Path $starter_path "eccodes-2.47.0-Source"

function Get-FirstExistingPath( [string[]]$candidates )
{
    foreach ( $candidate in $candidates )
    {
        if ( Test-Path $candidate )
        {
            return $candidate
        }
    }

    return $null
}

function Exit-IfFailed( [string]$message )
{
    if ( $LASTEXITCODE -ne 0 )
    {
        Write-Host $message
        exit $LASTEXITCODE
    }
}

$aec_include_dir = Get-FirstExistingPath @(
    ( Join-Path $env:OSGEO4W_ROOT "include/aec.h" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/include/aec.h" )
)
if ( $aec_include_dir )
{
    $aec_include_dir = Split-Path $aec_include_dir -Parent
}
$aec_library = Get-FirstExistingPath @(
    ( Join-Path $env:OSGEO4W_ROOT "lib/aec.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "lib/libaec.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/lib/aec.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/lib/libaec.lib" )
)
$aec_library_dir = $null
if ( $aec_library )
{
    $aec_library_dir = Split-Path $aec_library -Parent
}

$cmake_include_path = @(
    ( Join-Path $env:OSGEO4W_ROOT "include" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/include" )
) | Where-Object { Test-Path $_ } | Select-Object -Unique
$cmake_library_path = @(
    ( Join-Path $env:OSGEO4W_ROOT "lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/lib" )
) | Where-Object { Test-Path $_ } | Select-Object -Unique

Invoke-WebRequest -Uri $url -OutFile $eccodes_archive

if (Test-Path $eccodes_source_dir) {
    Remove-Item $eccodes_source_dir -Recurse -Force
}

tar -xzf $eccodes_archive -C $starter_path

mkdir -p $eccodes_source_dir/eccodes_src/build 

cd $eccodes_source_dir/eccodes_src/build 

$cmake_args = @(
    "-DCMAKE_INSTALL_PREFIX=$ECCODES_INSTALL",
    "-DCMAKE_BUILD_TYPE=Release",
    "-DCMAKE_PREFIX_PATH=$env:OSGEO4W_ROOT",
    "-DENABLE_NETCDF=OFF",
    "-DENABLE_FORTRAN=OFF",
    "-DPRODUCT_BUFR=OFF",
    "-DEXAMPLES=OFF"
)

if ( $aec_include_dir )
{
    $cmake_args += "-DAEC_INCLUDE_DIR=$aec_include_dir"
}

if ( $aec_library )
{
    $cmake_args += "-DAEC_LIBRARY=$aec_library"
}

if ( $cmake_include_path.Count -gt 0 )
{
    $cmake_args += "-DCMAKE_INCLUDE_PATH=$($cmake_include_path -join ';')"
}

if ( $cmake_library_path.Count -gt 0 )
{
    $cmake_args += "-DCMAKE_LIBRARY_PATH=$($cmake_library_path -join ';')"
}

if ( $aec_library_dir )
{
    Write-Host "Using AEC library: $aec_library"
}

if ( $aec_include_dir )
{
    Write-Host "Using AEC include directory: $aec_include_dir"
}

cmake @cmake_args $eccodes_source_dir
Exit-IfFailed "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to configure ecCodes"

cmake --build .  
Exit-IfFailed "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to build ecCodes"

cmake --install .
Exit-IfFailed "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to install ecCodes"

ls $ECCODES_INSTALL

cd ..
