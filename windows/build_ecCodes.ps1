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
    ( Join-Path $env:OSGEO4W_ROOT "include/libaec.h" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/include/aec.h" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/include/libaec.h" )
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

# libaec also ships a SZIP-compatibility library (sz); ecCodes links libaec::sz
$sz_library = Get-FirstExistingPath @(
    ( Join-Path $env:OSGEO4W_ROOT "lib/sz.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "lib/libsz.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "lib/szip.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/lib/sz.lib" ),
    ( Join-Path $env:OSGEO4W_ROOT "apps/gdal-dev/lib/libsz.lib" )
)

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
    "-DCMAKE_CONFIGURATION_TYPES=Release",
    "-DCMAKE_PREFIX_PATH=$env:OSGEO4W_ROOT",
    "-DENABLE_NETCDF=OFF",
    "-DENABLE_FORTRAN=OFF",
    "-DENABLE_PRODUCT_BUFR=OFF",
    "-DENABLE_EXAMPLES=OFF"
)

if ( $cmake_include_path.Count -gt 0 )
{
    $cmake_args += "-DCMAKE_INCLUDE_PATH=$($cmake_include_path -join ';')"
}

if ( $cmake_library_path.Count -gt 0 )
{
    $cmake_args += "-DCMAKE_LIBRARY_PATH=$($cmake_library_path -join ';')"
}

# ecCodes 2.47.0 uses find_package(libaec CONFIG) and ignores AEC_INCLUDE_DIR/
# AEC_LIBRARY.  Generate a minimal libaecConfig.cmake that wraps the OSGeo4W
# libraries so CMake's CONFIG-mode search succeeds.
if ( $aec_library )
{
    $libaec_cmake_dir = Join-Path $env:TEMP "libaec_config"
    New-Item -ItemType Directory -Force -Path $libaec_cmake_dir | Out-Null

    # Use sz.lib for the libaec::sz target when available; fall back to aec.lib
    $sz_lib_for_target = if ( $sz_library ) { $sz_library } else { $aec_library }

    $aec_lib_fwd = $aec_library      -replace '\\', '/'
    $sz_lib_fwd  = $sz_lib_for_target -replace '\\', '/'

    # Use the found include dir, or fall back to the standard OSGeo4W include path
    if ( $aec_include_dir ) {
        $aec_inc_fwd = $aec_include_dir -replace '\\', '/'
    } else {
        $fallback_inc = Join-Path $env:OSGEO4W_ROOT "include"
        $aec_inc_fwd  = $fallback_inc -replace '\\', '/'
    }

    $config_content = @"
# libaecConfig.cmake - auto-generated wrapper around the OSGeo4W AEC library
set(libaec_VERSION "1.1.4")
set(libaec_FOUND TRUE)

if(NOT TARGET libaec::aec)
    add_library(libaec::aec UNKNOWN IMPORTED)
    set_target_properties(libaec::aec PROPERTIES
        IMPORTED_LOCATION "$aec_lib_fwd"
        INTERFACE_INCLUDE_DIRECTORIES "$aec_inc_fwd")
endif()

if(NOT TARGET libaec::sz)
    add_library(libaec::sz UNKNOWN IMPORTED)
    set_target_properties(libaec::sz PROPERTIES
        IMPORTED_LOCATION "$sz_lib_fwd"
        INTERFACE_INCLUDE_DIRECTORIES "$aec_inc_fwd")
endif()

set(libaec_LIBRARIES libaec::aec libaec::sz)
set(libaec_INCLUDE_DIRS "$aec_inc_fwd")
"@

    $version_content = @"
set(PACKAGE_VERSION "1.1.4")
if(PACKAGE_VERSION VERSION_LESS PACKAGE_FIND_VERSION)
    set(PACKAGE_VERSION_COMPATIBLE FALSE)
else()
    set(PACKAGE_VERSION_COMPATIBLE TRUE)
    if(PACKAGE_FIND_VERSION STREQUAL PACKAGE_VERSION)
        set(PACKAGE_VERSION_EXACT TRUE)
    endif()
endif()
"@

    $config_content  | Out-File -FilePath (Join-Path $libaec_cmake_dir "libaecConfig.cmake")        -Encoding ascii
    $version_content | Out-File -FilePath (Join-Path $libaec_cmake_dir "libaecConfigVersion.cmake") -Encoding ascii

    $cmake_args += "-Dlibaec_DIR=$libaec_cmake_dir"
    Write-Host "Created libaecConfig.cmake for ecCodes CONFIG-mode search: $libaec_cmake_dir"
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

cmake --build . --config Release
Exit-IfFailed "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to build ecCodes"

cmake --install . --config Release
Exit-IfFailed "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      Unable to install ecCodes"

ls $ECCODES_INSTALL

cd ..
