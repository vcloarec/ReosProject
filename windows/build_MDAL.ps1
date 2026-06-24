$starter_path = Get-Location

$OSGEO_DIR=$env:OSGEO4W_ROOT
$MDAL_DIR=$env:MDAL_ROOT

Write-Host "============================= dependencies directory:"
Write-Host "=== OSGEO:"
$OSGEO_DIR
ls $OSGEO_DIR

$mdal_source=Join-Path $starter_path MDAL

md MDAL_building -Force | Out-Null
cd MDAL_building

Write-Host "============================= MDAL will be installed in the following folder:"
$MDAL_DIR
md $MDAL_DIR -Force | Out-Null

Write-Host "===================================== Current PATH:"
$env:Path

cmake   -S $mdal_source `
		-B . `
        -D CMAKE_PREFIX_PATH="$OSGEO_DIR" `
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

$mdalLibDestination = Join-Path $MDAL_DIR "lib\mdal.lib"
New-Item -ItemType Directory -Path (Join-Path $MDAL_DIR "lib") -Force | Out-Null

if ( -not ( Test-Path $mdalLibDestination ) )
{
    $mdalLibCandidates = @(
        '.\mdal\Release\mdal.lib',
        '.\Release\mdal.lib',
        '.\lib\mdal.lib',
        '.\bin\mdal.lib'
    )

    foreach ( $candidate in $mdalLibCandidates )
    {
        if ( Test-Path $candidate )
        {
            Copy-Item $candidate $mdalLibDestination -Force
            break
        }
    }
}

if ( -not ( Test-Path $mdalLibDestination ) )
{
    Write-Error "Unable to find mdal.lib after MDAL build/install."
    exit 1
}

cd ..

Remove-Item MDAL_building -Recurse
