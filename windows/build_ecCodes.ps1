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

mkdir -p $eccodes_source_dir/build -Force | Out-Null
mkdir -p $ECCODES_INSTALL -Force | Out-Null
ls $OSGEO_DIR\lib\cmake
Write-Host "=== libaec CMake directory:"
$LIBAEC_CMAKE_DIR
ls $LIBAEC_CMAKE_DIR

# ecCodes (and its ecbuild framework) need a full POSIX toolbox: bash, sh,
# awk, sed, grep, perl, ... ecbuild can be told where bash is via
# -DBASH_EXE, but ecCodes itself runs find_program(... bash awk sed ...) which
# only consults PATH. So we both pass -DBASH_EXE and prepend the directories
# that hold those utilities.
#
# Git for Windows' usr\bin ships the most complete set; Cygwin's bin (which
# load_cygwin.ps1 installed) covers most of the rest. We add every directory
# that exists, most-complete first, so the first matching binary wins.
$utilDirCandidates = @(
    'C:\Program Files\Git\usr\bin',
    'C:\Program Files (x86)\Git\usr\bin',
    "$env:CYGWIN_ROOT\bin",
    'C:\Program Files\Git\bin'
)
$utilDirs = @()
foreach ($d in $utilDirCandidates) {
    if ((Test-Path $d) -and (Test-Path (Join-Path $d 'bash.exe'))) {
        $utilDirs += $d
    }
}
if ($utilDirs.Count -eq 0) {
    Write-Error "Could not locate bash.exe. ecCodes needs Git-for-Windows or Cygwin. Install Git for Windows or rerun load_cygwin.ps1."
    exit 1
}

$BASH_EXE = Join-Path $utilDirs[0] 'bash.exe'
$BASH_EXE_CMAKE = $BASH_EXE.Replace('\', '/')
Write-Host "=== Using BASH_EXE: $BASH_EXE_CMAKE"

# Prepend every util dir so find_program() picks them up.
foreach ($d in $utilDirs) {
    Write-Host "=== Prepended to PATH: $d"
}
$env:PATH = ($utilDirs -join ';') + ';' + $env:PATH

cd $eccodes_source_dir/build 
cmake -S .. `
  -B $eccodes_source_dir/build `
  -DBUILD_SHARED_LIBS=ON `
  -DENABLE_PKGCONFIG=OFF `
  "-DCMAKE_INSTALL_PREFIX:PATH=$ECCODES_CMAKE_INSTALL" `
  -DCMAKE_BUILD_TYPE=Release `
  "-DCMAKE_PREFIX_PATH:PATH=$OSGEO_CMAKE_PREFIX" `
  -DENABLE_NETCDF=OFF `
  -DENABLE_FORTRAN=OFF `
  -DPRODUCT_BUFR=OFF `
  -DEXAMPLES=OFF `
  "-Dlibaec_DIR:PATH=$LIBAEC_CMAKE_DIR" `
  "-DBASH_EXE:FILEPATH=$BASH_EXE_CMAKE"

if ($LASTEXITCODE -ne 0) {
    Write-Error "ecCodes CMake configure failed with exit code $LASTEXITCODE."
    exit $LASTEXITCODE
}

cmake --build . --config Release
if ($LASTEXITCODE -ne 0) {
    Write-Error "ecCodes build failed with exit code $LASTEXITCODE."
    exit $LASTEXITCODE
}

cmake --install . --config Release
if ($LASTEXITCODE -ne 0) {
    Write-Error "ecCodes install failed with exit code $LASTEXITCODE."
    exit $LASTEXITCODE
}

ls $ECCODES_INSTALL

cd ..

Remove-Item $eccodes_source_dir/build -Recurse
