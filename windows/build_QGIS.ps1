$starter_path = Get-Location

cd QGIS
$env:QGIS_SRC=Get-Location

$OSGEO_DIR=$env:OSGEO4W_ROOT
$BUILDCONF="Release"
$PYTHONHOME=Join-Path $OSGEO_DIR "apps\Python312"
Write-Host "=== Python HOME"
ls $PYTHONHOME

# --- Qt5 / MSVC 2026 compatibility patch ----------------------------------
# Newer MSVC (VS 17.10+ / VS 2026) fully removed stdext::checked_array_iterator
# and stdext::unchecked_array_iterator (the _HAS_DEPRECATED_STDEXT_ARRAY_ITERATORS
# re-enable macro no longer exists either). Qt5's qcompilerdetection.h still
# expands QT_MAKE_(UN)CHECKED_ARRAY_ITERATOR to those removed symbols on MSVC,
# which breaks compilation of any TU including qvector.h / qlist.h / qvarlengtharray.h.
# Patch the Qt header in-place to use the passthrough fallback Qt itself provides
# for non-MSVC compilers. Idempotent via a sentinel comment.
$qtCompilerDet = Join-Path $OSGEO_DIR "apps\Qt5\include\QtCore\qcompilerdetection.h"
if (Test-Path $qtCompilerDet) {
    $sentinel = "// REOS_PATCH_STDEXT_ARRAY_ITERATOR"
    $content = Get-Content -LiteralPath $qtCompilerDet -Raw
    if ($content -notmatch [regex]::Escape($sentinel)) {
        Write-Host "=== Patching Qt5 qcompilerdetection.h for MSVC 2026 stdext removal"
        Copy-Item -LiteralPath $qtCompilerDet -Destination "$qtCompilerDet.reos.bak" -Force
        $patched = $content `
            -replace '#  define QT_MAKE_UNCHECKED_ARRAY_ITERATOR\(x\) stdext::make_unchecked_array_iterator\(x\)[^\r\n]*', "#  define QT_MAKE_UNCHECKED_ARRAY_ITERATOR(x) (x) $sentinel" `
            -replace '#  define QT_MAKE_CHECKED_ARRAY_ITERATOR\(x, N\) stdext::make_checked_array_iterator\(x, size_t\(N\)\)[^\r\n]*', "#  define QT_MAKE_CHECKED_ARRAY_ITERATOR(x, N) (x) $sentinel"
        if ($patched -eq $content) {
            Write-Warning "Qt5 header patch did not match expected macro definitions; leaving file untouched."
        } else {
            Set-Content -LiteralPath $qtCompilerDet -Value $patched -NoNewline
        }
    } else {
        Write-Host "=== Qt5 qcompilerdetection.h already patched, skipping"
    }
} else {
    Write-Warning "Qt5 qcompilerdetection.h not found at $qtCompilerDet; skipping stdext patch"
}
# --------------------------------------------------------------------------

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

$cpuCount = [Environment]::ProcessorCount
Write-Host "=== Building QGIS with $cpuCount parallel jobs"

cmake -S $env:QGIS_SRC `
      -B . `
      -D CMAKE_CXX_FLAGS="/MP$cpuCount" `
      "-D CMAKE_CXX_FLAGS_$($BUILDCONF.ToUpper())=/MD /Z7 /Od /D NDEBUG" `
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
      -D USE_OPENCL=FALSE `
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
		
cmake --build . --config Release --parallel $cpuCount
if ($LASTEXITCODE -ne 0) {
	Write-Error "QGIS build failed with exit code $LASTEXITCODE."
	exit 1
}
cmake --install .

cd ..

Remove-Item $env:QGIS_BUILDING -Recurse

Set-Location $starter_path
