<#
.SYNOPSIS
    Local equivalent of .github/workflows/windows_test.yml.

.DESCRIPTION
    Reproduces, on a developer machine, the same dependency download,
    build, test and packaging steps run by the
    "Build Test Package Windows" GitHub Actions workflow.

    The same directory layout is used: every dependency install/build
    tree is created as a sibling directory of the REOS source tree
    (i.e. inside the repository root, just like ${{github.workspace}}).

    The GitHub Actions cache (actions/cache) is approximated by checking
    a step-specific marker file (e.g. lib\mdal.lib for MDAL); a step that
    already produced that marker is skipped. Use the -Skip* switches to
    bypass an individual step.

    REQUIREMENTS
        - PowerShell must be started "Run as administrator". The OSGeo4W,
          Cygwin and HEC-RAS installers all require elevation.
        - Git, CMake, Visual Studio (with C++), Intel Fortran (oneAPI) and
          NSIS must already be installed.

.PARAMETER BuildRoot
    Folder where every dependency download, source clone, build tree,
    install tree and the final installer are placed. By default this
    is <repository>\build, so nothing is ever written to the
    repository root. The folder is created if it does not exist.

.PARAMETER BuildType
    CMake build type. Matches BUILD_TYPE in the workflow. Default: Release.

.PARAMETER CMakePath
    Folder containing cmake.exe. Matches CMAKE_PATH in the workflow.
    Default: C:\Program Files\CMake\bin.

.PARAMETER IntelOneApiRoot
    Root folder of an existing Intel oneAPI installation. The workflow
    uses the modflowpy/install-intelfortran-action action; locally
    the toolchain must already be installed.
    Default: C:\Program Files (x86)\Intel\oneAPI.

.PARAMETER IntelCompilerVersion
    Name of the version sub-folder under <IntelOneApiRoot>\compiler
    (e.g. "2024.1"). If empty, the most recent version is auto-detected.

.PARAMETER NsisPath
    Folder containing makensis.exe (used in place of the
    joncloud/makensis-action action). Default: C:\Program Files (x86)\NSIS.

.PARAMETER QgisRef
    Git ref to checkout on the QGIS clone. Default: final-3_44_11
    (same value used by the workflow).

.PARAMETER SkipCygwin, SkipOsgeo, SkipEcCodes, SkipQgis, SkipMdal,
    SkipGmsh, SkipHecRas, SkipTests, SkipInstaller
    Switches that skip the matching workflow step. By default a step
    is also skipped automatically when its install/build folder is
    already present (mimicking the workflow cache hit).

.EXAMPLE
    .\windows\build_localy.ps1

.EXAMPLE
    .\windows\build_localy.ps1 -SkipInstaller -BuildType Release
#>

[CmdletBinding()]
param(
    [string]$BuildRoot            = '',
    [string]$BuildType            = 'Release',
    [string]$CMakePath            = 'C:\Program Files\CMake\bin',
    [string]$IntelOneApiRoot      = 'C:\Program Files (x86)\Intel\oneAPI',
    [string]$IntelCompilerVersion = '',
    [string]$NsisPath             = 'C:\Program Files (x86)\NSIS',
    [string]$QgisRef              = 'final-4_2_1',
    [switch]$SkipCygwin,
    [switch]$SkipOsgeo,


    [switch]$SkipEcCodes,
    [switch]$SkipQgis,
    [switch]$SkipMdal,
    [switch]$SkipGmsh,
    [switch]$SkipHecRas,
    [switch]$SkipTests,
    [switch]$SkipInstaller
)

$ErrorActionPreference = 'Stop'

# ---------------------------------------------------------------------------
# 0. Require an elevated PowerShell session.
#    Several installer steps (osgeo4w-setup.exe, cygwin setup-x86_64.exe,
#    HEC-RAS_631_Setup.exe) require administrator privileges. On the
#    GitHub Actions runner the workflow always runs elevated; locally
#    we must check explicitly to fail fast with a clear message.
# ---------------------------------------------------------------------------
$currentPrincipal = [Security.Principal.WindowsPrincipal]::new(
    [Security.Principal.WindowsIdentity]::GetCurrent())
if ( -not $currentPrincipal.IsInRole([Security.Principal.WindowsBuiltInRole]::Administrator) )
{
    throw 'build_localy.ps1 must be run from an elevated PowerShell session. ' +
          'Close this window, right-click PowerShell and choose ' +
          '"Run as administrator", then run the script again.'
}

# ---------------------------------------------------------------------------
# 1. "config env variables" step
# ---------------------------------------------------------------------------
$ReosSource = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path
$workspace  = $ReosSource

if ([string]::IsNullOrWhiteSpace($BuildRoot))
{
    $BuildRoot = Join-Path $workspace 'build'
}
New-Item -ItemType Directory -Path $BuildRoot -Force | Out-Null
$BuildRoot = (Resolve-Path -LiteralPath $BuildRoot).Path

$env:BUILD_TYPE      = $BuildType
if ( -not $env:WINDIR ) { $env:WINDIR = 'C:\Windows' }
$env:CMAKE_PATH      = $CMakePath

$env:CYGWIN_ROOT     = Join-Path $BuildRoot 'cygwin64'
$env:OSGEO4W_ROOT    = Join-Path $BuildRoot 'OSGEO4W'
$env:GDAL_ROOT       = $env:OSGEO4W_ROOT
$env:ECCODES_ROOT    = Join-Path $BuildRoot 'ECCODES_Install'
$env:MDAL_ROOT       = Join-Path $BuildRoot 'MDAL_Install'
$env:GMSH_BUILT      = Join-Path $BuildRoot 'GMSH_Built'
$env:QGIS_SRC        = Join-Path $BuildRoot 'QGIS'
$env:QGIS_BUILDING   = Join-Path $BuildRoot 'QGIS_building'
$env:QGIS_BUILT      = Join-Path $BuildRoot 'QGIS_install'
$env:REOS_SOURCE     = $workspace
$env:REOS_BUILDING   = Join-Path $BuildRoot 'REOS_build'
$env:REOS_INSTALL    = Join-Path $BuildRoot 'REOS_Install'
$env:NSI_DESTINATION = $BuildRoot

Push-Location $workspace
try
{
    $env:LEKAN_EXPERIMENTAL_VERSION = (git rev-parse --short HEAD).Trim()
}
finally
{
    Pop-Location
}

Write-Host '================ Local build environment ================'
Write-Host "REOS_SOURCE                = $env:REOS_SOURCE"
Write-Host "BUILD_ROOT                 = $BuildRoot"
Write-Host "BUILD_TYPE                 = $env:BUILD_TYPE"
Write-Host "CMAKE_PATH                 = $env:CMAKE_PATH"
Write-Host "CYGWIN_ROOT                = $env:CYGWIN_ROOT"
Write-Host "OSGEO4W_ROOT               = $env:OSGEO4W_ROOT"
Write-Host "ECCODES_ROOT               = $env:ECCODES_ROOT"
Write-Host "MDAL_ROOT                  = $env:MDAL_ROOT"
Write-Host "GMSH_BUILT                 = $env:GMSH_BUILT"
Write-Host "QGIS_SRC                   = $env:QGIS_SRC"
Write-Host "QGIS_BUILT                 = $env:QGIS_BUILT"
Write-Host "REOS_BUILDING              = $env:REOS_BUILDING"
Write-Host "REOS_INSTALL               = $env:REOS_INSTALL"
Write-Host "LEKAN_EXPERIMENTAL_VERSION = $env:LEKAN_EXPERIMENTAL_VERSION"
Write-Host '========================================================='

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------
# A step is considered already done only when the marker file produced at
# the end of that step exists. Using a generic "folder has content" check
# would wrongly skip a step after a partially aborted install (e.g. the
# OSGEO4W folder left behind by a failed setup-x86_64.exe run).
function Test-StepComplete
{
    param([string]$MarkerPath)
    return Test-Path -LiteralPath $MarkerPath
}

function Invoke-Step
{
    param(
        [Parameter(Mandatory)] [string]      $Name,
        [Parameter(Mandatory)] [scriptblock] $Action
    )

    Write-Host ''
    Write-Host "==================== STEP: $Name ===================="
    $global:LASTEXITCODE = 0
    & $Action
    if ($LASTEXITCODE -ne 0)
    {
        throw "Step '$Name' failed with exit code $LASTEXITCODE."
    }
}

# ---------------------------------------------------------------------------
# 2. "load CYGWIN" step
# ---------------------------------------------------------------------------
$cygwinMarker = Join-Path $env:CYGWIN_ROOT 'bin\flex.exe'
if ($SkipCygwin -or (Test-StepComplete $cygwinMarker))
{
    Write-Host "[SKIP] Cygwin already present at $env:CYGWIN_ROOT"
}
else
{
    Invoke-Step 'load Cygwin' {
        Push-Location $BuildRoot
        try     { & "$PSScriptRoot\load_cygwin.ps1" }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 3. "load OSGEO dependencies" step
# ---------------------------------------------------------------------------
$osgeoMarker = Join-Path $env:OSGEO4W_ROOT 'apps\Qt6\bin\qmake.exe'
if ($SkipOsgeo -or (Test-StepComplete $osgeoMarker))
{
    Write-Host "[SKIP] OSGeo4W already present at $env:OSGEO4W_ROOT"
}
else
{
    Invoke-Step 'load OSGeo4W' {
        Push-Location $BuildRoot
        try     { & "$PSScriptRoot\load_osgeo.ps1" }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 4. "build ECCODES" step
# ---------------------------------------------------------------------------
$ecCodesMarker = Join-Path $env:ECCODES_ROOT 'include\eccodes.h'
if ($SkipEcCodes -or (Test-StepComplete $ecCodesMarker))
{
    Write-Host "[SKIP] ecCodes already present at $env:ECCODES_ROOT"
}
else
{
    Invoke-Step 'build ecCodes' {
        Push-Location $BuildRoot
        try     { & "$PSScriptRoot\build_ecCodes.ps1" }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 5. "clone QGIS" step
# ---------------------------------------------------------------------------
if ( -not (Test-Path (Join-Path $env:QGIS_SRC '.git')) )
{
    Invoke-Step 'clone QGIS' {
        git clone https://github.com/qgis/QGIS.git $env:QGIS_SRC
        if ($LASTEXITCODE -ne 0) { return }
        Push-Location $env:QGIS_SRC
        try     { git checkout $QgisRef }
        finally { Pop-Location }
    }
}
else
{
    Write-Host "[SKIP] QGIS sources already present at $env:QGIS_SRC"
}

# ---------------------------------------------------------------------------
# 6. "clone MDAL" step
# ---------------------------------------------------------------------------
$mdalSourceDir = Join-Path $BuildRoot 'MDAL'
if ( -not (Test-Path (Join-Path $mdalSourceDir '.git')) )
{
    Invoke-Step 'clone MDAL' {
        git clone https://github.com/lutraconsulting/MDAL.git $mdalSourceDir
    }
}
else
{
    Write-Host "[SKIP] MDAL sources already present at $mdalSourceDir"
}

# ---------------------------------------------------------------------------
# 7. "build QGIS" step (uses the OSGeo4W shell)
# ---------------------------------------------------------------------------
$qgisMarker = Join-Path $env:QGIS_BUILT 'bin\qgis_core.dll'
if ($SkipQgis -or (Test-StepComplete $qgisMarker))
{
    Write-Host "[SKIP] QGIS already built at $env:QGIS_BUILT"
}
else
{
    Invoke-Step 'build QGIS' {
        Push-Location $BuildRoot
        try
        {
            & "$PSScriptRoot\qgis_env_before_powershell.bat" "$PSScriptRoot\build_QGIS.ps1"
        }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 8. "build MDAL" step
# ---------------------------------------------------------------------------
$mdalMarker = Join-Path $env:MDAL_ROOT 'lib\mdal.lib'
if ($SkipMdal -or (Test-StepComplete $mdalMarker))
{
    Write-Host "[SKIP] MDAL already built at $env:MDAL_ROOT"
}
else
{
    Invoke-Step 'build MDAL' {
        Push-Location $BuildRoot
        try
        {
            & "$PSScriptRoot\qgis_env_before_powershell.bat" "$PSScriptRoot\build_MDAL.ps1"
        }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 9. "Setup Intel Fortran" + "config FORTRAN path" steps
#    (the modflowpy/install-intelfortran-action action is not available
#    locally, so we point at an existing Intel oneAPI installation).
# ---------------------------------------------------------------------------
Invoke-Step 'configure Intel Fortran' {
    if ( -not (Test-Path $IntelOneApiRoot) )
    {
        throw "Intel oneAPI not found at '$IntelOneApiRoot'. Install the " +
              'Intel Fortran Compiler (oneAPI HPC Toolkit) or pass ' +
              '-IntelOneApiRoot pointing to your installation.'
    }

    $env:INTEL_HPCKIT_INSTALL_PATH = $IntelOneApiRoot

    if ([string]::IsNullOrWhiteSpace($IntelCompilerVersion))
    {
        $compilerRoot = Join-Path $IntelOneApiRoot 'compiler'
        $detected = Get-ChildItem -Path $compilerRoot -Directory -ErrorAction SilentlyContinue |
                    Where-Object { $_.Name -ne 'latest' } |
                    Sort-Object Name -Descending |
                    Select-Object -First 1
        if ( -not $detected )
        {
            throw "Unable to detect any Intel compiler version under '$compilerRoot'. " +
                  'Pass it explicitly with -IntelCompilerVersion.'
        }
        $IntelCompilerVersion = $detected.Name
    }

    $env:INTEL_COMPILER_VERSION = $IntelCompilerVersion
    $env:FORTRAN_COMPILER_PATH  = Join-Path (Join-Path $IntelOneApiRoot 'compiler') $IntelCompilerVersion

    Write-Host "INTEL_HPCKIT_INSTALL_PATH = $env:INTEL_HPCKIT_INSTALL_PATH"
    Write-Host "INTEL_COMPILER_VERSION    = $env:INTEL_COMPILER_VERSION"
    Write-Host "FORTRAN_COMPILER_PATH     = $env:FORTRAN_COMPILER_PATH"
}

# ---------------------------------------------------------------------------
# 10. "build GMSH" step
# ---------------------------------------------------------------------------
$gmshMarker = Join-Path $env:GMSH_BUILT 'lib\gmsh.lib'
if ($SkipGmsh -or (Test-StepComplete $gmshMarker))
{
    Write-Host "[SKIP] GMSH already built at $env:GMSH_BUILT"
}
else
{
    Invoke-Step 'build GMSH' {
        Push-Location $BuildRoot
        try     { & "$PSScriptRoot\load_build_gmsh.ps1" }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 11. "install hecras" step
# ---------------------------------------------------------------------------
$hecRasMarker = 'C:\Program Files (x86)\HEC\HEC-RAS\6.3.1'
if ($SkipHecRas -or (Test-Path $hecRasMarker))
{
    Write-Host "[SKIP] HEC-RAS 6.3.1 already installed"
}
else
{
    Invoke-Step 'install HEC-RAS 6.3.1' {
        Push-Location $BuildRoot
        try
        {
            $installer = Join-Path $BuildRoot 'hec_inst.exe'
            if ( -not (Test-Path $installer) )
            {
                Invoke-WebRequest `
                    -Uri 'https://github.com/HydrologicEngineeringCenter/hec-downloads/releases/download/1.0.26/HEC-RAS_631_Setup.exe' `
                    -OutFile $installer
            }
            $proc = Start-Process $installer -ArgumentList '/S /v/qn' -Wait -PassThru
            if ($proc.ExitCode -ne 0)
            {
                throw "HEC-RAS installer exited with code $($proc.ExitCode)."
            }
        }
        finally { Pop-Location }
    }
}

# ---------------------------------------------------------------------------
# 12. "build and test REOS" step
# ---------------------------------------------------------------------------
Invoke-Step 'build and test REOS' {
    Push-Location $BuildRoot
    try
    {
        if ($SkipTests)
        {
            & "$PSScriptRoot\qgis_env_before_powershell.bat" "$PSScriptRoot\build_REOS.ps1"
        }
        else
        {
            & "$PSScriptRoot\build_tests_REOS.bat"
        }
    }
    finally { Pop-Location }
}

# ---------------------------------------------------------------------------
# 13. "Create installer" + "Archive installer" steps
# ---------------------------------------------------------------------------
if ($SkipInstaller)
{
    Write-Host '[SKIP] installer creation'
}
else
{
    Invoke-Step 'create Windows installer (NSIS)' {
        $makensis = Join-Path $NsisPath 'makensis.exe'
        if ( -not (Test-Path $makensis) )
        {
            throw "makensis.exe not found at '$makensis'. " +
                  'Install NSIS (https://nsis.sourceforge.io) or pass -NsisPath.'
        }

        $script = Join-Path $workspace 'package\ms-windows\nsi\lekan_exp.nsi'
        & $makensis /V4 $script
    }

    $installer = Join-Path $env:NSI_DESTINATION `
        ("Lekan-{0}-install-win64.exe" -f $env:LEKAN_EXPERIMENTAL_VERSION)
    if (Test-Path $installer)
    {
        Write-Host ''
        Write-Host "Installer produced: $installer"
    }
    else
    {
        Write-Warning "Expected installer not found at $installer"
    }
}

Write-Host ''
Write-Host '========================================================='
Write-Host '  Local build finished.'
Write-Host '========================================================='
