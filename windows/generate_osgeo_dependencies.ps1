param(
    [string]$ReosInstall = $env:REOS_INSTALL,
    [string]$ReosSource = $env:REOS_SOURCE,
    [string]$OsgeoRoot = $env:OSGEO4W_ROOT,
    [string]$QgisInstall = $env:QGIS_BUILT,
    [string]$GdalRoot = $env:GDAL_ROOT,
    [string]$MdalRoot = $env:MDAL_ROOT,
    [string]$GmshRoot = $env:GMSH_BUILT,
    [string]$EccodesRoot = $env:ECCODES_ROOT,
    [string]$OutputList,
    [switch]$UpdateSourceList
)

$ErrorActionPreference = "Stop"

function Get-FirstCommand([string[]]$Names)
{
    foreach ($name in $Names)
    {
        $command = Get-Command $name -ErrorAction SilentlyContinue | Select-Object -First 1
        if ($command)
        {
            return $command.Source
        }
    }

    if ($Names -contains "dumpbin")
    {
        $vswhereCandidates = @(
            (Join-OptionalPath ${env:ProgramFiles(x86)} "Microsoft Visual Studio\Installer\vswhere.exe"),
            (Join-OptionalPath $env:ProgramFiles "Microsoft Visual Studio\Installer\vswhere.exe")
        )

        foreach ($vswhere in $vswhereCandidates)
        {
            if (-not $vswhere -or -not (Test-Path $vswhere -PathType Leaf))
            {
                continue
            }

            $visualStudioPath = & $vswhere -latest -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath 2>$null
            if (-not $visualStudioPath)
            {
                continue
            }

            $dumpbin = Get-ChildItem -Path (Join-Path $visualStudioPath "VC\Tools\MSVC") -Filter dumpbin.exe -Recurse -ErrorAction SilentlyContinue |
                Sort-Object FullName -Descending |
                Select-Object -First 1

            if ($dumpbin)
            {
                return $dumpbin.FullName
            }
        }
    }

    return $null
}

function Add-ExistingDirectory([System.Collections.Generic.List[string]]$List, [string]$Path)
{
    if ($Path -and (Test-Path $Path -PathType Container))
    {
        $resolved = (Resolve-Path $Path).Path
        if (-not $List.Contains($resolved))
        {
            $List.Add($resolved)
        }
    }
}

function Join-OptionalPath([string]$Path, [string]$ChildPath)
{
    if (-not $Path)
    {
        return $null
    }

    return Join-Path $Path $ChildPath
}

function Add-RootFiles([System.Collections.Generic.Queue[string]]$Queue, [string]$Path, [string[]]$Include, [switch]$Recurse)
{
    if (-not $Path)
    {
        return
    }

    if (Test-Path $Path -PathType Leaf)
    {
        $Queue.Enqueue((Resolve-Path $Path).Path)
        return
    }

    if (Test-Path $Path -PathType Container)
    {
        foreach ($pattern in $Include)
        {
            Get-ChildItem -Path $Path -Filter $pattern -File -Recurse:$Recurse -ErrorAction SilentlyContinue |
                ForEach-Object { $Queue.Enqueue($_.FullName) }
        }
    }
}

function Get-ImportedDlls([string]$File)
{
    if ($script:DependencyToolKind -eq "dumpbin")
    {
        $output = & $script:DependencyTool /DEPENDENTS $File 2>$null
        return $output |
            ForEach-Object {
                if ($_ -match '^\s*([A-Za-z0-9_.+\-]+\.dll)\s*$')
                {
                    $matches[1]
                }
            } |
            Where-Object { $_ }
    }

    $output = & $script:DependencyTool -p $File 2>$null
    return $output |
        ForEach-Object {
            if ($_ -match 'DLL Name:\s*([^\s]+\.dll)')
            {
                $matches[1]
            }
        } |
        Where-Object { $_ }
}

function Resolve-Dll([string]$Name, [string[]]$SearchDirectories)
{
    foreach ($directory in $SearchDirectories)
    {
        $candidate = Join-Path $directory $Name
        if (Test-Path $candidate -PathType Leaf)
        {
            return (Resolve-Path $candidate).Path
        }
    }

    return $null
}

if (-not $ReosInstall -or -not (Test-Path $ReosInstall -PathType Container))
{
    Write-Warning "REOS install directory not found. Set REOS_INSTALL or pass -ReosInstall."
    exit 1
}

if (-not $OsgeoRoot -or -not (Test-Path $OsgeoRoot -PathType Container))
{
    Write-Warning "OSGeo4W root directory not found. Set OSGEO4W_ROOT or pass -OsgeoRoot."
    exit 1
}

if (-not $OutputList)
{
    $OutputList = Join-Path $ReosInstall "osgeo_dependencies_bin.generated.txt"
}

$script:DependencyTool = Get-FirstCommand @("llvm-objdump", "objdump", "dumpbin")
if (-not $script:DependencyTool)
{
    Write-Warning "No PE dependency tool found. Install LLVM tools, MinGW binutils, or run from a Visual Studio Developer shell."
    exit 2
}

$toolName = Split-Path $script:DependencyTool -Leaf
$script:DependencyToolKind = if ($toolName -ieq "dumpbin.exe" -or $toolName -ieq "dumpbin") { "dumpbin" } else { "objdump" }

$searchDirectories = New-Object 'System.Collections.Generic.List[string]'
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $ReosInstall "bin")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $ReosInstall "bin\providers")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $ReosInstall "bin\engines")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $ReosInstall "bin\qgisProvider")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $QgisInstall "bin")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $GdalRoot "bin")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $OsgeoRoot "bin")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $OsgeoRoot "apps\Qt6\bin")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $MdalRoot "lib")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $GmshRoot "lib")
Add-ExistingDirectory $searchDirectories (Join-OptionalPath $EccodesRoot "bin")

$osgeoBin = (Resolve-Path (Join-Path $OsgeoRoot "bin")).Path.TrimEnd('\')
$queue = New-Object 'System.Collections.Generic.Queue[string]'
$seen = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)
$osgeoDlls = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)
$unresolved = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)

Add-RootFiles $queue (Join-OptionalPath $ReosInstall "bin") @("*.exe", "*.dll") -Recurse
Add-RootFiles $queue (Join-OptionalPath $QgisInstall "bin") @("*.dll")
Add-RootFiles $queue (Join-OptionalPath $GdalRoot "bin") @("gdal*.dll")
Add-RootFiles $queue (Join-OptionalPath $MdalRoot "lib\mdal.dll") @("*.dll")
Add-RootFiles $queue (Join-OptionalPath $GmshRoot "lib\gmsh.dll") @("*.dll")
Add-RootFiles $queue (Join-OptionalPath $EccodesRoot "bin\eccodes.dll") @("*.dll")

while ($queue.Count -gt 0)
{
    $file = $queue.Dequeue()
    if (-not $seen.Add($file))
    {
        continue
    }

    foreach ($dll in Get-ImportedDlls $file)
    {
        $resolved = Resolve-Dll $dll $searchDirectories.ToArray()
        if (-not $resolved)
        {
            [void]$unresolved.Add($dll)
            continue
        }

        $resolvedDirectory = (Split-Path $resolved -Parent).TrimEnd('\')
        if ($resolvedDirectory.Equals($osgeoBin, [System.StringComparison]::OrdinalIgnoreCase))
        {
            [void]$osgeoDlls.Add((Split-Path $resolved -Leaf))
        }

        $queue.Enqueue($resolved)
    }
}

$explicitNames = New-Object 'System.Collections.Generic.HashSet[string]' ([System.StringComparer]::OrdinalIgnoreCase)
@("gmsh.dll", "mdal.dll", "eccodes.dll") | ForEach-Object { [void]$explicitNames.Add($_) }

$result = $osgeoDlls |
    Where-Object {
        -not $explicitNames.Contains($_) -and
        $_ -notlike "gdal*.dll" -and
        $_ -notlike "qgis_*.dll"
    } |
    Sort-Object

$outputDirectory = Split-Path $OutputList -Parent
if ($outputDirectory -and -not (Test-Path $outputDirectory -PathType Container))
{
    New-Item -ItemType Directory -Path $outputDirectory -Force | Out-Null
}

$result | Set-Content -Path $OutputList -Encoding ASCII
Write-Host "Generated $($result.Count) OSGeo4W runtime dependencies: $OutputList"

if ($unresolved.Count -gt 0)
{
    Write-Warning "Unresolved imported DLLs were ignored: $(([string[]]$unresolved | Sort-Object) -join ', ')"
}

if ($UpdateSourceList)
{
    if (-not $ReosSource)
    {
        Write-Warning "REOS source directory not found. Set REOS_SOURCE or pass -ReosSource to update the checked-in list."
        exit 1
    }

    $sourceList = Join-Path $ReosSource "windows\osgeo_dependencies_bin.txt"
    $result | Set-Content -Path $sourceList -Encoding ASCII
    Write-Host "Updated checked-in OSGeo4W dependency list: $sourceList"
}