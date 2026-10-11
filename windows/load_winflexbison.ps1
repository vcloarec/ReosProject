$ErrorActionPreference = 'Stop'

$version = '2.5.25'
$archive = Join-Path $env:WINFLEXBISON_ROOT "win_flex_bison-$version.zip"

New-Item -ItemType Directory -Path $env:WINFLEXBISON_ROOT -Force | Out-Null

Invoke-WebRequest -Uri "https://github.com/lexxmark/winflexbison/releases/download/v$version/win_flex_bison-$version.zip" -OutFile $archive
Expand-Archive -Path $archive -DestinationPath $env:WINFLEXBISON_ROOT -Force

foreach ($exe in 'win_flex.exe', 'win_bison.exe')
{
    $path = Join-Path $env:WINFLEXBISON_ROOT $exe
    if (-not (Test-Path $path))
    {
        Write-Error "$exe not found in $env:WINFLEXBISON_ROOT"
        exit 1
    }
    & $path --version
    if ($LASTEXITCODE -ne 0)
    {
        Write-Error "$exe failed with exit code $LASTEXITCODE"
        exit 1
    }
}
