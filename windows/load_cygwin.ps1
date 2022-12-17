# from https://gist.github.com/Guts/6303dc5eb941eb24be3e27609cd46985

$starter_path = Get-Location

md $env:CYGWIN_ROOT

# Download and install flex and bison
if (-Not (Test-Path "$env:CYGWIN_ROOT\setup-x86_64.exe" -PathType leaf )) {
	Write-Host "= Start downloading the Cygwin installer to $env:CYGWIN_ROOT"
	Invoke-WebRequest -Uri  "https://cygwin.com/setup-x86_64.exe" -OutFile $env:CYGWIN_ROOT\setup-x86_64.exe
	if (Test-Path "$env:CYGWIN_ROOT\setup-x86_64.exe" -PathType leaf)
    {
       Write-Host "== Installer downloaded"
    }
    else
    {
       Write-Host "== Installer not downloaded"
    }
}
else
{ Write-Host "= Cygwin installer already exists. Let's use it!" -ForegroundColor Blue }

Write-Host "========================= Start installing the Cygwin dependencies..."

cd $env:CYGWIN_ROOT
$cygwin_root_install = Get-Location

Write-Host "Cygwin will be installed in the following directory: "
$cygwin_root_install

.\setup-x86_64.exe `
    --arch x86_64 `
    --delete-orphans `
    --local-package-dir "$env:APPDATA/OSGeo4W-Packages" ` `
    --no-desktop `
    --packages flex,bison `
    --quiet-mode `
    --root $cygwin_root_install `
    --site "https://mirror.easyname.at/cygwin/" `
    --upgrade-also `
| out-null

Write-Host "========================= Contant of directory after installation:"
ls $cygwin_root_install

if (-Not (Test-Path "$env:CYGWIN_ROOT\setup-x86_64.exe" -PathType leaf ))
{
    Write-Host "***************************************************************************** ERROR Cygwin not installed"
    }

Set-Location $starter_path

