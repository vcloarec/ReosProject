rem batch

echo off
set PATH=%WINDIR%\system32;%WINDIR%\system32\WindowsPowerShell\v1.0;%WINDIR%;%WINDIR%\system32\WBem;%CMAKE_PATH%
set PATH=%PATH%;%OSGEO4W_ROOT%\bin;%OSGEO4W_ROOT%\apps\Python39;%OSGEO4W_ROOT%\apps\gdal-dev;%OSGEO4W_ROOT%\apps\gdal-dev\bin
for %%f in ("%OSGEO4W_ROOT%\etc\ini\*.bat") do call "%%f"

powershell -Command %1


