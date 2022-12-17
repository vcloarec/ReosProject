@echo off
if DEFINED OSGEO4W_ROOT set OSGEO4W_ROOT=C:\OSGeo4W
set WINDIR=C:\Windows
set CMAKE_PATH="C:\Program Files\CMake\bin"
set QGIS_INSTALL=C:\built\QGIS_Debug
set QGIS_BUILDING=C:\building\QGIS
set QGIS_SRC=C:\QGIS
set CYGWIN_ROOT=C:\cygwin64
set PATH=%PATH%;%OSGEO4W_ROOT%\bin;%WINDIR%\system32;%WINDIR%;%WINDIR%\system32\WBem;%OSGEO4W_ROOT%\apps\Python39;%CMAKE_PATH%;%OSGEO4W_ROOT%\apps\gdal-dev;%OSGEO4W_ROOT%\apps\gdal-dev\bin
for %%f in ("%OSGEO4W_ROOT%\etc\ini\*.bat") do call "%%f"