rem batch

setlocal
echo off
set PATH=%WINDIR%\system32;%WINDIR%\system32\WindowsPowerShell\v1.0;%WINDIR%;%WINDIR%\system32\WBem;%CMAKE_PATH%
set PATH=%PATH%;%OSGEO4W_ROOT%\bin;%OSGEO4W_ROOT%\apps\Python39
set PATH=%PATH%;%OSGEO4W_ROOT%\apps\Python39
for %%f in ("%OSGEO4W_ROOT%\etc\ini\*.bat") do call "%%f"

powershell -Command "%REOS_SOURCE%/windows/build_REOS.ps1"
if %ERRORLEVEL% NEQ 0 exit %ERRORLEVEL%
endlocal

echo "/////////////////////////////////////////////////////////////////////////////////"
echo "                               Tests in independant PATH (almost)                "
echo "/////////////////////////////////////////////////////////////////////////////////"
rem For tests we still use the QT dependencies binaries of OSGEO, because, for now no way to make it works without 
rem The Qt binaries are copied after tests.
setlocal
set PATH=%WINDIR%\system32;%WINDIR%\system32\WindowsPowerShell\v1.0;%WINDIR%;%WINDIR%\system32\WBem;%CMAKE_PATH%
set PATH=%PATH%;%REOS_INSTALL%\bin
set PATH=%PATH%;%OSGEO4W_ROOT%\apps\Qt5\bin;
set GDAL_DATA=%REOS_INSTALL%\share\gdal
cd %REOS_BUILDING%
ctest -C %BUILD_TYPE% -VV
if %ERRORLEVEL% NEQ 0 exit %ERRORLEVEL%
endlocal

echo off
rem Now we copy Qt binaries
for /f "tokens=*" %%i in (%REOS_SOURCE%\windows\qt_dependencies_bin.txt) DO (
    xcopy /S/E "%OSGEO4W_ROOT%\apps\Qt5\bin\%%i" "%REOS_INSTALL%\bin\")
    
xcopy /S/E  %OSGEO4W_ROOT%\apps\Qt5\plugins\imageformats\qsvg.dll %REOS_INSTALL%\bin\imageformats\
xcopy /S/E %OSGEO4W_ROOT%\apps\Qt5\plugins\platforms\qwindows.dll %REOS_INSTALL%\bin\platforms\
robocopy %OSGEO4W_ROOT%\apps\Qt5\plugins\renderers %REOS_INSTALL%\bin\plugins\renderers /E /S /NFL /NDL /NJH /NJS /nc /ns /np
xcopy /S/E %OSGEO4W_ROOT%\apps\Qt5\plugins\iconengines\*.* %REOS_INSTALL%\bin\iconengines\
xcopy /S/E %OSGEO4W_ROOT%\apps\Qt5\plugins\styles\qwindowsvistastyle.dll %REOS_INSTALL%\bin\styles\

echo on