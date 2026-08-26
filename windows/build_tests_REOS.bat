rem batch

setlocal
echo off
set QT_ROOT=%OSGEO4W_ROOT%\apps\Qt6
set PATH=%WINDIR%\system32;%WINDIR%\system32\WindowsPowerShell\v1.0;%WINDIR%;%WINDIR%\system32\WBem;%CMAKE_PATH%
set PATH=%PATH%;%OSGEO4W_ROOT%\bin;%OSGEO4W_ROOT%\apps\Python312;%OSGEO4W_ROOT%\apps\gdal-dev;%OSGEO4W_ROOT%\apps\gdal-dev\bin
set PATH=%PATH%;%OSGEO4W_ROOT%\apps\Python312
for %%f in ("%OSGEO4W_ROOT%\etc\ini\*.bat") do call "%%f"

powershell -Command "%REOS_SOURCE%/windows/build_REOS.ps1"
if %ERRORLEVEL% NEQ 0 exit %ERRORLEVEL%
endlocal

echo "/////////////////////////////////////////////////////////////////////////////////"
echo "                               Tests in independant PATH (almost)                "
echo "/////////////////////////////////////////////////////////////////////////////////"
rem For tests we still use the Qt dependencies binaries of OSGEO, because for now, no way to make it work without
rem The Qt binaries are copied after tests.
setlocal
set QT_ROOT=%OSGEO4W_ROOT%\apps\Qt6
set PATH=%WINDIR%\system32;%WINDIR%\system32\WindowsPowerShell\v1.0;%WINDIR%;%WINDIR%\system32\WBem;%CMAKE_PATH%
set PATH=%PATH%;%REOS_INSTALL%\bin
set PATH=%PATH%;%QT_ROOT%\bin;
set GDAL_DATA=%REOS_INSTALL%\share\gdal

rem ecCodes needs its definitions and samples folders at runtime. Copy them
rem into the REOS install tree so the install is self-contained, then point
rem the eccodes runtime env vars at the copies for the ctest run.
robocopy %ECCODES_ROOT%\share\eccodes\definitions %REOS_INSTALL%\share\eccodes\definitions /E /S /NFL /NDL /NJH /NJS /nc /ns /np
robocopy %ECCODES_ROOT%\share\eccodes\samples     %REOS_INSTALL%\share\eccodes\samples     /E /S /NFL /NDL /NJH /NJS /nc /ns /np
set ECCODES_DEFINITION_PATH=%REOS_INSTALL%\share\eccodes\definitions
set ECCODES_SAMPLES_PATH=%REOS_INSTALL%\share\eccodes\samples

cd %REOS_BUILDING%
ctest -C %BUILD_TYPE% -VV --output-on-failure --output-log "%REOS_BUILDING%\ctest_results.txt"
if %ERRORLEVEL% NEQ 0 exit %ERRORLEVEL%
endlocal

rem Now we copy Qt files
set QT_ROOT=%OSGEO4W_ROOT%\apps\Qt6
for /f "usebackq tokens=*" %%i in ("%REOS_SOURCE%\windows\qt_dependencies_bin.txt") DO (
    if exist "%QT_ROOT%\bin\%%i" (
        copy /v /y "%QT_ROOT%\bin\%%i" "%REOS_INSTALL%\bin\%%i"
    ) else (
        echo WARNING: Qt dependency not found: %QT_ROOT%\bin\%%i
    )
)

xcopy /S/E "%QT_ROOT%\plugins\imageformats\*.*" "%REOS_INSTALL%\bin\imageformats\"
xcopy /S/E "%QT_ROOT%\plugins\platforms\qwindows.dll" "%REOS_INSTALL%\bin\platforms\"
if exist "%QT_ROOT%\plugins\renderers" robocopy "%QT_ROOT%\plugins\renderers" "%REOS_INSTALL%\bin\plugins\renderers" /E /S /NFL /NDL /NJH /NJS /nc /ns /np
if exist "%QT_ROOT%\plugins\iconengines" xcopy /S/E "%QT_ROOT%\plugins\iconengines\*.*" "%REOS_INSTALL%\bin\iconengines\"
if exist "%QT_ROOT%\plugins\styles\qwindowsvistastyle.dll" xcopy /S/E "%QT_ROOT%\plugins\styles\qwindowsvistastyle.dll" "%REOS_INSTALL%\bin\styles\"
copy /v /y "%QT_ROOT%\translations\qtbase_fr.qm" "%REOS_INSTALL%\i18n\qtbase_fr.qm"
copy /v /y "%QT_ROOT%\translations\qtbase_it.qm" "%REOS_INSTALL%\i18n\qtbase_it.qm"
copy /v /y "%QT_ROOT%\translations\qtbase_es.qm" "%REOS_INSTALL%\i18n\qtbase_es.qm"

echo "///////////////////// Test launch Lekan application, start it and wait 30s
start %REOS_INSTALL%\bin\Lekan.exe test
ping -n 30 127.0.0.1
tasklist /fi "ImageName eq Lekan.exe" /fo csv 2>NUL | find /I "Lekan.exe">NUL
if %ERRORLEVEL% NEQ 0 (
echo "///////////////////// Test launch Lekan application fails
exit %ERRORLEVEL%
 ) else (
echo "///////////////////// Test launch Lekan application success
taskkill /F /IM Lekan.exe
 )

echo on
