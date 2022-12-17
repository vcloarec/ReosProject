
copy /v /y %QGIS_BUILT%\bin\qgis_3d.dll  "%REOS_INSTALL%\bin\qgis_3d.dll"
copy /v /y %QGIS_BUILT%\bin\qgis_analysis.dll  "%REOS_INSTALL%\bin\qgis_analysis.dll"
copy /v /y %QGIS_BUILT%\bin\qgis_app.dll  "%REOS_INSTALL%\bin\qgis_app.dll"
copy /v /y %QGIS_BUILT%\bin\qgis_core.dll  "%REOS_INSTALL%\bin\qgis_core.dll"
copy /v /y %QGIS_BUILT%\bin\qgis_gui.dll  "%REOS_INSTALL%\bin\qgis_gui.dll"
copy /v /y %QGIS_BUILT%\bin\qgis_native.dll  "%REOS_INSTALL%\bin\qgis_native.dll"
xcopy /s /Y /D /I %QGIS_BUILT%\plugins  "%REOS_INSTALL%\bin\qgisProvider"

copy /v /y %GDAL_ROOT%\bin\gdal*.dll "%REOS_INSTALL%\bin"

copy /v /y  %GMSH_BUILT%\lib\gmsh.dll "%REOS_INSTALL%\bin\gmsh.dll"
copy /v /y %MDAL_ROOT%\lib\mdal.dll  "%REOS_INSTALL%\bin\engines\mdal.dll"

rem OSGEO Dependencies
for /f "tokens=*" %%i in (%REOS_SOURCE%\windows\osgeo_dependencies_bin.txt) DO (
     xcopy /S/E "%OSGEO4W_ROOT%\bin\%%i" "%REOS_INSTALL%\bin")
    
rem QGIS resources
robocopy %QGIS_SRC%\resources %REOS_INSTALL%\resources /E /S /NFL /NDL /NJH /NJS /nc /ns /np /Xd "%QGIS_SRC%\resources\opencl_programs" "%QGIS_SRC%\resources\server" "%QGIS_SRC%\resources\themes" "%QGIS_SRC%\resources\metadata-ISO" "%QGIS_SRC%\resources\data"

robocopy %REOS_SOURCE%\windows\apps %REOS_INSTALL%\apps /E /S /NFL /NDL /NJH /NJS /nc /ns /np
copy /v /y %REOS_SOURCE%\package\dem.qml %REOS_INSTALL%\resources\dem.qml

robocopy %OSGEO4W_ROOT%\apps\gdal\share\gdal %REOS_INSTALL%\share\gdal /E /S /NFL /NDL /NJH /NJS /nc /ns /np
robocopy %OSGEO4W_ROOT%\share\proj %REOS_INSTALL%\share\proj /E /S /Xf *.tif *.tiff /NFL /NDL /NJH /NJS /nc /ns /np
