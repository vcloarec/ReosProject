# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(GDAL REQUIRED)

SET(REOS_CORE_SOURCES
  reosapplication.cpp
  reosdocumentation.cpp
  reosencodedelement.cpp
  reosmemoryraster.cpp
  reosmodule.cpp
  reossettings.cpp
  reosversion.cpp
  reosexception.cpp
  reosparameter.cpp

  GIS/reosdigitalelevationmodel.cpp
  GIS/reosgisengine.cpp
  GIS/reosmapextent.cpp
  GIS/private/reosdigitalelevationmodel_p.cpp

  process/reosprocess.cpp

  raster/reosrasterfilling.cpp
  raster/reosrasterline.cpp
  raster/reosrastertrace.cpp
  raster/reosrasterwatershed.cpp
  raster/reosrastercompressed.cpp

  utils/reosgeometryutils.cpp
  
  watershed/reoswatershed.cpp
  watershed/reoswatersheddelineating.cpp
  watershed/reoswatershedtree.cpp
  watershed/reoswatershedmodule.cpp
  watershed/reosconcentrationtimecalculation.cpp
  watershed/reosmeteorologicmodel.cpp

  rainfall/reosrainfallitem.cpp
  rainfall/reosrainfallmodel.cpp
  rainfall/reosidfcurves.cpp
  rainfall/reossyntheticrainfall.cpp
  rainfall/reosrainfallregistery.cpp

  quantity/reosarea.cpp
  quantity/reosduration.cpp

  data/reostimeserie.cpp
  data/reostextfiledata.cpp
  data/reosdataobject.cpp
)

SET(REOS_CORE_HEADERS
    reoscore.h
    reosapplication.h
    reosdocumentation.h
    reosencodedelement.h
    reosmemoryraster.h
    reosmodule.h
    reossettings.h
    reosversion.h
    reosexception.h
    reosparameter.h

    GIS/reosdigitalelevationmodel.h
    GIS/reosgisengine.h
    GIS/reosmapextent.h

    process/reosprocess.h

    raster/reosrasterfilling.h
    raster/reosrasterline.h
    raster/reosrastertrace.h
    raster/reosrasterwatershed.h
    raster/reosrastercompressed.h

    utils/reosgeometryutils.h

    watershed/reoswatershed.h
    watershed/reoswatersheddelineating.h
    watershed/reoswatershedtree.h
    watershed/reoswatershedmodule.h
    watershed/reosconcentrationtimecalculation.h
    watershed/reosmeteorologicmodel.h

    rainfall/reosrainfallitem.h
    rainfall/reosrainfallmodel.h
    rainfall/reosidfcurves.h
    rainfall/reossyntheticrainfall.h
    rainfall/reosrainfallregistery.h

    quantity/reosarea.h
    quantity/reosduration.h

    data/reostimeserie.h
    data/reostextfiledata.h
    data/reosdataobject.h

)

SET(REOS_CORE_HEADERS_PRIVATE
    GIS/private/reosdigitalelevationmodel_p.h
    )

SET(IMAGE_RCCS ../../images/images.qrc
               ../../images/cursors/cursors.qrc
               ../../images/formulas/formulas.qrc)

SET(REOS_CORE_LIBS ${REOS_CORE_LIBS} reosCore)
ADD_LIBRARY(reosCore SHARED
    ${REOS_CORE_SOURCES}
    ${REOS_CORE_HEADERS}
    ${IMAGE_RCCS}
  )
  
IF (MSVC)
  add_compile_definitions(_USE_MATH_DEFINES)
ENDIF (MSVC)

IF(WIN32)
  INSTALL(TARGETS reosCore RUNTIME DESTINATION lib)
ELSE(WIN32)
  INSTALL(TARGETS reosCore LIBRARY DESTINATION lib)
ENDIF(WIN32)


TARGET_INCLUDE_DIRECTORIES(reosCore
    PUBLIC
    $<INSTALL_INTERFACE:include>
    PRIVATE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>
  )

TARGET_LINK_LIBRARIES(reosCore PUBLIC
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        ${QGIS_LIBS}
)

IF(GDAL_FOUND)
    TARGET_INCLUDE_DIRECTORIES(reosCore PUBLIC ${GDAL_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES(reosCore PUBLIC ${GDAL_LIBRARIES} )
ENDIF(GDAL_FOUND)

INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/GIS/private
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/watershed

    ${QGIS_INCLUDE_DIR}
)


SET(QGIS_PLUGINS_DIR "${QGIS_LIB_PATH}/qgis/plugins/")
ADD_DEFINITIONS(-DQGIS_PLUGINS="${QGIS_PLUGINS_DIR}")

# INSTALL HEADER

