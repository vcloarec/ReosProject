# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(GDAL REQUIRED)

SET(REOS_CORE_SOURCES
  reosapplication.cpp
  reosdocumentation.cpp
  reosmemoryraster.cpp
  reosmodule.cpp

  reossettings.cpp
  reosversion.cpp

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
  
  watershed/reoswatershed.cpp
  watershed/reoswatersheddelineating.cpp
  watershed/reoswatershedstore.cpp
)

SET(REOS_CORE_HEADERS
    reosapplication.h
    reosdocumentation.h
    reosmemoryraster.h
    reosmodule.h
    reossettings.h
    reosversion.h

    GIS/reosdigitalelevationmodel.h
    GIS/reosgisengine.h
    GIS/reosmapextent.h

    process/reosprocess.h

    raster/reosrasterfilling.h
    raster/reosrasterline.h
    raster/reosrastertrace.h
    raster/reosrasterwatershed.h
    raster/reosrastercompressed.h

    watershed/reoswatershed.h
    watershed/reoswatersheddelineating.h
    watershed/reoswatershedstore.h

)

SET(REOS_CORE_HEADERS_PRIVATE
    GIS/private/reosdigitalelevationmodel_p.h
    )

SET(IMAGE_RCCS ../../images/images.qrc)

SET(REOS_CORE_LIBS ${REOS_CORE_LIBS} reosCore)
ADD_LIBRARY(reosCore SHARED
    ${REOS_CORE_SOURCES}
    ${REOS_CORE_HEADERS}
    ${IMAGE_RCCS}
  )

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
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/GIS/private
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${QGIS_INCLUDE_DIR}
)


SET(QGIS_PLUGINS_DIR "${QGIS_LIB_PATH}/qgis/plugins/")
ADD_DEFINITIONS(-DQGIS_PLUGINS="${QGIS_PLUGINS_DIR}")

# INSTALL HEADER

