# Reos licence GPL version 2
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

FIND_PACKAGE(GDAL REQUIRED)

SET(REOS_CORE_SOURCES
  reosapplication.cpp
  reosencodedelement.cpp
  reosmemoryraster.cpp
  reosmodule.cpp
  reossettings.cpp
  reosversion.cpp
  reosexception.cpp
  reosparameter.cpp
  reosstyleregistery.cpp
  reoscalculationcontext.cpp
  reosremoteinformation.cpp

  GIS/reosdigitalelevationmodel.cpp
  GIS/reosgisengine.cpp
  GIS/reosmapextent.cpp
  GIS/reosexporttovectorfile.cpp
  GIS/reosgeometrystructure.cpp
  GIS/reospolygonstructure.cpp
  GIS/reospolylinesstructure.cpp
  GIS/reosmesh.cpp
  GIS/reostopographycollection.cpp
  GIS/reosrenderedobject.cpp
  GIS/reos3dmapsettings.cpp
  GIS/private/reosdigitalelevationmodel_p.cpp
  GIS/private/reospolygonstructure_p.cpp
  GIS/private/reospolylinesstructure_p.cpp
  GIS/private/reosmesh_p.cpp
  GIS/private/reosmeshdataprovider_p.cpp
  GIS/private/reostopographycollection_p.cpp

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
  watershed/reosrunoffmodel.cpp
  watershed/reostransferfunction.cpp

  rainfall/reosrainfallitem.cpp
  rainfall/reosrainfallmodel.cpp
  rainfall/reosidfcurves.cpp
  rainfall/reossyntheticrainfall.cpp
  rainfall/reosrainfallregistery.cpp

  quantity/reosarea.cpp
  quantity/reosduration.cpp

  data/reostimeserie.cpp
  data/reostimeserieprovider.cpp
  data/reostextfiledata.cpp
  data/reosdataobject.cpp
  data/reosdataprovider.cpp

  hydrograph/reoshydrograph.cpp
  hydrograph/reoshydrographsource.cpp
  hydrograph/reoshydrographrouting.cpp

  hydraulicNetwork/reoshydraulicscheme.cpp
  hydraulicNetwork/reoshydrauliclink.cpp
  hydraulicNetwork/reoshydraulicnode.cpp
  hydraulicNetwork/reoshydraulicnetwork.cpp
  hydraulicNetwork/reoshydraulicstructure2d.cpp
  hydraulicNetwork/simulation/reoshydraulicsimulation.cpp

  mesh/reosmeshgenerator.cpp
  mesh/reosgmshgenerator.cpp
)

SET(REOS_CORE_HEADERS
    reoscore.h
    reosapplication.h
    reosencodedelement.h
    reosmemoryraster.h
    reosmodule.h
    reossettings.h
    reosversion.h
    reosexception.h
    reosparameter.h
    reosstyleregistery.h
    reoscalculationcontext.h
    reosremoteinformation.h

    GIS/reosdigitalelevationmodel.h
    GIS/reosgisengine.h
    GIS/reosmapextent.h
    GIS/reosexporttovectorfile.h
    GIS/reosgeometrystructure.h
    GIS/reospolygonstructure.h
    GIS/reospolylinesstructure.h
    GIS/reosmesh.h
    GIS/reostopographycollection.h
    GIS/reosrenderedobject.h
    GIS/reos3dmapsettings.h

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
    watershed/reosrunoffmodel.h
    watershed/reostransferfunction.h

    rainfall/reosrainfallitem.h
    rainfall/reosrainfallmodel.h
    rainfall/reosidfcurves.h
    rainfall/reossyntheticrainfall.h
    rainfall/reosrainfallregistery.h

    quantity/reosarea.h
    quantity/reosduration.h

    data/reostimeserie.h
    data/reostimeserieprovider.h
    data/reostextfiledata.h
    data/reosdataobject.h
    data/reosdataprovider.h

    hydrograph/reoshydrograph.h
    hydrograph/reoshydrographsource.h
    hydrograph/reoshydrographrouting.h

    hydraulicNetwork/reoshydraulicscheme.h
    hydraulicNetwork/reoshydrauliclink.h
    hydraulicNetwork/reoshydraulicnode.h
    hydraulicNetwork/reoshydraulicnetwork.h
    hydraulicNetwork/reoshydraulicstructure2d.h
    hydraulicNetwork/simulation/reoshydraulicsimulation.h

    mesh/reosmeshgenerator.h
    mesh/reosgmshgenerator.h
)

SET(REOS_CORE_HEADERS_PRIVATE
    GIS/private/reosdigitalelevationmodel_p.h
    GIS/private/reospolylinesstructure_p.h
    GIS/private/reospolygonstructure_p.h
    GIS/private/reosmesh_p.h
    GIS/private/reosmeshdataprovider_p.h
    GIS/private/reostopographycollection_p.h
    )

SET(IMAGE_RCCS ${CMAKE_SOURCE_DIR}/images/images.qrc
               ${CMAKE_SOURCE_DIR}/images/cursors/cursors.qrc
               ${CMAKE_SOURCE_DIR}/images/formulas/formulas.qrc
               ${CMAKE_SOURCE_DIR}/images/flags/flags.qrc)

SET(CREDITS_RCCS ${CMAKE_SOURCE_DIR}/credits/credits.qrc)

set(REOS_CORE_SOURCES ${REOS_CORE_SOURCES}
  ${CMAKE_SOURCE_DIR}/external/poly2tri/common/shapes.cc
  ${CMAKE_SOURCE_DIR}/external/poly2tri/sweep/advancing_front.cc
  ${CMAKE_SOURCE_DIR}/external/poly2tri/sweep/cdt.cc
  ${CMAKE_SOURCE_DIR}/external/poly2tri/sweep/sweep_context.cc
  ${CMAKE_SOURCE_DIR}/external/poly2tri/sweep/sweep.cc
)

SET(REOS_CORE_LIBS ${REOS_CORE_LIBS} reosCore)
ADD_LIBRARY(reosCore SHARED
    ${REOS_CORE_SOURCES}
    ${REOS_CORE_HEADERS}
    ${IMAGE_RCCS}
    ${CREDITS_RCCS}
  )

set_target_properties(reosCore
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
    RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
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
        ${GMSH_LIB}
)


TARGET_INCLUDE_DIRECTORIES(reosCore SYSTEM PUBLIC
    ${QCA_INCLUDE_DIR}
    ${QTKEYCHAIN_INCLUDE_DIR})

IF(GDAL_FOUND)
    TARGET_INCLUDE_DIRECTORIES(reosCore PUBLIC ${GDAL_INCLUDE_DIRS})
    TARGET_LINK_LIBRARIES(reosCore PUBLIC ${GDAL_LIBRARIES} )
ENDIF(GDAL_FOUND)

INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    ${CMAKE_SOURCE_DIR}/src/core/GIS/private
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork/simulation
    ${CMAKE_SOURCE_DIR}/src/core/mesh

    ${QGIS_INCLUDE_DIR}
    ${GMSH_INCLUDE_DIR}
)


include_directories(${CMAKE_SOURCE_DIR}/external/poly2tri)

SET(QGIS_PLUGINS_DIR "${QGIS_LIB_PATH}/qgis/plugins/")
ADD_DEFINITIONS(-DQGIS_PLUGINS="${QGIS_PLUGINS_DIR}")

# INSTALL HEADER

