# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_TELEMAC_SOURCES
    reostelemac2dsimulation.cpp
    reostelemacsimulationeditwidget.cpp
    reostelemac2dsimulationresults.cpp
    reostelemac2dinitialcondition.cpp
)

SET(REOS_TELEMAC_HEADERS
    reostelemac2dsimulation.h
    reostelemacsimulationeditwidget.h
    reostelemac2dsimulationresults.h
    reostelemac2dinitialcondition.h
)

ADD_LIBRARY(telemac_engine MODULE
    ${REOS_TELEMAC_SOURCES}
    ${REOS_TELEMAC_HEADERS}
)


TARGET_LINK_LIBRARIES(telemac_engine
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
        ${MDAL_LIB}
        reosGui
        reosCore
)


set_target_properties(telemac_engine
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/${REOS_ENGINES_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/${REOS_ENGINES_DIR}
    )

IF (MSVC)
  add_compile_definitions(_USE_MATH_DEFINES)
ENDIF (MSVC)

INCLUDE_DIRECTORIES(
    ${CMAKE_BINARY_DIR}/src/ui
    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/form
    ${CMAKE_SOURCE_DIR}/src/gui/data
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/watershed
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork/structure2d
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/mesh
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork/simulation

    ${QGIS_INCLUDE_DIR}
    ${MDAL_INCLUDE_DIR}
    )



INSTALL(TARGETS telemac_engine 
  RUNTIME DESTINATION ${REOS_ENGINES_DIR}
  LIBRARY DESTINATION ${REOS_ENGINES_DIR})



