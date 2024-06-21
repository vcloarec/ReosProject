# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_HECRAS_SOURCES
    reoshecrascontroller.cpp
    reoshecrassimulation.cpp
    reoshecrassimulationresults.cpp
    reoshecrasproject.cpp
)

SET(REOS_HECRAS_HEADERS
    reoshecrascontroller.h
    reoshecrassimulation.h
    reoshecrassimulationresults.h
    reoshecrasproject.h
)

IF(WITH_GUI)
SET(REOS_HECRAS_SOURCES
    reoshecrassimulationimportwidget.cpp
    reoshecrassimulationeditwidget.cpp
)

SET(REOS_HECRAS_HEADERS
    reoshecrassimulationimportwidget.h
    reoshecrassimulationeditwidget.h
)
ENDIF(WITH_GUI)


ADD_LIBRARY(hecras_engine MODULE
    ${REOS_HECRAS_SOURCES}
    ${REOS_HECRAS_HEADERS}
)

TARGET_LINK_LIBRARIES(hecras_engine
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        ${MDAL_LIB}
        reosCore
        reosHecDss
)

IF(WITH_GUI)
    TARGET_LINK_LIBRARIES(hecras_engine
            reosGui)
ENDIF(WITH_GUI)

set_target_properties(hecras_engine
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_ENGINES_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_ENGINES_DIR}
    )

IF (MSVC)
  add_compile_definitions(_USE_MATH_DEFINES)
ENDIF (MSVC)

INCLUDE_DIRECTORIES(
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

    ${CMAKE_SOURCE_DIR}/src/dataProviders/hec-dss
    ${CMAKE_SOURCE_DIR}/src/simulationEngines/hecras

    ${QGIS_INCLUDE_DIR}
    ${MDAL_INCLUDE_DIR}
    )

IF(WITH_GUI)
    INCLUDE_DIRECTORIES(
        ${CMAKE_BINARY_DIR}/src/ui
        ${CMAKE_SOURCE_DIR}/src/gui
        ${CMAKE_SOURCE_DIR}/src/gui/chart
        ${CMAKE_SOURCE_DIR}/src/gui/form
        ${CMAKE_SOURCE_DIR}/src/gui/data
        ${CMAKE_SOURCE_DIR}/src/gui/GIS
        ${CMAKE_SOURCE_DIR}/src/gui/watershed
        ${CMAKE_SOURCE_DIR}/src/gui/rainfall
        ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork
        ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork/structure2d

        ${QGIS_INCLUDE_DIR}
        ${MDAL_INCLUDE_DIR}
        )
ENDIF(WITH_GUI)

INSTALL(TARGETS hecras_engine
  RUNTIME DESTINATION bin/${REOS_ENGINES_DIR}
  LIBRARY DESTINATION bin/${REOS_ENGINES_DIR})
