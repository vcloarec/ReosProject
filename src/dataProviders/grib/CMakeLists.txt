# Reos licence GPL version 2
# Copyright (C) 2022 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_GRIB_SOURCES
    reosgribprovider.cpp
    reoseccodesreader.cpp
)

SET(REOS_GRIB_HEADERS
    reosgribprovider.h
    reoseccodesreader.h
)

IF(WITH_GUI)
    SET(REOS_GRIB_SOURCES ${REOS_GRIB_SOURCES}
        reosgribprecipitationwidget.cpp
    )
    SET(REOS_GRIB_HEADERS ${REOS_GRIB_HEADERS}
        reosgribprecipitationwidget.h
    )
ENDIF(WITH_GUI)

ADD_LIBRARY(provider_grib MODULE
    ${REOS_GRIB_SOURCES}
    ${REOS_GRIB_HEADERS}
)


TARGET_LINK_LIBRARIES(provider_grib
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        reosCore
)

IF (HAS_ECCODES)
    TARGET_LINK_LIBRARIES(provider_grib
        ${ECCODES_LIB})
ENDIF(HAS_ECCODES)

IF(WITH_GUI)
    TARGET_LINK_LIBRARIES(provider_grib
            reosGui)
ENDIF(WITH_GUI)


set_target_properties(provider_grib
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}/$<0:>${REOS_PROVIDERS_DIR}
    )

INCLUDE_DIRECTORIES(
    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    )

IF (HAS_ECCODES)
    INCLUDE_DIRECTORIES(${ECCODES_INCLUDE_DIR})
ENDIF(HAS_ECCODES)

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
    )
ENDIF(WITH_GUI)



INSTALL(TARGETS provider_grib
  LIBRARY DESTINATION bin/${REOS_PROVIDERS_DIR})



