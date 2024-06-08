# Reos licence GPL version 2
# Copyright (C) 2025 vortex-io (vcloarec at vortex-io.fr)

SET(REOS_METEO_HDF5_SOURCES
    reosmeteohdf5.cpp
)

SET(REOS_METEO_HDF5_HEADERS
    reosmeteohdf5.h

)

#IF(WITH_GUI)
    #SET(REOS_MF_API_SOURCES ${REOS_MF_API_SOURCES}
        #reosmeteofrancearomeapiwidget.cpp
    #)

    #SET(REOS_MF_API_HEADERS ${REOS_MF_API_HEADERS}
     #reosmeteofrancearomeapiwidget.h
    #)
#ENDIF(WITH_GUI)

#SET(LOGO_RCCS ${CMAKE_SOURCE_DIR}/images/providers/hub-eau/hub-eau-images.qrc)


ADD_LIBRARY(provider_meteo_hdf5 MODULE
    ${REOS_METEO_HDF5_SOURCES}
    ${REOS_METEO_HDF5_HEADERS})

TARGET_LINK_LIBRARIES(provider_meteo_hdf5
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

IF(WITH_GUI)
    TARGET_LINK_LIBRARIES(provider_meteo_hdf5
            reosGui)
ENDIF(WITH_GUI)

set_target_properties(provider_meteo_hdf5
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
    ${CMAKE_SOURCE_DIR}/src/core/network
    )

#IF(WITH_GUI)
    #INCLUDE_DIRECTORIES(
        #${CMAKE_BINARY_DIR}/src/ui
        #${CMAKE_SOURCE_DIR}/src/gui
        #${CMAKE_SOURCE_DIR}/src/gui/chart
        #${CMAKE_SOURCE_DIR}/src/gui/form
        #${CMAKE_SOURCE_DIR}/src/gui/data
        #${CMAKE_SOURCE_DIR}/src/gui/GIS
        #${CMAKE_SOURCE_DIR}/src/gui/watershed
        #${CMAKE_SOURCE_DIR}/src/gui/rainfall
    #)
#ENDIF(WITH_GUI)


INSTALL(TARGETS provider_meteo_hdf5
 LIBRARY DESTINATION bin/${REOS_PROVIDERS_DIR})



