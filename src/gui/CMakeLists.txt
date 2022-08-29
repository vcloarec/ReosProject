# Reos GPL v2.0
# Copyright (C) 2020 Vincent Cloarec (vcloarec at gmail dot com)

SET(REOS_GUI_SOURCES
    reosaboutwidget.cpp
    reosdockwidget.cpp
    reoslanguageselectionwidget.cpp
    reosmainwindow.cpp
    reosmessagebox.cpp
    reosstartingwidget.cpp
    reosversionmessagebox.cpp
    reosprocesscontroler.cpp
    reosmenupopulator.cpp
    reosactionwidget.cpp
    reosnotificationbutton.cpp
    reosguicontext.cpp
    reoscolorbutton.cpp
    reosspeedwidget.cpp

    GIS/reosdigitalelevationmodelcombobox.cpp
    GIS/reosmap.cpp
    GIS/reosmapitem.cpp
    GIS/reosmaptool.cpp
    GIS/reosgislayerswidget.cpp
    GIS/reosmaptooleditgeometrystructure.cpp
    GIS/reoseditpolylinestructurewidget.cpp
    GIS/reosmeshscalarrenderingwidget.cpp
    GIS/reosmeshvectorrenderingwidget.cpp
    GIS/reosmaptooleditmeshframe.cpp
    GIS/reostemporalcontrollerwidget.cpp
    GIS/3D/reos3dview.cpp
    GIS/3D/reoslightwidget.cpp
    GIS/3D/reosverticalexaggerationwidget.cpp
    GIS/3D/reos3dterrainsettingswidget.cpp

    GIS/private/reosmappolygon_p.cpp
    GIS/private/reosmaptool_p.cpp
    GIS/private/reoslayertreecontextmenuprovider_p.cpp
    GIS/private/reoslayertreeviewdemindicator.cpp
    GIS/private/reosmaptooleditpolylinestructure_p.cpp
    GIS/private/reosmappolylinesstructure_p.cpp
    GIS/private/reosmaptooleditpolygonstructure_p.cpp
    GIS/private/reosmappolygonstructure_p.cpp
    GIS/private/reosmaptooleditmeshframe_p.cpp
    GIS/private/reostemporalcontroller_p.cpp

    watershed/reosdelineatingwatershedwidget.cpp
    watershed/reoswatershedwidget.cpp
    watershed/reoslongitudinalprofilewidget.cpp
    watershed/reosconcentrationtimewidget.cpp
    watershed/reosmeteorologicmodelwidget.cpp
    watershed/reosrunoffmanager.cpp
    watershed/reosrunoffhydrographwidget.cpp
    watershed/reosexportwatershedtovectordialog.cpp

    rainfall/reosrainfallmanager.cpp
    rainfall/reosrainfallintensitydurationwidget.cpp
    rainfall/reosintensitydurationselectedcurvewidget.cpp
    rainfall/reosrainfalldataform.cpp

    chart/reosplotwidget.cpp
    chart/reoseditableprofile.cpp
    chart/reosplottimeconstantinterval.cpp
    chart/reosplotidfcurve.cpp
    chart/reosplotitemlist.cpp
    chart/private/reosprofileplot_p.cpp
    chart/private/reosplot_p.cpp
    chart/private/reosplotpicker_p.cpp
    chart/private/reosidfplot_p.cpp
    chart/private/reosplottimeline.cpp

    form/reosparameterwidget.cpp
    form/reosformwidget.cpp
    form/reostableview.cpp

    data/reosimportfromtextfile.cpp
    data/reosdataprovidergui.cpp
    data/reostimeseriesvariabletimestepreadonlymodel.cpp
    data/reosvariabletimesteptimeseriesgroupwidget.cpp

    hydrograph/reoshydrographeditingwidget.cpp
    hydrograph/reoshydrographroutingpropertieswidget.cpp
    hydrograph/reosgaugedhydrographwidget.cpp

    mesh/reosmeshgeneratorgui.cpp

    hydraulicNetwork/reoshydraulicnetworkwidget.cpp
    hydraulicNetwork/reoshydraulicnetworkmapitemfactory.cpp
    hydraulicNetwork/reosmaptoolhydraulicnetwork.cpp
    hydraulicNetwork/reoshydraulicelementpropertieswidget.cpp
    hydraulicNetwork/reoshydraulichydrographjunctionpropertieswidget.cpp
    hydraulicNetwork/reoshydraulicschemewidget.cpp
    hydraulicNetwork/structure2d/reoshydraulicstructure2dproperties.cpp
    hydraulicNetwork/structure2d/reosedithydraulicstructure2dwidget.cpp
    hydraulicNetwork/structure2d/reosgmshresolutioncontrollerwidget.cpp
    hydraulicNetwork/structure2d/reosmeshtopographywidget.cpp
    hydraulicNetwork/structure2d/reoseditmeshelementwidget.cpp
    hydraulicNetwork/structure2d/reosroughnesswidget.cpp
    hydraulicNetwork/structure2d/reoszvaluemodificationwidget.cpp
    hydraulicNetwork/structure2d/reoshydraulic2dsimulationwidget.cpp
    hydraulicNetwork/structure2d/reoshydraulicsimulationconsole.cpp
    hydraulicNetwork/structure2d/reoshydraulicstructureresultexport.cpp
    hydraulicNetwork/structure2d/reoshydraulicstructureprofileswidget.cpp

    hydraulicNetwork/private/reosmaptoolhydraulicnetwork_p.cpp
    )

SET(REOS_GUI_HEADERS
    reosgui.h
    reosaboutwidget.h
    reosdockwidget.h
    reoslanguageselectionwidget.h
    reosmainwindow.h
    reosmessagebox.h
    reosstartingwidget.h
    reosversionmessagebox.h
    reosprocesscontroler.h
    reosmenupopulator.h
    reosactionwidget.h
    reosnotificationbutton.h
    reosguicontext.h
    reoscolorbutton.h
    reosspeedwidget.h

    GIS/reosdigitalelevationmodelcombobox.h
    GIS/reosmap.h
    GIS/reosmapitem.h
    GIS/reosmaptool.h
    GIS/reosgislayerswidget.h
    GIS/reosmaptooleditgeometrystructure.h
    GIS/reoseditpolylinestructurewidget.h
    GIS/reosmeshscalarrenderingwidget.h
    GIS/reosmeshvectorrenderingwidget.h
    GIS/reosmaptooleditmeshframe.h
    GIS/reostemporalcontrollerwidget.h
    GIS/3D/reos3dview.h
    GIS/3D/reoslightwidget.h
    GIS/3D/reosverticalexaggerationwidget.h
    GIS/3D/reos3dterrainsettingswidget.h

    watershed/reosdelineatingwatershedwidget.h
    watershed/reoswatershedwidget.h
    watershed/reoslongitudinalprofilewidget.h
    watershed/reosconcentrationtimewidget.h
    watershed/reosmeteorologicmodelwidget.h
    watershed/reosrunoffmanager.h
    watershed/reosrunoffhydrographwidget.h
    watershed/reosexportwatershedtovectordialog.h

    rainfall/reosrainfallmanager.h
    rainfall/reosrainfallintensitydurationwidget.h
    rainfall/reosintensitydurationselectedcurvewidget.h
    rainfall/reosrainfalldataform.h

    chart/reosplotwidget.h
    chart/reoseditableprofile.h
    chart/reosplottimeconstantinterval.h
    chart/reosplotidfcurve.h
    chart/reosplotitemlist.h

    form/reosparameterwidget.h
    form/reosformwidget.h
    form/reostableview.h

    data/reosimportfromtextfile.h
    data/reosdataprovidergui.h
    data/reostimeseriesvariabletimestepreadonlymodel.h
    data/reosvariabletimesteptimeseriesgroupwidget.h

    hydrograph/reoshydrographeditingwidget.h
    hydrograph/reoshydrographroutingpropertieswidget.h
    hydrograph/reosgaugedhydrographwidget.h

    mesh/reosmeshgeneratorgui.h

    hydraulicNetwork/reoshydraulicnetworkwidget.h
    hydraulicNetwork/reosmaptoolhydraulicnetwork.h
    hydraulicNetwork/reoshydraulicnetworkmapitemfactory.h
    hydraulicNetwork/reoshydraulicelementpropertieswidget.h
    hydraulicNetwork/reoshydraulichydrographjunctionpropertieswidget.h
    hydraulicNetwork/reoshydraulicschemewidget.h
    hydraulicNetwork/structure2d/reoshydraulicstructure2dproperties.h
    hydraulicNetwork/structure2d/reosedithydraulicstructure2dwidget.h
    hydraulicNetwork/structure2d/reosgmshresolutioncontrollerwidget.h
    hydraulicNetwork/structure2d/reosroughnesswidget.cpp
    hydraulicNetwork/structure2d/reosmeshtopographywidget.h
    hydraulicNetwork/structure2d/reoseditmeshelementwidget.h
    hydraulicNetwork/structure2d/reoszvaluemodificationwidget.h    
    hydraulicNetwork/structure2d/reoshydraulic2dsimulationwidget.h
    hydraulicNetwork/structure2d/reoshydraulicsimulationconsole.h
    hydraulicNetwork/structure2d/reoshydraulicstructureresultexport.h
    hydraulicNetwork/structure2d/reoshydraulicstructureprofileswidget.h
)

SET(REOS_GUI_HEADERS_PRIVATE
    GIS/private/reosmappolygon_p.h
    GIS/private/reosmaptool_p.h
    GIS/private/reoslayertreecontextmenuprovider_p.h
    GIS/private/reoslayertreeviewdemindicator.h
    GIS/private/reosmaptooleditpolylinestructure_p.h
    GIS/private/reosmappolylinesstructure_p.h
    GIS/private/reosmaptooleditpolygonstructure_p.h
    GIS/private/reosmappolygonstructure_p.h
    GIS/private/reosmaptooleditmeshframe_p.h
    GIS/private/reostemporalcontroller_p.h

    chart/private/reosplot_p.h
    chart/private/reosidfplot_p.h
    chart/private/reosplottimeline.h

    hydraulicNetwork/private/reosmaptoolhydraulicnetwork_p.h
    )

ADD_LIBRARY(reosGui SHARED
    ${REOS_GUI_HEADERS}
    #${REOS_GUI_HEADERS_PRIVATE}
    ${REOS_GUI_SOURCES}
  )

TARGET_LINK_LIBRARIES(reosGui PUBLIC
        ${Qt5Xml_LIBRARIES}
        ${Qt5Core_LIBRARIES}
        ${Qt5Gui_LIBRARIES}
        ${Qt5Widgets_LIBRARIES}
        ${Qt5Svg_LIBRARIES}
        ${Qt5PrintSupport_LIBRARIES}
        ${Qt5Network_LIBRARIES}
        ${Qt5Sql_LIBRARIES}
        ${Qt5Concurrent_LIBRARIES}
        ${Qt5Charts_LIBRARIES}
        ${Qt5WebKitWidgets_LIBRARIES}
        ${QGIS_LIBS}
        ${QGIS_APP_LIB}
        ${QWT_LIB}
        reosCore
)

IF (MSVC)
  add_compile_definitions(_USE_MATH_DEFINES)
  add_compile_definitions(QWT_DLL)
ENDIF (MSVC)

IF(WITH_QTWEBKIT)
  TARGET_LINK_LIBRARIES(reosGui PUBLIC
        ${Qt5WebKitWidgets_LIBRARIES}
  )
  add_compile_definitions(WITH_QTWEBKIT)
ENDIF(WITH_QTWEBKIT)

set_target_properties(reosGui
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
    LIBRARY_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
    RUNTIME_OUTPUT_DIRECTORY ${REOS_OUTPUT_DIRECTORY}
    )

IF(WIN32)
  INSTALL(TARGETS reosGui RUNTIME DESTINATION lib)
ELSE(WIN32)
  INSTALL(TARGETS reosGui LIBRARY DESTINATION lib)
ENDIF(WIN32)

INCLUDE_DIRECTORIES(
    ${CMAKE_BINARY_DIR}/src/ui

    ${CMAKE_SOURCE_DIR}/src/core
    ${CMAKE_SOURCE_DIR}/src/core/GIS
    ${CMAKE_SOURCE_DIR}/src/core/process
    ${CMAKE_SOURCE_DIR}/src/core/quantity
    ${CMAKE_SOURCE_DIR}/src/core/raster
    ${CMAKE_SOURCE_DIR}/src/core/utils
    ${CMAKE_SOURCE_DIR}/src/core/watershed
    ${CMAKE_SOURCE_DIR}/src/core/rainfall
    ${CMAKE_SOURCE_DIR}/src/core/data
    ${CMAKE_SOURCE_DIR}/src/core/hydrograph
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/core/hydraulicNetwork/simulation/
    ${CMAKE_SOURCE_DIR}/src/core/mesh

    ${CMAKE_SOURCE_DIR}/src/gui
    ${CMAKE_SOURCE_DIR}/src/gui/data
    ${CMAKE_SOURCE_DIR}/src/gui/hydrograph
    ${CMAKE_SOURCE_DIR}/src/gui/GIS
    ${CMAKE_SOURCE_DIR}/src/gui/GIS/3D
    ${CMAKE_SOURCE_DIR}/src/gui/GIS/private
    ${CMAKE_SOURCE_DIR}/src/gui/chart
    ${CMAKE_SOURCE_DIR}/src/gui/chart/private
    ${CMAKE_SOURCE_DIR}/src/gui/form
    ${CMAKE_SOURCE_DIR}/src/gui/rainfall
    ${CMAKE_SOURCE_DIR}/src/gui/hydrograph
    ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork
    ${CMAKE_SOURCE_DIR}/src/gui/hydraulicNetwork/structure2d
    ${CMAKE_SOURCE_DIR}/src/gui/watershed
    ${CMAKE_SOURCE_DIR}/src/gui/mesh
    ${QGIS_INCLUDE_DIR}
    ${QSCISCINTILLA_INCLUDE_DIR}
    ${QWT_INCLUDE}
    ${QGIS_APP_INCLUDE}
    ${QGIS_3D_EXTRA_HEADER}

)


#TARGET_INCLUDE_DIRECTORIES(reosGui
#    PUBLIC
#    $<INSTALL_INTERFACE:include>
#    PRIVATE
#    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
#    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}>
#  )



# INSTALL HEADER
INSTALL(FILES ${REOS_GUI_HEADERS} DESTINATION include)
