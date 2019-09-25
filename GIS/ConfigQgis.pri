INCLUDEPATH += $${QGIS_INCLUDE_PATH}

INCLUDEPATH += $${QGIS_SRC_PATH}/app
INCLUDEPATH += $${QGIS_SRC_PATH}/app/mesh
INCLUDEPATH += $${QGIS_SRC_PATH}/gui
INCLUDEPATH += $${QGIS_SRC_PATH}/core
INCLUDEPATH += $${QGIS_SRC_PATH}/core/symbology
INCLUDEPATH += $${QGIS_SRC_PATH}/core/raster
INCLUDEPATH += $${QGIS_SRC_PATH}/../external

RESOURCES += $${QGIS_SRC_PATH}/../images/images.qrc

FORMS += \
        $${QGIS_SRC_PATH}/ui/qgsrendererpropsdialogbase.ui \
        $${QGIS_SRC_PATH}/ui/effects/qgseffectstackpropertieswidgetbase.ui \
        $${QGIS_SRC_PATH}/ui/qgspalettedrendererwidgetbase.ui \
        $${QGIS_SRC_PATH}/ui/qgsrasterlayerpropertiesbase.ui \
        $${QGIS_SRC_PATH}/ui/qgslayertreeembeddedconfigwidgetbase.ui \
        $${QGIS_SRC_PATH}/ui/qgslabelingwidget.ui \
        $${QGIS_SRC_PATH}/ui/qgsvectorlayerpropertiesbase.ui \
        $${QGIS_SRC_PATH}/ui/mesh/qgsmeshlayerpropertiesbase.ui \
        $${QGIS_SRC_PATH}/ui/qgsvectorlayersaveasdialogbase.ui \




CONFIG(debug, debug|release) {
    message(debug mode)
    LIBS +=-L$${QGIS_LIB_DEBUG_PATH} -lqgis_core -lqgis_gui -lqgis_app -lqgis_analysis
}
else {
    message(release mode)
    LIBS +=-L$${QGIS_LIB_RELEASE_PATH} -lqgis_core -lqgis_gui -lqgis_app -lqgis_analysis
}


QT +=xml
QT +=svg

