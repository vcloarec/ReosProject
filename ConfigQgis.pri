INCLUDEPATH += $${QGIS_INCLUDE_PATH}/

INCLUDEPATH += $${QGIS_SRC_PATH}/app
INCLUDEPATH += $${QGIS_SRC_PATH}/app/3d
INCLUDEPATH += $${QGIS_SRC_PATH}/app/mesh
INCLUDEPATH += $${QGIS_SRC_PATH}/gui
INCLUDEPATH += $${QGIS_SRC_PATH}/core
INCLUDEPATH += $${QGIS_SRC_PATH}/core/symbology
INCLUDEPATH += $${QGIS_SRC_PATH}/core/raster
INCLUDEPATH += $${QGIS_SRC_PATH}/../external
INCLUDEPATH += $${QGIS_SRC_PATH}/../external/qt3dextra-headers

INCLUDEPATH += $${QGIS_SRC_PATH}/providers/mdal

RESOURCES += $${QGIS_SRC_PATH}/../images/images.qrc

FORMS +=\
        $${QGIS_SRC_PATH}/ui/effects/qgseffectstackpropertieswidgetbase.ui \
        #$${QGIS_SRC_PATH}/ui/qgslabelingwidget.ui \
        $${QGIS_SRC_PATH}/ui/qgsrendererpropsdialogbase.ui \
        $${QGIS_SRC_PATH}/ui/qgspalettedrendererwidgetbase.ui \
        $${QGIS_SRC_PATH}/ui/qgsrasterlayerpropertiesbase.ui \
        $${QGIS_SRC_PATH}/ui/qgslayertreeembeddedconfigwidgetbase.ui \
        $${QGIS_SRC_PATH}/ui/mesh/qgsmeshstaticdatasetwidgetbase.ui \
        #$${QGIS_SRC_PATH}/ui/qgsvectorlayerpropertiesbase.ui \
        $${QGIS_SRC_PATH}/ui/mesh/qgsmeshlayerpropertiesbase.ui \
        $${QGIS_SRC_PATH}/ui/mesh/qgsmeshdatasetgrouptreewidgetbase.ui \
        $${QGIS_SRC_PATH}/ui/qgsvectorlayersaveasdialogbase.ui


CONFIG(debug, debug|release) {
    message(debug mode)
    LIBS +=-L$${QGIS_LIB_DEBUG_PATH} -lqgis_core -lqgis_gui -lqgis_app -lqgis_analysis -lqgis_native -lqgis_3d
}
else {
    message(release mode)
    LIBS +=-L$${QGIS_LIB_RELEASE_PATH} -lqgis_core -lqgis_gui -lqgis_app -lqgis_analysis -lqgis_native -lqgis_3d
}


QT +=xml
QT +=svg

