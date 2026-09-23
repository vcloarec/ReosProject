# The following has been generated automatically from src/core/GIS/reosgisengine.h
ReosGisEngine.NoLayer = ReosGisEngine.LayerType.NoLayer
ReosGisEngine.VectorLayer = ReosGisEngine.LayerType.VectorLayer
ReosGisEngine.RasterLayer = ReosGisEngine.LayerType.RasterLayer
ReosGisEngine.MeshLayer = ReosGisEngine.LayerType.MeshLayer
ReosGisEngine.NotSupported = ReosGisEngine.LayerType.NotSupported
try:
    ReosGisEngine.staticName = staticmethod(ReosGisEngine.staticName)
    ReosGisEngine.crsFromEPSG = staticmethod(ReosGisEngine.crsFromEPSG)
    ReosGisEngine.crsFromProj = staticmethod(ReosGisEngine.crsFromProj)
    ReosGisEngine.crsWkt1 = staticmethod(ReosGisEngine.crsWkt1)
    ReosGisEngine.crsEsriWkt = staticmethod(ReosGisEngine.crsEsriWkt)
    ReosGisEngine.crsIsValid = staticmethod(ReosGisEngine.crsIsValid)
    ReosGisEngine.projStringToWkt = staticmethod(ReosGisEngine.projStringToWkt)
    ReosGisEngine.createRasterDigitalElevationModel = staticmethod(ReosGisEngine.createRasterDigitalElevationModel)
    ReosGisEngine.polygonAreaWithCrs = staticmethod(ReosGisEngine.polygonAreaWithCrs)
    ReosGisEngine.openPolygonVectorLayerSource = staticmethod(ReosGisEngine.openPolygonVectorLayerSource)
    ReosGisEngine.transformToCoordinates = staticmethod(ReosGisEngine.transformToCoordinates)
    ReosGisEngine.distance = staticmethod(ReosGisEngine.distance)
    ReosGisEngine.locateOnPolyline = staticmethod(ReosGisEngine.locateOnPolyline)
    ReosGisEngine.setPointOnPolyline = staticmethod(ReosGisEngine.setPointOnPolyline)
    ReosGisEngine.factorUnitToMeter = staticmethod(ReosGisEngine.factorUnitToMeter)
    ReosGisEngine.createProjectFile = staticmethod(ReosGisEngine.createProjectFile)
    ReosGisEngine.gisEngineName = staticmethod(ReosGisEngine.gisEngineName)
    ReosGisEngine.gisEngineVersion = staticmethod(ReosGisEngine.gisEngineVersion)
    ReosGisEngine.gisEngineLink = staticmethod(ReosGisEngine.gisEngineLink)
    ReosGisEngine.wktEPSGCrs = staticmethod(ReosGisEngine.wktEPSGCrs)
    ReosGisEngine.__signal_arguments__ = {'crsChanged': ['wktCrs: str'], 'layerRemoved': ['layerId: str'], 'temporalRangeChanged': ['startTime: QDateTime', 'endTime: QDateTime']}
    ReosGisEngine.__group__ = ['GIS']
except (NameError, AttributeError):
    pass
