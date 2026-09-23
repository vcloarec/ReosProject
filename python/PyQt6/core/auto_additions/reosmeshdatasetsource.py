# The following has been generated automatically from src/core/mesh/reosmeshdatasetsource.h
# monkey patching scoped based enum
ReosMeshDatasetSource.Location.Vertex.__doc__ = ""
ReosMeshDatasetSource.Location.Face.__doc__ = ""
ReosMeshDatasetSource.Location.__doc__ = """

* ``Vertex``: 
* ``Face``: 

"""
# --
try:
    ReosMeshDatasetSource.__abstract_methods__ = ['groupCount', 'datasetCount', 'groupName', 'groupLocation', 'groupIsScalar', 'groupMinMax', 'groupReferenceTime', 'datasetRelativeTime', 'datasetIsValid', 'datasetMinMax', 'datasetValuesCount', 'datasetValues', 'activeFaces', 'datasetIndexClosestBeforeTime']
    ReosMeshDatasetSource.__group__ = ['mesh']
except (NameError, AttributeError):
    pass
