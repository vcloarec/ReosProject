# The following has been generated automatically from src/core/hydrograph/reoshydrographsource.h
ReosHydrographJunction.None_ = ReosHydrographJunction.InternalHydrographOrigin.None_
ReosHydrographJunction.RunoffHydrograph = ReosHydrographJunction.InternalHydrographOrigin.RunoffHydrograph
ReosHydrographJunction.GaugedHydrograph = ReosHydrographJunction.InternalHydrographOrigin.GaugedHydrograph
try:
    ReosHydrographJunction.__attribute_docs__ = {'internalHydrographPointerChange': 'Emitted when the internal hydrograph pointer change\n'}
    ReosHydrographJunction.staticType = staticmethod(ReosHydrographJunction.staticType)
    ReosHydrographJunction.__overridden_methods__ = ['outputHydrograph', 'type', 'position', 'spatialPosition', 'setPosition', 'defaultDisplayName', 'calculationInProgress', 'calculationMaxProgression', 'calculationProgression', 'timeWindow', 'icon']
    ReosHydrographJunction.__group__ = ['hydrograph']
except (NameError, AttributeError):
    pass
try:
    ReosHydrographNode.staticType = staticmethod(ReosHydrographNode.staticType)
    ReosHydrographNode.__overridden_methods__ = ['type', 'position', 'spatialPosition']
    ReosHydrographNode.__group__ = ['hydrograph']
except (NameError, AttributeError):
    pass
try:
    ReosHydrographSource.staticType = staticmethod(ReosHydrographSource.staticType)
    ReosHydrographSource.__abstract_methods__ = ['outputHydrograph']
    ReosHydrographSource.__overridden_methods__ = ['type']
    ReosHydrographSource.__group__ = ['hydrograph']
except (NameError, AttributeError):
    pass
