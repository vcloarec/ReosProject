# The following has been generated automatically from src/core/hydraulicNetwork/reoshydraulicnetwork.h
try:
    ReosHydraulicNetworkElement.__attribute_docs__ = {'timeWindowChanged': 'Emitted whrn the time window of the element is changed\n', 'mapTimeStepChanged': 'Emitted whrn the map time step of the element is changed\n'}
    ReosHydraulicNetworkElement.hydraulicStructure2DType = staticmethod(ReosHydraulicNetworkElement.hydraulicStructure2DType)
    ReosHydraulicNetworkElement.hydrographJunction = staticmethod(ReosHydraulicNetworkElement.hydrographJunction)
    ReosHydraulicNetworkElement.staticType = staticmethod(ReosHydraulicNetworkElement.staticType)
    ReosHydraulicNetworkElement.__virtual_methods__ = ['defaultDisplayName', 'destroy', 'calculationInProgress', 'calculationMaxProgression', 'calculationProgression', 'isAutoSelectable', 'isRemovable', 'currentElementTimeStep', 'timeWindow', 'mapTimeStep', 'icon']
    ReosHydraulicNetworkElement.__abstract_methods__ = ['extent']
    ReosHydraulicNetworkElement.__overridden_methods__ = ['type']
    ReosHydraulicNetworkElement.__group__ = ['hydraulicNetwork']
except (NameError, AttributeError):
    pass
try:
    ReosHydraulicNetwork.__attribute_docs__ = {'schemeChanged': 'Emited when the current scheme had changes or had been changed (not the\nsame current scheme anymore)\n'}
    ReosHydraulicNetwork.staticName = staticmethod(ReosHydraulicNetwork.staticName)
    ReosHydraulicNetwork.__overridden_methods__ = ['uselessFiles']
    ReosHydraulicNetwork.__signal_arguments__ = {'elementAdded': ['elem: ReosHydraulicNetworkElement', 'select: bool'], 'elementWillBeRemoved': ['elem: ReosHydraulicNetworkElement'], 'elementPositionHasChanged': ['elem: ReosHydraulicNetworkElement']}
    ReosHydraulicNetwork.__group__ = ['hydraulicNetwork']
except (NameError, AttributeError):
    pass
try:
    ReosHydraulicNetworkContext.__group__ = ['hydraulicNetwork']
except (NameError, AttributeError):
    pass
