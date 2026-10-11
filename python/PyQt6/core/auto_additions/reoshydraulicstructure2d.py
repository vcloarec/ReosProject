# The following has been generated automatically from src/core/hydraulicNetwork/reoshydraulicstructure2d.h
ReosHydraulicStructure2D.GeometryEditable = ReosHydraulicStructure2D.Structure2DCapability.GeometryEditable
ReosHydraulicStructure2D.MultiSimulation = ReosHydraulicStructure2D.Structure2DCapability.MultiSimulation
ReosHydraulicStructure2D.DefinedExternally = ReosHydraulicStructure2D.Structure2DCapability.DefinedExternally
ReosHydraulicStructure2D.GriddedPrecipitation = ReosHydraulicStructure2D.Structure2DCapability.GriddedPrecipitation
ReosHydraulicStructure2D.Structure2DCapability.baseClass = ReosHydraulicStructure2D
ReosHydraulicStructure2D.Structure2DCapabilities = lambda flags=0: ReosHydraulicStructure2D.Structure2DCapability(flags)
ReosHydraulicStructure2D.Structure2DCapabilities.baseClass = ReosHydraulicStructure2D
Structure2DCapabilities = ReosHydraulicStructure2D  # dirty hack since SIP seems to introduce the flags in module
try:
    ReosHydraulicStructure2D.staticType = staticmethod(ReosHydraulicStructure2D.staticType)
    ReosHydraulicStructure2D.__overridden_methods__ = ['type', 'extent', 'defaultDisplayName', 'currentElementTimeStep', 'mapTimeStep', 'timeWindow', 'icon']
    ReosHydraulicStructure2D.__group__ = ['hydraulicNetwork']
except (NameError, AttributeError):
    pass
