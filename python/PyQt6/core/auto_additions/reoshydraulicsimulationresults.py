# The following has been generated automatically from src/core/hydraulicNetwork/simulation/reoshydraulicsimulationresults.h
# monkey patching scoped based enum
ReosHydraulicSimulationResults.DatasetType.NoType.__doc__ = "None type"
ReosHydraulicSimulationResults.DatasetType.WaterLevel.__doc__ = "WaterLevel type"
ReosHydraulicSimulationResults.DatasetType.WaterDepth.__doc__ = "WaterDepth type"
ReosHydraulicSimulationResults.DatasetType.Velocity.__doc__ = "Velocity type"
ReosHydraulicSimulationResults.DatasetType.__doc__ = """

* ``NoType``: None type
* ``WaterLevel``: WaterLevel type
* ``WaterDepth``: WaterDepth type
* ``Velocity``: Velocity type

"""
# --
ReosHydraulicSimulationResults.DatasetType.baseClass = ReosHydraulicSimulationResults
try:
    ReosHydraulicSimulationResults.__overridden_methods__ = ['groupCount', 'groupName', 'groupIsScalar']
    ReosHydraulicSimulationResults.__group__ = ['hydraulicNetwork', 'simulation']
except (NameError, AttributeError):
    pass
