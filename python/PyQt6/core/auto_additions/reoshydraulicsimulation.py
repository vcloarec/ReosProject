# The following has been generated automatically from src/core/hydraulicNetwork/simulation/reoshydraulicsimulation.h
# monkey patching scoped based enum
ReosHydraulicSimulation.Capability.Hotstart.__doc__ = "If the simulation support hot start"
ReosHydraulicSimulation.Capability.__doc__ = """

* ``Hotstart``: If the simulation support hot start

"""
# --
ReosHydraulicSimulation.Capability.baseClass = ReosHydraulicSimulation
ReosHydraulicSimulation.Capabilities = lambda flags=0: ReosHydraulicSimulation.Capability(flags)
ReosHydraulicSimulation.Capabilities.baseClass = ReosHydraulicSimulation
Capabilities = ReosHydraulicSimulation  # dirty hack since SIP seems to introduce the flags in module
ReosSimulationEngineFactory.ImportStructure2D = ReosSimulationEngineFactory.SimulationEngineCapability.ImportStructure2D
ReosSimulationEngineFactory.CanBeCreated = ReosSimulationEngineFactory.SimulationEngineCapability.CanBeCreated
ReosSimulationEngineFactory.SimulationEngineCapability.baseClass = ReosSimulationEngineFactory
ReosSimulationEngineFactory.SimulationEngineCapabilities = lambda flags=0: ReosSimulationEngineFactory.SimulationEngineCapability(flags)
ReosSimulationEngineFactory.SimulationEngineCapabilities.baseClass = ReosSimulationEngineFactory
SimulationEngineCapabilities = ReosSimulationEngineFactory  # dirty hack since SIP seems to introduce the flags in module
try:
    ReosSimulationEngineRegistery.instance = staticmethod(ReosSimulationEngineRegistery.instance)
    ReosSimulationEngineRegistery.__group__ = ['hydraulicNetwork', 'simulation']
except (NameError, AttributeError):
    pass
try:
    ReosHydraulicSimulation.__virtual_methods__ = ['hasCapability', 'setHotStartSchemeId', 'setHotStartTimeStepIndex', 'setHotStartUseLastTimeStep']
    ReosHydraulicSimulation.__abstract_methods__ = ['key', 'representativeTimeStep', 'representative2DTimeStep', 'hasResult', 'removeResults', 'engineName', 'externalTimeWindow']
    ReosHydraulicSimulation.__group__ = ['hydraulicNetwork', 'simulation']
except (NameError, AttributeError):
    pass
try:
    ReosSimulationEngineFactory.__group__ = ['hydraulicNetwork', 'simulation']
except (NameError, AttributeError):
    pass
