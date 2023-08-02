# The following has been generated automatically from src/core/hydraulicNetwork/simulation/reoshydraulicsimulation.h
# monkey patching scoped based enum
ReosHydraulicSimulation.Capability.Hotstart.__doc__ = "If the simulation support hot start"
ReosHydraulicSimulation.Capability.__doc__ = '\n\n' + '* ``Hotstart``: ' + ReosHydraulicSimulation.Capability.Hotstart.__doc__
# --
ReosHydraulicSimulation.Capability.baseClass = ReosHydraulicSimulation
ReosHydraulicSimulation.Capabilities.baseClass = ReosHydraulicSimulation
Capabilities = ReosHydraulicSimulation  # dirty hack since SIP seems to introduce the flags in module
ReosSimulationEngineFactory.SimulationEngineCapability.baseClass = ReosSimulationEngineFactory
ReosSimulationEngineFactory.SimulationEngineCapabilities.baseClass = ReosSimulationEngineFactory
SimulationEngineCapabilities = ReosSimulationEngineFactory  # dirty hack since SIP seems to introduce the flags in module
