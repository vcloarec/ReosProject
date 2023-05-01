from reos.core import *
import sys
import os

app = ReosApplication(list(map(os.fsencode,sys.argv)))
reos_core = app.coreModule()

reos_core.openProject('/home/vincent/lekan/Fango-use-case/fango.lkn')

network = reos_core.hydraulicNetwork()
extent = network.networkExtent()
print(extent.width(), ' x ', extent.height())

structures=network.hydraulicNetworkElements(ReosHydraulicStructure2D.staticType())
print(str(len(structures)))

structure2d=structures[0]
print('Structure name: ',structure2d.elementName())

structure2d.runSimulation(network.calculationContext())