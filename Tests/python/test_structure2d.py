""".. note:: This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
"""
__author__ = 'Vincent Cloarec'
__date__ = '30/04/2023'
__copyright__ = 'Copyright 2012, Reos Project'

import os
import sys
import unittest

import apport.fileutils

from test_utils import *

from reos.core import *


class TestRunningStructure2d(unittest.TestCase):
    def setUp(self):
        self.app = ReosApplication(list(map(os.fsencode, sys.argv)))
        self.core_module = self.app.coreModule()

    def test_running_simulation(self):
        path = os.path.join(test_data_path(), 'structure2D', 'steep-channel', 'steep_channel.lkn')
        print(path)
        self.assertTrue(self.core_module.openProject(path))
        network = self.core_module.hydraulicNetwork()
        calculation_context = network.calculationContext()
        structures = network.hydraulicNetworkElements(ReosHydraulicStructure2D.staticType())
        print(str(len(structures)))
        structure2d = structures[0]
        self.assertTrue(structure2d.runSimulation(calculation_context))


if __name__ == '__main__':
    unittest.main()
