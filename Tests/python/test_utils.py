"""Helper utilities for Reos python unit tests.

.. note:: This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
"""
__author__ = 'Vincent Cloarec'
__date__ = '30/04/2023'
__copyright__ = 'Copyright 2012, Reos Project'

import os

def test_data_path(theSubdir=None):
    """Return the absolute path to the REOS test data dir.

    Args:
       * theSubdir: (Optional) Additional subdir to add to the path
    """
    myPath = __file__
    myPath = os.path.split(os.path.dirname(myPath))
    if theSubdir is not None:
        myPath = os.path.abspath(os.path.join(myPath[0],
                                              'testData',
                                              theSubdir))
    else:
        myPath = os.path.abspath(os.path.join(myPath[0], 'testData'))
    return myPath