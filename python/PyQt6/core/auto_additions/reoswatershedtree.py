# The following has been generated automatically from src/core/watershed/reoswatershedtree.h
try:
    ReosWatershedTree.__attribute_docs__ = {'treeWillBeReset': 'Emitted before the tree will be reset\n', 'treeReset': 'Emitted when the tree is reset\n', 'watershedWillBeAdded': 'Emitted before a watershed will be added\n', 'watershedAdded': 'emitted when watershed is added with the pointer to the directly\ndownsteam watershed (None if added watershed is a the extreme\ndownstream)\n', 'watershedWillBeRemoved': 'Emitted when a watershed will be remove with a pointer to the removed\nwatershed\n', 'watershedRemoved': 'Emitted whan a whatershed is removed\n', 'watershedChanged': 'Emitted when a watershed changed\n'}
    ReosWatershedTree.__signal_arguments__ = {'watershedAdded': [': ReosWatershed'], 'watershedWillBeRemoved': [': ReosWatershed']}
    ReosWatershedTree.__group__ = ['watershed']
except (NameError, AttributeError):
    pass
