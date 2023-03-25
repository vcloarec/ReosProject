from reos.core import *
from PyQt5.QtCore import QTimer


def on_data_reloaded( hyd ):
    value_count = hyd.valueCount()
    if value_count > 0:
        print(str(hyd.valueAt(value_count-1)))

def reload(hyd):
    hyd.reload()


app = ReosApplication([])
core_module = app.coreModule()

station_id = 'J881301001'

hydrograph = ReosHydrograph(None, 'hub-eau-hydrometry', station_id)

timer = QTimer()
timer.timeout.connect(lambda: reload(hydrograph))
timer.start(5000)
hydrograph.dataReset.connect(lambda: on_data_reloaded(hydrograph))


app.exec()



