from jedi.api import file_name
from pip._internal import network
from reos.core import *
from PyQt5.QtCore import Qt, QTimer, QDateTime
import sys
import os


def to_std_out(string):
    print(string, file=sys.stdout)


# A Class that will run a model when a specified hydrograph has new value
class RunController:
    def __init__(self, nw, struct, hydr, run_ini, run1, run2, crs):
        self.network = nw
        self.structure = struct
        self.hydrograph = hydr
        self.run_ini = run_ini
        self.next_run_scheme_id = run1
        self.last_run_scheme_id = run2
        self.crs = crs
        self.timer = QTimer()
        self.last_time = self.hydrograph.timeWindow().end()
        self.runCount = 0
        self.water_level_position = ReosSpatialPosition(6049297.8,7886801.5, self.crs)
        self.water_level_threshold = 28.5

    def start(self):
        self.timer.timeout.connect(self.new_run)
        self.export_to_tif(self.run_ini)
        self.network.setCurrentScheme(self.next_run_scheme_id)
        to_std_out('Current scheme is now {}'.format(self.network.currentSchemeName()))
        self.structure.currentSimulation().setHotStartSchemeId(self.run_ini)
        self.swap_scheme()
        self.timer.start(60000)

    def swap_scheme(self):
        self.next_run_scheme_id, self.last_run_scheme_id = self.last_run_scheme_id, self.next_run_scheme_id

    def export_to_tif(self, run_id):
        print('Export to GeoTIFF for time: ', str(self.last_time.toString(Qt.ISODate)))
        file_name_wd = '/home/vincent/fango_real_time/waterdepth_{}.tif'.format(str(self.runCount))
        self.structure.rasterizeResult(self.last_time, ReosHydraulicSimulationResults.DatasetType.WaterDepth, run_id,
                                       file_name_wd, self.crs, 1)

        file_name_vel = '/home/vincent/fango_real_time/velocity_{}.tif'.format(str(self.runCount))
        self.structure.rasterizeResult(self.last_time, ReosHydraulicSimulationResults.DatasetType.Velocity, run_id,
                                       file_name_vel, self.crs, 1)
        self.runCount = self.runCount + 1
        #send_raster_somewhere(file_name_wd, file_name_vel)

    def check_water_level(self, run_id):
        water_level=self.structure.resultsValueAt(self.last_time,
                                      self.water_level_position,
                                      ReosHydraulicSimulationResults.DatasetType.WaterLevel,
                                      run_id)
        #if water_level>=self.water_level_threshold:
        #    send_alert_somewhere()

    def new_run(self):
        self.timer.stop()  # we stop the timer until we finished the new run
        self.hydrograph.reloadBlocking(60000, True)
        new_hydrograph_time = self.hydrograph.timeWindow().end()

        to_std_out('*******************************************************************')
        if new_hydrograph_time <= self.last_time:
            to_std_out(
                'At {} , the last hydrograph time is not updated ({})'.format(QDateTime.currentDateTime().toString(),
                                                                              self.last_time.toString()))
            self.timer.start(60000)
            return

        to_std_out('*******************************************************************')
        to_std_out('New run will start')

        run_duration = ReosDuration(-self.last_time.msecsTo(new_hydrograph_time), ReosDuration.millisecond)
        self.last_time = new_hydrograph_time
        self.structure.timeWindowSettings().setStartOffset(run_duration)
        self.structure.currentSimulation().setHotStartUseLastTimeStep(True)
        self.structure.runSimulation(network.calculationContext())
        self.export_to_tif(self.last_run_scheme_id)

        self.network.setCurrentScheme(self.next_run_scheme_id)
        to_std_out('Current scheme is now {}'.format(self.network.currentSchemeName()))
        self.structure.currentSimulation().setHotStartSchemeId(self.last_run_scheme_id)
        self.swap_scheme()

        self.timer.start(60000)


# Creation of the ReosApplication that contains the modules
app = ReosApplication(list(map(os.fsencode, sys.argv)))
reos_core = app.coreModule()

# We open the project and get the network
reos_core.openProject('/home/vincent/lekan/Fango-use-case/fango.lkn')
network = reos_core.hydraulicNetwork()
crs = reos_core.gisEngine().crs()

# Get the junction upstream of the structure 2D and the associated hydrograph
junctions = network.hydraulicNetworkElements(ReosHydrographJunction.staticType())
for junct in junctions:
    if (junct.elementName() == 'Le Fango à Galéria'):
        upstream_junction = junct

if upstream_junction is None:
    sys.exit('Upstream junction not found')

hydrograph = upstream_junction.internalHydrograph()
if hydrograph is None:
    sys.exit('No hydrograph associated with the upstream junction')

# make sure we have all the loaded
hydrograph.reloadBlocking(60000, True)

# get the 2D structure
structures = network.hydraulicNetworkElements(ReosHydraulicStructure2D.staticType())
if len(structures) == 0:
    sys.exit('No 2D structure found')
structure2d = structures[0]
to_std_out('Structure name: {}'.format(structure2d.elementName()))

# The aim of the first run is to initialize the model so we take a time window with 120 mn before end to end
# to be sure that all the model will be well "wet".
# we use the hydraulic scheme with index 1 that is configured to start with a interpolation line (done under Lekan)
network.setCurrentScheme(1)
init_run_scheme_id = network.currentSchemeId()  # id of the scheme is stored to be used with the controller
time_window_settings = structure2d.timeWindowSettings()
time_window_settings.setAutomaticallyDefined(True)
time_window_settings.setOriginStart(ReosTimeWindowSettings.End)
offset_from_end = ReosDuration(-120, ReosDuration.minute)
time_window_settings.setStartOffset(offset_from_end)

last_time = hydrograph.timeWindow().end()
structure2d.runSimulation(network.calculationContext())

# We change to schemes that have a hot start configuration and set them to use the last time step of the used scheme
network.setCurrentScheme(2)
run_id_1 = network.currentSchemeId()  # id of the scheme is stored to be used with the controller
structure2d.currentSimulation().setHotStartUseLastTimeStep(True)
network.setCurrentScheme(3)
run_id_2 = network.currentSchemeId()  # id of the scheme is stored to be used with the controller
structure2d.currentSimulation().setHotStartUseLastTimeStep(True)
network.setCurrentScheme(init_run_scheme_id)

controller = RunController(network, structure2d, hydrograph, init_run_scheme_id, run_id_1, run_id_2, crs)
controller.start()

app.exec()
