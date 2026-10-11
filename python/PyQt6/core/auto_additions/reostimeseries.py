# The following has been generated automatically from src/core/data/reostimeseries.h
ReosTimeSeriesConstantInterval.Value = ReosTimeSeriesConstantInterval.ValueMode.Value
ReosTimeSeriesConstantInterval.Intensity = ReosTimeSeriesConstantInterval.ValueMode.Intensity
ReosTimeSeriesConstantInterval.Cumulative = ReosTimeSeriesConstantInterval.ValueMode.Cumulative
try:
    ReosTimeSeriesConstantInterval.staticType = staticmethod(ReosTimeSeriesConstantInterval.staticType)
    ReosTimeSeriesConstantInterval.__overridden_methods__ = ['relativeTimeAt', 'valueAt', 'setValueAt', 'type']
    ReosTimeSeriesConstantInterval.__group__ = ['data']
except (NameError, AttributeError):
    pass
try:
    ReosTimeSeriesVariableTimeStep.staticType = staticmethod(ReosTimeSeriesVariableTimeStep.staticType)
    ReosTimeSeriesVariableTimeStep.__overridden_methods__ = ['type', 'relativeTimeAt']
    ReosTimeSeriesVariableTimeStep.__group__ = ['data']
except (NameError, AttributeError):
    pass
try:
    ReosTimeSeries.__virtual_methods__ = ['timeAt', 'valueAt', 'setValueAt', 'clear']
    ReosTimeSeries.__abstract_methods__ = ['relativeTimeAt']
    ReosTimeSeries.__overridden_methods__ = ['type']
    ReosTimeSeries.__group__ = ['data']
except (NameError, AttributeError):
    pass
