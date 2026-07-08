# The following has been generated automatically from src/core/data/reosgriddeddata.h
try:
    ReosGriddedData.staticType = staticmethod(ReosGriddedData.staticType)
    ReosGriddedData.__overridden_methods__ = ['updateData']
    ReosGriddedData.__group__ = ['data']
except (NameError, AttributeError):
    pass
try:
    ReosSeriesFromGriddedDataOnWatershed.create = staticmethod(ReosSeriesFromGriddedDataOnWatershed.create)
    ReosSeriesFromGriddedDataOnWatershed.createWithTimeStep = staticmethod(ReosSeriesFromGriddedDataOnWatershed.createWithTimeStep)
    ReosSeriesFromGriddedDataOnWatershed.__overridden_methods__ = ['valueAt', 'preCalculate']
    ReosSeriesFromGriddedDataOnWatershed.__group__ = ['data']
except (NameError, AttributeError):
    pass
try:
    ReosDataGriddedOnWatershed.__abstract_methods__ = ['preCalculate', 'onCalculationFinished', 'onDataChanged', 'timeAtIndex', 'setDataActualized']
    ReosDataGriddedOnWatershed.__group__ = ['data']
except (NameError, AttributeError):
    pass
