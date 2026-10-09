# The following has been generated automatically from src/core/watershed/reoswatersheddelineating.h
ReosWatershedDelineating.NoDigitalElevationModel = ReosWatershedDelineating.State.NoDigitalElevationModel
ReosWatershedDelineating.WaitingForDownstream = ReosWatershedDelineating.State.WaitingForDownstream
ReosWatershedDelineating.WaitingForExtent = ReosWatershedDelineating.State.WaitingForExtent
ReosWatershedDelineating.WaitingWithBroughtBackExtent = ReosWatershedDelineating.State.WaitingWithBroughtBackExtent
ReosWatershedDelineating.WaitingforProceed = ReosWatershedDelineating.State.WaitingforProceed
ReosWatershedDelineating.Delineating = ReosWatershedDelineating.State.Delineating
ReosWatershedDelineating.WaitingForValidate = ReosWatershedDelineating.State.WaitingForValidate
ReosWatershedDelineating.WaitingToRecord = ReosWatershedDelineating.State.WaitingToRecord
try:
    ReosWatershedDelineating.staticName = staticmethod(ReosWatershedDelineating.staticName)
    ReosWatershedDelineating.delineateWatershed = staticmethod(ReosWatershedDelineating.delineateWatershed)
    ReosWatershedDelineating.directionFromDem = staticmethod(ReosWatershedDelineating.directionFromDem)
    ReosWatershedDelineating.__group__ = ['watershed']
except (NameError, AttributeError):
    pass
try:
    ReosWatershedDelineating.DelineateResult.__group__ = ['watershed']
except (NameError, AttributeError):
    pass
