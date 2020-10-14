#ifndef REOSWATERSHED_H
#define REOSWATERSHED_H

#include "reosmapextent.h"
#include "memory"

class ReosWatershed
{

  protected:
    ReosWatershed();
    std::unique_ptr<ReosMapExtent> mExtent;
};

class ReosWatershedFromRaster : public ReosWatershed
{

};

#endif // REOSWATERSHED_H
