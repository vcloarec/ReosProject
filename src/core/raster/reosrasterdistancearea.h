#ifndef REOSRASTERDISTANCEAREA_H
#define REOSRASTERDISTANCEAREA_H

#include "reoscore.h"
#include "reosmemoryraster.h"


/**
 * Class use to adjust the distance or the area depending on the position of the cell in the raster and on the CRS of the raster.
 */
class REOSCORE_EXPORT ReosRasterDistanceArea
{
  public:
    //! Constructs an invalid ReosRasterDistanceArea instance.
    ReosRasterDistanceArea();
    //! Constructs a ReosRasterDistanceArea object with the given \a extent and reduction factor \a reductionFactor
    ReosRasterDistanceArea( const ReosRasterExtent &extent, int reductionFactor = 10 );

    //! Adjusts the distance values \a xDistance and \a yDistance based on the position of the cell at \a row and \a col in the raster.
    void adjustDistance( float &xDistance, float &yDistance, int row, int col ) const;

    //! Returns the area factor for the cell at \a row and \a col in the raster.
    float areaFactor( int row, int col ) const;

    //! Returns whether the ReosRasterDistanceArea instance is valid.
    bool isValid() const;

  private:
    ReosRasterExtent mExtent;
    int mReductionFactor = 10;
    ReosRasterMemory<float> mXDistanceScale;
    ReosRasterMemory<float> mYDistanceScale;
    bool mIsvalid = false;

    void initGrids();
};

#endif // REOSRASTERDISTANCEAREA_H
