#ifndef REOSWATERSHED_H
#define REOSWATERSHED_H

#include "reosmapextent.h"
#include "reosrastercompressed.h"
#include "reosrasterwatershed.h"
#include "memory"

#include <reosrasterwatershed.h>

enum class ReosInclusionType
{
  None,
  Partial,
  Total
};


class ReosWatershed
{
  public:
    ReosWatershed() = default;

    ReosWatershed( const QPolygonF &delineating,
                   const QPointF &outletPoint,
                   const QPolygonF &downstreamLine = QPolygonF(),
                   const ReosRasterWatershed::Directions &direction = ReosRasterWatershed::Directions(),
                   const ReosRasterExtent directionExent = ReosRasterExtent() );

    //! Returns the extent of the watershed
    ReosMapExtent extent() const;

    //! Returns whether the watershed include the \a point
    bool contains( const QPointF &point ) const;

    //! Returns how the polygon or polyline \a poly is contained in the watershed
    ReosInclusionType contains( const QPolygonF &poly ) const;

    //! Returns whether the watrshed or its parent contoins direction data
    bool hasDirectiondata() const;

    //! Returns the directions raster associated with this watershed, returned raster is invalid if there is none
    ReosRasterWatershed::Directions directions() const;

    //! Returns the extent od the raster direction, \see directions()
    ReosRasterExtent directionExtent() const;

    //! Returns the outlet point of the watershed
    QPointF outletPoint() const;

    //! Returns the count if upstream watershed
    int upstreamWatershedCount() const;

    //! Returns the count of watershed that are directky upstream
    int directUpstreamWatershedCount() const;

    //! Returns the ith direct upstream watershed, return nullptr if there is not
    ReosWatershed *directUpstreamWatershed( int i ) const
    {
      if ( i<0 or i >= int( mUpstreamWatersheds.size() ) )
        return nullptr;
      return mUpstreamWatersheds.at( i ).get();
    }

    //! Adds a upstream watershed (take ownership) and update this one
    ReosWatershed *addUpstreamWatershed( ReosWatershed *upstreamWatershed );

    //! Returns the smallest watershed that is downstream the line, if the line is partially included by any watershed, ok is false
    //! If there is no watershed downstrean, return nullptr
    ReosWatershed *upstreamWatershed( const QPolygonF &poly, bool &ok );

    //! Returns, if exists a pointer to the direct downstream watershed, if not returns nullptr
    ReosWatershed *downstreamWatershed() const;

    int positionInDownstreamWatershed() const
    {
      if ( !mDownstreamWatershed )
        return -1;

      for ( size_t i = 0; i < mDownstreamWatershed->mUpstreamWatersheds.size(); ++i )
      {
        if ( this == mDownstreamWatershed->mUpstreamWatersheds.at( i ).get() )
          return i;
      }

      return -1;

    }

  private:
    ReosMapExtent mExtent;
    QPolygonF mDelineating;
    QPointF mOutletPoint;
    QPolygonF mDownstreamLine;
    ReosRasterByteCompressed mDirectionRaster;
    ReosRasterExtent mDirectionExtent;

    std::vector<std::unique_ptr<ReosWatershed>> mUpstreamWatersheds;
    ReosWatershed *mDownstreamWatershed = nullptr;
};


#endif // REOSWATERSHED_H
