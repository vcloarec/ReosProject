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
                   const QPolygonF &downstreamLine = QPolygonF(),
                   const ReosRasterWatershed::Directions &direction = ReosRasterWatershed::Directions(),
                   const ReosRasterExtent directionExent = ReosRasterExtent() );

    //! Returns the extent of the watershed
    ReosMapExtent extent() const;

    //! Returns whether the watershed include the \a point
    bool contains( const QPointF &point ) const;

    //! Returns how the plygon or plyline \a poly is cotained in the watershed
    ReosInclusionType contains( const QPolygonF &poly ) const;

    //! Returns the directions raster associated with this watershed, returned raster is invalid if there is none
    ReosRasterWatershed::Directions directions() const;

    //! Returns the extent od the raster direction, \see directions()
    ReosRasterExtent directionExtent() const;

  private:
    ReosMapExtent mExtent;
    QPolygonF mDelineating;
    QPolygonF mDownstreamLine;
    ReosRasterByteCompressed mDirectionRaster;
    ReosRasterExtent mDirectionExtent;

    std::vector<std::unique_ptr<ReosWatershed>> mSubWatershed;
};


#endif // REOSWATERSHED_H
