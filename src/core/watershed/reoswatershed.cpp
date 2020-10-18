#include "reoswatershed.h"


ReosWatershed::ReosWatershed( const QPolygonF &delineating, const QPointF &outletPoint, const QPolygonF &downstreamLine, const ReosRasterWatershed::Directions &direction, const ReosRasterExtent directionExent ):
  mExtent( delineating ),
  mDelineating( delineating ),
  mOutletPoint( outletPoint ),
  mDownstreamLine( downstreamLine ),
  mDirectionRaster( direction ),
  mDirectionExtent( directionExent )
{}

ReosMapExtent ReosWatershed::extent() const
{
  return mExtent;
}

bool ReosWatershed::contains( const QPointF &point ) const
{
  if ( !mExtent.contains( point ) )
    return false;

  return mDelineating.containsPoint( point, Qt::OddEvenFill );
}

ReosInclusionType ReosWatershed::contains( const QPolygonF &poly ) const
{
  bool partial = false;
  bool total = true;

  for ( const QPointF &pt : poly )
  {
    partial |= contains( pt );
    total &= contains( pt );
  }

  if ( !partial )
    return ReosInclusionType::None;

  if ( partial && !total )
    return ReosInclusionType::Partial;

  return ReosInclusionType::Total;
}

bool ReosWatershed::hasDirectiondata() const
{
  return mDirectionRaster.hasData() || ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata() );
}

ReosRasterWatershed::Directions ReosWatershed::directions() const
{
  if ( mDirectionRaster.hasData() )
    return mDirectionRaster.uncompressRaster();

  if ( mDownstreamWatershed )
    return mDownstreamWatershed->directions();

  return mDirectionRaster.uncompressRaster();
}

ReosRasterExtent ReosWatershed::directionExtent() const
{
  if ( mDirectionExtent.isValid() )
    return mDirectionExtent;

  if ( mDownstreamWatershed )
    return mDownstreamWatershed->directionExtent();

  return mDirectionExtent;
}

QPointF ReosWatershed::outletPoint() const {return mOutletPoint;}

int ReosWatershed::upstreamWatershedCount()
{
  int count = 0;
  for ( const std::unique_ptr<ReosWatershed> &watershed : mUpstreamWatersheds )
    count += watershed->upstreamWatershedCount() + 1;

  return count;
}

ReosWatershed *ReosWatershed::addUpstreamWatershed( ReosWatershed *upstreamWatershed )
{
  mUpstreamWatersheds.emplace_back( upstreamWatershed );
  if ( hasDirectiondata() )
  {
    // the upstream raster does not need anymore the direction data
    upstreamWatershed->mDirectionExtent = ReosRasterExtent();
    upstreamWatershed->mDirectionRaster = ReosRasterByteCompressed();
  }
  upstreamWatershed->mDownstreamWatershed = this;
  return mUpstreamWatersheds.back().get();
}

ReosWatershed *ReosWatershed::upstreamWatershed( const QPolygonF &poly, bool &ok )
{

}


