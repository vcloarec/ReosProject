#include "reoswatershed.h"


ReosWatershed::ReosWatershed( const QPolygonF &delineating, const QPolygonF &downstreamLine, const ReosRasterWatershed::Directions &direction, const ReosRasterExtent directionExent ):
  mExtent( delineating ),
  mDelineating( delineating ),
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

ReosRasterWatershed::Directions ReosWatershed::directions() const
{
  return mDirectionRaster.uncompressRaster();
}

ReosRasterExtent ReosWatershed::directionExtent() const {return mDirectionExtent;}


