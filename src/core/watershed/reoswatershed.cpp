#include "reoswatershed.h"

ReosWatershed::ReosWatershed( const QPolygonF &delineating, const QPointF &outletPoint, const QPolygonF &downstreamLine, const ReosRasterWatershed::Directions &direction, const ReosRasterExtent directionExent ):
  mExtent( delineating ),
  mDelineating( delineating ),
  mOutletPoint( outletPoint ),
  mDownstreamLine( downstreamLine ),
  mDirectionRaster( direction ),
  mDirectionExtent( directionExent )
{}

QString ReosWatershed::name() const
{
  return mName;
}

void ReosWatershed::setName( const QString &name )
{
  mName = name;
}

ReosMapExtent ReosWatershed::extent() const
{
  return mExtent;
}

bool ReosWatershed::contain( const QPointF &point ) const
{
  if ( !mExtent.contains( point ) )
    return false;

  return ReosGeometryUtils::pointIsInsidePolygon( point, delineating() );
}

ReosInclusionType ReosWatershed::contain( const QPolygonF &line ) const
{
  return ReosGeometryUtils::polylineIsInsidePolygon( line, mDelineating );
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

QPolygonF ReosWatershed::delineating() const {return mDelineating;}

QPointF ReosWatershed::outletPoint() const {return mOutletPoint;}

int ReosWatershed::upstreamWatershedCount() const
{
  int count = 0;
  for ( const std::unique_ptr<ReosWatershed> &watershed : mUpstreamWatersheds )
    count += watershed->upstreamWatershedCount() + 1;

  return count;
}

int ReosWatershed::directUpstreamWatershedCount() const
{
  return mUpstreamWatersheds.size();
}

ReosWatershed *ReosWatershed::directUpstreamWatershed( int i ) const
{
  if ( i<0 or i >= int( mUpstreamWatersheds.size() ) )
    return nullptr;
  return mUpstreamWatersheds.at( i ).get();
}

ReosWatershed *ReosWatershed::addUpstreamWatershed( ReosWatershed *upstreamWatershed )
{
  std::unique_ptr<ReosWatershed> ws( upstreamWatershed );
  if ( hasDirectiondata() )
  {
    // the upstream raster does not need anymore the direction data
    ws->mDirectionExtent = ReosRasterExtent();
    ws->mDirectionRaster = ReosRasterByteCompressed();
  }

  if ( ws->name().isEmpty() && !name().isEmpty() )
    ws->setName( name().append( "-%1" ).arg( mUpstreamWatersheds.size() + 1 ) );
  mUpstreamWatersheds.emplace_back( ws.release() );
  return this;
}

ReosWatershed *ReosWatershed::upstreamWatershed( const QPolygonF &line, bool &ok ) const
{
  for ( const std::unique_ptr<ReosWatershed> &watershed : mUpstreamWatersheds )
  {
    assert( watershed );
    switch ( watershed->contain( line ) )
    {
      case ReosInclusionType::None:
        continue;
        break;
      case ReosInclusionType::Partial:
        ok = false;
        return nullptr;
        break;
      case ReosInclusionType::Total:
      {
        ReosWatershed *upstream = watershed->upstreamWatershed( line, ok );
        if ( upstream && ok )
          return upstream;
        else if ( !ok )
          return nullptr;

        ok = true;
        return watershed.get();
      }
      break;
    }
  }

  ok = true;
  return nullptr;
}

const ReosWatershed *ReosWatershed::upstreamWatershed( const QPointF &point ) const
{
  if ( !contain( point ) )
    return nullptr;
  for ( const std::unique_ptr<ReosWatershed> &uws : mUpstreamWatersheds )
  {
    if ( uws->contain( point ) )
      return uws->upstreamWatershed( point );
  }

  return this;
}

ReosWatershed *ReosWatershed::downstreamWatershed() const
{
  return mDownstreamWatershed;
}

int ReosWatershed::positionInDownstreamWatershed() const
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

QList<ReosWatershed *> ReosWatershed::allDownstreamWatershed() const
{
  QList<ReosWatershed *> list;
  for ( const std::unique_ptr<ReosWatershed> &ws : mUpstreamWatersheds )
  {
    list.append( ws->allDownstreamWatershed() );
    list.append( ws.get() );
  }

  return list;
}

ReosInclusionType ReosWatershed::contain( const ReosWatershed &other ) const
{
  return ReosGeometryUtils::polygonIsInsidePolygon( delineating(), other.delineating() );
}

void ReosWatershed::removeDirectionData()
{
  if ( mDirectionRaster.hasData() )
    mDirectionRaster = ReosRasterByteCompressed();
  if ( mDirectionExtent.isValid() )
    mDirectionExtent = ReosRasterExtent();

  for ( size_t i = 0; i < mUpstreamWatersheds.size(); ++i )
    mUpstreamWatersheds.at( i )->removeDirectionData();
}

void ReosWatershed::fitIn( const ReosWatershed &other )
{
  if ( ReosGeometryUtils::polygonIsInsidePolygon( mDelineating, other.mDelineating ) == ReosInclusionType::Partial )
  {
    QPolygonF newDelinetating = ReosGeometryUtils::polygonFitInPolygon( mDelineating, other.mDelineating );
    mDelineating = newDelinetating;
  }

  //remove intersection with other sub watershed
  for ( const std::unique_ptr<ReosWatershed> &sibling : mUpstreamWatersheds )
  {
    adjust( *sibling.get() );
  }
}

void ReosWatershed::adjust( const ReosWatershed &other )
{
  if ( ReosInclusionType::Partial == other.contain( *this ) )
  {
    QPolygonF newDelinetating = ReosGeometryUtils::polygonCutByPolygon( mDelineating, other.mDelineating );
    mDelineating = newDelinetating;
  }
}


