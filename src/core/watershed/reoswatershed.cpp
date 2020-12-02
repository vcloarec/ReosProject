#include "reoswatershed.h"
#include "reosgeometryutils.h"


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

bool ReosWatershed::contains( const QPointF &point ) const
{
  if ( !mExtent.contains( point ) )
    return false;

  return ReosGeometryUtils::pointIsInsidePolygon( point, delineating() );
}

ReosInclusionType ReosWatershed::contains( const QPolygonF &line ) const
{
  bool partial = false;
  bool total = true;

  for ( const QPointF &pt : line )
  {
    bool ct = contains( pt );
    partial |= ct;
    total &= ct;
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

ReosWatershed *ReosWatershed::addUpstreamWatershed( ReosWatershed *upstreamWatershed, bool adaptUpstreamDelineating )
{
  std::unique_ptr<ReosWatershed> ws( upstreamWatershed );
  if ( hasDirectiondata() )
  {
    // the upstream raster does not need anymore the direction data
    ws->mDirectionExtent = ReosRasterExtent();
    ws->mDirectionRaster = ReosRasterByteCompressed();
  }

  // check interaction with existing watershed
  for ( size_t i = 0; i < mUpstreamWatersheds.size(); ++i )
  {
    std::unique_ptr<ReosWatershed> &usW = mUpstreamWatersheds.at( i );
    if ( ws->contains( usW->outletPoint() ) )
    {
      //existing watershed is in the new one
      ws->addUpstreamWatershed( usW.release() );
      mUpstreamWatersheds.erase( mUpstreamWatersheds.begin() + i );
      break;
    }

    if ( usW->contains( ws->outletPoint() ) )
    {
      //existing watershed contains new one --> propagate the adding upstream
      return usW->addUpstreamWatershed( ws.release() );
    }
  }

  // if we are here, the watershed will be added in this watershed
  ReosInclusionType inclusionType = upstreamWatershed->isInside( *this );
  if ( inclusionType == ReosInclusionType::None ) //new watershed ids not in this one
    throw ReosWatershedException( QObject::tr( "Try to add a watershed in a watershed that not contain the new one." ), inclusionType );

  if ( !adaptUpstreamDelineating && inclusionType == ReosInclusionType::Partial )
    throw ReosWatershedException( QObject::tr( "Try to add a watershed in a not including other watershed." ), inclusionType );

  if ( ws->name().isEmpty() && !name().isEmpty() )
    ws->setName( name().append( "-%1" ).arg( mUpstreamWatersheds.size() + 1 ) );
  ws->mDownstreamWatershed = this;
  mUpstreamWatersheds.emplace_back( ws.release() );
  return this;
}

ReosWatershed *ReosWatershed::upstreamWatershed( const QPolygonF &line, bool &ok )
{

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

ReosInclusionType ReosWatershed::isInside( const ReosWatershed &other ) const
{
  if ( ReosGeometryUtils::polygonIsInsidePolygon( delineating(), other.delineating() ) )
    return ReosInclusionType::Total;

  if ( ReosGeometryUtils::polygonIntersectPolygon( delineating(), other.delineating() ) )
    return ReosInclusionType::Partial;

  return ReosInclusionType::None;
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


