/***************************************************************************
                      reoswatershed.cpp
                     --------------------------------------
Date                 : 10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoswatershed.h"
#include "reosgisengine.h"

ReosWatershed::ReosWatershed():
  mArea( new ReosParameterArea( tr( "Watershed area" ), this ) )
{
  init();
}

ReosWatershed::ReosWatershed( const QPolygonF &delineating, const QPointF &outletPoint, ReosWatershed::Type type ):
  mType( type )
  , mExtent( delineating )
  , mDelineating( delineating )
  , mOutletPoint( outletPoint )
  , mName( new ReosParameterString( tr( "Watershed name" ), false, this ) )
  , mArea( new ReosParameterArea( tr( "Watershed area" ), true, this ) )
  , mSlope( new ReosParameterSlope( tr( "Average slope" ), true, this ) )
{
  init();
}

ReosWatershed::ReosWatershed( const QPolygonF &delineating, const QPointF &outletPoint, ReosWatershed::Type type, const QPolygonF &downstreamLine, const QPolygonF &streamPath ):
  mType( type ),
  mExtent( delineating ),
  mDelineating( delineating ),
  mOutletPoint( outletPoint ),
  mDownstreamLine( downstreamLine ),
  mStreamPath( streamPath )
  , mName( new ReosParameterString( tr( "Watershed name" ), false, this ) )
  , mArea( new ReosParameterArea( tr( "Watershed area" ), true, this ) )
  , mSlope( new ReosParameterSlope( tr( "Average slope" ), true, this ) )
{
  init();
}

ReosWatershed::ReosWatershed( const QPolygonF &delineating, const QPointF &outletPoint, ReosWatershed::Type type, const QPolygonF &downstreamLine, const QPolygonF &streamPath, const ReosRasterWatershed::Directions &direction, const ReosRasterExtent &directionExent, const QString &refLayerId ):
  mType( type ),
  mExtent( delineating ),
  mDelineating( delineating ),
  mOutletPoint( outletPoint ),
  mDownstreamLine( downstreamLine ),
  mStreamPath( streamPath )
  , mName( new ReosParameterString( tr( "Watershed name" ), false, this ) )
  , mArea( new ReosParameterArea( tr( "Watershed area" ), true, this ) )
  , mSlope( new ReosParameterSlope( tr( "Average slope" ), true, this ) )
{
  init();
  DirectionData dir {direction, directionExent};
  mDirectionData.insert( {refLayerId, dir} );
}

ReosParameterString *ReosWatershed::name() const
{
//  if ( mType == Residual && mDownstreamWatershed )
//    return mDownstreamWatershed->name() + QObject::tr( " residual" );

  return mName;
}

void ReosWatershed::setName( const QString &name )
{
  mName->setValue( name );
  void changed();
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

bool ReosWatershed::hasDirectiondata( const QString &layerId ) const
{
  bool localDirectionPresent =  mDirectionData.find( layerId ) != mDirectionData.end();
  return localDirectionPresent || ( mDownstreamWatershed && mDownstreamWatershed->hasDirectiondata( layerId ) );
}

ReosRasterWatershed::Directions ReosWatershed::directions( const QString &layerId ) const
{
  std::map<QString, DirectionData>::const_iterator it =  mDirectionData.find( layerId );

  if ( it != mDirectionData.end() )
    return it->second.directionRaster.uncompressRaster();

  if ( mDownstreamWatershed )
    return mDownstreamWatershed->directions( layerId );

  return ReosRasterWatershed::Directions();
}

ReosRasterExtent ReosWatershed::directionExtent( const QString &layerId ) const
{
  std::map<QString, DirectionData>::const_iterator it =  mDirectionData.find( layerId );

  if ( it != mDirectionData.end() )
    return it->second.directionExtent;

  if ( mDownstreamWatershed )
    return mDownstreamWatershed->directionExtent( layerId );

  return ReosRasterExtent();
}

QPolygonF ReosWatershed::delineating() const {return mDelineating;}

QPointF ReosWatershed::outletPoint() const
{
  if ( mType == Residual && mDownstreamWatershed )
    return mDownstreamWatershed->outletPoint();

  return mOutletPoint;
}

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
  if ( i < 0 || i >= int( mUpstreamWatersheds.size() ) )
    return nullptr;
  return mUpstreamWatersheds.at( i ).get();
}

ReosWatershed *ReosWatershed::addUpstreamWatershed( ReosWatershed *newUpstreamWatershed, bool adjustIfNeeded )
{
  std::unique_ptr<ReosWatershed> ws( newUpstreamWatershed );

  // Remove direction data from upstream if it has direction data from the same layer (not need anymore)
  for ( std::map<QString, DirectionData>::const_iterator it = mDirectionData.begin(); it != mDirectionData.end(); ++it )
  {
    QString layerId = it->first;
    std::map<QString, DirectionData>::const_iterator upIt = ws->mDirectionData.find( layerId );
    if ( upIt != ws->mDirectionData.end() )
      ws->mDirectionData.erase( upIt );
  }

  const QPointF &op = ws->outletPoint();
  // Look if the added watershed is in a sub watershed
  for ( std::unique_ptr<ReosWatershed> &existingUpstream : mUpstreamWatersheds )
  {
    if ( existingUpstream->contain( op ) && existingUpstream->type() != Residual )
      return existingUpstream->addUpstreamWatershed( ws.release(), adjustIfNeeded ); // let the existing watershed dealing with the new one
  }

  if ( !ws->name()->isValid() && name()->isValid() )
    ws->setName( name()->value().append( "-%1" ).arg( mUpstreamWatersheds.size() + ( mUpstreamWatersheds.empty() ? 1 : 0 ) ) );

  ws->mDownstreamWatershed = this;
  if ( adjustIfNeeded )
    ws->fitIn( *this );

  size_t i = 1; //no consider the residual
  while ( i < mUpstreamWatersheds.size() )
  {
    std::unique_ptr<ReosWatershed> &sibling = mUpstreamWatersheds.at( i );
    if ( ws->contain( sibling->outletPoint() ) ) // the sibling is in the new watershed -> move it in the new watershed
    {
      if ( adjustIfNeeded )
        ws->extentTo( *sibling.get() );
      ws->addUpstreamWatershed( sibling.release(), false );
      mUpstreamWatersheds.erase( mUpstreamWatersheds.begin() + i );
    }
    else
    {
      if ( adjustIfNeeded )
        ws->adjust( *sibling );
      ++i;
    }
  }

  mUpstreamWatersheds.emplace_back( ws.release() );

  updateResidual();

  return mUpstreamWatersheds.back().get();
}

ReosWatershed *ReosWatershed::extractOnlyDirectUpstreamWatershed( int i )
{
  std::unique_ptr<ReosWatershed> ws( extractCompleteDirectUpstreamWatershed( i ) );

  for ( size_t i = 1; i < ws->mUpstreamWatersheds.size(); ++i )
  {
    mUpstreamWatersheds.emplace_back( ws->mUpstreamWatersheds.at( i ).release() );
    mUpstreamWatersheds.back()->mDownstreamWatershed = this;
  }
  ws->mUpstreamWatersheds.clear();

  updateResidual();
  return ws.release();
}

ReosWatershed *ReosWatershed::extractCompleteDirectUpstreamWatershed( int i )
{
  size_t pos = static_cast<int>( i );
  std::unique_ptr<ReosWatershed> ret;
  if ( pos > 0 && pos < mUpstreamWatersheds.size() ) // the first one is the residual watershed
  {
    ret.reset( mUpstreamWatersheds.at( i ).release() );
    mUpstreamWatersheds.erase( mUpstreamWatersheds.begin() + i );
  }

  ret->mDownstreamWatershed = nullptr;

  updateResidual();
  return ret.release();
}

ReosWatershed *ReosWatershed::upstreamWatershed( const QPolygonF &line, bool &ok ) const
{
  for ( const std::unique_ptr<ReosWatershed> &watershed : mUpstreamWatersheds )
  {
    assert( watershed );
    if ( watershed->type() == Residual )
      continue;
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

ReosWatershed *ReosWatershed::upstreamWatershed( const QPointF &point, bool excludeResidual )
{
  if ( !contain( point ) )
    return nullptr;
  for ( const std::unique_ptr<ReosWatershed> &uws : mUpstreamWatersheds )
  {
    if ( uws->contain( point ) && ( uws->type() != Residual || !excludeResidual ) )
    {

      ReosWatershed *ret = uws->upstreamWatershed( point );
      if ( !ret )
        return uws.get();
      return ret;
    }
  }

  return nullptr;
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

QList<ReosWatershed *> ReosWatershed::allUpstreamWatershed() const
{
  QList<ReosWatershed *> list;
  for ( const std::unique_ptr<ReosWatershed> &ws : mUpstreamWatersheds )
  {
    list.append( ws->allUpstreamWatershed() );
    list.append( ws.get() );
  }

  return list;
}

ReosInclusionType ReosWatershed::isContainedBy( const ReosWatershed &other ) const
{
  return ReosGeometryUtils::polygonIsInsidePolygon( delineating(), other.delineating() );
}

void ReosWatershed::removeDirectionData()
{
  mDirectionData.clear();
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
    if ( sibling->type() != Residual )
      adjust( *sibling.get() );
  }
}

void ReosWatershed::adjust( const ReosWatershed &other )
{
  if ( ReosInclusionType::Partial == isContainedBy( other ) )
  {
    QPolygonF newDelinetating = ReosGeometryUtils::polygonCutByPolygon( mDelineating, other.mDelineating );
    mDelineating = newDelinetating;
  }
}

void ReosWatershed::extentTo( const ReosWatershed &other )
{
  if ( ReosInclusionType::Partial == other.isContainedBy( *this ) )
  {
    QPolygonF newDelinetating = ReosGeometryUtils::polygonUnion( mDelineating, other.mDelineating );
    mDelineating = newDelinetating;
  }
}

QPolygonF ReosWatershed::streamPath() const
{
  return mStreamPath;
}

void ReosWatershed::setStreamPath( const QPolygonF &streamPath )
{
  mStreamPath = streamPath;
  emit changed();
}

ReosWatershed *ReosWatershed::residualWatershed() const
{
  if ( mUpstreamWatersheds.size() > 1 )
    return mUpstreamWatersheds.at( 0 ).get();

  return nullptr;
}

QPolygonF ReosWatershed::profile() const
{
  return mProfile;
}

void ReosWatershed::setProfile( const QPolygonF &profile )
{
  mProfile = profile;
  if ( mSlope->isDerived() )
    calculateDerivedSlope();
  emit changed();
}

void ReosWatershed::setGeographicalContext( ReosGisEngine *gisEngine )
{
  mGisEngine = gisEngine;
}

ReosParameterArea *ReosWatershed::area() const
{
  return mArea;
}

ReosParameterSlope *ReosWatershed::slope() const
{
  return mSlope;
}

ReosEncodedElement ReosWatershed::encode() const
{
  ReosEncodedElement ret( QStringLiteral( "watershed" ) );

  ret.addData( QStringLiteral( "type" ), mType );
  ret.addEncodedData( QStringLiteral( "extent" ), mExtent.encode() );
  ret.addData( QStringLiteral( "delineating" ), mDelineating );
  ret.addData( QStringLiteral( "outlet-point" ), mOutletPoint );
  ret.addData( QStringLiteral( "downstream-line" ), mDownstreamLine );
  ret.addData( QStringLiteral( "stream-path" ), mStreamPath );
  ret.addData( QStringLiteral( "profile" ), mProfile );

  QList<QString> directionKeys;
  QList<QByteArray> directionExtents;
  QList<QByteArray> directionData;

  for ( auto it : mDirectionData )
  {
    directionKeys.append( it.first );
    directionExtents.append( it.second.directionExtent.encode().bytes() );
    directionData.append( it.second.directionRaster.encode().bytes() );
  }

  ret.addData( QStringLiteral( "direction-keys" ), directionKeys );
  ret.addData( QStringLiteral( "direction-extents" ), directionExtents );
  ret.addData( QStringLiteral( "direction-data" ), directionData );

  QList<QByteArray> upstreamWatersheds;
  for ( const std::unique_ptr<ReosWatershed> &ws : mUpstreamWatersheds )
    upstreamWatersheds.append( ws->encode().bytes() );

  ret.addData( QStringLiteral( "upstream-watersheds" ), upstreamWatersheds );

  ret.addEncodedData( QStringLiteral( "name" ), mName->encode() );
  ret.addEncodedData( QStringLiteral( "area-parameter" ), mArea->encode() );
  ret.addEncodedData( QStringLiteral( "slope-parameter" ), mSlope->encode() );
  return ret;
}

ReosWatershed *ReosWatershed::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "watershed" ) )
    return nullptr;

  std::unique_ptr<ReosWatershed> ws = std::make_unique<ReosWatershed>();
  int intType;
  if ( !element.getData( QStringLiteral( "type" ), intType ) )
    return nullptr;
  ws->mType = static_cast<ReosWatershed::Type>( intType );

  ws->mExtent = ReosMapExtent::decode( element.getEncodedData( QStringLiteral( "extent" ) ) );

  if ( !element.getData( QStringLiteral( "delineating" ), ws->mDelineating ) )
    return nullptr;
  if ( !element.getData( QStringLiteral( "outlet-point" ), ws->mOutletPoint ) )
    return nullptr;
  if ( !element.getData( QStringLiteral( "downstream-line" ), ws->mDownstreamLine ) )
    return nullptr;
  if ( !element.getData( QStringLiteral( "stream-path" ), ws->mStreamPath ) )
    return nullptr;
  if ( !element.getData( QStringLiteral( "profile" ), ws->mProfile ) )
    return nullptr;

  QList<QString> directionKeys;
  QList<QByteArray> directionExtents;
  QList<QByteArray> directionData;

  if ( !element.getData( QStringLiteral( "direction-keys" ), directionKeys ) )
    return nullptr;
  if ( !element.getData( QStringLiteral( "direction-extents" ), directionExtents ) )
    return nullptr;
  if ( !element.getData( QStringLiteral( "direction-data" ), directionData ) )
    return nullptr;

  if ( directionKeys.count() != directionExtents.count() &&
       directionExtents.count() != directionData.count() )
    return nullptr;

  for ( int i = 0; i < directionKeys.count(); ++i )
  {
    DirectionData dirData{ReosRasterByteCompressed::decode( ReosEncodedElement( directionData.at( i ) ) ),
                          ReosRasterExtent::decode( ReosEncodedElement( directionExtents.at( i ) ) )};
    ws->mDirectionData.insert( {directionKeys.at( i ), dirData} );
  }

  QList<QByteArray> upstreamWatersheds;

  if ( !element.getData( QStringLiteral( "upstream-watersheds" ), upstreamWatersheds ) )
    return nullptr;

  for ( const QByteArray &ba : upstreamWatersheds )
  {
    std::unique_ptr<ReosWatershed> uws( ReosWatershed::decode( ReosEncodedElement( ba ) ) );
    if ( uws )
    {
      uws->mDownstreamWatershed = ws.get();
      ws->mUpstreamWatersheds.emplace_back( uws.release() );
    }
  }

  if ( ws->mArea )
    ws->mArea->deleteLater();
  ws->mArea = ReosParameterArea::decode( element.getEncodedData( QStringLiteral( "area-parameter" ) ), true, ws.get() );
  connect( ws->mArea, &ReosParameter::needDerivation, ws.get(), &ReosWatershed::calculateDerivedArea );

  if ( ws->mSlope )
    ws->mSlope->deleteLater();
  ws->mSlope = ReosParameterSlope::decode( element.getEncodedData( QStringLiteral( "slope-parameter" ) ), true, ws.get() );
  connect( ws->mSlope, &ReosParameter::needDerivation, ws.get(), &ReosWatershed::calculateDerivedSlope );

  if ( ws->mName )
    ws->mName->deleteLater();
  ws->mName = ReosParameterString::decode( element.getEncodedData( QStringLiteral( "name" ) ), false, ws.get() );

  return ws.release();
}

bool ReosWatershed::operator==( const ReosWatershed &other ) const
{
  if ( mType != other.mType )
    return false;

  if ( mName != other.mName )
    return false;

  if ( mExtent != other.mExtent )
    return false;
  if ( mDelineating != other.mDelineating )
    return false;
  if ( mOutletPoint != other.mOutletPoint )
    return false;
  if ( mDownstreamLine != other.mDownstreamLine )
    return false;
  if ( mStreamPath != other.mStreamPath )
    return false;

  if ( mProfile != other.mProfile )
    return false;

  if ( mDirectionData.size() != other.mDirectionData.size() )
    return false;

  for ( auto it : mDirectionData )
  {
    QString key = it.first;

    if ( other.mDirectionData.find( key ) == other.mDirectionData.end() )
      return false;

    auto otherIt = other.mDirectionData.find( key );

    if ( it.second.directionExtent != otherIt->second.directionExtent )
      return false;

    if ( it.second.directionRaster != otherIt->second.directionRaster )
      return false;
  }

  return true;
}

void ReosWatershed::init()
{
  connect( mArea, &ReosParameter::needDerivation, this, &ReosWatershed::calculateDerivedArea );
  connect( mSlope, &ReosParameter::needDerivation, this, &ReosWatershed::calculateDerivedSlope );
}

QPolygonF ReosWatershed::downstreamLine() const
{
  return mDownstreamLine;
}



void ReosWatershed::updateResidual()
{
  if ( mUpstreamWatersheds.empty() )
    return;

  if ( mUpstreamWatersheds.size() == 1 &&  mUpstreamWatersheds.at( 0 )->type() == ReosWatershed::Residual )
  {
    mUpstreamWatersheds.clear(); //only one the ersidual completly alone --> remove
    return;
  }

  if ( mUpstreamWatersheds.at( 0 )->type() != ReosWatershed::Residual )
  {
    mUpstreamWatersheds.emplace( mUpstreamWatersheds.begin(), new ReosWatershed( QPolygonF(), QPointF(), Residual ) );
    mUpstreamWatersheds[0]->mDownstreamWatershed = this;
  }

  //Calculate the residual delineating
  QPolygonF residualDelineating = mDelineating;
  for ( size_t i = 1 ; i < mUpstreamWatersheds.size(); ++i )
  {
    residualDelineating = ReosGeometryUtils::polygonCutByPolygon( residualDelineating, mUpstreamWatersheds[i]->delineating() );
  }

  mUpstreamWatersheds[0]->mDelineating = residualDelineating;
  mUpstreamWatersheds[0]->mExtent = ReosMapExtent( residualDelineating );
  mUpstreamWatersheds[0]->mName->setValue( mName->value() + QObject::tr( " residual" ) );
  mUpstreamWatersheds[0]->mDownstreamWatershed = this;

  if ( mUpstreamWatersheds[0]->mArea->isDerived() )
    mUpstreamWatersheds[0]->mArea->askForDerivation();
}

void ReosWatershed::calculateDerivedArea()
{
  ReosGisEngine *engine = geographicalContext();

  if ( engine )
  {
    mArea->setDerivedValue( engine->polygonArea( mDelineating ) );
  }
}

void ReosWatershed::calculateDerivedSlope()
{
  double length = 0;
  double totalDenom = 0;

  for ( int i = 0; i < mProfile.size() - 1; ++i )
  {
    const QPointF &p1 = mProfile.at( i );
    const QPointF &p2 = mProfile.at( i + 1 );
    double dx = fabs( p1.x() - p2.x() );
    double dy = fabs( p1.y() - p2.y() );
    double dl = sqrt( std::pow( dx, 2 ) + pow( dy, 2 ) );
    double sl = dy / dx;
    length += dl;
    totalDenom += dl / sqrt( sl );
  }

  double averageSlope = pow( length / totalDenom, 2 );

  mSlope->setDerivedValue( averageSlope );
}

ReosGisEngine *ReosWatershed::geographicalContext() const
{
  if ( mGisEngine )
    return mGisEngine;

  if ( mDownstreamWatershed )
    return mDownstreamWatershed->geographicalContext();

  return nullptr;
}



