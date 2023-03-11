/***************************************************************************
                      reosmapextent.h
                     --------------------------------------
Date                 : 01-10-2020
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

#include "reosmapextent.h"
#include "reosgisengine.h"

ReosMapExtent::ReosMapExtent( const QRectF &extent )
{
  QRectF normalizedExtent = extent.normalized();
  mXMin = normalizedExtent.left();
  mXMax = normalizedExtent.right();
  mYMin = normalizedExtent.top();
  mYMax = normalizedExtent.bottom();
}

ReosMapExtent::ReosMapExtent( double xMin, double yMin, double xMax, double yMax ):
  mXMin( xMin ), mXMax( xMax ), mYMin( yMin ), mYMax( yMax )
{}

ReosMapExtent::ReosMapExtent( const QPolygonF &polygon, const QString &crs )
  : mCrs( crs )
{
  if ( polygon.isEmpty() )
    return;

  for ( const QPointF &pt : polygon )
    addPointToExtent( pt );
}

ReosMapExtent::ReosMapExtent( const ReosSpatialPosition &pos1, const ReosSpatialPosition &pos2 )
{
  QPointF pos2InCrs1 = ReosGisEngine::transformToCoordinates( pos2, pos1.crs() );
  mXMin = std::min( pos1.position().x(), pos2InCrs1.x() );
  mXMax = std::max( pos1.position().x(), pos2InCrs1.x() );
  mYMin = std::min( pos1.position().y(), pos2InCrs1.y() );
  mYMax = std::max( pos1.position().y(), pos2InCrs1.y() );

  mCrs = pos1.crs();
}

double ReosMapExtent::width() const
{return mXMax - mXMin;}

double ReosMapExtent::height() const
{return mYMax - mYMin;}

double ReosMapExtent::xMapMin() const {return mXMin;}

double ReosMapExtent::xMapMax() const {return mXMax;}

double ReosMapExtent::yMapMin() const {return mYMin;}

double ReosMapExtent::yMapMax() const {return mYMax;}


bool ReosMapExtent::operator==( const ReosMapExtent &other ) const
{
  return mXMin == other.mXMin && mXMax == other.mXMax && mYMin == other.mYMin && mYMax == other.mYMax;

}

bool ReosMapExtent::operator!=( const ReosMapExtent &other ) const
{
  return !operator==( other );
}

ReosMapExtent ReosMapExtent::operator*( const ReosMapExtent &other ) const
{
  ReosMapExtent ret;

  ret.mXMin = std::max( mXMin, other.mXMin );
  ret.mXMax = std::min( mXMax, other.mXMax );
  ret.mYMin = std::max( mYMin, other.mYMin );
  ret.mYMax = std::min( mYMax, other.mYMax );

  if ( ret.mXMin > ret.mXMax || ret.mYMin > ret.mYMax )
    return ReosMapExtent();

  return ret;

}

QPolygonF ReosMapExtent::toPolygon() const
{
  QPolygonF ret;
  ret << QPointF( mXMin, mYMin );
  ret << QPointF( mXMin, mYMax );
  ret << QPointF( mXMax, mYMax );
  ret << QPointF( mXMax, mYMin );

  return ret;
}

QRectF ReosMapExtent::toRectF() const
{
  return QRectF( QPointF( mXMin, mYMin ), QPointF( mXMax, mYMax ) );
}

ReosMapExtent ReosMapExtent::decode( const ReosEncodedElement &element )
{
  if ( element.description() == QStringLiteral( "map-extent" ) )
  {
    double xMin;
    if ( !element.getData( QStringLiteral( "xmin" ), xMin ) )
      return ReosMapExtent();
    double xMax;
    if ( !element.getData( QStringLiteral( "xmax" ), xMax ) )
      return ReosMapExtent();
    double yMin;
    if ( !element.getData( QStringLiteral( "ymin" ), yMin ) )
      return ReosMapExtent();
    double yMax;
    if ( !element.getData( QStringLiteral( "ymax" ), yMax ) )
      return ReosMapExtent();
    QString crs;
    if ( !element.getData( QStringLiteral( "crs" ), crs ) )
      return ReosMapExtent();

    ReosMapExtent ret( xMin, yMin, xMax, yMax );
    ret.setCrs( crs );

    return ret;
  }
  else
  {
    return ReosMapExtent();
  }
}

ReosEncodedElement ReosMapExtent::encode() const
{
  ReosEncodedElement ret( QStringLiteral( "map-extent" ) );

  ret.addData( QStringLiteral( "xmin" ), mXMin );
  ret.addData( QStringLiteral( "xmax" ), mXMax );
  ret.addData( QStringLiteral( "ymin" ), mYMin );
  ret.addData( QStringLiteral( "ymax" ), mYMax );
  ret.addData( QStringLiteral( "crs" ), mCrs );

  return ret;
}

bool ReosMapExtent::isValid() const
{
    return mXMax >= mXMin && mYMax > mYMin;
}

QString ReosMapExtent::crs() const
{
    return mCrs;
}

void ReosMapExtent::setCrs( const QString &crs )
{
  mCrs = crs;
}

bool ReosMapExtent::contains( const QPointF &point ) const
{
  return point.x() >= mXMin && point.x() <= mXMax &&
         point.y() >= mYMin && point.y() <= mYMax;
}

bool ReosMapExtent::containsPartialy( const QPolygonF &line ) const
{
  for ( auto &point : line )
    if ( contains( point ) )
      return true;

  return false;
}

void ReosMapExtent::addPointToExtent( const QPointF &pt )
{
  if ( mXMin > pt.x() )
    mXMin = pt.x();
  if ( mXMax < pt.x() )
    mXMax = pt.x();
  if ( mYMin > pt.y() )
    mYMin = pt.y();
  if ( mYMax < pt.y() )
    mYMax = pt.y();
}

void ReosMapExtent::extendWithExtent( const ReosMapExtent &other )
{
  if ( mCrs.isEmpty() )
    mCrs = other.crs();

  ReosSpatialPosition otherCornerMin( other.xMapMin(), other.yMapMin(), other.crs() );
  ReosSpatialPosition otherCornerMax( other.xMapMax(), other.yMapMax(), other.crs() );

  QPointF otherCornerMinInCrs = ReosGisEngine::transformToCoordinates( otherCornerMin, mCrs );
  QPointF otherCornerMaxInCrs = ReosGisEngine::transformToCoordinates( otherCornerMax, mCrs );

  mXMin = std::min( mXMin, std::min( otherCornerMinInCrs.x(), otherCornerMaxInCrs.x() ) );
  mXMax = std::max( mXMax, std::max( otherCornerMinInCrs.x(), otherCornerMaxInCrs.x() ) );
  mYMin = std::min( mYMin, std::min( otherCornerMinInCrs.y(), otherCornerMaxInCrs.y() ) );
  mYMax = std::max( mYMax, std::max( otherCornerMinInCrs.y(), otherCornerMaxInCrs.y() ) );
}

ReosSpatialPosition::ReosSpatialPosition( const QPointF &position, const QString &crs )
  : mPosition( position )
  , mCrs( crs )
{
  mIsValid = true;
}

ReosSpatialPosition::ReosSpatialPosition( double x, double y, const QString &crs ): ReosSpatialPosition( QPointF( x, y ), crs )
{}

QPointF ReosSpatialPosition::position() const
{
  return mPosition;
}

QString ReosSpatialPosition::crs() const
{
  return mCrs;
}

bool ReosSpatialPosition::isValid() const
{
  return mIsValid;
}

ReosSpatialPosition ReosSpatialPosition::decode( const ReosEncodedElement &element )
{
  ReosSpatialPosition ret;

  if ( element.description() != QStringLiteral( "spatial-position" ) )
    return ret;

  ret.mIsValid = ( element.getData( QStringLiteral( "position" ), ret.mPosition ) &&
                   element.getData( QStringLiteral( "crs" ), ret.mCrs ) );

  if ( ret.mIsValid )
  {
    ret.mIsValid = false;
    element.getData( QStringLiteral( "is-valid" ), ret.mIsValid );
  }

  return ret;
}

ReosEncodedElement ReosSpatialPosition::encode() const
{
  ReosEncodedElement element( QStringLiteral( "spatial-position" ) );

  element.addData( QStringLiteral( "position" ), mPosition );;
  element.addData( QStringLiteral( "crs" ), mCrs );
  element.addData( QStringLiteral( "is-valid" ), mIsValid );

  return element;
}

