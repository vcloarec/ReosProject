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

ReosMapExtent::ReosMapExtent( const QPolygonF &polygon )
{
  if ( polygon.isEmpty() )
    return;

  for ( const QPointF &pt : polygon )
    addPointToExtent( pt );
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
