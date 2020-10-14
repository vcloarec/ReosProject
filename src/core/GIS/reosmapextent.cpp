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


ReosMapExtent::ReosMapExtent( QRectF extent )
{
  mXMin = extent.left();
  mXMax = extent.right();
  mYMin = extent.top();
  mYMax = extent.bottom();
}

ReosMapExtent::ReosMapExtent( double xMin, double yMin, double xMax, double yMax ):
  mXMin( xMin ), mXMax( xMax ), mYMin( yMin ), mYMax( yMax )
{}

double ReosMapExtent::width() const
{return mXMax - mXMin;}

double ReosMapExtent::height() const
{return mYMax - mYMin;}

double ReosMapExtent::xMapMin() const {return mXMin;}

double ReosMapExtent::xMapMax() const {return mXMax;}

double ReosMapExtent::yMapMin() const {return mYMin;}

double ReosMapExtent::yMapMax() const {return mYMax;}

bool ReosMapExtent::inExtent( const QPointF &point ) const
{
  return point.x() >= mXMin &&
         point.x() <= mXMax &&
         point.y() >= mYMin &&
         point.y() <= mYMax;
}

bool ReosMapExtent::operator==( const ReosMapExtent &other ) const
{
  return mXMin == other.mXMin && mXMax == other.mXMax && mYMin == other.mYMin && mYMax == other.mYMax;

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
