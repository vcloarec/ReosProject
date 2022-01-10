/***************************************************************************
  reospolylinesstructures.cpp - ReosPolylinesStructures

 ---------------------
 begin                : 10.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reospolylinesstructure.h"

ReosPolylinesStructure::ReosPolylinesStructure( const QString &crs )
  : mCrs( crs )
{

}

void ReosPolylinesStructure::addPolylines( const QPolygonF &polylines, const QString &id )
{
  mPolylines.append( polylines );
  mPolylinesId.append( id );
}

QPolygonF ReosPolylinesStructure::polyline( const QString &id ) const
{
  for ( int i = 0; i < mPolylinesId.count(); ++i )
  {
    if ( mPolylinesId.at( i ) == id && i < mPolylines.count() )
      return mPolylines.at( i );
  }

  return QPolygonF();
}
