/***************************************************************************
                      reosmappolygon.cpp
                     --------------------------------------
Date                 : 17-09-2020
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

#include "reosmappolygon_p.h"

#include <QPainter>
#include <qgspoint.h>

ReosMapPolygon_p::ReosMapPolygon_p( QgsMapCanvas *canvas ):
  QgsMapCanvasItem( canvas )
{

}

void ReosMapPolygon_p::updatePosition()
{
  prepareGeometryChange();
  mViewPolygon.clear();
  if ( mapPolygon.count() < 1 )
    return;
  const QPointF pview0 = toCanvasCoordinates( QgsPoint( mapPolygon.at( 0 ) ) );

  for ( auto p : mapPolygon )
  {
    const QPointF pview = toCanvasCoordinates( QgsPoint( p ) );
    mViewPolygon.append( QPointF( pview.x() - pview0.x(), pview.y() - pview0.y() ) );
  }
  setPos( pview0 );
}

void ReosMapPolygon_p::paint( QPainter *painter )
{
  painter->save();
  painter->drawPolygon( mViewPolygon );
  painter->restore();
}
