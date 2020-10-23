/***************************************************************************
                      reosmapitem.cpp
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

#include "reosmapitem.h"
#include "reosmap.h"
#include <qgsmapcanvas.h>
#include "reosmappolygon_p.h"

ReosMapPolygon::ReosMapPolygon( ReosMap *map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
    d = std::make_unique<ReosMapPolygon_p>( canvas );
}

void ReosMapPolygon::resetPolygon( const QPolygonF &polygon )
{
  d->mapPolygon = polygon;
  d->updatePosition();
}

QPolygonF ReosMapPolygon::mapPolygon() const
{
  return d->mapPolygon;
}

void ReosMapPolygon::movePoint( int pointIndex, const QPointF &p )
{
  if ( !d )
    return;
  if ( pointIndex < 0 || pointIndex >= d->mapPolygon.count() )
    return;

  d->mapPolygon.replace( pointIndex, p );
  d->updatePosition();
}


ReosMapPolyline::ReosMapPolyline( ReosMap *map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
    d = std::make_unique<ReosMapPolyline_p>( canvas );
}

void ReosMapPolyline::resetPolyline( const QPolygonF &polyline )
{
  d->mapPolygon = polyline;
  d->updatePosition();
}

QPolygonF ReosMapPolyline::mapPolyline() const
{
  return d->mapPolygon;
}

void ReosMapPolyline::movePoint( int pointIndex, const QPointF &p )
{
  if ( !d )
    return;
  if ( pointIndex < 0 || pointIndex >= d->mapPolygon.count() )
    return;

  d->mapPolygon.replace( pointIndex, p );
  d->updatePosition();
}
