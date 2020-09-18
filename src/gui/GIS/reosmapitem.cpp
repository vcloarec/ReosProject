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

ReosMapItem *ReosMapPolygonFactory::create( QgsMapCanvas *map )
{
  return new ReosMapPolygon( new ReosMapPolygon_p( map ) );
}

ReosMapPolygon *ReosMapPolygonFactory::mapItem( ReosMap *map, const QPolygonF &polygon )
{
  ReosMapPolygon *item = static_cast<ReosMapPolygon *>( map->createMapItem( this ) );
  item->setPolygon( polygon );
  return item;
}


ReosMapPolygon::~ReosMapPolygon()
{
  if ( d )
    delete d;
}

void ReosMapPolygon::setPolygon( const QPolygonF &polygon )
{
  d->mapPolygon = polygon;
  d->updatePosition();
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

ReosMapPolygon::ReosMapPolygon( ReosMapPolygon_p *p_d ): d( p_d )
{}

ReosMapItem::~ReosMapItem() {}
