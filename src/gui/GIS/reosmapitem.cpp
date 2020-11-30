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

ReosMapItem::ReosMapItem( ReosMap *map ): mMap( map )
{}

bool ReosMapItem::isMapExist() const
{
  return ( mMap && mMap->mapCanvas() );
}

ReosMapPolygon::ReosMapPolygon( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
    d = new ReosMapPolygon_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
}

ReosMapPolygon::ReosMapPolygon( ReosMap *map, const QPolygonF &polygon ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d = new ReosMapPolygon_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
    d->mapPolygon = polygon;
    d->updatePosition();
  }
}

ReosMapPolygon::~ReosMapPolygon()
{
  if ( isMapExist() && d )
    delete d; //deleting this will remove it from the map
}

ReosMapPolygon::ReosMapPolygon( const ReosMapPolygon &other ): ReosMapItem( other.mMap )
{
  if ( other.isMapExist() && other.d )
    d = other.d->clone();
}

void ReosMapPolygon::resetPolygon( const QPolygonF &polygon )
{
  if ( isMapExist() && d )
  {
    d->mapPolygon = polygon;
    d->updatePosition();
  }
}

QPolygonF ReosMapPolygon::mapPolygon() const
{
  if ( isMapExist() && d )
    return d->mapPolygon;

  return QPolygonF();
}

void ReosMapPolygon::movePoint( int pointIndex, const QPointF &p )
{
  if ( !isMapExist() || !d )
    return;
  if ( pointIndex < 0 || pointIndex >= d->mapPolygon.count() )
    return;

  d->mapPolygon.replace( pointIndex, p );
  d->updatePosition();
}

void ReosMapPolygon::setColor( const QColor &color )
{
  if ( !isMapExist() || !d )
    return;
  d->color = color;
}

void ReosMapPolygon::setExternalColor( const QColor &color )
{
  if ( !isMapExist() || !d )
    return;
  d->externalColor = color;
}

void ReosMapPolygon::setWidth( double width )
{
  if ( !isMapExist() || !d )
    return;
  d->width = width;
}

void ReosMapPolygon::setExternalWidth( double externalWidth )
{
  if ( !isMapExist() || !d )
    return;
  d->externalWidth = externalWidth;
}

void ReosMapPolygon::setStyle( Qt::PenStyle style )
{
  if ( !isMapExist() || !d )
    return;
  d->style = style;
}

void ReosMapPolyline::setColor( const QColor &color )
{
  if ( !isMapExist() || !d )
    return;
  d->color = color;
}

void ReosMapPolyline::setExternalColor( const QColor &color )
{
  if ( !isMapExist() || !d )
    return;
  d->externalColor = color;
}

void ReosMapPolyline::setWidth( double width )
{
  if ( !isMapExist() || !d )
    return;
  d->width = width;
}

void ReosMapPolyline::setExternalWidth( double externalWidth )
{
  if ( !isMapExist() || !d )
    return;
  d->externalWidth = externalWidth;
}

void ReosMapPolyline::setStyle( Qt::PenStyle style )
{
  if ( !isMapExist() || !d )
    return;
  d->style = style;
}


ReosMapPolyline::ReosMapPolyline( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
    d = new ReosMapPolyline_p( canvas );
}

ReosMapPolyline::ReosMapPolyline( ReosMap *map, const QPolygonF &polyline ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d = new ReosMapPolyline_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
    d->mapPolygon = polyline;
    d->updatePosition();
  }
}

ReosMapPolyline::~ReosMapPolyline()
{
  if ( isMapExist() && d )
    delete d;
}

ReosMapPolyline::ReosMapPolyline( const ReosMapPolyline &other ): ReosMapItem( other.mMap )
{
  if ( other.isMapExist() && other.d )
    d = other.d->clone();
}

void ReosMapPolyline::resetPolyline( const QPolygonF &polyline )
{
  if ( !isMapExist() || !d )
    return;
  d->mapPolygon = polyline;
  d->updatePosition();
}

QPolygonF ReosMapPolyline::mapPolyline() const
{
  if ( isMapExist() && d )
    return d->mapPolygon;
  return QPolygonF();
}

void ReosMapPolyline::movePoint( int pointIndex, const QPointF &p )
{
  if ( !isMapExist() || !d )
    return;
  if ( pointIndex < 0 || pointIndex >= d->mapPolygon.count() )
    return;

  d->mapPolygon.replace( pointIndex, p );
  d->updatePosition();
}
