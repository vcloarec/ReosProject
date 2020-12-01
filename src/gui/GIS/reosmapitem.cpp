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

bool ReosMapItem::isItem( QGraphicsItem *item ) const
{
  return d_ == item;
}

bool ReosMapItem::isMapExist() const
{
  return ( mMap && mMap->mapCanvas() );
}

void ReosMapItem::setDescription( const QString &description )
{
  mDescription = description;
}

ReosMapPolygon::ReosMapPolygon( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapPolygon_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
    d_->base = this;
  }
}

ReosMapPolygon::ReosMapPolygon( ReosMap *map, const QPolygonF &polygon ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapPolygon_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
    static_cast<ReosMapPolygon_p *>( d_ )->mapPolygon = polygon;
    d_->updatePosition();
    d_->base = this;
  }
}

ReosMapPolygon::~ReosMapPolygon()
{
  if ( isMapExist() && d_ )
    delete d_; //deleting this will remove it from the map
}

ReosMapPolygon::ReosMapPolygon( const ReosMapPolygon &other ): ReosMapItem( other.mMap )
{
  if ( other.isMapExist() && other.d_ )
  {
    d_ = other.d_->clone();
    d_->base = this;
  }
}

void ReosMapPolygon::resetPolygon( const QPolygonF &polygon )
{
  if ( isMapExist() && d_ )
  {
    static_cast<ReosMapPolygon_p *>( d_ )->mapPolygon = polygon;
    d_->updatePosition();
  }
}

QPolygonF ReosMapPolygon::mapPolygon() const
{
  if ( isMapExist() && d_ )
    return static_cast<ReosMapPolygon_p *>( d_ )->mapPolygon;

  return QPolygonF();
}

void ReosMapPolygon::movePoint( int pointIndex, const QPointF &p )
{
  if ( !isMapExist() || !d_ )
    return;
  if ( pointIndex < 0 || pointIndex >= static_cast<ReosMapPolygon_p *>( d_ )->mapPolygon.count() )
    return;

  static_cast<ReosMapPolygon_p *>( d_ )->mapPolygon.replace( pointIndex, p );
  d_->updatePosition();
}

void ReosMapItem::setColor( const QColor &color )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->color = color;
}

void ReosMapItem::setExternalColor( const QColor &color )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->externalColor = color;
}

void ReosMapItem::setWidth( double width )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->width = width;
}

void ReosMapItem::setExternalWidth( double externalWidth )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->externalWidth = externalWidth;
}

void ReosMapItem::setStyle( Qt::PenStyle style )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->style = style;
}

QString ReosMapItem::description() const {return mDescription;}

ReosMapPolyline::ReosMapPolyline( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapPolyline_p( canvas );
    d_->base = this;
  }
}

ReosMapPolyline::ReosMapPolyline( ReosMap *map, const QPolygonF &polyline ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapPolyline_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
    static_cast<ReosMapPolyline_p *>( d_ )->mapPolygon = polyline;
    d_->updatePosition();
    d_->base = this;
  }
}

ReosMapPolyline::~ReosMapPolyline()
{
  if ( isMapExist() && d_ )
    delete d_;
}

ReosMapPolyline::ReosMapPolyline( const ReosMapPolyline &other ): ReosMapItem( other.mMap )
{
  if ( other.isMapExist() && other.d_ )
  {
    d_ = other.d_->clone();
    d_->base = this;
  }
}

void ReosMapPolyline::resetPolyline( const QPolygonF &polyline )
{
  if ( !isMapExist() || !d_ )
    return;
  static_cast<ReosMapPolyline_p *>( d_ )->mapPolygon = polyline;
  d_->updatePosition();
}

QPolygonF ReosMapPolyline::mapPolyline() const
{
  if ( isMapExist() && d_ )
    return static_cast<ReosMapPolyline_p *>( d_ )->mapPolygon;
  return QPolygonF();
}

void ReosMapPolyline::movePoint( int pointIndex, const QPointF &p )
{
  if ( !isMapExist() || !d_ )
    return;
  if ( pointIndex < 0 || pointIndex >= static_cast<ReosMapPolyline_p *>( d_ )->mapPolygon.count() )
    return;

  static_cast<ReosMapPolyline_p *>( d_ )->mapPolygon.replace( pointIndex, p );
  d_->updatePosition();
}
