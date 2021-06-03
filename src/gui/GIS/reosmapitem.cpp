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

ReosMapItem::ReosMapItem() {}

ReosMapItem::ReosMapItem( ReosMap *map ): mMap( map )
{}

ReosMapItem::ReosMapItem( const ReosMapItem &other )
{
  if ( other.isMapExist() && other.d_ )
  {
    d_ = other.d_->clone();
    d_->base = this;
    mMap = other.mMap;
  }
  mDescription = other.mDescription;
}

bool ReosMapItem::isItem( QGraphicsItem *item ) const
{
  return d_ == item;
}

bool ReosMapItem::isItem( ReosMapItem *item ) const
{
  if ( !item )
    return false;

  return d_ == item->d_;
}

bool ReosMapItem::isMapExist() const
{
  return ( mMap && mMap->mapCanvas() );
}

void ReosMapItem::setDescription( const QString &description )
{
  mDescription = description;
}

void ReosMapItem::setVisible( bool visible )
{
  if ( isMapExist() )
    d_->setVisible( visible );
}

QGraphicsItem *ReosMapItem::graphicItem()
{
  return d_;
}

ReosMapPolygon::ReosMapPolygon(): ReosMapItem()
{}

ReosMapPolygon::ReosMapPolygon( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapPolygon_p( canvas ); //the owner ship of d pointer is taken by the scene of the map canvas
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

ReosMapPolygon::ReosMapPolygon( const ReosMapPolygon &other ): ReosMapItem( other )
{
}

void ReosMapPolygon::resetPolygon( const QPolygonF &polygon )
{
  if ( isMapExist() && d_ )
  {
    static_cast<ReosMapPolygon_p *>( d_ )->mapPolygon = polygon;
    if ( polygon.isEmpty() )
      static_cast<ReosMapPolygon_p *>( d_ )->setMarkerDistance( -1 );
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


void ReosMapPolygon::setFillColor( const QColor &color )
{
  if ( !isMapExist() || !d_ )
    return;
  if ( color.isValid() )
  {
    d_->brushStyle = Qt::SolidPattern;
    d_->fillColor = color;
  }
  else
  {
    d_->brushStyle = Qt::NoBrush;
  }

  d_->update();
}

void ReosMapItem::setColor( const QColor &color )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->color = color;
  d_->update();
}

void ReosMapItem::setExternalColor( const QColor &color )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->externalColor = color;
  d_->update();
}

void ReosMapItem::setWidth( double width )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->width = width;
  d_->update();
}

void ReosMapItem::setExternalWidth( double externalWidth )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->externalWidth = externalWidth;
  d_->update();
}

void ReosMapItem::setStyle( Qt::PenStyle style )
{
  if ( !isMapExist() || !d_ )
    return;
  d_->style = style;
  d_->update();
}

void ReosMapItem::setZValue( double Z )
{
  d_->setZValue( Z );
}

QString ReosMapItem::description() const {return mDescription;}

ReosMapPolyline::ReosMapPolyline(): ReosMapItem() {}

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

ReosMapPolyline::ReosMapPolyline( const ReosMapPolyline &other ): ReosMapItem( other )
{}

void ReosMapPolyline::resetPolyline( const QPolygonF &polyline )
{
  if ( !isMapExist() || !d_ )
    return;
  static_cast<ReosMapPolyline_p *>( d_ )->mapPolygon = polyline;
  if ( polyline.isEmpty() )
    static_cast<ReosMapPolyline_p *>( d_ )->setMarkerDistance( -1 );
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

void ReosMapPolyline::activeMarker( bool b )
{
  static_cast<ReosMapPolyline_p *>( d_ )->activeMarker( b ) ;
}

void ReosMapPolyline::setMarkerDistance( double d )
{
  static_cast<ReosMapPolyline_p *>( d_ )->setMarkerDistance( d );
}

ReosMapMarker::ReosMapMarker(): ReosMapItem() {}

ReosMapMarker::ReosMapMarker( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapMarker_p( canvas ); //the owner ship of d pointer is taken by the scene of the map canvas
    d_->base = this;
  }
}

ReosMapMarker::ReosMapMarker( ReosMap *map, const QPointF &point ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosMapMarker_p( canvas ); //the owner ship of d pointer is takeny the scene of the map canvas
    static_cast<ReosMapMarker_p *>( d_ )->mapPoint = point;
    static_cast<ReosMapMarker_p *>( d_ )->isEmpty = false;
    d_->updatePosition();
    d_->base = this;
  }
}

ReosMapMarker::~ReosMapMarker()
{
  if ( isMapExist() && d_ )
    delete d_; //deleting this will remove it from the map
}

ReosMapMarker::ReosMapMarker( const ReosMapMarker &other ): ReosMapItem( other )
{}

void ReosMapMarker::resetPoint( const QPointF &point )
{
  if ( isMapExist() && d_ )
  {
    static_cast<ReosMapMarker_p *>( d_ )->mapPoint = point;
    static_cast<ReosMapMarker_p *>( d_ )->isEmpty = false;
    d_->updatePosition();
  }
}

void ReosMapMarker::resetPoint()
{
  if ( isMapExist() && d_ )
  {
    static_cast<ReosMapMarker_p *>( d_ )->isEmpty = true;
    d_->updatePosition();
  }
}

QPointF ReosMapMarker::mapPoint() const
{
  if ( isMapExist() && d_ )
    if ( !static_cast<ReosMapMarker_p *>( d_ )->isEmpty )
      return static_cast<ReosMapMarker_p *>( d_ )->mapPoint;

  return QPointF();
}

void ReosMapMarker::move( const QPointF &p )
{
  resetPoint( p );
}

bool ReosMapMarker::isEmpty() const
{
  if ( isMapExist() && d_ )
    return static_cast<ReosMapMarker_p *>( d_ )->isEmpty;

  return true;
}

ReosMapPolyline &ReosMapPolylineFormater::operator()( ReosMapPolyline &&polyline )
{
  polyline.setColor( mColor );
  polyline.setExternalColor( mExternalColor );
  polyline.setWidth( mWidth );
  polyline.setExternalWidth( mExternalWidth );
  polyline.setStyle( mStyle );
  polyline.setZValue( mZ );
  polyline.setDescription( mDescription );

  return polyline;
}

ReosMapPolyline &ReosMapPolylineFormater::operator()( ReosMapPolyline &polyline )
{
  polyline.setColor( mColor );
  polyline.setExternalColor( mExternalColor );
  polyline.setWidth( mWidth );
  polyline.setExternalWidth( mExternalWidth );
  polyline.setStyle( mStyle );
  polyline.setZValue( mZ );
  polyline.setDescription( mDescription );

  return polyline;
}

QString ReosMapPolylineFormater::description() const
{
  return mDescription;
}

void ReosMapPolylineFormater::setDescription( const QString &descritpion )
{
  mDescription = descritpion;
}

double ReosMapPolylineFormater::z() const
{
  return mZ;
}

void ReosMapPolylineFormater::setZ( double z )
{
  mZ = z;
}

Qt::PenStyle ReosMapPolylineFormater::style() const
{
  return mStyle;
}

void ReosMapPolylineFormater::setStyle( const Qt::PenStyle &style )
{
  mStyle = style;
}

double ReosMapPolylineFormater::externalWidth() const
{
  return mExternalWidth;
}

void ReosMapPolylineFormater::setExternalWidth( double externalWidth )
{
  mExternalWidth = externalWidth;
}

double ReosMapPolylineFormater::width() const
{
  return mWidth;
}

void ReosMapPolylineFormater::setWidth( double width )
{
  mWidth = width;
}

QColor ReosMapPolylineFormater::externalColor() const
{
  return mExternalColor;
}

void ReosMapPolylineFormater::setExternalColor( const QColor &externalColor )
{
  mExternalColor = externalColor;
}

QColor ReosMapPolylineFormater::color() const
{
  return mColor;
}

void ReosMapPolylineFormater::setColor( const QColor &color )
{
  mColor = color;
}
