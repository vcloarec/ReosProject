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
  ReosMapItem_p( canvas )
{

}

ReosMapPolygon_p *ReosMapPolygon_p::clone()
{
  ReosMapPolygon_p *other = new ReosMapPolygon_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->brushStyle = brushStyle;
  other->fillColor = fillColor;
  other->mapPolygon = mapPolygon;
  other->updatePosition();
  return other;
}

QRectF ReosMapPolygon_p::boundingRect() const {return mViewPolygon.boundingRect();}

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

QPainterPath ReosMapPolygon_p::shape() const
{
  QPen pen;
  pen.setWidthF( std::max( externalWidth, width ) );
  QPainterPathStroker pps( pen );
  QPainterPath path;
  path.addPolygon( mViewPolygon );
  return pps.createStroke( path );
}

void ReosMapPolygon_p::paint( QPainter *painter )
{
  painter->save();
  QPen pen;
  if ( externalWidth > width )
  {
    pen.setWidthF( externalWidth );
    pen.setColor( externalColor );
    QBrush brush( Qt::NoBrush );
    painter->setBrush( brush );
    painter->setPen( pen );
    draw( painter );
  }
  pen.setWidthF( width );
  pen.setColor( color );
  pen.setStyle( style );
  QBrush brush( brushStyle );
  brush.setColor( fillColor );
  painter->setBrush( brush );
  painter->setPen( pen );
  draw( painter );
  painter->restore();
}

void ReosMapPolygon_p::draw( QPainter *painter )
{
  painter->drawPolygon( mViewPolygon );
}

ReosMapPolyline_p::ReosMapPolyline_p( QgsMapCanvas *canvas ):
  ReosMapPolygon_p( canvas )
{}

ReosMapPolyline_p *ReosMapPolyline_p::clone()
{
  ReosMapPolyline_p *other = new ReosMapPolyline_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->mapPolygon = mapPolygon;
  other->updatePosition();
  return other;
}

void ReosMapPolyline_p::draw( QPainter *painter )
{
  painter->drawPolyline( mViewPolygon );
}

ReosMapMarker_p::ReosMapMarker_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{
}

ReosMapMarker_p *ReosMapMarker_p::clone()
{
  ReosMapMarker_p *other = new ReosMapMarker_p( mMapCanvas );
  other->color = color;
  other->externalColor = externalColor;
  other->width = width;
  other->externalWidth = externalWidth;
  other->style = style;
  other->mapPoint = mapPoint;
  other->isEmpty = isEmpty;
  other->updatePosition();
  return other;
}

QRectF ReosMapMarker_p::boundingRect() const
{
  if ( isEmpty )
    return QRectF();

  double w = std::max( externalWidth, width );
  return QRectF( mViewPoint - QPointF( w / 2, w / 2 ), QSizeF( w, w ) );
}

void ReosMapMarker_p::updatePosition()
{
  if ( isEmpty )
    return;
  prepareGeometryChange();
  mViewPoint = toCanvasCoordinates( mapPoint );
}

QPainterPath ReosMapMarker_p::shape() const
{
  if ( isEmpty )
    return QPainterPath();
  QPen pen;
  QPainterPathStroker pps( pen );
  QPainterPath path;
  double w = std::max( externalWidth, width );
  path.addEllipse( mViewPoint, w / 2, w / 2 );
  return pps.createStroke( path );
}

void ReosMapMarker_p::paint( QPainter *painter )
{
  if ( isEmpty )
    return;

  painter->save();
  QPen pen;
  if ( externalWidth > width )
  {
    pen.setColor( externalColor );
    QBrush brush( Qt::SolidPattern );
    brush.setColor( externalColor );
    painter->setBrush( brush );
    painter->setPen( pen );
    painter->drawEllipse( mViewPoint, externalWidth / 2, externalWidth / 2 );
  }
  pen.setColor( color );
  QBrush brush( Qt::SolidPattern );
  brush.setColor( color );
  painter->setPen( pen );
  painter->setBrush( brush );
  painter->drawEllipse( mViewPoint, width / 2, width / 2 );
  painter->restore();
}
