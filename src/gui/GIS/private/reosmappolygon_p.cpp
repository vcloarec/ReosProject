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

QRectF ReosMapPolygon_p::boundingRect() const
{
  QRectF bb = mViewPolygon.boundingRect();
  if ( externalWidth > width )
    return bb.adjusted( -externalWidth, -externalWidth, externalWidth, externalWidth );
  else
    return bb.adjusted( -width, -width, width, width );
}

void ReosMapPolygon_p::updatePosition()
{
  prepareGeometryChange();
  mViewPolygon.clear();
  if ( mapPolygon.count() < 1 )
    return;
  const QPointF pview0 = toCanvasCoordinates( QgsPoint( mapPolygon.at( 0 ) ) );

  for ( auto &p : mapPolygon )
  {
    const QPointF pview = toCanvasCoordinates( QgsPoint( p ) );
    mViewPolygon.append( QPointF( pview.x() - pview0.x(), pview.y() - pview0.y() ) );
  }
  setPos( pview0 );

  if ( mIsMarkerOnLine )
    mMarkerPositionOnView = toCanvasCoordinates( QgsPoint( mMarkerposition ) );
}

QPainterPath ReosMapPolygon_p::shape() const
{
  QPainterPath path;
  path.addPolygon( mViewPolygon );
  path.closeSubpath();
  return path ;
}

void ReosMapPolygon_p::setEditing( bool b )
{
  mIsEditing = b;
  update();
}

void ReosMapPolygon_p::move( const QPointF &translation )
{
  QPolygonF newPoints( mapPolygon.size() );

  for ( int i = 0; i < mapPolygon.size(); ++i )
    newPoints[i] = mapPolygon.at( i ) + translation;

  mapPolygon = newPoints;
  updatePosition();
}

QPointF ReosMapPolygon_p::mapPos() const
{
  if ( mapPolygon.isEmpty() )
    return QPointF();
  else
    return mapPolygon.at( 0 );
}

int ReosMapPolygon_p::findVertexInView( const QRectF &zone ) const
{
  for ( int i = 0; i < mViewPolygon.size(); ++i )
    if ( zone.contains( mViewPolygon.at( i ) + pos() ) )
      return i;

  return -1;
}

void ReosMapPolygon_p::activeMarker( bool b )
{
  mIsMarkerActive = b;
}

void ReosMapPolygon_p::setMarkerDistance( double d )
{
  if ( !mIsMarkerActive || d < 0 || mapPolygon.empty() )
  {
    mIsMarkerOnLine = false;
    updatePosition();
    return;
  }

  double distFromBegin = 0;
  int i = 0;
  do
  {
    const QPointF &p1 = mapPolygon.at( i );
    const QPointF &p2 = mapPolygon.at( i + 1 );
    distFromBegin += sqrt( pow( p1.x() - p2.x(), 2 ) + pow( p1.y() - p2.y(), 2 ) );
  }
  while ( d >= distFromBegin && ++i < mapPolygon.count() - 1 );

  if ( d > distFromBegin )
  {
    mIsMarkerOnLine = false;
    updatePosition();
    return;
  }

  if ( i >= mapPolygon.count() - 1 )
  {
    mIsMarkerOnLine = false;
    updatePosition();
    return;
  }
  mIsMarkerOnLine = true;
  const QPointF &p1 = mapPolygon.at( i );
  const QPointF &p2 = mapPolygon.at( i + 1 );
  double segDist = sqrt( pow( p1.x() - p2.x(), 2 ) + pow( p1.y() - p2.y(), 2 ) );
  double distFromP2 = distFromBegin - d;
  double ratio = distFromP2 / segDist;
  mMarkerposition = p2 - ratio * ( p2 - p1 );
  updatePosition();
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

  if ( mIsEditing )
  {

    pen.setWidthF( 1 );
    QBrush brush( Qt::SolidPattern );
    brush.setColor( externalColor );
    painter->setBrush( brush );
    painter->setPen( pen );

    for ( const QPointF &pt : std::as_const( mViewPolygon ) )
    {
      QRectF rect( pt - QPointF( externalWidth, externalWidth ), QSizeF( externalWidth * 2, externalWidth * 2 ) );
      painter->drawRect( rect );
    }

    pen.setColor( color );
    brush.setColor( color );
    painter->setBrush( brush );
    painter->setPen( pen );
    for ( const QPointF &pt : std::as_const( mViewPolygon ) )
    {
      QRectF rect( pt - QPointF( width, width ), QSizeF( width * 2, width * 2 ) );
      painter->drawRect( rect );
    }

  }

  if ( mIsMarkerOnLine )
  {
    pen.setWidthF( 1 );
    QBrush brush( Qt::SolidPattern );
    brush.setColor( externalColor );
    painter->setBrush( brush );
    painter->setPen( pen );
    painter->drawEllipse( mMarkerPositionOnView - pos(), externalWidth, externalWidth );

    pen.setColor( color );
    brush.setColor( color );
    painter->setBrush( brush );
    painter->setPen( pen );
    painter->drawEllipse( mMarkerPositionOnView - pos(), width, width );
  }

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
{}

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
  QPainterPath path;
  path.addEllipse( boundingRect() );
  return path;
}

void ReosMapMarker_p::move( const QPointF &translation )
{
  mapPoint += translation;
  updatePosition();
}

QPointF ReosMapMarker_p::mapPos() const {return mapPoint;}

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
