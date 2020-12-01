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
