/***************************************************************************
  reosmappolylinesstructure_p.cpp - ReosMapPolylinesStructure_p

 ---------------------
 begin                : 12.1.2022
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
#include "reosmappolylinesstructure_p.h"

#include "reospolylinesstructure.h"
#include "reosmapextent.h"

ReosMapPolylinesStructure_p::ReosMapPolylinesStructure_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{}

ReosMapItem_p *ReosMapPolylinesStructure_p::clone()
{
  return new ReosMapPolylinesStructure_p( mMapCanvas );
}

QPointF ReosMapPolylinesStructure_p::mapPos() const
{
  ReosMapExtent extent = mStructure->extent( crs() );
  return QPointF( extent.xMapMin(), extent.yMapMin() );
}

void ReosMapPolylinesStructure_p::updatePosition()
{
  prepareGeometryChange();
  mOriginInView = toCanvasCoordinates( mapPos() );
  if ( mExterior )
    mExterior->updatePosition( mStructure->boundary( crs() ), this );
}

void ReosMapPolylinesStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  mExterior = new ReosMapStructureExteriorItem( this );
  updatePosition();
}

void ReosMapPolylinesStructure_p::paint( QPainter *painter )
{

}

ReosMapStructureExteriorItem::ReosMapStructureExteriorItem( ReosMapPolylinesStructure_p *parent ): QGraphicsItem( parent )
{
  setZValue( 10 );
}

void ReosMapStructureExteriorItem::updatePosition( const QPolygonF &poly, ReosMapPolylinesStructure_p *parent )
{
  prepareGeometryChange();
  polyInLocalView.clear();

  for ( const QPointF &pt : poly )
  {
    QPointF ptCanvas = parent->toCanvasCoordinates( pt );
    QPointF ptLocal = ptCanvas;
    polyInLocalView.append( ptLocal );
  }
  mBBox = polyInLocalView.boundingRect();
}

QRectF ReosMapStructureExteriorItem::boundingRect() const {return mBBox;}

void ReosMapStructureExteriorItem::paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * )
{
  painter->save();
  painter->setRenderHint( QPainter::Antialiasing, true );
  QPen pen;
  pen.setColor( QColor( 250, 175, 100 ) );
  pen.setWidthF( 5 );
  painter->setPen( pen );
  painter->drawPolygon( polyInLocalView );

  pen.setColor( Qt::gray );
  pen.setWidthF( 2 );
  painter->setPen( pen );
  painter->drawPolygon( polyInLocalView );

  QBrush brush;
  brush.setColor( Qt::gray );
  brush.setStyle( Qt::SolidPattern );
  pen.setColor( QColor( 250, 175, 100 ) );
  pen.setWidth( 2 );
  painter->setBrush( brush );
  painter->setPen( pen );
  for ( const QPointF pt : std::as_const( polyInLocalView ) )
    painter->drawEllipse( pt, 4, 4 );

  painter->restore();
}
