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
#include "reosstyleregistery.h"

ReosMapPolylinesStructure_p::ReosMapPolylinesStructure_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{}

ReosMapItem_p *ReosMapPolylinesStructure_p::clone()
{
  return nullptr;
}

QPointF ReosMapPolylinesStructure_p::mapPos() const
{
  ReosMapExtent extent = mStructure->extent( crs() );
  return QPointF( extent.xMapMin(), extent.yMapMin() );
}

void ReosMapPolylinesStructure_p::setExteriorBaseWidth( double width )
{
  mExterior->setBaseWidth( width );
}

void ReosMapPolylinesStructure_p::updatePosition()
{
  prepareGeometryChange();
  mOriginInView = toCanvasCoordinates( mapPos() );
  const QString &destCrs = crs();

  if ( mExterior )
    mExterior->updatePosition( mStructure->boundary( destCrs ), this );
  if ( mLines )
    mLines->updatePosition( mStructure, this, destCrs );
}

void ReosMapPolylinesStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  mExterior = new ReosMapStructureExteriorItem( this );
  mLines = new ReosMapStructureLinesItem( this );

  mExterior->setVisible( false );
  updatePosition();
}

void ReosMapPolylinesStructure_p::paint( QPainter *painter )
{}

ReosMapStructureExteriorItem::ReosMapStructureExteriorItem( ReosMapPolylinesStructure_p *parent ): QGraphicsItem( parent )
{
  setZValue( 11 );
}

void ReosMapStructureExteriorItem::updatePosition( const QPolygonF &poly, ReosMapPolylinesStructure_p *parent )
{
  prepareGeometryChange();
  mPolyInLocalView.clear();

  for ( const QPointF &pt : poly )
  {
    QPointF ptCanvas = parent->toCanvasCoordinates( pt );
    QPointF ptLocal = ptCanvas;
    mPolyInLocalView.append( ptLocal );
  }
  mBBox = mPolyInLocalView.boundingRect().adjusted( - 5, -5, 5, 5 );
}

QRectF ReosMapStructureExteriorItem::boundingRect() const
{
  return mBBox;
}

void ReosMapStructureExteriorItem::paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * )
{
  painter->save();

  painter->setRenderHint( QPainter::Antialiasing, true );
  QPen pen;
  pen.setColor( ReosStyleRegistery::instance()->orangeReos() );
  pen.setWidthF( mBaseWidth );
  painter->setPen( pen );
  painter->drawPolygon( mPolyInLocalView );

  pen.setColor( Qt::gray );
  pen.setWidthF( mBaseWidth / 5 * 2 );
  painter->setPen( pen );
  painter->drawPolygon( mPolyInLocalView );

  QBrush brush;
  brush.setColor( Qt::gray );
  brush.setStyle( Qt::SolidPattern );
  pen.setColor( ReosStyleRegistery::instance()->orangeReos( 100 ) );
  pen.setWidth( mBaseWidth / 5 * 2 );
  painter->setBrush( brush );
  painter->setPen( pen );
  for ( const QPointF pt : std::as_const( mPolyInLocalView ) )
    painter->drawEllipse( pt,  mBaseWidth / 5 * 4,  mBaseWidth / 5 * 4 );

  painter->restore();
}

void ReosMapStructureExteriorItem::setBaseWidth( double baseWidth )
{
  mBaseWidth = baseWidth;
  update();
}

ReosMapStructureLinesItem::ReosMapStructureLinesItem( ReosMapPolylinesStructure_p *parent ): QGraphicsItem( parent )
{
  setZValue( 10 );
}

void ReosMapStructureLinesItem::updatePosition(
  const ReosPolylinesStructure *structure,
  ReosMapPolylinesStructure_p *parent,
  const QString &destinationCrs )
{
  prepareGeometryChange();
  mLinesInLocalView.clear();

  const QVector<QLineF> &mRawLines = structure->rawLines( destinationCrs );
  QgsRectangle extent;

  for ( const QLineF &line : mRawLines )
  {
    mLinesInLocalView.append( QLineF( parent->toCanvasCoordinates( line.p1() ),  parent->toCanvasCoordinates( line.p2() ) ) );
    extent.include( mLinesInLocalView.last().p1() );
    extent.include( mLinesInLocalView.last().p2() );
  }
  mBBox = extent.toRectF().adjusted( - 5, -5, 5, 5 );
}

QRectF ReosMapStructureLinesItem::boundingRect() const
{
  return mBBox;
}

void ReosMapStructureLinesItem::paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * )
{
  painter->save();

  painter->setRenderHint( QPainter::Antialiasing, true );
  QPen pen;
  pen.setColor( ReosStyleRegistery::instance()->orangeReos() );
  pen.setWidthF( mBaseWidth );
  painter->setPen( pen );
  for ( const QLineF &line : std::as_const( mLinesInLocalView ) )
    painter->drawLine( line );

  pen.setColor( Qt::gray );
  pen.setWidthF( mBaseWidth / 5 * 2 );
  painter->setPen( pen );
  for ( const QLineF &line : std::as_const( mLinesInLocalView ) )
    painter->drawLine( line );

  QBrush brush;
  brush.setColor( Qt::gray );
  brush.setStyle( Qt::SolidPattern );
  pen.setColor( ReosStyleRegistery::instance()->orangeReos( 100 ) );
  pen.setWidth( mBaseWidth / 5 * 2 );
  painter->setBrush( brush );
  painter->setPen( pen );

  for ( const QLineF &line : std::as_const( mLinesInLocalView ) )
  {
    const QPointF &pt1 = line.p1();
    painter->drawEllipse( pt1,  mBaseWidth / 5 * 4,  mBaseWidth / 5 * 4 );
    const QPointF &pt2 = line.p2();
    painter->drawEllipse( pt2,  mBaseWidth / 5 * 4,  mBaseWidth / 5 * 4 );
  }

  painter->restore();
}
