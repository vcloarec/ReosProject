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

void ReosMapPolylinesStructure_p::setLineWidth( double width )
{
  mExterior->setWidth( width );
  mLines->setWidth( width );
}

void ReosMapPolylinesStructure_p::updatePosition()
{
  prepareGeometryChange();
  mOriginInView = toCanvasCoordinates( mapPos() );
  const QString &destCrs = crs();

  if ( mExterior )
    mExterior->updatePosition( mStructure, this, destCrs );
  if ( mLines )
    mLines->updatePosition( mStructure, this, destCrs );
  if ( mHolePoints )
    mHolePoints->updatePosition( mStructure, this, destCrs );
}

void ReosMapPolylinesStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  mExterior = new ReosMapStructureExteriorItem( this );
  mLines = new ReosMapStructureLinesItem( this );
  mHolePoints = new ReosMapStructureHolePointsItem( this );

  mHolePoints->setZValue( 30 );
  mExterior->setZValue( 20 );
  mLines->setZValue( 10 );

  updatePosition();
}

void ReosMapPolylinesStructure_p::paint( QPainter *painter )
{}

ReosMapStructureExteriorItem::ReosMapStructureExteriorItem( ReosMapPolylinesStructure_p *parent ): QGraphicsItem( parent )
{
  setZValue( 10 );
}

void ReosMapStructureExteriorItem::updatePosition( const ReosPolylinesStructure *structure, ReosMapPolylinesStructure_p *parent, const QString &destinationCrs )
{
  prepareGeometryChange();
  mPolyInLocalView.clear();
  mIsCondition.clear();
  mIsSelected.clear();
  const QPolygonF boundaryPolygon = structure->boundary();

  mIsCondition.reserve( boundaryPolygon.count() );
  for ( int i = 0; i < boundaryPolygon.count(); ++i )
  {
    const QPointF &pt = boundaryPolygon.at( i );
    QPointF ptCanvas = parent->toCanvasCoordinates( pt );
    QPointF ptLocal = ptCanvas;
    mPolyInLocalView.append( ptLocal );
    QString classId = structure->boundaryClassId( i );
    mIsCondition.append( !classId.isEmpty() );
    mIsSelected.append( classId == structure->selectedClass() );
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

  int polySize = mPolyInLocalView.count();
  for ( int i = 0; i < polySize; ++i )
  {
    if ( mIsCondition.at( i ) )
    {
      QLineF line( mPolyInLocalView.at( i ), mPolyInLocalView.at( ( i + 1 ) % polySize ) );

      pen.setColor( ReosStyleRegistery::instance()->blueReos() );
      pen.setWidthF( mWidth );
      painter->setPen( pen );
      painter->drawLine( line );

      pen.setWidthF( mWidth / 5 * 3 );
      if ( mIsSelected.at( i ) )
        pen.setColor( Qt::red );
      else
        pen.setColor( Qt::gray );
      painter->setPen( pen );

      painter->drawLine( line );
      QBrush brush;
      if ( mIsSelected.at( i ) )
        brush.setColor( Qt::red );
      else
        brush.setColor( Qt::gray );

      brush.setStyle( Qt::SolidPattern );
      pen.setColor( ReosStyleRegistery::instance()->blueReos() );
      pen.setWidth( mWidth / 5 * 3 );
      painter->setBrush( brush );
      painter->setPen( pen );

      painter->drawEllipse( line.p1(),  mWidth / 5 * 4,  mWidth / 5 * 4 );
      painter->drawEllipse( line.p2(),  mWidth / 5 * 4,  mWidth / 5 * 4 );
    }
    else
      continue;

  }

  painter->restore();
}

void ReosMapStructureExteriorItem::setWidth( double width )
{
  mWidth = width;
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
  pen.setWidthF( mWidth );
  painter->setPen( pen );
  for ( const QLineF &line : std::as_const( mLinesInLocalView ) )
    painter->drawLine( line );

  pen.setColor( Qt::gray );
  pen.setWidthF( mWidth / 5 * 2 );
  painter->setPen( pen );
  for ( const QLineF &line : std::as_const( mLinesInLocalView ) )
    painter->drawLine( line );

  QBrush brush;
  brush.setColor( Qt::gray );
  brush.setStyle( Qt::SolidPattern );
  pen.setColor( ReosStyleRegistery::instance()->orangeReos( 100 ) );
  pen.setWidth( mWidth / 5 * 2 );
  painter->setBrush( brush );
  painter->setPen( pen );

  for ( const QLineF &line : std::as_const( mLinesInLocalView ) )
  {
    const QPointF &pt1 = line.p1();
    painter->drawEllipse( pt1,  mWidth / 5 * 4,  mWidth / 5 * 4 );
    const QPointF &pt2 = line.p2();
    painter->drawEllipse( pt2,  mWidth / 5 * 4,  mWidth / 5 * 4 );
  }

  painter->restore();
}

void ReosMapStructureLinesItem::setWidth( double width )
{
  mWidth = width;
}

ReosMapStructureHolePointsItem::ReosMapStructureHolePointsItem( ReosMapPolylinesStructure_p *parent ): QGraphicsItem( parent )
{
  setZValue( 11 );
}

void ReosMapStructureHolePointsItem::updatePosition( const ReosPolylinesStructure *structure, ReosMapPolylinesStructure_p *parent, const QString &destinationCrs )
{
  prepareGeometryChange();
  const QList<QPointF> points = structure->holePoints( destinationCrs );
  mViewPoints.clear();
  mPointValidity.clear();
  mViewPoints.reserve( points.count() );
  mPointValidity.reserve( points.count() );
  QgsRectangle extent;
  extent.setMinimal();

  for ( const QPointF &pt : points )
  {
    const QPointF vp = parent->toCanvasCoordinates( pt ) ;
    mViewPoints.append( vp );
    extent.include( vp );
    mPointValidity.append( !structure->searchPolygon( ReosSpatialPosition( pt, destinationCrs ), false ).isEmpty() );
  }
  mBBox = extent.toRectF().adjusted( - 8, -8, 8, 8 );
}

QRectF ReosMapStructureHolePointsItem::boundingRect() const
{
  return mBBox;
}

void ReosMapStructureHolePointsItem::paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * )
{
  painter->save();

  painter->setRenderHint( QPainter::Antialiasing, true );
  QPen pen;
  QBrush brush;
  brush.setStyle( Qt::SolidPattern );
  pen.setStyle( Qt::NoPen );
  painter->setPen( pen );

  for ( int i = 0; i < mViewPoints.count(); ++i )
  {
    const QPointF &pt = mViewPoints.at( i );

    if ( mPointValidity.at( i ) )
    {
      brush.setColor( ReosStyleRegistery::instance()->orangeReos() );
    }
    else
    {
      brush.setColor( Qt::red );
    }

    painter->setBrush( brush );
    painter->drawEllipse( pt, 8, 8 );
    brush.setColor( Qt::black );
    painter->setBrush( brush );
    painter->drawEllipse( pt, 3, 3 );
  }

  painter->restore();
}
