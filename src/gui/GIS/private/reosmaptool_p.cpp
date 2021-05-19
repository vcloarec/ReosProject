/***************************************************************************
                      reosmaptool_p.cpp
                     --------------------------------------
Date                 : October-2020
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

#include "reosmaptool_p.h"
#include "reosmappolygon_p.h"
#include "reosgeometryutils.h"
#include <QGraphicsScene>
#include <QMenu>

ReosMapTool_p::ReosMapTool_p( QgsMapCanvas *canvas ):
  QgsMapTool( canvas ), mContextMenuPopulator( new ReosMenuPopulator )
{

}

void ReosMapTool_p::activate()
{
  QgsMapTool::activate();
}

void ReosMapTool_p::deactivate()
{
  QgsMapTool::deactivate();
}


bool ReosMapTool_p::populateContextMenuWithEvent( QMenu *menu,  QgsMapMouseEvent * )
{
  if ( mContextMenuPopulator )
  {
    mContextMenuPopulator->populate( menu );
    return true;
  }
  return false;
}

void ReosMapTool_p::setContextMenuPopulator( ReosMenuPopulator *populator )
{
  mContextMenuPopulator.reset( populator );
}


ReosMapToolDrawPolyline_p::ReosMapToolDrawPolyline_p( QgsMapCanvas *map, bool closed ):
  ReosMapTool_p( map ),
  mClosed( closed )
{
  mRubberBand = new QgsRubberBand( map, closed ? QgsWkbTypes::PolygonGeometry : QgsWkbTypes::LineGeometry );
}

ReosMapToolDrawPolyline_p::~ReosMapToolDrawPolyline_p()
{
  if ( mRubberBand )
    delete mRubberBand.data();
}

void ReosMapToolDrawPolyline_p::deactivate()
{
  if ( mRubberBand )
    mRubberBand->reset( mClosed ? QgsWkbTypes::PolygonGeometry : QgsWkbTypes::LineGeometry );
  ReosMapTool_p::deactivate();
}

void ReosMapToolDrawPolyline_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  mRubberBand->movePoint( e->mapPoint() );
}

void ReosMapToolDrawPolyline_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( e->button() == Qt::LeftButton )
    mRubberBand->addPoint( e->mapPoint() );

  if ( e->button() == Qt::RightButton )
  {
    QPolygonF polyline = mRubberBand->asGeometry().asQPolygonF();
    if ( !polyline.isEmpty() )
      polyline.removeLast();
    emit polylineDrawn( polyline );
    mRubberBand->reset( mClosed ? QgsWkbTypes::PolygonGeometry : QgsWkbTypes::LineGeometry );
  }
}


ReosMapToolDrawExtent_p::ReosMapToolDrawExtent_p( QgsMapCanvas *map ): ReosMapTool_p( map )
{
  mRubberBand = new QgsRubberBand( map, QgsWkbTypes::PolygonGeometry );
}

ReosMapToolDrawExtent_p::~ReosMapToolDrawExtent_p()
{
  if ( mRubberBand )
    delete mRubberBand.data();
}

void ReosMapToolDrawExtent_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( !mIsDrawing )
    return;

  mEndPoint = e->mapPoint();
  drawExtent();
}

void ReosMapToolDrawExtent_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  mIsDrawing = true;
  mStartPoint = e->mapPoint();
}

void ReosMapToolDrawExtent_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  mIsDrawing = false;
  mEndPoint = e->mapPoint();
  mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  QRectF extent( mStartPoint.toQPointF(), mEndPoint.toQPointF() );
  emit extentDrawn( extent );
}

void ReosMapToolDrawExtent_p::deactivate()
{
  if ( mRubberBand )
    mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  mIsDrawing = false;
  ReosMapTool_p::deactivate();
}

void ReosMapToolDrawExtent_p::drawExtent()
{
  QgsRectangle rect( mStartPoint, mEndPoint );

  mRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  mRubberBand->addPoint( QgsPointXY( rect.xMinimum(), rect.yMinimum() ), false );
  mRubberBand->addPoint( QgsPointXY( rect.xMaximum(), rect.yMinimum() ), false );
  mRubberBand->addPoint( QgsPointXY( rect.xMaximum(), rect.yMaximum() ), false );
  mRubberBand->addPoint( QgsPointXY( rect.xMinimum(), rect.yMaximum() ), true );
}

ReosMapToolSelectMapItem_p::ReosMapToolSelectMapItem_p( QgsMapCanvas *map, int targetType ):
  ReosMapTool_p( map ),
  mTargetType( targetType )
{}

ReosMapToolSelectMapItem_p::ReosMapToolSelectMapItem_p( QgsMapCanvas *map, const QString &targetDescription ):
  ReosMapToolSelectMapItem_p( map )
{
  mTargetDescritpion = targetDescription;
}

void ReosMapToolSelectMapItem_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  QPointF p = e->localPos();

  QList<QGraphicsItem *> listItems;
  if ( mUnderPoint )
    listItems  = canvas()->scene()->items( p );
  else
  {
    QRectF rectf( p.x(), p.y(), 5, 5 );
    listItems  = canvas()->scene()->items( rectf );
  }

  QGraphicsItem *item = nullptr;
  ReosMapItem_p *mapItem = nullptr;
  int i = 0;
  while ( !( mapItem ) && i < listItems.count() )
  {

    item = listItems.at( i );
    if ( ( item && item->type() == mTargetType ) || mTargetType == -1 )
    {
      item = listItems.at( i );
      mapItem = dynamic_cast<ReosMapItem_p *>( item );
      if ( mapItem && !mTargetDescritpion.isEmpty() && mapItem->base->description() != mTargetDescritpion )
        mapItem = nullptr;
    }

    ++i;
  }

  if ( mapItem )
    emit found( mapItem->base, e->mapPoint().toQPointF() );
  else
    emit found( nullptr, e->mapPoint().toQPointF() );

}

bool ReosMapToolSelectMapItem_p::populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event )
{
  canvasReleaseEvent( event );
  return ReosMapTool_p::populateContextMenuWithEvent( menu, event );
}

void ReosMapToolSelectMapItem_p::setSearchUnderPoint( bool underPoint )
{
  mUnderPoint = underPoint;
}

ReosMapToolDrawPoint_p::ReosMapToolDrawPoint_p( QgsMapCanvas *map ): ReosMapTool_p( map )
{
}

ReosMapToolDrawPoint_p::~ReosMapToolDrawPoint_p() {}

void ReosMapToolDrawPoint_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( e->button() == Qt::LeftButton )
    emit pointDrawn( e->mapPoint().toQPointF() );
}

ReosMapToolEditPolygon_p::ReosMapToolEditPolygon_p( QgsMapCanvas *map ):
  ReosMapTool_p( map )
{
}

void ReosMapToolEditPolygon_p::setMapPolygon( ReosMapPolygon_p *polygon )
{
  if ( mPolygon )
    mPolygon->setEditing( false );

  mPolygon = polygon;

  if ( mPolygon )
    mPolygon->setEditing( isActive() );
}

void ReosMapToolEditPolygon_p::activate()
{
  if ( mPolygon )
    mPolygon->setEditing( true );

  ReosMapTool_p::activate();
}

void ReosMapToolEditPolygon_p::deactivate()
{
  if ( mPolygon )
    mPolygon->setEditing( false );

  ReosMapTool_p::deactivate();
}

bool ReosMapToolEditPolygon_p::populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event )
{
  if ( !menu )
    return false;

  // remove QQIS default menu
  menu->clear();

  const QPointF mapPoint = event->mapPoint().toQPointF();
  menu->addAction( tr( "Insert vertex" ), this, [mapPoint, this]
  {
    int index = ReosGeometryUtils::closestSegment( mapPoint, mPolygon->mapPolygon );
    if ( index != -1 )
    {
      mPolygon->mapPolygon.insert( index, mapPoint );
      mPolygon->updatePosition();
      emit this->polygonEdited();
    }

  } );

  int existingVertex = mPolygon->findVertexInView( viewSearchZone( event->pos() ) );
  if ( existingVertex >= 0 )
  {
    menu->addAction( tr( "Remove vertex" ), [existingVertex, this]
    {
      mPolygon->mapPolygon.removeAt( existingVertex );
      mPolygon->updatePosition();
      emit this->polygonEdited();
    } );
  }

  return true;
}

void ReosMapToolEditPolygon_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  if ( !mPolygon )
    return;

  mMovingVertex = mPolygon->findVertexInView( viewSearchZone( e->pos() ) );
}

void ReosMapToolEditPolygon_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( mMovingVertex < 0 || !mPolygon )
    return;
  mIsEdited = true;
  mPolygon->mapPolygon[mMovingVertex] = e->mapPoint().toQPointF();
  mPolygon->updatePosition();

}

void ReosMapToolEditPolygon_p::canvasReleaseEvent( QgsMapMouseEvent * )
{
  if ( mIsEdited )
    emit polygonEdited();
  mIsEdited = false;
  mMovingVertex = -1;
}


QRectF ReosMapTool_p::viewSearchZone( const QPoint &pt )
{
  QPoint zone( mSearchZone.width() / 2, mSearchZone.height() / 2 );
  return QRectF( QPointF( pt - zone ),  QPointF( pt + zone ) );
}


ReosMapToolMoveItem_p::ReosMapToolMoveItem_p( QgsMapCanvas *map ): ReosMapTool_p( map )
{

}

void ReosMapToolMoveItem_p::setCurrentItem( ReosMapItem_p *item )
{
  if ( item )
    item->setEditing( false );

  mCurrentItem = item;

  if ( item )
    item->setEditing( isActive() );
}

void ReosMapToolMoveItem_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  if ( !mCurrentItem || !searchItem( e->pos() ) )
    return;

  mMovingItem.reset( mCurrentItem->clone() );
  mMovingItem->color = mMovingColor;
  mMovingItem->externalColor = mMovingColor;
  mStartPoint = e->mapPoint().toQPointF();
  mIsMoving = true;

}

void ReosMapToolMoveItem_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( !mCurrentItem || !mIsMoving || !mMovingItem )
    return;
  QPointF translation = e->mapPoint().toQPointF() - mStartPoint;
  QPointF oldItemTranslation = mMovingItem->mapPos() - mCurrentItem->mapPos();
  QPointF diff = translation - oldItemTranslation;

  mMovingItem->move( diff );
}

void ReosMapToolMoveItem_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( !mMovingItem )
    return;

  mMovingItem.reset();

  if ( mCurrentItem )
  {
    mCurrentItem->move( e->mapPoint().toQPointF() - mStartPoint );
    emit itemMoved( mCurrentItem->base );
  }

  mIsMoving = false;
}

bool ReosMapToolMoveItem_p::searchItem( const QPoint &p )
{
  QList<QGraphicsItem *> listItems;

  listItems  = canvas()->scene()->items( viewSearchZone( p ) );

  for ( QGraphicsItem *item : std::as_const( listItems ) )
    if ( item == mCurrentItem )
      return true;


  return false;

}

void ReosMapToolMoveItem_p::setMovingColor( const QColor &movingColor )
{
  mMovingColor = movingColor;
}
