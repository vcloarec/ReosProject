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

ReosMapToolEditPolyline_p::ReosMapToolEditPolyline_p( QgsMapCanvas *map ):
  ReosMapTool_p( map )
{
}

void ReosMapToolEditPolyline_p::setMapPolyline( ReosMapPolyline_p *polyline )
{
  if ( mPolyline )
    mPolyline->setEditing( false );

  mPolyline = polyline;

  if ( mPolyline )
    mPolyline->setEditing( isActive() );
}

void ReosMapToolEditPolyline_p::activate()
{
  if ( mPolyline )
    mPolyline->setEditing( true );

  ReosMapTool_p::activate();
}

void ReosMapToolEditPolyline_p::deactivate()
{
  if ( mPolyline )
    mPolyline->setEditing( false );

  ReosMapTool_p::deactivate();
}

bool ReosMapToolEditPolyline_p::populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event )
{
  if ( !menu )
    return false;

  // remove QQIS default menu
  menu->clear();

  const QPointF mapPoint = event->mapPoint().toQPointF();
  menu->addAction( tr( "Insert vertex" ), [mapPoint, this]
  {
    int index = ReosGeometryUtils::closestSegment( mapPoint, mPolyline->mapPolygon );
    if ( index != -1 )
    {
      mPolyline->mapPolygon.insert( index, mapPoint );
      mPolyline->updatePosition();
      emit this->polylineEdited();
    }

  } );


  int existingVertex = mPolyline->findVertexInView( viewSearchZone( event->pos() ) );
  if ( existingVertex >= 0 )
  {
    menu->addAction( tr( "Remove vertex" ), [existingVertex, this]
    {
      mPolyline->mapPolygon.removeAt( existingVertex );
      mPolyline->updatePosition();
      emit this->polylineEdited();
    } );
  }

  return true;
}

void ReosMapToolEditPolyline_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  if ( !mPolyline )
    return;

  mMovingVertex = mPolyline->findVertexInView( viewSearchZone( e->pos() ) );
}

void ReosMapToolEditPolyline_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( mMovingVertex < 0 || !mPolyline )
    return;
  mIsEdited = true;
  mPolyline->mapPolygon[mMovingVertex] = e->mapPoint().toQPointF();
  mPolyline->updatePosition();

}

void ReosMapToolEditPolyline_p::canvasReleaseEvent( QgsMapMouseEvent * )
{
  if ( mIsEdited )
    emit polylineEdited();
  mIsEdited = false;
  mMovingVertex = -1;
}

QgsRectangle ReosMapToolEditPolyline_p::mapSearchZone( const QPoint &pt )
{
  QPoint zone( mSearchZone.width(), mSearchZone.height() );
  return QgsRectangle( toMapCoordinates( pt ), toMapCoordinates( pt + zone ) );
}

QRectF ReosMapToolEditPolyline_p::viewSearchZone( const QPoint &pt )
{
  QPoint zone( mSearchZone.width() / 2, mSearchZone.height() / 2 );
  return QRectF( QPointF( pt - zone ),  QPointF( pt + zone ) );
}
