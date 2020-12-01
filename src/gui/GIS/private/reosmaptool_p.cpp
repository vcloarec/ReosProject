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

ReosMapTool_p::ReosMapTool_p( QgsMapCanvas *canvas ): QgsMapTool( canvas )
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


ReosMapToolDrawPolyline_p::ReosMapToolDrawPolyline_p( QgsMapCanvas *map ): ReosMapTool_p( map )
{
  mRubberBand = new QgsRubberBand( map );
}

ReosMapToolDrawPolyline_p::~ReosMapToolDrawPolyline_p()
{
  if ( mRubberBand )
    delete mRubberBand.data();
}

void ReosMapToolDrawPolyline_p::deactivate()
{
  if ( mRubberBand )
    mRubberBand->reset();
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
    polyline.removeLast();
    emit polylineDrawn( polyline );
    mRubberBand->reset();
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
  QRectF rectf( p.x(), p.y(), 5, 5 );
  QList<QGraphicsItem *> listItems = canvas()->scene()->items( rectf );

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
      if ( !mTargetDescritpion.isEmpty() && mapItem->base->description() != mTargetDescritpion )
        mapItem = nullptr;
    }

    ++i;
  }

  if ( mapItem )
    emit found( mapItem->base );
}
