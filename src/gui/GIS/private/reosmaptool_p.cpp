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

void ReosMapTool_p::keyPressEvent( QKeyEvent *e )
{
  if ( e->key() == Qt::Key_Escape )
    deactivate();
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
  mRubberBand->reset();
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

