/***************************************************************************
                      reosmaptool.cpp
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

#include "reosmaptool.h"

#include <qgsmapcanvas.h>
#include <QObject>

#include "reosmaptool_p.h"

ReosMapToolDrawPolyline::ReosMapToolDrawPolyline( ReosMap *map ): ReosMapToolDrawPolyRubberBand( map, false )
{
  d->setCursor( QCursor( QPixmap( ":/cursors/linearDrawing.png" ), 3, 3 ) );
  connect( d, &ReosMapToolDrawPolyline_p::polylineDrawn, this, &ReosMapToolDrawPolyline::drawn );
}


ReosMapToolDrawPolygon::ReosMapToolDrawPolygon( ReosMap *map ): ReosMapToolDrawPolyRubberBand( map, true )
{
  d->setCursor( QCursor( QPixmap( ":/cursors/linearDrawing.png" ), 3, 3 ) );
  connect( d, &ReosMapToolDrawPolyline_p::polylineDrawn, this, &ReosMapToolDrawPolygon::drawn );
}

void ReosMapToolDrawPolygon::setFillColor( const QColor &color )
{
  d->mRubberBand->setFillColor( color );
}

void ReosMapTool::setCurrentToolInMap() const
{
  tool_p()->canvas()->setMapTool( tool_p() );
}

void ReosMapTool::setAction( QAction *action )
{
  tool_p()->setAction( action );
  connect( action, &QAction::triggered, [this]() {setCurrentToolInMap();} );
}

ReosMapToolDrawPolyRubberBand::ReosMapToolDrawPolyRubberBand( ReosMap *map, bool closed ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawPolyline_p( canvas, closed );
}

void ReosMapToolDrawPolyRubberBand::setStrokeWidth( double width )
{
  d->mRubberBand->setWidth( width );
}

void ReosMapToolDrawPolyRubberBand::setColor( const QColor &color )
{
  d->mRubberBand->setColor( color );
}

void ReosMapToolDrawPolyRubberBand::setSecondaryStrokeColor( const QColor &color )
{
  d->mRubberBand->setSecondaryStrokeColor( color );
}

void ReosMapToolDrawPolyRubberBand::setLineStyle( Qt::PenStyle style )
{
  d->mRubberBand->setLineStyle( style );
}

ReosMapTool_p *ReosMapToolDrawPolyRubberBand::tool_p() const
{
  return d;
}


ReosMapTool::ReosMapTool( ReosMap *map ):
  mMap( map )
{

}

void ReosMapTool::activate()
{
  tool_p()->activate();
}

void ReosMapTool::deactivate()
{
  tool_p()->deactivate();
}

ReosMapToolDrawExtent::ReosMapToolDrawExtent( ReosMap *map ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawExtent_p( canvas );
  d->setCursor( QCursor( QPixmap( ":/cursors/rectangularDrawing.png" ), 3, 3 ) );
  connect( d, &ReosMapToolDrawExtent_p::extentDrawn, this, &ReosMapToolDrawExtent::extentDrawn );
}

ReosMapTool_p *ReosMapToolDrawExtent::tool_p() const
{
  return d;
}

void ReosMapToolDrawExtent::setStrokeWidth( double width )
{
  d->mRubberBand->setWidth( width );
}

void ReosMapToolDrawExtent::setColor( const QColor &color )
{
  d->mRubberBand->setColor( color );
}

void ReosMapToolDrawExtent::setSecondaryStrokeColor( const QColor &color )
{
  d->mRubberBand->setSecondaryStrokeColor( color );
}

void ReosMapToolDrawExtent::setFillColor( const QColor &color )
{
  d->mRubberBand->setFillColor( color );
}

void ReosMapToolDrawExtent::setLineStyle( Qt::PenStyle style )
{
  d->mRubberBand->setLineStyle( style );
}

ReosMapToolSelectMapItem::ReosMapToolSelectMapItem( ReosMap *map, int targetType ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolSelectMapItem_p( canvas, targetType );
  d->setCursor( QCursor( QPixmap( ":/cursors/removeItem.png" ), 3, 3 ) );
  connect( d, &ReosMapToolSelectMapItem_p::found, this, &ReosMapToolSelectMapItem::found );
}

ReosMapToolSelectMapItem::ReosMapToolSelectMapItem( ReosMap *map, const QString &targetDescription ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolSelectMapItem_p( canvas, targetDescription );
  d->setCursor( QCursor( QPixmap( ":/cursors/removeItem.png" ), 3, 3 ) );
  connect( d, &ReosMapToolSelectMapItem_p::found, this, &ReosMapToolSelectMapItem::found );
}

ReosMapTool_p *ReosMapToolSelectMapItem::tool_p() const
{
  return d;
}

ReosMapToolDrawPoint::ReosMapToolDrawPoint( ReosMap *map ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawPoint_p( canvas );
  connect( d, &ReosMapToolDrawPoint_p::pointDrawn, this, &ReosMapToolDrawPoint::drawn );
}

ReosMapTool_p *ReosMapToolDrawPoint::tool_p() const
{
  return d;
}

ReosMapToolNeutral::ReosMapToolNeutral( ReosMap *map ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapTool_p( canvas );
}

ReosMapTool_p *ReosMapToolNeutral::tool_p() const
{
  return d;
}
