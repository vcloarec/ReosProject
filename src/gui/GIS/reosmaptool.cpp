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
#include "reosmappolygon_p.h"

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

void ReosMapTool::quitMap()
{
  mMap->setDefaultMapTool();
}

bool ReosMapTool::isCurrentToolInMap() const
{
  return tool_p()->isActive();
}

void ReosMapTool::setAction( QAction *action )
{
  tool_p()->setAction( action );
  connect( action, &QAction::triggered, this, [this]() {setCurrentToolInMap();} );
}

void ReosMapTool::setCursor( const QCursor &cursor )
{
  tool_p()->setCursor( cursor );
}

void ReosMapTool::setContextMenuPopulator( ReosMenuPopulator *populator )
{
  tool_p()->setContextMenuPopulator( populator );
}

ReosMapToolDrawPolyRubberBand::ReosMapToolDrawPolyRubberBand( ReosMap *map, bool closed ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawPolyline_p( canvas, closed );
}

ReosMapToolDrawPolyRubberBand::~ReosMapToolDrawPolyRubberBand()
{
  if ( d )
    d->deleteLater();
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
  QObject( map )
  , mMap( map )
{

}

ReosMapTool::~ReosMapTool()
{}

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

ReosMapToolDrawExtent::~ReosMapToolDrawExtent()
{
  if ( d )
    d->deleteLater();
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

ReosMapToolSelectMapItem::~ReosMapToolSelectMapItem()
{
  if ( d )
    d->deleteLater();
}

void ReosMapToolSelectMapItem::setSearchUnderPoint( bool b )
{
  d->setSearchUnderPoint( b );
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

ReosMapToolDrawPoint::~ReosMapToolDrawPoint()
{
  if ( d )
    d->deleteLater();
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

ReosMapToolNeutral::~ReosMapToolNeutral()
{
  if ( d )
    d->deleteLater();
}

ReosMapTool_p *ReosMapToolNeutral::tool_p() const
{
  return d;
}

ReosMapToolEditMapPolyline::ReosMapToolEditMapPolyline( ReosMap *map ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygon_p( canvas );
  setCursor( QCursor( QPixmap( ":/cursors/moveElement.png" ), 16, 16 ) );
  connect( d, &ReosMapToolEditPolygon_p::polygonEdited, this, &ReosMapToolEditMapPolyline::polylineEdited );
}

void ReosMapToolEditMapPolyline::setMapPolyline( ReosMapPolyline *polyline )
{
  d->setMapPolygon( static_cast<ReosMapPolyline_p *>( polyline->graphicItem() ) );
}

ReosMapTool_p *ReosMapToolEditMapPolyline::tool_p() const
{
  return d;
}

ReosMapToolEditMapPolygon::ReosMapToolEditMapPolygon( ReosMap *map ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygon_p( canvas );
  setCursor( QCursor( QPixmap( ":/cursors/moveElement.png" ), 16, 16 ) );
  connect( d, &ReosMapToolEditPolygon_p::polygonEdited, this, &ReosMapToolEditMapPolygon::polygonEdited );
}

void ReosMapToolEditMapPolygon::setMapPolygon( ReosMapPolygon *polygon )
{
  if ( polygon )
    d->setMapPolygon( static_cast<ReosMapPolygon_p *>( polygon->graphicItem() ) );
  else
    d->setMapPolygon( nullptr );
}

ReosMapTool_p *ReosMapToolEditMapPolygon::tool_p() const
{
  return d;
}
