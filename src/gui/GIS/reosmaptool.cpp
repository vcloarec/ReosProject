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
#include "reosapplication.h"

ReosMapToolDrawPolyline::ReosMapToolDrawPolyline( ReosMap *map ): ReosMapToolDrawPolyline( map, map )
{
}

ReosMapToolDrawPolyline::ReosMapToolDrawPolyline( QObject *parent, ReosMap *map ): ReosMapToolDrawPolyRubberBand( parent, map, false )
{
  d->setCursor( QCursor( QPixmap( ":/cursors/linearDrawing.png" ), 3, 3 ) );
  connect( d, &ReosMapToolDrawPolyline_p::polylineDrawn, this, &ReosMapToolDrawPolyline::drawn );

  setUp();
}


ReosMapToolDrawPolygon::ReosMapToolDrawPolygon( ReosMap *map ): ReosMapToolDrawPolygon( map, map )
{}

ReosMapToolDrawPolygon::ReosMapToolDrawPolygon( QObject *parent, ReosMap *map ): ReosMapToolDrawPolyRubberBand( parent, map, true )
{
  d->setCursor( QCursor( QPixmap( ":/cursors/linearDrawing.png" ), 3, 3 ) );
  connect( d, &ReosMapToolDrawPolyline_p::polylineDrawn, this, &ReosMapToolDrawPolygon::drawn );

  setUp();
}

void ReosMapToolDrawPolygon::setFillColor( const QColor &color )
{
  d->mRubberBand->setFillColor( color );
  d->setFillColor( color );
}

void ReosMapTool::setCurrentToolInMap() const
{
  tool_p()->canvas()->setMapTool( tool_p() );
  ReosApplication::setActiveWindow( mMap->mapCanvas() );
}

void ReosMapTool::quitMap()
{
  if ( !mMap.isNull() && this->isActive() )
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

void ReosMapTool::setSearchingItemDecription( const QString &description )
{
  tool_p()->setSearchTargetDescription( description );
}

void ReosMapTool::activateMovingSignal( bool activate )
{
  tool_p()->setActivateMovingSignal( activate );
}

void ReosMapTool::setSearchItemWhenMoving( bool b )
{
  tool_p()->setSeachWhenMoving( b );
}

ReosMapToolDrawPolyRubberBand::ReosMapToolDrawPolyRubberBand( QObject *parent, ReosMap *map, bool closed ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawPolyline_p( canvas, closed );

  setUp();
}

ReosMapToolDrawPolyRubberBand::ReosMapToolDrawPolyRubberBand( ReosMap *map, bool closed ): ReosMapToolDrawPolyRubberBand( map, map, closed )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawPolyline_p( canvas, closed );

  setUp();
}

ReosMapToolDrawPolyRubberBand::~ReosMapToolDrawPolyRubberBand()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolDrawPolyRubberBand::setStrokeWidth( double width )
{
  d->mRubberBand->setWidth( width );
}

void ReosMapToolDrawPolyRubberBand::setColor( const QColor &color )
{
  d->setColor( color );
}

void ReosMapToolDrawPolyRubberBand::setSecondaryStrokeColor( const QColor &color )
{
  d->mRubberBand->setSecondaryStrokeColor( color );
}

void ReosMapToolDrawPolyRubberBand::setLineStyle( Qt::PenStyle style )
{
  d->mRubberBand->setLineStyle( style );
}

void ReosMapToolDrawPolyRubberBand::setAllowSelfIntersect( bool b )
{
  d->setAllowSelfIntersect( b );
}

ReosMapTool_p *ReosMapToolDrawPolyRubberBand::tool_p() const
{
  return d;
}

ReosMapTool::ReosMapTool( QObject *parent, ReosMap *map )
  : QObject( parent )
  , mMap( map )
{}

void ReosMapTool::setUp()
{
  connect( tool_p(), &ReosMapTool_p::keyPressed, this, &ReosMapTool::keyPressed );
  connect( tool_p(), &ReosMapTool_p::activated, this, &ReosMapTool::activated );
  connect( tool_p(), &ReosMapTool_p::deactivated, this, &ReosMapTool::deactivated );
  connect( tool_p(), &ReosMapTool_p::move, this, &ReosMapTool::move );
}

ReosMap *ReosMapTool::map() const
{
  return mMap;
}

void ReosMapTool::keyPressed( int key )
{
  if ( key == Qt::Key_Escape )
    quitMap();
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

bool ReosMapTool::isActive() const
{
  return tool_p()->isActive();
}

void ReosMapTool::enableSnapping( bool enable )
{
  tool_p()->enableSnapping( enable );
}

QString ReosMapTool::crs() const
{
  return map()->mapCrs();
}


ReosMapToolDrawExtent::ReosMapToolDrawExtent( ReosMap *map ): ReosMapToolDrawExtent( map, map )
{}

ReosMapToolDrawExtent::ReosMapToolDrawExtent( QObject *parent, ReosMap *map ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawExtent_p( canvas );
  d->setCursor( QCursor( QPixmap( ":/cursors/rectangularDrawing.png" ), 3, 3 ) );
  connect( d, &ReosMapToolDrawExtent_p::extentDrawn, this, &ReosMapToolDrawExtent::extentDrawn );

  setUp();
}

ReosMapToolDrawExtent::~ReosMapToolDrawExtent()
{
  if ( !d.isNull() )
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

ReosMapToolSelectMapItem::ReosMapToolSelectMapItem( ReosMap *map, const QString &targetDescription ): ReosMapToolSelectMapItem( map, map, targetDescription )
{}

ReosMapToolSelectMapItem::ReosMapToolSelectMapItem( QObject *parent, ReosMap *map, const QString &targetDescription ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolSelectMapItem_p( canvas, targetDescription );
  d->setCursor( QCursor( QPixmap( ":/cursors/removeItem.png" ), 3, 3 ) );
  connect( d, &ReosMapToolSelectMapItem_p::found, this, &ReosMapToolSelectMapItem::found );

  setUp();
}

ReosMapToolSelectMapItem::~ReosMapToolSelectMapItem()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolSelectMapItem::setSearchUnderPoint( bool b )
{
  d->setSearchUnderPoint( b );
}

void ReosMapToolSelectMapItem::clearHoveredItem()
{
  d->clearHoveredItem();
}

ReosMapTool_p *ReosMapToolSelectMapItem::tool_p() const
{
  return d;
}

ReosMapToolDrawPoint::ReosMapToolDrawPoint( QObject *parent, ReosMap *map ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawPoint_p( canvas );
  connect( d, &ReosMapToolDrawPoint_p::pointDrawn, this, &ReosMapToolDrawPoint::drawn );

  setUp();
}

ReosMapToolDrawPoint::ReosMapToolDrawPoint( ReosMap *map ): ReosMapToolDrawPoint( map, map )
{}

ReosMapToolDrawPoint::~ReosMapToolDrawPoint()
{
  if ( !d.isNull() )
    d->deleteLater();
}

ReosMapTool_p *ReosMapToolDrawPoint::tool_p() const
{
  return d;
}

ReosMapToolNeutral::ReosMapToolNeutral( ReosMap *map ): ReosMapToolNeutral( map, map )
{
}

ReosMapToolNeutral::ReosMapToolNeutral( QObject *parent, ReosMap *map ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapTool_p( canvas );
  setUp();
}

ReosMapToolNeutral::~ReosMapToolNeutral()
{
  if ( !d.isNull() )
    d->deleteLater();
}

ReosMapTool_p *ReosMapToolNeutral::tool_p() const
{
  return d;
}

ReosMapToolEditMapPolyline::ReosMapToolEditMapPolyline( ReosMap *map ): ReosMapToolEditMapPolyline( map, map )
{}

ReosMapToolEditMapPolyline::ReosMapToolEditMapPolyline( QObject *parent, ReosMap *map ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygon_p( canvas );
  setCursor( QCursor( QPixmap( ":/cursors/moveElement.png" ), 16, 16 ) );
  connect( d, &ReosMapToolEditPolygon_p::polygonEdited, this, &ReosMapToolEditMapPolyline::polylineEdited );

  setUp();
}

ReosMapToolEditMapPolyline::~ReosMapToolEditMapPolyline()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolEditMapPolyline::setMapPolyline( ReosMapPolyline *polyline )
{
  if ( polyline )
    d->setMapPolygon( static_cast<ReosMapPolyline_p *>( polyline->graphicItem() ) );
  else
  {
    d->setMapPolygon( nullptr );
    quitMap();
  }
}

ReosMapTool_p *ReosMapToolEditMapPolyline::tool_p() const
{
  return d;
}

ReosMapToolEditMapPolygon::ReosMapToolEditMapPolygon( ReosMap *map ): ReosMapToolEditMapPolygon( map, map )
{}

ReosMapToolEditMapPolygon::ReosMapToolEditMapPolygon( QObject *parent, ReosMap *map ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygon_p( canvas );
  setCursor( QCursor( QPixmap( ":/cursors/moveElement.png" ), 16, 16 ) );
  connect( d, &ReosMapToolEditPolygon_p::polygonEdited, this, &ReosMapToolEditMapPolygon::polygonEdited );

  setUp();
}

ReosMapToolEditMapPolygon::~ReosMapToolEditMapPolygon()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolEditMapPolygon::setMapPolygon( ReosMapPolygon *polygon )
{
  if ( polygon )
    d->setMapPolygon( static_cast<ReosMapPolygon_p *>( polygon->graphicItem() ) );
  else
  {
    d->setMapPolygon( nullptr );
    quitMap();
  }
}

ReosMapTool_p *ReosMapToolEditMapPolygon::tool_p() const
{
  return d;
}

ReosMapToolMoveMapItem::ReosMapToolMoveMapItem( ReosMap *map ): ReosMapToolMoveMapItem( map, map )
{}

ReosMapToolMoveMapItem::ReosMapToolMoveMapItem( QObject *parent, ReosMap *map ): ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolMoveItem_p( canvas );
  setCursor( QCursor( QPixmap( ":/cursors/moveElement.png" ), 16, 16 ) );
  connect( d, &ReosMapToolMoveItem_p::itemMoved, this, &ReosMapToolMoveMapItem::itemMoved );

  setUp();
}

ReosMapToolMoveMapItem::~ReosMapToolMoveMapItem()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolMoveMapItem::setCurrentMapItem( ReosMapItem *item )
{
  if ( item )
    d->setCurrentItem( static_cast<ReosMapItem_p *>( item->graphicItem() ) );
  else
  {
    d->setCurrentItem( nullptr );
    quitMap();
  }
}

void ReosMapToolMoveMapItem::setMovingColor( const QColor &movingColor )
{
  d->setMovingColor( movingColor );
}

ReosMapTool_p *ReosMapToolMoveMapItem::tool_p() const
{
  return d;
}
