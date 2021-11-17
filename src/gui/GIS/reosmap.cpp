/***************************************************************************
                      reosmap.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <QDockWidget>
#include <QLayout>
#include <QMenu>
#include <qgsmapcanvas.h>
#include <qgslayertreemapcanvasbridge.h>
#include <qgslayertreemodel.h>
#include <qgstemporalcontrollerwidget.h>

#include "reosmap.h"
#include "reosgisengine.h"
#include "reosmaptool.h"
#include "reosparameter.h"


ReosMap::ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget ):
  ReosModule( gisEngine )
  , mEngine( gisEngine )
  , mCanvas( new QgsMapCanvas( parentWidget ) )
  , mActionNeutral( new QAction( QPixmap( QStringLiteral( ":/images/neutral.svg" ) ), tr( "Deactivate Tool" ), this ) )
  , mDefaultMapTool( new ReosMapToolNeutral( this ) )
  , mActionZoom( new QAction( QPixmap( QStringLiteral( ":/images/zoomInExtent.svg" ) ), tr( "Zoom In" ), this ) )
  , mZoomMapTool( new ReosMapToolDrawExtent( this ) )
  , mActionZoomIn( new QAction( QPixmap( QStringLiteral( ":/images/zoomIn.svg" ) ), tr( "Zoom In" ), this ) )
  , mActionZoomOut( new QAction( QPixmap( QStringLiteral( ":/images/zoomOut.svg" ) ), tr( "Zoom Out" ), this ) )
  , mActionPreviousZoom( new QAction( QPixmap( QStringLiteral( ":/images/zoomPrevious.svg" ) ), tr( "Previous Zoom" ), this ) )
  , mActionNextZoom( new QAction( QPixmap( QStringLiteral( ":/images/zoomNext.svg" ) ), tr( "Next Zoom" ), this ) )
  , mTemporalControllerAction( new QAction( QPixmap( QStringLiteral( ":/images/temporal.svg" ) ), tr( "Temporal controller" ), this ) )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setExtent( QgsRectangle( 0, 0, 200, 200 ) );
  canvas->setObjectName( "map canvas" );

  connect( canvas, &QgsMapCanvas::extentsChanged, this, &ReosMap::extentChanged );

  QgsLayerTreeModel *layerTreeModel = qobject_cast<QgsLayerTreeModel *>( gisEngine->layerTreeModel() );
  if ( layerTreeModel )
  {
    auto bridge = new QgsLayerTreeMapCanvasBridge( layerTreeModel->rootGroup(), canvas, this );
    bridge->setAutoSetupOnFirstLayer( true );

    //Thses two following connections seem weird, but that is for avoiding that the QgsProjectInstance call the lambda while this is destroyed
    //connect( QgsProject::instance(), &QgsProject::readProject, this, &ReosMap::readProject );

    //connect( this, &ReosMap::readProject, [this, bridge]( const QDomDocument & doc )
    connect( QgsProject::instance(), &QgsProject::readProject, [this, bridge]( const QDomDocument & doc )
    {
      bool autoSetupOnFirstLayer = bridge->autoSetupOnFirstLayer();
      bridge->setAutoSetupOnFirstLayer( false );
      bridge->setCanvasLayers();
      if ( autoSetupOnFirstLayer )
        bridge->setAutoSetupOnFirstLayer( true );
      QgsMapCanvas *c = qobject_cast<QgsMapCanvas *>( mCanvas );
      c->readProject( doc );
      c->setDestinationCrs( QgsProject::instance()->crs() );
      emit crsChanged( mapCrs() );
    } );
  }

  connect( canvas, &QgsMapCanvas::xyCoordinates, this, [this]( const QgsPointXY & p )
  {
    emit cursorMoved( p.toQPointF() );
  } );

  connect( gisEngine, &ReosGisEngine::crsChanged, this, &ReosMap::setCrs );

  mDefaultMapTool->setAction( mActionNeutral );
  mDefaultMapTool->setCurrentToolInMap();

  mZoomMapTool->setAction( mActionZoom );
  mZoomMapTool->setColor( QColor( 9, 150, 230 ) );
  mZoomMapTool->setFillColor( QColor( 9, 150, 230, 20 ) );
  mZoomMapTool->setStrokeWidth( 1 );
  mZoomMapTool->setLineStyle( Qt::DotLine );
  mZoomMapTool->setCursor( QCursor( QPixmap( ":/cursors/zoomInExtent.svg" ), 5, 5 ) );
  mActionZoom->setCheckable( true );

  connect( mZoomMapTool, &ReosMapToolDrawExtent::extentDrawn, this, [this]( const QRectF & extent )
  {
    this->setExtent( extent );
  } );

  connect( mActionZoomIn, &QAction::triggered, this, [canvas]
  {
    canvas->zoomByFactor( 0.5 );
  } );

  connect( mActionZoomOut, &QAction::triggered, this, [canvas]
  {
    canvas->zoomByFactor( 2 );
  } );


  connect( mActionPreviousZoom, &QAction::triggered, canvas, &QgsMapCanvas::zoomToPreviousExtent );
  connect( mActionNextZoom, &QAction::triggered, canvas, &QgsMapCanvas::zoomToNextExtent );
  connect( canvas, &QgsMapCanvas::zoomLastStatusChanged, mActionPreviousZoom, &QAction::setEnabled );
  connect( canvas, &QgsMapCanvas::zoomNextStatusChanged, mActionNextZoom, &QAction::setEnabled );

  mActionPreviousZoom->setEnabled( false );
  mActionNextZoom->setEnabled( false );
  emit crsChanged( mapCrs() );

  mTemporalDockWidget = new QDockWidget( tr( "Temporal controller" ), canvas );
  QgsTemporalControllerWidget *temporalControlerWidget = new QgsTemporalControllerWidget( mTemporalDockWidget );
  canvas->setTemporalController( temporalControlerWidget->temporalController() );
  mTemporalDockWidget->setWidget( temporalControlerWidget );
  mTemporalControllerAction->setCheckable( true );
  connect( mTemporalControllerAction, &QAction::triggered, this, [this]
  {
    mTemporalDockWidget->setVisible( mTemporalControllerAction->isChecked() );
  } );
  mTemporalDockWidget->hide();
}

ReosMap::~ReosMap()
{
  if ( mCanvas )
    mCanvas->scene()->clear();
}

QWidget *ReosMap::mapCanvas() const
{
  if ( mCanvas )
    return mCanvas;
  else
    return nullptr;
}

void ReosMap::refreshCanvas()
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->refresh();
}

ReosGisEngine *ReosMap::engine() const
{
  return mEngine;
}

QString ReosMap::mapCrs() const
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  return canvas->mapSettings().destinationCrs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}

void ReosMap::setDefaultMapTool()
{
  if ( mDefaultMapTool )
    mDefaultMapTool->setCurrentToolInMap();
}

void ReosMap::setExtent( const ReosMapExtent &extent )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setExtent( QgsRectangle( extent.xMapMin(), extent.yMapMin(), extent.xMapMax(), extent.yMapMax() ) );
  canvas->refresh();
}

void ReosMap::setCenter( const QPointF &center )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setCenter( center );
  canvas->refresh();
}

void ReosMap::setCenter( const ReosSpatialPosition &center )
{
  if ( mEngine && center.isValid() )
    setCenter( mEngine->transformToProjectCoordinates( center ) ) ;
}

ReosMapExtent ReosMap::extent() const
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  QgsRectangle extent = canvas->extent();
  return ReosMapExtent( extent.toRectF() );
}

QList<QAction *> ReosMap::mapToolActions()
{
  QList<QAction *> ret;
  ret << mActionNeutral;
  ret << mActionZoom;
  ret << mActionZoomIn;
  ret << mActionZoomOut;
  ret << mActionPreviousZoom;
  ret << mActionNextZoom;
  ret << mTemporalControllerAction;

  return ret;
}

QDockWidget *ReosMap::temporalControllerDockWidget()
{
  return mTemporalDockWidget;
}

void ReosMap::setCrs( const QString &crsWkt )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setDestinationCrs( QgsCoordinateReferenceSystem::fromWkt( crsWkt ) );
  emit crsChanged( crsWkt );
}

ReosMapCursorPosition::ReosMapCursorPosition( ReosMap *map, QWidget *parent ):
  QWidget( parent )
  , mCoordinates( new QLabel( this ) )
  , mCrs( new QLabel( this ) )
{
  setLayout( new QHBoxLayout );
  layout()->addWidget( mCoordinates );
  QFrame *line = new QFrame( this );
  line->setFrameShape( QFrame::VLine );
  line->setFrameShadow( QFrame::Sunken );
  layout()->addWidget( line );
  layout()->addWidget( mCrs );

  QRect rect( 0, 0, 150, 15 );
  mCoordinates->setGeometry( rect );

  connect( map, &ReosMap::cursorMoved, this, &ReosMapCursorPosition::setPosition );
  connect( map, &ReosMap::crsChanged, this, &ReosMapCursorPosition::setCrs );

  setCrs( map->mapCrs() );
}

ReosMapCursorPosition::~ReosMapCursorPosition()
{
}

void ReosMapCursorPosition::setPosition( const  QPointF &p )
{
  QString position = tr( "Map Coordinate : " );
  position.append( ReosParameter::doubleToString( p.x(), 2 ) );
  position.append( " : " );
  position.append( ReosParameter::doubleToString( p.y(), 2 ) );
  position.append( "  " );
  mCoordinates->setText( position );
}

void ReosMapCursorPosition::setCrs( const QString &crs )
{
  QgsCoordinateReferenceSystem qgsCrs = QgsCoordinateReferenceSystem::fromWkt( crs );
  QString str = qgsCrs.authid();
  mCrs->setText( qgsCrs.authid() );

}
