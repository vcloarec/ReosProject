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
#include <qgsmapcanvassnappingutils.h>
#include <qgssnappingconfig.h>
#include <qgstemporalcontroller.h>

#include "reosmap.h"
#include "reosgisengine.h"
#include "reosmaptool.h"
#include "reosparameter.h"
#include "reospolylinesstructure.h"

#include "reosmesh.h"


class ReosRendererObjectHandler_p
{

  public:
    ReosRendererObjectHandler_p( QgsMapCanvas *canvas )
      : mCanvas( canvas )
    {}

    struct CacheRendering
    {
      QImage image;
      QgsRectangle extent;
      QgsMapToPixel mapToPixel;
      QgsDateTimeRange timeRange;
      bool obsolete = false;
    };

    QHash<ReosRenderedObject *, CacheRendering> mCacheRenderings;
    QPointer<QgsMapCanvas> mCanvas;
    QgsMapToPixel currentMapToPixel;
    QgsRectangle currentExtent;
    QgsDateTimeRange currentTimeRange;
    QMutex mMutex;
    QHash<ReosRenderedObject *, ReosObjectRenderer *> mRenderingObjects;
};


ReosRendererObjectHandler::ReosRendererObjectHandler( QGraphicsView *view )
  : d( new ReosRendererObjectHandler_p( qobject_cast<QgsMapCanvas*>( view ) ) )
{

}

ReosRendererObjectHandler::~ReosRendererObjectHandler() {}

void ReosRendererObjectHandler::makeObsolete( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );
  auto it = d->mCacheRenderings.find( renderedObject );

  if ( it != d->mCacheRenderings.end() )
  {
    ReosRendererObjectHandler_p::CacheRendering cache = it.value();
    cache.obsolete = true;
    d->mCacheRenderings[renderedObject] = cache;
  }
}

void ReosRendererObjectHandler::startRender( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );
  if ( !d->mRenderingObjects.contains( renderedObject ) && !hasUpToDateCache( renderedObject ) )
  {
    std::unique_ptr<ReosObjectRenderer> renderer( renderedObject->createRenderer( d->mCanvas ) );
    connect( renderer.get(), &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
    renderer->startOnOtherThread();
    d->mRenderingObjects.insert( renderedObject, renderer.release() );
  }
}


bool ReosRendererObjectHandler::hasCache( ReosRenderedObject *renderedObject )
{
  auto it = d->mCacheRenderings.find( renderedObject );

  if ( it != d->mCacheRenderings.end() )
  {
    bool timeRangeIsValid = it->timeRange.begin().isValid() && it->timeRange.end().isValid() &&
                            d->currentTimeRange.begin().isValid() && d->currentTimeRange.end().isValid();

    return it->extent == d->currentExtent &&
           it->mapToPixel == d->currentMapToPixel &&
           ( it->timeRange == d->currentTimeRange || !timeRangeIsValid ) ;
  }

  return false;
}

bool ReosRendererObjectHandler::hasUpToDateCache( ReosRenderedObject *renderedObject )
{
  auto it = d->mCacheRenderings.find( renderedObject );

  if ( it != d->mCacheRenderings.end() )
  {
    bool timeRangeIsValid = it->timeRange.begin().isValid() && it->timeRange.end().isValid() &&
                            d->currentTimeRange.begin().isValid() && d->currentTimeRange.end().isValid();

    return it->extent == d->currentExtent &&
           it->mapToPixel == d->currentMapToPixel &&
           ( it->timeRange == d->currentTimeRange || !timeRangeIsValid ) &&
           !it->obsolete;
  }

  return false;
}


QImage ReosRendererObjectHandler::image( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );
  if ( hasCache( renderedObject ) )
    return d->mCacheRenderings.value( renderedObject ).image;
  else
    return transformImage( renderedObject );
}


static QPointF _transform( const QgsMapToPixel &mtp, const QgsPointXY &point, double scale )
{
  qreal x = point.x(), y = point.y();
  mtp.transformInPlace( x, y );
  return QPointF( x, y ) * scale;
}

QImage ReosRendererObjectHandler::transformImage( ReosRenderedObject *renderedObject )
{
  auto it = d->mCacheRenderings.find( renderedObject );

  if ( it != d->mCacheRenderings.end() )
  {
    //from QGIS code, QgsMapRendererCache::transformedCacheImage
    const QgsMapToPixel &mtp = d->currentMapToPixel;
    if ( !qgsDoubleNear( mtp.mapRotation(), it->mapToPixel.mapRotation() ) )
      return QImage();

    QgsRectangle intersection = d->currentExtent.intersect( it->extent );
    if ( intersection.isNull() )
      return QImage();

    // Calculate target rect
    const QPointF ulT = _transform( mtp, QgsPointXY( intersection.xMinimum(), intersection.yMaximum() ), 1.0 );
    const QPointF lrT = _transform( mtp, QgsPointXY( intersection.xMaximum(), intersection.yMinimum() ), 1.0 );
    const QRectF targetRect( ulT.x(), ulT.y(), lrT.x() - ulT.x(), lrT.y() - ulT.y() );

    // Calculate source rect
    const QPointF ulS = _transform( it->mapToPixel, QgsPointXY( intersection.xMinimum(), intersection.yMaximum() ),  it->image.devicePixelRatio() );
    const QPointF lrS = _transform( it->mapToPixel, QgsPointXY( intersection.xMaximum(), intersection.yMinimum() ),  it->image.devicePixelRatio() );
    const QRectF sourceRect( ulS.x(), ulS.y(), lrS.x() - ulS.x(), lrS.y() - ulS.y() );

    // Draw image
    QImage ret( it->image.size(), it->image.format() );
    ret.setDevicePixelRatio( it->image.devicePixelRatio() );
    ret.setDotsPerMeterX( it->image.dotsPerMeterX() );
    ret.setDotsPerMeterY( it->image.dotsPerMeterY() );
    ret.fill( Qt::transparent );
    QPainter painter;
    painter.begin( &ret );
    painter.drawImage( targetRect, it->image, sourceRect );
    painter.end();
    return ret;
  }

  return QImage();
}

void ReosRendererObjectHandler::clearObject( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );
  d->mCacheRenderings.remove( renderedObject );
}

void ReosRendererObjectHandler::stopRendering( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );

  if ( d->mRenderingObjects.contains( renderedObject ) )
  {
    ReosObjectRenderer *renderer = objectToRenderer( renderedObject );
    disconnect( renderer, &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
    connect( renderer, &ReosObjectRenderer::finished, renderer,  &ReosObjectRenderer::deleteLater );
    renderer->stop( true );

    d->mRenderingObjects.remove( renderedObject );
  }
}

void ReosRendererObjectHandler::updateViewParameter()
{
  QMutexLocker locker( &( d->mMutex ) );
  d->currentMapToPixel = d->mCanvas->mapSettings().mapToPixel();
  d->currentExtent = d->mCanvas->mapSettings().visibleExtent();
  d->currentTimeRange = d->mCanvas->mapSettings().temporalRange();

  for ( ReosObjectRenderer *renderer : std::as_const( d->mRenderingObjects ) )
  {
    disconnect( renderer, &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
    connect( renderer, &ReosObjectRenderer::finished, renderer,  &ReosObjectRenderer::deleteLater );
    renderer->stop( true );
  }
  d->mRenderingObjects.clear();
}

ReosRenderedObject *ReosRendererObjectHandler::rendererToObject( ReosObjectRenderer *renderer ) const
{
  const QList<ReosRenderedObject *> objects =  d->mRenderingObjects.keys();
  for ( ReosRenderedObject *o : objects )
    if ( d->mRenderingObjects.value( o ) == renderer )
      return o;

  return nullptr;
}

ReosObjectRenderer *ReosRendererObjectHandler::objectToRenderer( ReosRenderedObject *o ) const
{
  return d->mRenderingObjects.value( o, nullptr );
}

void ReosRendererObjectHandler::onRendererFinished()
{
  QMutexLocker locker( &( d->mMutex ) );

  ReosObjectRenderer *renderer = qobject_cast<ReosObjectRenderer *>( sender() );
  if ( !renderer )
    return;

  ReosRenderedObject *object = rendererToObject( renderer );

  if ( object )
    d->mCacheRenderings.insert( object, ReosRendererObjectHandler_p::CacheRendering( {renderer->image(), d->currentExtent, d->currentMapToPixel, d->currentTimeRange} ) );

  disconnect( renderer, &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
  renderer->deleteLater();
  d->mRenderingObjects.remove( object );

  d->mCanvas->refresh();
}



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
  , mEnableSnappingAction( new QAction( tr( "Snapping" ), this ) )
  , mExtraRenderedObjectHandler( mCanvas )
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

  //*** handle temporal controller
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
  QPair<QDateTime, QDateTime> timeRange = gisEngine->temporalRange();
  temporalControlerWidget->temporalController()->setTemporalExtents( QgsDateTimeRange( {timeRange.first, timeRange.second} ) );
  connect( mEngine, &ReosGisEngine::temporalRangeChanged, temporalControlerWidget,
           [temporalControlerWidget]( const QDateTime & startTime, const QDateTime & endTime )
  {
    temporalControlerWidget->temporalController()->setTemporalExtents( QgsDateTimeRange( {startTime, endTime} ) );
  } );

  connect( temporalControlerWidget->temporalController(), &QgsTemporalNavigationObject::updateTemporalRange,
           this, &ReosMap::refreshCanvas );
  //****

  mEnableSnappingAction->setCheckable( true );

  connect( canvas, &QgsMapCanvas::renderStarting, this, &ReosMap::prepareExtraRenderedObject );
  connect( canvas, &QgsMapCanvas::renderComplete, this, &ReosMap::drawExtraRendering );

  mExtraRenderedObjectHandler.init();

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

void ReosMap::initialize()
{
  setDefaultMapTool();
  mExtraRenderedObjects.clear();

  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  QgsMapCanvasSnappingUtils *snappingUtils = new QgsMapCanvasSnappingUtils( canvas, this );
  canvas->setSnappingUtils( snappingUtils );

  connect( QgsProject::instance(), &QgsProject::snappingConfigChanged, snappingUtils, &QgsSnappingUtils::setConfig );

  QgsSnappingConfig snappingConfig = QgsProject::instance()->snappingConfig();
  snappingConfig.setEnabled( true );
  snappingConfig.setTypeFlag( Qgis::SnappingType::Vertex );
  snappingConfig.setMode( Qgis::SnappingMode::AllLayers );
  QgsProject::instance()->setSnappingConfig( snappingConfig );
}

void ReosMap::addSnappableStructure( ReosGeometryStructure *structure )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  QgsVectorLayer *vl = qobject_cast<QgsVectorLayer *>( structure->data() );
  if ( canvas && vl )
    canvas->snappingUtils()->addExtraSnapLayer( vl );
}

void ReosMap::removeSnappableStructure( ReosGeometryStructure *structure )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  QgsVectorLayer *vl = qobject_cast<QgsVectorLayer *>( structure->data() );
  if ( canvas && vl )
    canvas->snappingUtils()->removeExtraSnapLayer( vl );
}

void ReosMap::addExtraRenderedObject( ReosRenderedObject *obj )
{
  mExtraRenderedObjects.append( obj );
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( mapCanvas )
  {
    mapCanvas->refresh();
    connect( obj, &ReosRenderedObject::repaintRequested, this, &ReosMap::onExtraObjectRequestRepaint );
  }
}

void ReosMap::removeExtraRenderedObject( ReosRenderedObject *obj )
{
  mExtraRenderedObjects.removeOne( obj );
  mExtraRenderedObjectHandler.clearObject( obj );
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( mapCanvas )
  {
    mapCanvas->refresh();
    disconnect( obj, &ReosRenderedObject::repaintRequested, this, &ReosMap::onExtraObjectRequestRepaint );
  }
}

const QObject *ReosMap::temporalController() const
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( mapCanvas )
    return mapCanvas->temporalController();

  return nullptr;
}

void ReosMap::setCrs( const QString &crsWkt )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setDestinationCrs( QgsCoordinateReferenceSystem::fromWkt( crsWkt ) );
  emit crsChanged( crsWkt );
}

void ReosMap::prepareExtraRenderedObject()
{
  for ( ReosRenderedObject *obj : std::as_const( mExtraRenderedObjects ) )
    mExtraRenderedObjectHandler.startRender( obj );
}

void ReosMap::drawExtraRendering( QPainter *painter )
{
  for ( ReosRenderedObject *obj : std::as_const( mExtraRenderedObjects ) )
    painter->drawImage( QPointF( 0, 0 ), mExtraRenderedObjectHandler.image( obj ) );
}

void ReosMap::onExtraObjectRenderedFinished()
{
  qobject_cast<QgsMapCanvas *>( mCanvas )->refresh();
}

void ReosMap::onExtraObjectRequestRepaint()
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( canvas )
  {
    ReosRenderedObject *obj = qobject_cast<ReosRenderedObject *>( sender() );
    if ( obj )
    {
      mExtraRenderedObjectHandler.stopRendering( obj );
      mExtraRenderedObjectHandler.makeObsolete( obj );
    }
    canvas->refresh();
  }
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

void ReosRendererObjectHandler::init()
{
  QObject::connect( d->mCanvas, &QgsMapCanvas::extentsChanged, this, &ReosRendererObjectHandler::updateViewParameter );
  connect( d->mCanvas->temporalController(), &QgsTemporalController::updateTemporalRange,
           this, &ReosRendererObjectHandler::updateViewParameter );
}
