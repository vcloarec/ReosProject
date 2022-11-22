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
#include <qgsrasterlayer.h>

#include "reosmap.h"
#include "reosgisengine.h"
#include "reosmaptool.h"
#include "reosparameter.h"
#include "reospolylinesstructure.h"
#include "reostemporalcontrollerwidget.h"
#include "reostemporalcontroller_p.h"
#include "reosrenderersettings.h"
#include "reosmesh.h"
#include "reosmaplegenditem.h"


class ReosRendererObjectHandler_p
{

  public:
    ReosRendererObjectHandler_p( QgsMapCanvas *canvas )
      : mCanvas( canvas )
    {}

    struct CacheRendering
    {
      QImage image;
      QRectF extent;
      std::shared_ptr<ReosRendererObjectMapTimeStamp> mapTimeStamp;
      QgsMapToPixel mapToPixel;
      qint64 timeStamp;
      bool obsolete = false;
    };

    struct CurrentProcessing
    {
      ReosRenderedObject *renderedObject = nullptr;
      ReosObjectRenderer *renderer = nullptr;
      QRectF extent;
      bool obsolete = false;
    };

    QHash<ReosRenderedObject *, CacheRendering> mCacheRenderings;
    QPointer<QgsMapCanvas> mCanvas;
    QgsMapToPixel currentMapToPixel;
    QRectF currentExtent;
    std::shared_ptr<ReosRendererObjectMapTimeStamp> currentMapTimeStamp;
    QMutex mMutex;
    QList<ReosObjectRenderer *> mRenderers;
    QList<CurrentProcessing > mCurrentProcessing;
    QHash<ReosObjectRenderer *, QgsMapToPixel> mMapToPixels;
    QHash<ReosObjectRenderer *, qint64> mTimeStamps;
};


ReosRendererObjectHandler::ReosRendererObjectHandler( QGraphicsView *view )
  : d( new ReosRendererObjectHandler_p( qobject_cast<QgsMapCanvas*>( view ) ) )
{
}

ReosRendererObjectHandler::~ReosRendererObjectHandler()
{
  QMutexLocker locker( &( d->mMutex ) );
  for ( ReosRendererObjectHandler_p::CurrentProcessing &curProc :  d->mCurrentProcessing )
  {
    connect( curProc.renderer, &ReosObjectRenderer::finished, curProc.renderer, &QObject::deleteLater );
    curProc.renderer->stop( true );
  }
}

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

  for ( auto &curProc : d->mCurrentProcessing )
  {
    if ( renderedObject == curProc.renderedObject )
    {
      curProc.obsolete = true;
      curProc.renderer->stop( true );
    }
  }
}

void ReosRendererObjectHandler::startRender( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );

  const QgsMapSettings &mapSettings = d->mCanvas->mapSettings();
  std::unique_ptr<ReosRendererSettings> rendererSettings = ReosRenderedObject::createRenderSettings( &mapSettings );

  std::unique_ptr<ReosRendererObjectMapTimeStamp> mapTimeStamp( renderedObject->createMapTimeStamp( rendererSettings.get() ) );

  if ( !hasUpToDateCache( renderedObject, mapTimeStamp.get() ) )
  {
    //there is nothing in the cache, check if there is a renderer still working
    QRectF extent = d->mCanvas->mapSettings().extent().toRectF();
    for ( const auto &curProc : std::as_const( d->mCurrentProcessing ) )
    {
      if ( !curProc.obsolete &&
           renderedObject == curProc.renderedObject &&
           extent == curProc.extent &&
           mapTimeStamp->equal( curProc.renderer->mapTimeStamp() )
         )
        return;
    }

    std::unique_ptr<ReosObjectRenderer> renderer( renderedObject->createRenderer( rendererSettings.get() ) );
    if ( !renderer )
      return;

    renderer->setMapTimeStamp( mapTimeStamp.release() );

    connect( renderer.get(), &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
    d->mMapToPixels.insert( renderer.get(), d->mCanvas->mapSettings().mapToPixel() );
    d->mTimeStamps.insert( renderer.get(), QDateTime::currentMSecsSinceEpoch() );

    d->mCurrentProcessing.append(
    {
      renderedObject,
      renderer.release(),
      extent,
      false
    } );

    d->mCurrentProcessing.last().renderer->startOnOtherThread();
  }
}


bool ReosRendererObjectHandler::hasCache( ReosRenderedObject *renderedObject )
{
  auto it = d->mCacheRenderings.find( renderedObject );
  return it != d->mCacheRenderings.end();
}

bool ReosRendererObjectHandler::hasUpToDateCache( ReosRenderedObject *renderedObject, ReosRendererObjectMapTimeStamp *mapTimeStamp )
{
  auto it = d->mCacheRenderings.find( renderedObject );

  if ( it != d->mCacheRenderings.end() )
  {
    return it->extent == d->currentExtent &&
           ( !mapTimeStamp || it->mapTimeStamp->equal( mapTimeStamp ) ) &&
           !it->obsolete;
  }

  return false;
}

void ReosRendererObjectHandler::destroyRenderer( ReosObjectRenderer *renderer )
{
  disconnect( renderer, &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
  for ( int i = 0; i < d->mCurrentProcessing.count(); ++i )
  {
    if ( d->mCurrentProcessing.at( i ).renderer == renderer )
    {
      d->mCurrentProcessing.removeAt( i );
      break;
    }
  }
  d->mMapToPixels.remove( renderer );
  d->mTimeStamps.remove( renderer );
  renderer->deleteLater();
}


QImage ReosRendererObjectHandler::image( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );
  if ( hasUpToDateCache( renderedObject ) )
  {
    return d->mCacheRenderings.value( renderedObject ).image;
  }
  else
  {
    return transformImage( renderedObject );
  }
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
    {
      return QImage();
    }

    QgsRectangle intersection = QgsRectangle( d->currentExtent ).intersect( it->extent );
    if ( intersection.isNull() )
    {
      return QImage();
    }

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

void ReosRendererObjectHandler::updateViewParameter()
{
  QMutexLocker locker( &( d->mMutex ) );
  d->currentExtent = d->mCanvas->mapSettings().visibleExtent().toRectF();
  d->currentMapToPixel = d->mCanvas->mapSettings().mapToPixel();
}

void ReosRendererObjectHandler::onRendererFinished()
{
  QMutexLocker locker( &( d->mMutex ) );
  ReosObjectRenderer *renderer = qobject_cast<ReosObjectRenderer *>( sender() );
  if ( !renderer )
    return;

  ReosRenderedObject *object = renderer->object();

  if ( !renderer->isRenderingStopped() && object )
  {
    bool needUpdate = false;
    const QRectF renderedExtent = renderer->extent();

    if ( d->mCacheRenderings.contains( object ) )
    {
      const ReosRendererObjectHandler_p::CacheRendering &prevCach = d->mCacheRenderings.value( object );
      needUpdate = prevCach.obsolete || d->mTimeStamps.value( renderer, 0 ) > prevCach.timeStamp;
    }
    else
    {
      needUpdate = true;
    }

    if ( needUpdate )
    {
      std::shared_ptr<ReosRendererObjectMapTimeStamp> timeStamp;
      timeStamp.reset( renderer->releaseMapTimeStamp() );
      d->mCacheRenderings.insert( object, ReosRendererObjectHandler_p::CacheRendering(
      {
        renderer->image(),
        renderedExtent,
        timeStamp,
        d->mMapToPixels.value( renderer ),
        d->mTimeStamps.value( renderer )} ) );
    }
    d->mCanvas->refresh();
  }
  destroyRenderer( renderer );
}

ReosMap::ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget ):
  ReosModule( gisEngine )
  , mEngine( gisEngine )
  , mCanvas( new QgsMapCanvas( parentWidget ) )
  , mActionNeutral( new QAction( QIcon( QStringLiteral( ":/images/neutral.svg" ) ), tr( "Deactivate Tool" ), this ) )
  , mDefaultMapTool( new ReosMapToolNeutral( this ) )
  , mActionZoom( new QAction( QIcon( QStringLiteral( ":/images/zoomInExtent.svg" ) ), tr( "Zoom In" ), this ) )
  , mZoomMapTool( new ReosMapToolDrawExtent( this ) )
  , mActionZoomIn( new QAction( QIcon( QStringLiteral( ":/images/zoomIn.svg" ) ), tr( "Zoom In" ), this ) )
  , mActionZoomOut( new QAction( QIcon( QStringLiteral( ":/images/zoomOut.svg" ) ), tr( "Zoom Out" ), this ) )
  , mActionPreviousZoom( new QAction( QIcon( QStringLiteral( ":/images/zoomPrevious.svg" ) ), tr( "Previous Zoom" ), this ) )
  , mActionNextZoom( new QAction( QIcon( QStringLiteral( ":/images/zoomNext.svg" ) ), tr( "Next Zoom" ), this ) )
  , mTemporalControllerAction( new QAction( QIcon( QStringLiteral( ":/images/temporal.svg" ) ), tr( "Temporal controller" ), this ) )
  , mEnableSnappingAction( new QAction( tr( "Snapping" ), this ) )
  , mExtraRenderedObjectHandler( mCanvas )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setExtent( QgsRectangle( 0, 0, 200, 200 ) );
  canvas->setObjectName( "map canvas" );

  connect( canvas, &QgsMapCanvas::extentsChanged, this, &ReosMap::extentChanged );

  QgsLayerTreeModel *layerTreeModel = nullptr;
  if ( mEngine )
    layerTreeModel = qobject_cast<QgsLayerTreeModel *>( mEngine->layerTreeModel() );
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

  if ( mEngine )
    connect( mEngine, &ReosGisEngine::crsChanged, this, &ReosMap::setCrs );

  mDefaultMapTool->setAction( mActionNeutral );
  mDefaultMapTool->setCurrentToolInMap();

  mZoomMapTool->setAction( mActionZoom );
  mZoomMapTool->setColor( QColor( 9, 150, 230 ) );
  mZoomMapTool->setFillColor( QColor( 9, 150, 230, 20 ) );
  mZoomMapTool->setStrokeWidth( 1 );
  mZoomMapTool->setLineStyle( Qt::DotLine );
  mZoomMapTool->setCursor( QCursor( QStringLiteral( ":/cursors/zoomInExtent.svg" ), 5, 5 ) );
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
  ReosTemporalControllerWidget *temporalControlerWidget = new ReosTemporalControllerWidget( mTemporalDockWidget );

  mTemporalControler = qobject_cast<ReosTemporalController_p *>( temporalControlerWidget->temporalController() );
  canvas->setTemporalController( mTemporalControler );
  mTemporalDockWidget->setWidget( temporalControlerWidget );
  mTemporalControllerAction->setCheckable( true );
  connect( mTemporalControllerAction, &QAction::triggered, this, [this]
  {
    mTemporalDockWidget->setVisible( mTemporalControllerAction->isChecked() );
  } );
  connect( mTemporalControler, &ReosTemporalController_p::updateTemporalRange, this, [this]( const QgsDateTimeRange & timeRange )
  {emit timeChanged( timeRange.begin() );} );

  if ( mEngine )
    connect( mEngine, &ReosGisEngine::temporalRangeChanged, mTemporalControler, &ReosTemporalController_p::setTemporalExtent );

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
  if ( !extent.isValid() )
    return;
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  ReosMapExtent mapExtent;
  if ( mEngine )
    mapExtent = mEngine->transformToProjectExtent( extent );
  else
    mapExtent = ReosGisEngine::transformExtent( extent, mapCrs() );

  canvas->setExtent( QgsRectangle( mapExtent.xMapMin(), mapExtent.yMapMin(), mapExtent.xMapMax(), mapExtent.yMapMax() ) );
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
  snappingUtils->setConfig( snappingConfig );
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

    const QList<ReosColorShaderSettings *> colorRampSettings = obj->colorShaderSettings();
    for ( ReosColorShaderSettings *settings : colorRampSettings )
    {
      std::unique_ptr<ReosColorRampMapLegendItem> legend( new ReosColorRampMapLegendItem( settings ) );
      mCanvas->scene()->addItem( legend.get() );
      legend->setHorizontalDistanceFromCanvasBorder( 3 );
      legend->setVerticalDistanceFromCanvasBorder( 3 );
      legend->setZValue( std::numeric_limits<double>::max() );
      mColorRampLegendSettings.insert( settings, legend.release() );
    }
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

    const QList<ReosColorShaderSettings *> colorRampSettings = obj->colorShaderSettings();
    for ( ReosColorShaderSettings *settings : colorRampSettings )
    {

      ReosColorRampMapLegendItem *legendItem = mColorRampLegendSettings.value( settings, nullptr );
      if ( legendItem )
        delete legendItem;
    }
  }
}

const QObject *ReosMap::temporalController() const
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( mapCanvas )
    return mapCanvas->temporalController();

  return nullptr;
}

void ReosMap::setTimeStep( const ReosDuration &timeStep )
{
  mTemporalControler->setTimeStep( timeStep );
}

void ReosMap::setTemporalRange( const QDateTime &startTime, const QDateTime &endTime )
{
  mTemporalControler->setTemporalExtent( startTime, endTime );
}

ReosDuration ReosMap::timeStep() const
{
  return mTemporalControler->timeStep();
}

QDateTime ReosMap::currentTime() const
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( mapCanvas )
    return mapCanvas->mapSettings().temporalRange().begin();

  return QDateTime();
}

void ReosMap::activateOpenStreetMap()
{
  if ( !mEngine )
  {
    QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );

    QgsRasterLayer *osmLayer = new QgsRasterLayer(
      QStringLiteral( "type=xyz&url=https://tile.openstreetmap.org/%7Bz%7D/%7Bx%7D/%7By%7D.png&zmax=19&zmin=0&http-header:r" ),
      tr( "Open Street Map" ),
      QStringLiteral( "wms" ) );

    osmLayer->setParent( this );

    QList<QgsMapLayer *> layers;
    layers << osmLayer;
    canvas->setLayers( layers );
    canvas->setDestinationCrs( osmLayer->crs() );
    canvas->zoomToFullExtent();
  }
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
  mCrs->setText( qgsCrs.authid() );
}

void ReosRendererObjectHandler::init()
{
  QObject::connect( d->mCanvas, &QgsMapCanvas::extentsChanged, this, &ReosRendererObjectHandler::updateViewParameter );
  connect( d->mCanvas->temporalController(), &QgsTemporalController::updateTemporalRange,
           this, &ReosRendererObjectHandler::updateViewParameter );
}

ReosDataVizMapWidget::ReosDataVizMapWidget( QWidget *parent )
  : QWidget( parent )
  , mMap( new ReosMap( nullptr, this ) )
{
  QVBoxLayout *lay = new QVBoxLayout( this );
  setLayout( lay );
  lay->addWidget( mMap->mapCanvas() );
  lay->addWidget( mMap->temporalControllerDockWidget() );
  mMap->activateOpenStreetMap();
  lay->setStretch( 1, 0 );
  mMap->temporalControllerDockWidget()->setFeatures( QDockWidget::NoDockWidgetFeatures );
}

void ReosDataVizMapWidget::addRenderedDataObject( ReosRenderedObject *object )
{
  if ( !object )
    return;
  mMap->addExtraRenderedObject( object );
}

void ReosDataVizMapWidget::removeRenderedObject( ReosRenderedObject *object )
{
  if ( !object )
    return;
  mMap->removeExtraRenderedObject( object );
}

void ReosDataVizMapWidget::setTimeExtent( const QDateTime &startTime, const QDateTime &endTime )
{
  mMap->setTemporalRange( startTime, endTime );
}

void ReosDataVizMapWidget::setTimeStep( const ReosDuration &timeStep )
{
  mMap->setTimeStep( timeStep );
}

void ReosDataVizMapWidget::setExtent( const ReosMapExtent &extent )
{
  mMap->setExtent( extent );
}

ReosMap *ReosDataVizMapWidget::map()
{
  return mMap;
}
