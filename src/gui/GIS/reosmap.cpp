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
#include <qgsmaptoolpan.h>

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
#include "reossettings.h"


class ReosRendererObjectHandler_p
{

  public:
    explicit ReosRendererObjectHandler_p( QgsMapCanvas *canvas )
      : mCanvas( canvas )
    {}

    struct CacheRenderedObject
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
      ReosObjectRenderer *renderer = nullptr;
      QRectF extent;
      bool obsolete = false;
    };

    struct CurrentProcessingQueue
    {
      CurrentProcessing firstToRender;
      CurrentProcessing lastStarted;
    };

    QHash<ReosRenderedObject *, CacheRenderedObject> mCacheRenderings;
    QPointer<QgsMapCanvas> mCanvas;
    QgsMapToPixel mCurrentMapToPixel;
    QRectF mCurrentExtent;
    QMutex mMutex;
    QHash<ReosObjectRenderer *, QgsMapToPixel> mMapToPixels;
    QHash<ReosObjectRenderer *, qint64> mTimeStamps;
    QHash<ReosRenderedObject *, CurrentProcessingQueue> mProcessingQueues;
    QList<QPointer<ReosObjectRenderer >> mRendererGarbage;
};


ReosRendererObjectHandler::ReosRendererObjectHandler( QGraphicsView *view )
  : d( new ReosRendererObjectHandler_p( qobject_cast<QgsMapCanvas*>( view ) ) )
{
}

ReosRendererObjectHandler::~ReosRendererObjectHandler()
{
  QMutexLocker locker( &( d->mMutex ) );

  for ( auto it = d->mProcessingQueues.constBegin(); it != d->mProcessingQueues.constEnd(); ++it )
  {
    if ( it.value().firstToRender.renderer )
    {
      connect( it.value().firstToRender.renderer, &ReosObjectRenderer::finished, it.value().firstToRender.renderer, &QObject::deleteLater );
      it.value().firstToRender.renderer->stop( true );
    }

    if ( it.value().lastStarted.renderer )
    {
      connect( it.value().lastStarted.renderer, &ReosObjectRenderer::finished, it.value().lastStarted.renderer, &QObject::deleteLater );
      it.value().lastStarted.renderer->stop( true );
    }
  }

  for ( ReosObjectRenderer *render : std::as_const( d->mRendererGarbage ) )
  {
    if ( !render )
      continue;
    connect( render, &ReosObjectRenderer::finished, render, &QObject::deleteLater );
    render->stop( true );
  }
}

void ReosRendererObjectHandler::makeRenderingObsolete( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );

  if ( d->mCacheRenderings.contains( renderedObject ) )
    d->mCacheRenderings[renderedObject].obsolete = true;

  if ( d->mProcessingQueues.contains( renderedObject ) )
  {
    d->mProcessingQueues[renderedObject].firstToRender.obsolete = true;
    d->mProcessingQueues[renderedObject].lastStarted.obsolete = true;
  }
}

void ReosRendererObjectHandler::startRender( ReosRenderedObject *renderedObject )
{
  QMutexLocker locker( &( d->mMutex ) );

  if ( !renderedObject )
    return;
  const QgsMapSettings &mapSettings = d->mCanvas->mapSettings();
  std::unique_ptr<ReosRendererSettings> rendererSettings = ReosRenderedObject::createRenderSettings( &mapSettings );

  std::unique_ptr<ReosRendererObjectMapTimeStamp> mapTimeStamp( renderedObject->createMapTimeStamp( rendererSettings.get() ) );

  if ( !hasUpToDateCache( renderedObject, mapTimeStamp.get() ) )
  {
    //there is nothing in the cache, check if there is a renderer still working
    const QRectF extent = d->mCanvas->mapSettings().extent().toRectF();

    if ( d->mProcessingQueues.contains( renderedObject ) )
    {
      const auto curProc1 = d->mProcessingQueues[renderedObject].firstToRender;
      if ( curProc1.renderer &&
           !curProc1.obsolete &&
           extent == curProc1.extent &&
           mapTimeStamp->equal( curProc1.renderer->mapTimeStamp() ) )
        return;

      const auto curProc2 = d->mProcessingQueues[renderedObject].lastStarted;
      if ( curProc2.renderer &&
           !curProc2.obsolete &&
           extent == curProc2.extent &&
           mapTimeStamp->equal( curProc2.renderer->mapTimeStamp() ) )
        return;
    }

    std::unique_ptr<ReosObjectRenderer> renderer( renderedObject->createRenderer( rendererSettings.get() ) );
    if ( !renderer )
      return;

    renderer->setMapTimeStamp( mapTimeStamp.release() );

    connect( renderer.get(), &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
    d->mMapToPixels.insert( renderer.get(), d->mCanvas->mapSettings().mapToPixel() );
    d->mTimeStamps.insert( renderer.get(), QDateTime::currentMSecsSinceEpoch() );


    ReosObjectRenderer *rendererToStart = nullptr;
    if ( d->mProcessingQueues.contains( renderedObject ) )
    {
      ReosRendererObjectHandler_p::CurrentProcessing &curProc1 = d->mProcessingQueues[renderedObject].firstToRender;
      if ( !curProc1.renderer )
      {
        curProc1.extent = extent;
        curProc1.obsolete = false;
        curProc1.renderer = renderer.release();
        rendererToStart = curProc1.renderer;
      }
      else
      {
        ReosRendererObjectHandler_p::CurrentProcessing &curProc2 = d->mProcessingQueues[renderedObject].lastStarted;
        if ( curProc2.renderer )
        {
          curProc2.renderer->stop( true );
          d->mRendererGarbage.append( curProc2.renderer );
        }
        curProc2.extent = extent;
        curProc2.obsolete = false;
        curProc2.renderer = renderer.release();

        rendererToStart = curProc2.renderer;
      }
    }
    else
    {
      ReosRendererObjectHandler_p::CurrentProcessing curProc;
      curProc.extent = extent;
      curProc.renderer = renderer.release();
      curProc.obsolete = false;
      d->mProcessingQueues.insert( renderedObject, {curProc, ReosRendererObjectHandler_p::CurrentProcessing()} );

      rendererToStart = curProc.renderer;
    }

    if ( rendererToStart )
      rendererToStart->startOnOtherThread();
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
    return it->extent == d->mCurrentExtent &&
           ( !mapTimeStamp || it->mapTimeStamp->equal( mapTimeStamp ) ) &&
           !it->obsolete;
  }

  return false;
}

void ReosRendererObjectHandler::destroyRenderer( ReosObjectRenderer *renderer )
{
  disconnect( renderer, &ReosObjectRenderer::finished, this, &ReosRendererObjectHandler::onRendererFinished );
  d->mMapToPixels.remove( renderer );
  d->mTimeStamps.remove( renderer );
  d->mRendererGarbage.removeOne( renderer );
  delete renderer;
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


static QPointF transform_( const QgsMapToPixel &mtp, const QgsPointXY &point, double scale )
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
    const QgsMapToPixel &mtp = d->mCurrentMapToPixel;
    if ( !qgsDoubleNear( mtp.mapRotation(), it->mapToPixel.mapRotation() ) )
    {
      return QImage();
    }

    QgsRectangle intersection = QgsRectangle( d->mCurrentExtent ).intersect( it->extent );
    if ( intersection.isNull() )
    {
      return QImage();
    }

    // Calculate target rect
    const QPointF ulT = transform_( mtp, QgsPointXY( intersection.xMinimum(), intersection.yMaximum() ), 1.0 );
    const QPointF lrT = transform_( mtp, QgsPointXY( intersection.xMaximum(), intersection.yMinimum() ), 1.0 );
    const QRectF targetRect( ulT.x(), ulT.y(), lrT.x() - ulT.x(), lrT.y() - ulT.y() );

    // Calculate source rect
    const QPointF ulS = transform_( it->mapToPixel, QgsPointXY( intersection.xMinimum(), intersection.yMaximum() ),  it->image.devicePixelRatio() );
    const QPointF lrS = transform_( it->mapToPixel, QgsPointXY( intersection.xMaximum(), intersection.yMinimum() ),  it->image.devicePixelRatio() );
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
  d->mCurrentExtent = d->mCanvas->mapSettings().visibleExtent().toRectF();
  d->mCurrentMapToPixel = d->mCanvas->mapSettings().mapToPixel();
}

void ReosRendererObjectHandler::onRendererFinished()
{
  QMutexLocker locker( &( d->mMutex ) );
  ReosObjectRenderer *renderer = qobject_cast<ReosObjectRenderer *>( sender() );
  if ( !renderer )
    return;

  QPointer<ReosRenderedObject> object = renderer->object();

  if ( !renderer->isRenderingStopped() && object )
  {
    bool needUpdate = false;
    const QRectF renderedExtent = renderer->extent();

    if ( d->mCacheRenderings.contains( object ) )
    {
      const ReosRendererObjectHandler_p::CacheRenderedObject &prevCach = d->mCacheRenderings.value( object );
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
      d->mCacheRenderings.insert( object, ReosRendererObjectHandler_p::CacheRenderedObject(
      {
        renderer->image(),
        renderedExtent,
        timeStamp,
        d->mMapToPixels.value( renderer ),
        d->mTimeStamps.value( renderer )} ) );

      object->updateInternalCache( renderer );
    }
    emit requestCanvasRefesh();
  }

  if ( d->mProcessingQueues.contains( object ) )
  {
    ReosRendererObjectHandler_p::CurrentProcessing &curProc1 = d->mProcessingQueues[object].firstToRender;
    ReosRendererObjectHandler_p::CurrentProcessing &curProc2 = d->mProcessingQueues[object].lastStarted;
    if ( curProc1.renderer == renderer )
    {
      curProc1 = curProc2;
      curProc2.renderer = nullptr;
    }

    if ( curProc2.renderer == renderer )
    {
      curProc1.renderer->stop( true );
      d->mRendererGarbage.append( curProc1.renderer );
      curProc1.renderer = nullptr;
      curProc2.renderer = nullptr;
    }
  }
  destroyRenderer( renderer );
}

class ReosQgsMapCanvas : public QgsMapCanvas
{
    Q_OBJECT
  public:
    explicit ReosQgsMapCanvas( QWidget *parent = nullptr )
      : QgsMapCanvas( parent )
    {
      setParallelRenderingEnabled( true );
      setCachingEnabled( true );
    }

  signals:
    void resized();

  protected:
    void resizeEvent( QResizeEvent *e ) override
    {
      QgsMapCanvas::resizeEvent( e );
      emit resized();
    }
};

ReosMap::ReosMap( ReosGisEngine *gisEngine, QWidget *parentWidget ):
  ReosModule( gisEngine )
  , mEngine( gisEngine )
  , mCanvas( new ReosQgsMapCanvas( parentWidget ) )
  , mActionNeutral( new QAction( QIcon( QStringLiteral( ":/images/neutral.svg" ) ), tr( "Deactivate Tool" ), this ) )
  , mDefaultMapTool( new ReosMapToolSelectMapItem( this ) )
  , mActionPan( new QAction( QIcon( QStringLiteral( ":/images/pan.svg" ) ), tr( "Pan" ), this ) )
  , mActionZoom( new QAction( QIcon( QStringLiteral( ":/images/zoomInExtent.svg" ) ), tr( "Zoom In" ), this ) )
  , mZoomMapTool( new ReosMapToolDrawExtent( this ) )
  , mActionZoomIn( new QAction( QIcon( QStringLiteral( ":/images/zoomIn.svg" ) ), tr( "Zoom In" ), this ) )
  , mActionZoomOut( new QAction( QIcon( QStringLiteral( ":/images/zoomOut.svg" ) ), tr( "Zoom Out" ), this ) )
  , mActionPreviousZoom( new QAction( QIcon( QStringLiteral( ":/images/zoomPrevious.svg" ) ), tr( "Previous Zoom" ), this ) )
  , mActionNextZoom( new QAction( QIcon( QStringLiteral( ":/images/zoomNext.svg" ) ), tr( "Next Zoom" ), this ) )
  , mEnableSnappingAction( new QAction( tr( "Snapping" ), this ) )
  , mActionEnableLegend( new QAction( QIcon( QStringLiteral( ":/images/plotLegend.svg" ) ), tr( "Enable/Disable Legend" ), this ) )
  , mExtraRenderedObjectHandler( mCanvas )
{
  ReosQgsMapCanvas *canvas = qobject_cast<ReosQgsMapCanvas *>( mCanvas );
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

  mActionNeutral->setCheckable( true );
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

  QgsMapToolPan *panMapTool = new QgsMapToolPan( canvas );
  mActionPan->setCheckable( true );
  panMapTool->setAction( mActionPan );
  connect( mActionPan, &QAction::toggled, canvas, [this, canvas, panMapTool]
  {
    if ( mActionPan->isChecked() )
      canvas->setMapTool( panMapTool );
  } );

  emit crsChanged( mapCrs() );

  //*** handle temporal controller
  mTemporalDockWidget = new QDockWidget( tr( "Temporal Controller" ), canvas );
  ReosTemporalControllerWidget *temporalControlerWidget = new ReosTemporalControllerWidget( mTemporalDockWidget );

  mTemporalControler = qobject_cast<ReosTemporalController_p *>( temporalControlerWidget->temporalController() );
  canvas->setTemporalController( mTemporalControler );
  mTemporalDockWidget->setWidget( temporalControlerWidget );
  mTemporalControllerAction = mTemporalDockWidget->toggleViewAction();
  mTemporalControllerAction->setIcon( QIcon( QStringLiteral( ":/images/temporal.svg" ) ) );

  connect( mTemporalControler,
           &ReosTemporalController_p::updateTemporalRange, this,
           [this]( const QgsDateTimeRange & timeRange )
  {
    emit timeChanged( timeRange.begin() );
  } );

  if ( mEngine )
    connect( mEngine, &ReosGisEngine::temporalRangeChanged, mTemporalControler, &ReosTemporalController_p::setTemporalExtent );

  mEnableSnappingAction->setCheckable( true );

  connect( canvas, &QgsMapCanvas::renderStarting, this, &ReosMap::onMapStartRendering );
  connect( canvas, &QgsMapCanvas::mapCanvasRefreshed, this, &ReosMap::onMapRenderingFinish );
  connect( canvas, &QgsMapCanvas::renderComplete, this, &ReosMap::drawExtraRendering );

  mExtraRenderedObjectHandler.init();

  mDefaultMapTool->setCursor( Qt::ArrowCursor );
  //mDefaultMapTool->setSearchUnderPoint( true );
  mDefaultMapTool->setSearchItemWhenMoving( true );
  connect( mDefaultMapTool, &ReosMapToolSelectMapItem::found, this, &ReosMap::mapItemFound );
  connect( mDefaultMapTool, &ReosMapToolSelectMapItem::foundDoubleClick, this, &ReosMap::mapItemFoundDoubleClick );

  mActionEnableLegend->setCheckable( true );
  connect( mActionEnableLegend, &QAction::toggled, this, [this]( bool checked )
  {
    const QList<ReosColorRampMapLegendItem *> items = mColorRampLegendSettings.values();
    for ( ReosColorRampMapLegendItem *item : items )
      item->setVisible( checked );

    refreshCanvas();
    ReosSettings settings;
    settings.setValue( QStringLiteral( "MainMap/EnableLegend" ), checked );
  } );
  ReosSettings settings;
  mActionEnableLegend->setChecked( settings.value( QStringLiteral( "MainMap/EnableLegend" ), true ).toBool() );
  connect( canvas, &ReosQgsMapCanvas::resized, this, &ReosMap::resizeLegend );

  connect( &mExtraRenderedObjectHandler, &ReosRendererObjectHandler::requestCanvasRefesh, this, &ReosMap::refreshCanvas );
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
  updateLegend();
  if ( !mMapIsRendering )
    canvas->refresh();
  else
    mNeedOtherRefresh = true;
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
  ret << mActionPan;
  ret << mActionZoom;
  ret << mActionZoomIn;
  ret << mActionZoomOut;
  ret << mActionPreviousZoom;
  ret << mActionNextZoom;
  ret << mTemporalControllerAction;
  ret << mActionEnableLegend;

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


static QString settingsPointerToString( ReosColorShaderSettings *settings )
{
  return QString::number( reinterpret_cast<long long>( settings ) );
}


void ReosMap::addExtraRenderedObject( ReosRenderedObject *obj )
{
  if ( !obj || mExtraRenderedObjects.contains( obj ) )
    return;
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
      legend->setZValue( std::numeric_limits<double>::max() );
      mColorRampLegendSettings.insert( settingsPointerToString( settings ), legend.release() );
    }
    updateLegend();
  }
}

void ReosMap::removeExtraRenderedObject( ReosRenderedObject *obj )
{
  if ( !obj )
    return;
  if ( mExtraRenderedObjects.removeOne( obj ) )
  {
    mExtraRenderedObjectHandler.clearObject( obj );
    QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mCanvas );
    if ( mapCanvas )
    {
      mapCanvas->refresh();
      disconnect( obj, &ReosRenderedObject::repaintRequested, this, &ReosMap::onExtraObjectRequestRepaint );

      const QList<ReosColorShaderSettings *> colorRampSettings = obj->colorShaderSettings();
      for ( ReosColorShaderSettings *settings : colorRampSettings )
      {
        QString legId = settingsPointerToString( settings );
        ReosColorRampMapLegendItem *legendItem = mColorRampLegendSettings.value( legId, nullptr );
        if ( legendItem )
          delete legendItem;
        mColorRampLegendSettings.remove( legId );
      }
      updateLegend();
    }
  }
}

void ReosMap::removeAllExtraRendererObjects()
{
  while ( !mExtraRenderedObjects.empty() )
    removeExtraRenderedObject( mExtraRenderedObjects.first() );

  refreshCanvas();
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

void ReosMap::addSelectToolTarget( const QString &targetDescription )
{
  mDefaultMapTool->addSearchingItemDescription( targetDescription );
}

void ReosMap::setCrs( const QString &crsWkt )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  canvas->setDestinationCrs( QgsCoordinateReferenceSystem::fromWkt( crsWkt ) );
  emit crsChanged( crsWkt );
}

void ReosMap::onMapStartRendering()
{
  mMapIsRendering = true;
  prepareExtraRenderedObject();
}

void ReosMap::onMapRenderingFinish()
{
  mMapIsRendering = false;
  if ( mNeedOtherRefresh )
  {
    mNeedOtherRefresh = false;
    qobject_cast<QgsMapCanvas *>( mCanvas )->refresh();
  }
}

void ReosMap::prepareExtraRenderedObject()
{
  for ( ReosRenderedObject *obj : std::as_const( mExtraRenderedObjects ) )
    mExtraRenderedObjectHandler.startRender( obj );
}

void ReosMap::drawExtraRendering( QPainter *painter )
{
  for ( ReosRenderedObject *obj : std::as_const( mExtraRenderedObjects ) )
  {
    painter->drawImage( QPointF( 0, 0 ), mExtraRenderedObjectHandler.image( obj ) );
  }
}

void ReosMap::onExtraObjectRequestRepaint()
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( mCanvas );
  if ( canvas )
  {
    ReosRenderedObject *obj = qobject_cast<ReosRenderedObject *>( sender() );
    if ( obj )
    {
      mExtraRenderedObjectHandler.makeRenderingObsolete( obj );
    }
    if ( !mMapIsRendering )
      canvas->refresh();
  }
}

void ReosMap::updateLegend() const
{
  const QList<ReosColorRampMapLegendItem *> colorRampLegendSettings = mColorRampLegendSettings.values();
  for ( int i = 0; i < colorRampLegendSettings.count(); ++i )
  {
    colorRampLegendSettings.at( i )->setVisible( mActionEnableLegend->isChecked() );
  }

  resizeLegend();
}

void ReosMap::resizeLegend() const
{
  int activeLegendCount = 0;
  for ( ReosColorRampMapLegendItem *item : mColorRampLegendSettings )
    if ( item->isActive() )
      activeLegendCount++;

  int order = 0;
  for ( ReosColorRampMapLegendItem *item : mColorRampLegendSettings )
  {
    if ( item->isActive() )
    {
      item->resize( mCanvas->viewport(), activeLegendCount, order );
      order++;
    }
  }
}

ReosMapToolSelectMapItem *ReosMap::defaultMapTool() const
{
  return mDefaultMapTool;
}

void ReosMap::deactivateCurrentTool()
{
  QgsMapTool *currentTool = qobject_cast<QgsMapCanvas *>( mCanvas )->mapTool();
  if ( currentTool )
    currentTool->deactivate();
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
  , mExtentOnMap( mMap )
{
  QVBoxLayout *lay = new QVBoxLayout( this );
  setLayout( lay );
  lay->setContentsMargins( 0, 0, 0, 0 );
  lay->addWidget( mMap->mapCanvas() );
  lay->addWidget( mMap->temporalControllerDockWidget() );
  mMap->activateOpenStreetMap();
  lay->setStretch( 1, 0 );
  mMap->temporalControllerDockWidget()->setFeatures( QDockWidget::NoDockWidgetFeatures );

  mExtentOnMap.setColor( Qt::red );
  mExtentOnMap.setExternalColor( Qt::white );
  mExtentOnMap.setStyle( Qt::DashLine );
  mExtentOnMap.setWidth( 3 );
  mExtentOnMap.setExternalWidth( 5 );
}

ReosDataVizMapWidget::~ReosDataVizMapWidget()
{
  mMap->deactivateCurrentTool();
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

void ReosDataVizMapWidget::removeAllRenderedObjects()
{
  mMap->removeAllExtraRendererObjects();
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

void ReosDataVizMapWidget::showExtentOnMap( const ReosMapExtent &extent )
{
  if ( extent != ReosMapExtent() )
    mExtentOnMap.resetPolygon( ReosGisEngine::transformToCoordinates( extent.crs(), extent.toPolygon(), mMap->mapCrs() ) );
  else
    mExtentOnMap.resetPolygon();
}

void ReosDataVizMapWidget::hideExtentOnMap()
{
  mExtentOnMap.resetPolygon();
}

ReosMap *ReosDataVizMapWidget::map()
{
  return mMap;
}

#include "reosmap.moc"
