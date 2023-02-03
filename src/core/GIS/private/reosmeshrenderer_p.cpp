/***************************************************************************
  reosmeshrenderer_p.cpp - ReosMeshRenderer_p

 ---------------------
 begin                : 30.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmeshrenderer_p.h"

#include <QElapsedTimer>
#include <QThread>

#include "reosmesh_p.h"

ReosMeshRenderer_p::ReosMeshRenderer_p( ReosRendererSettings *settings, QgsMeshLayer *layer, ReosMeshFrame_p *reosMesh )
  : ReosQgisLayerRenderer_p( settings, layer, reosMesh )
{
  ReosMeshRendererCache_p *cache = reosMesh->rendererCache();
  if ( cache )
  {
    cache->updateCache( layer, renderContext() );
    mStaticRendering = cache->staticRendering();
    mTraceImage = cache->traceImage();
  }
}

ReosMeshRenderer_p::~ReosMeshRenderer_p()
{}

void ReosMeshRenderer_p::render()
{
  QElapsedTimer timer;
  timer.start();
  if ( mStaticRendering.isNull() )
  {
    ReosQgisLayerRenderer_p::render();
    mStaticRendering = mImage;
    timer.restart();
  }
  else
  {
    mPainter->drawImage( 0, 0, mStaticRendering );
    timer.restart();
  }

  if ( !mTraceImage.isNull() /*&& mTraceImage.size() == mImage.size()*/ )
    mPainter->drawImage( 0, 0, mTraceImage );
}

const QImage &ReosMeshRenderer_p::staticRendering() const
{
  return mStaticRendering;
}

ReosMeshRendererCache_p::ReosMeshRendererCache_p( ReosMeshFrame_p *mesh, int datasetIndex )
  : mDatasetGroupsIndex( datasetIndex )
{
  ReosEncodedElement symbology = mesh->datasetVectorGroupSymbology( mesh->currentdVectorDatasetId() );

  symbology.getData( QStringLiteral( "dynamic-traces" ), mTracesSettings.dynamicTracesEnable );
  symbology.getData( QStringLiteral( "dynamic-traces-life-time" ), mTracesSettings.lifeTime );
  symbology.getData( QStringLiteral( "dynamic-traces-max-speed" ), mTracesSettings.maxSpeed );
  symbology.getData( QStringLiteral( "dynamic-traces-fps" ), mTracesSettings.fps );
  symbology.getData( QStringLiteral( "dynamic-traces-tails-persistence" ), mTracesSettings.persistence );
  symbology.getData( QStringLiteral( "dynamic-traces-tail-factor" ), mTracesSettings.tailFactor );

  if ( mTracesSettings.dynamicTracesEnable )
    QObject::connect( &mTraceController, &ReosMovingTracesController::imageReady, mesh, &ReosRenderedObject::repaintRequested );
}

ReosMeshRendererCache_p::~ReosMeshRendererCache_p() {}

void ReosMeshRendererCache_p::updateCache( QgsMeshLayer *layer, const QgsRenderContext &renderContext )
{
  if ( updateDataset( layer, renderContext ) ||  updateExtent( renderContext ) )
  {
    if ( mTracesSettings.dynamicTracesEnable )
      mTraceController.resetData( layer, renderContext, mDatasetGroupsIndex, mTracesSettings );
  }
}

QImage ReosMeshRendererCache_p::traceImage() const
{
  return mTraceController.lastTracesImage();
}

quint64 ReosMeshRendererCache_p::tracesAges() const
{
  return mTraceController.traceAges();
}

bool ReosMeshRendererCache_p::updateDataset( QgsMeshLayer *layer, const QgsRenderContext &renderContext )
{
  //****** get the current dataset index
  QgsMeshDatasetIndex vectorDatasetIndex;
  if ( renderContext.isTemporal() )
    vectorDatasetIndex = layer->activeVectorDatasetAtTime( renderContext.temporalRange() );
  else
    vectorDatasetIndex = layer->staticVectorDatasetIndex();

  QgsMeshDatasetIndex scalarDatasetIndex;
  if ( renderContext.isTemporal() )
    scalarDatasetIndex = layer->activeScalarDatasetAtTime( renderContext.temporalRange() );
  else
    scalarDatasetIndex = layer->staticScalarDatasetIndex();

  if ( mScalarDataset != scalarDatasetIndex || mVectorDataset != vectorDatasetIndex )
  {
    mStaticRendering = QImage();
    mScalarDataset = scalarDatasetIndex;
    mVectorDataset = vectorDatasetIndex;
    return true;
  }

  return false;
}

bool ReosMeshRendererCache_p::updateExtent( const QgsRenderContext &renderContext )
{
  const QRectF newExtent = renderContext.extent().toRectF();
  if ( newExtent != mExtent )
  {
    mStaticRendering = QImage();
    mExtent = newExtent;
    return true;
  }

  return false;
}

const QRectF &ReosMeshRendererCache_p::extent() const
{
  return mExtent;
}

const QImage &ReosMeshRendererCache_p::staticRendering() const
{
  return mStaticRendering;
}

void ReosMeshRendererCache_p::updateInternalCache( ReosMeshRenderer_p *renderer )
{
  if ( renderer && renderer->extent() == mExtent )
    mStaticRendering = renderer->staticRendering();
}


ReosMovingTracesController::ReosMovingTracesController( QObject *parent )
  : QObject( parent )
{
}

ReosMovingTracesController::~ReosMovingTracesController()
{
  if ( mRenderer )
    mRenderer->feedback()->cancel();
  emit askStop();
  mThread.quit();
  mThread.wait();
  if ( mRenderer )
    mRenderer->deleteLater();
}

void ReosMovingTracesController::resetData(
  QgsMeshLayer *layer,
  const QgsRenderContext &context,
  int vectorDatasetGroupIndex,
  const DynamicTracesSettings &tracesSettings )
{
  mTracesSettings = tracesSettings;

  mThread.quit();
  mThread.wait();
  if ( mRenderer )
    mRenderer->deleteLater();

  mTracesAge = 0;
  mLastTracesImage = QImage();

  //*****************
  QgsMeshDatasetIndex vectorDatasetIndex = layer->datasetIndexAtTime( context.temporalRange(), vectorDatasetGroupIndex );
  if ( !vectorDatasetIndex.isValid() )
    return;

  QElapsedTimer timer;
  timer.start();
  const QgsMeshDatasetGroupMetadata metadata = layer->datasetGroupMetadata( vectorDatasetIndex );
  const QgsMesh nativeMesh = *layer->nativeMesh();

  QgsMeshDatasetGroupMetadata::DataType mVectorDataType = QgsMeshLayerUtils::datasetValuesType( metadata.dataType() );

  double vectorDatasetGroupMagMaximum = metadata.maximum();

  const int count = QgsMeshLayerUtils::datasetValuesCount( &nativeMesh, mVectorDataType );
  const QgsMeshDataBlock vectorDatasetValues = QgsMeshLayerUtils::datasetValues( layer, vectorDatasetIndex, 0, count );

  // populate face active flag
  QgsMeshDataBlock vectorActiveFaceFlagValues = layer->areFacesActive( vectorDatasetIndex, 0, nativeMesh.faces.count() );

  mRenderer = new ReosMovingTracesRenderer(
    layer,
    vectorDatasetGroupIndex,
    context,
    vectorDatasetValues,
    vectorActiveFaceFlagValues,
    vectorDatasetGroupMagMaximum,
    tracesSettings );
  //*****************

  mRenderer->moveToThread( &mThread );

  connect( &mThread, &QThread::started, mRenderer, &ReosMovingTracesRenderer::start );
  connect( this, &ReosMovingTracesController::askStop, mRenderer, &ReosMovingTracesRenderer::stop );
  connect( mRenderer, &ReosMovingTracesRenderer::imageReady, this, &ReosMovingTracesController::setLastImage );

  start();
}

void ReosMovingTracesController::start()
{
  mThread.start();
}

void ReosMovingTracesController::stop()
{
  if ( mRenderer )
    mRenderer->feedback()->cancel();
  emit askStop();
}

quint64 ReosMovingTracesController::traceAges() const
{
  return mTracesAge;
}
QImage ReosMovingTracesController::lastTracesImage() const
{
  return mLastTracesImage;
}

void ReosMovingTracesController::setLastImage( QImage img, quint64 tracesAge )
{
  mLastTracesImage = img;
  mTracesAge = tracesAge;
  emit imageReady();
}

ReosMovingTracesRenderer::ReosMovingTracesRenderer(
  QgsMeshLayer *layer,
  int datasetGroupindex,
  const QgsRenderContext &context,
  const QgsMeshDataBlock &datasetVectorValues,
  const QgsMeshDataBlock &scalarActiveFaceFlagValues,
  double magnitudeMaximum,
  const DynamicTracesSettings &tracesSettings )
  : mFeedBack( new QgsFeedback )
  , mRenderContext( context )
  , mOutputSize( context.outputSize() )
  , mTimer( new QTimer( this ) )
{
  mRenderContext.setFeedback( mFeedBack.get() );
  connect( mTimer, &QTimer::timeout, this, &ReosMovingTracesRenderer::moveParticles );

  const QgsTriangularMesh triMesh = *layer->triangularMesh();
  const QgsMeshRendererSettings &settings = layer->rendererSettings();
  const QgsMeshDatasetGroupMetadata metadata = layer->datasetGroupMetadata( datasetGroupindex );

  mTraceGenerator.reset(
    new QgsMeshVectorTraceAnimationGenerator(
      triMesh,
      datasetVectorValues,
      scalarActiveFaceFlagValues,
      metadata.dataType() == QgsMeshDatasetGroupMetadata::DataOnVertices,
      mRenderContext,
      layer->extent(),
      magnitudeMaximum,
      settings.vectorSettings( datasetGroupindex ) ) );

  mTraceGenerator->setFPS( tracesSettings.fps );
  mTraceGenerator->setParticlesLifeTime( static_cast<double>( tracesSettings.lifeTime ) );
  mTraceGenerator->setTailFactor( tracesSettings.tailFactor );
  mTraceGenerator->setTailPersitence( tracesSettings.persistence );
  mTraceGenerator->setMaxSpeedPixel( tracesSettings.maxSpeed );
  mTraceGenerator->setParticlesSize(
    mRenderContext.convertToPainterUnits( settings.vectorSettings( datasetGroupindex ).lineWidth(), QgsUnitTypes::RenderUnit::RenderMillimeters ) );

  //****  For now, the class QgsMeshVectorTraceAnimationGenerator return an image with the effective extent of the layer int he map
  // We can't access to the top left point in device coordinate of the top left point, this an issue that has to be fixed in QGIS
  // So For now, we have to calculate here.
  QgsRectangle layerExtentInMapCoordinates;
  try
  {
    QgsCoordinateTransform extentTransform = mRenderContext.coordinateTransform();
    extentTransform.setBallparkTransformsAreAppropriate( true );
    layerExtentInMapCoordinates = extentTransform.transformBoundingBox( layer->extent() );
  }
  catch ( QgsCsException &cse )
  {
    Q_UNUSED( cse )
    //if the transform fails, consider the whole map
    layerExtentInMapCoordinates = mRenderContext.mapExtent();
  }

  QgsRectangle interestZoneExtent = layerExtentInMapCoordinates.intersect( mRenderContext.mapExtent() );

  if ( interestZoneExtent != QgsRectangle() )
  {
    QgsRectangle fieldInterestZoneInDeviceCoordinates = QgsMeshLayerUtils::boundingBoxToScreenRectangle( mRenderContext.mapToPixel(), interestZoneExtent );
    mTopLeft = QPoint( static_cast<int>( std::round( fieldInterestZoneInDeviceCoordinates.xMinimum() ) ),
                       static_cast<int>( std::round( fieldInterestZoneInDeviceCoordinates.yMinimum() ) ) );
  }
  //*************************************************
}

void ReosMovingTracesRenderer::resetVectorDataset( const QgsMeshDataBlock &datasetVectorValues, const QgsMeshDataBlock &scalarActiveFaceFlagValues, double magnitudeMaximum )
{
  //mTraceField->resetVectorDataset( datasetVectorValues, scalarActiveFaceFlagValues, magnitudeMaximum );
}

void ReosMovingTracesRenderer::start()
{
  mTracesAges = 0;
  mTraceGenerator->seedRandomParticles( 1000 );
  if ( !mFeedBack->isCanceled() )
  {
    mTimer->start( static_cast<int>( 1000.0 / static_cast<double>( mFramePerSeconds ) ) );
  }
}

void ReosMovingTracesRenderer::stop()
{
  mTimer->stop();
}

void ReosMovingTracesRenderer::moveParticles()
{
  QImage img = mTraceGenerator->imageRendered();
  QImage   output( mOutputSize, QImage::Format_ARGB32 );
  if ( !output.isNull() && !img.isNull() )
  {
    output.fill( 0X00000000 );
    QPainter painter( &output );
    painter.drawImage( mTopLeft, img );
    mTracesAges++;
    if ( !mFeedBack->isCanceled() )
      emit imageReady( output, mTracesAges );
  }
}
