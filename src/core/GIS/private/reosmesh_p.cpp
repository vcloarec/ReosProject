/***************************************************************************
  reosmesh_p.cpp - ReosMesh_p

 ---------------------
 begin                : 13.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmesh_p.h"

#include <qgsproject.h>
#include <qgsmesheditor.h>
#include <qgsmeshlayerrenderer.h>
#include <qgsmapcanvas.h>
#include <qgsrendercontext.h>
#include <qgsproviderregistry.h>

#include "reosmeshdataprovider_p.h"
#include "reosencodedelement.h"

ReosMeshFrame_p::ReosMeshFrame_p( const QString &crs, QObject *parent ): ReosMesh( parent )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );
  QgsCoordinateReferenceSystem qgsiCrs;
  qgsiCrs.createFromWkt( crs );
  mMeshLayer->setCrs( qgsiCrs );
  meshProvider()->overrideCrs( qgsiCrs );

  init();
}

ReosMeshFrame_p::ReosMeshFrame_p( const QString &dataPath )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );

  QDir dir( dataPath );
  if ( dir.exists() )
  {
    meshProvider()->loadMeshFrame( dir.filePath( QStringLiteral( "meshFrame.nc" ) ), QStringLiteral( "Ugrid" ) );
    mMeshLayer->reload();
  }

  init();
}


void ReosMeshFrame_p::init()
{
  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererMeshSettings meshSettings = settings.nativeMeshSettings();
  meshSettings.setEnabled( true );
  settings.setNativeMeshSettings( meshSettings );
  mMeshLayer->setRendererSettings( settings );

  QgsCoordinateTransform transform( QgsProject::instance()->crs(), QgsProject::instance()->crs(), QgsProject::instance() );
  mMeshLayer->updateTriangularMesh( transform );

  connect( mMeshLayer.get(), &QgsMapLayer::repaintRequested, this, &ReosMesh::repaintRequested );
}

void ReosMeshFrame_p::save( const QString &dataPath ) const
{
  QDir dir( dataPath );

  meshProvider()->setFilePath( dir.filePath( QStringLiteral( "meshFrame.nc" ) ) );
  meshProvider()->setMDALDriver( QStringLiteral( "Ugrid" ) );
  meshProvider()->saveMeshFrame( *mMeshLayer->nativeMesh() );
}

ReosEncodedElement ReosMeshFrame_p::meshSymbology() const
{
  QDomDocument doc( QStringLiteral( "mesh-layer" ) );
  QDomElement elem = doc.createElement( QStringLiteral( "symbology" ) );
  QgsReadWriteContext context;
  QString errorMessage;
  mMeshLayer->writeSymbology( elem, doc, errorMessage, context );
  doc.appendChild( elem );

  ReosEncodedElement encodedElem( QStringLiteral( "mesh-symbology" ) );
  encodedElem.addData( "xml-symbology", doc.toString() );

  return encodedElem;
}

void ReosMeshFrame_p::setMeshSymbology( const ReosEncodedElement &symbology )
{

  if ( symbology.description() != QStringLiteral( "mesh-symbology" ) )
    return;

  QString docString;
  symbology.getData( "xml-symbology", docString );

  QDomDocument doc( QStringLiteral( "mesh-layer" ) );
  if ( doc.setContent( docString ) )
  {
    QDomElement domElem = doc.firstChildElement( QStringLiteral( "symbology" ) );
    QgsReadWriteContext context;
    QString errorMessage;
    mMeshLayer->readSymbology( domElem, errorMessage, context );
  }

}

ReosObjectRenderer *ReosMeshFrame_p::createRenderer( QGraphicsView *view )
{
  return new ReosMeshRenderer_p( view, mMeshLayer.get() );
}

bool ReosMeshFrame_p::isValid() const
{
  if ( mMeshLayer )
    return mMeshLayer->isValid();

  return false;
}

void ReosMeshFrame_p::addVertex( const QPointF pt, double z, double tolerance )
{

}

int ReosMeshFrame_p::vertexCount() const
{
  return mMeshLayer->meshVertexCount();
}

int ReosMeshFrame_p::faceCount() const
{
  return mMeshLayer->meshFaceCount();
}

QString ReosMeshFrame_p::enableVertexElevationDataset( const QString &name )
{
  std::unique_ptr<QgsMeshDatasetGroup> group( new QgsMeshVerticesElevationDatasetGroup( name, mMeshLayer->nativeMesh() ) );

  mZVerticesDatasetGroup = group.get();
  QString id = addDatasetGroup( group.release() );

  int index = mDatasetGroupsIndex.value( id );
  mVerticesElevationDatasetIndex = index;

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( index );
  if ( scalarSettings.classificationMinimum() >= scalarSettings.classificationMaximum() )
  {
    QgsColorRampShader colorRamp = scalarSettings.colorRampShader();
    colorRamp.setMinimumValue( 0 );
    colorRamp.setMaximumValue( 0 );
    scalarSettings.setClassificationMinimumMaximum( 0, 0 );
    scalarSettings.setColorRampShader( colorRamp );
    settings.setScalarSettings( index, scalarSettings );
    mMeshLayer->setRendererSettings( settings );
  }

  return id;
}


QString ReosMeshFrame_p::addDatasetGroup( QgsMeshDatasetGroup *group )
{
  QString name = group->name();
  QString id = QUuid::createUuid().toString();
  mMeshLayer->addDatasets( group );
  QList<int> groupIndexes = mMeshLayer->datasetGroupsIndexes();
  int index = -1;
  for ( int i : groupIndexes )
  {
    QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( i ) );
    if ( meta.name() == name )
    {
      index = i;
      break;
    }
  }
  mDatasetGroupsIndex.insert( id, index );

  return id;
}

void ReosMeshFrame_p::firstUpdateOfTerrainScalarSetting()
{
  if ( !mZVerticesDatasetGroup || mVerticesElevationDatasetIndex == -1 )
    return;

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( mVerticesElevationDatasetIndex );
  QgsColorRampShader colorRamp = scalarSettings.colorRampShader();

  if ( colorRamp.colorRampItemList().count() < 2 )
  {
    double min = mZVerticesDatasetGroup->minimum();
    double max = mZVerticesDatasetGroup->maximum();

    if ( min <= max )
    {
      colorRamp.setMinimumValue( min );
      colorRamp.setMaximumValue( max );
      colorRamp.classifyColorRamp( 10, -1 );
      scalarSettings.setClassificationMinimumMaximum( min, max );
      scalarSettings.setColorRampShader( colorRamp );
      settings.setScalarSettings( mVerticesElevationDatasetIndex, scalarSettings );
      mMeshLayer->setRendererSettings( settings );
    }
  }
}


bool ReosMeshFrame_p::activateDataset( const QString &id )
{
  int index = mDatasetGroupsIndex.value( id, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  settings.setActiveScalarDatasetGroup( index );
  mMeshLayer->setRendererSettings( settings );

  return true;
}

void ReosMeshFrame_p::generateMesh( const ReosMeshFrameData &data )
{
  meshProvider()->generateMesh( data );
  mMeshLayer->reload();
  if ( mZVerticesDatasetGroup )
    mZVerticesDatasetGroup->setStatisticObsolete();
  mMeshLayer->trigger3DUpdate();
  emit repaintRequested();
}

QString ReosMeshFrame_p::crs() const
{
  return mMeshLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
}

QObject *ReosMeshFrame_p::data() const
{
  return mMeshLayer.get();
}

int ReosMeshFrame_p::datasetGroupIndex( const QString &id ) const
{
  return mDatasetGroupsIndex.value( id, -1 );
}

void ReosMeshFrame_p::applyTopographyOnVertices( ReosTopographyCollection *topographyCollection )
{
  meshProvider()->applyTopographyOnVertices( topographyCollection );
  mMeshLayer->reload();

  if ( mZVerticesDatasetGroup )
    mZVerticesDatasetGroup->setStatisticObsolete();

  firstUpdateOfTerrainScalarSetting();

  emit repaintRequested();
  mMeshLayer->trigger3DUpdate();
}

double ReosMeshFrame_p::datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const
{
  return mMeshLayer->datasetValue( QgsMeshDatasetIndex( datasetGroupIndex( datasetId ), 0 ), QgsPointXY( pos ) ).scalar();
}

ReosMeshDataProvider_p *ReosMeshFrame_p::meshProvider() const
{
  return qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
}


ReosMeshRenderer_p::ReosMeshRenderer_p( QGraphicsView *canvas, QgsMeshLayer *layer )
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( canvas );
  if ( mapCanvas )
  {
    const QgsMapSettings &settings = mapCanvas->mapSettings();
    mImage = QImage( settings.deviceOutputSize(), settings.outputImageFormat() );
    mImage.setDevicePixelRatio( settings.devicePixelRatio() );
    mImage.setDotsPerMeterX( static_cast<int>( settings.outputDpi() * 39.37 ) );
    mImage.setDotsPerMeterY( static_cast<int>( settings.outputDpi() * 39.37 ) );
    mImage.fill( Qt::transparent );

    mPainter.reset( new QPainter( &mImage ) );
    mRenderContext = QgsRenderContext::fromMapSettings( settings );
    mRenderContext.setPainter( mPainter.get() );
    mLayerRender.reset( layer->createMapRenderer( mRenderContext ) );
  }
}

void ReosMeshRenderer_p::render() const
{
  mLayerRender->render();
}

void ReosMeshRenderer_p::stopRendering()
{
  mRenderContext.setRenderingStopped( true );
}
