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

ReosMesh_p::ReosMesh_p( const QString &crs, QObject *parent ): ReosMesh( parent )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );
  mMeshLayer->setCrs( QgsProject::instance()->crs() );
  meshProvider()->overrideCrs( QgsProject::instance()->crs() );

  init();
}

ReosMesh_p::ReosMesh_p( const ReosEncodedElement &elem, const QString &dataPath )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );

  QString docString;
  elem.getData( "qgis-mesh-layer", docString );

  QDomDocument doc( QStringLiteral( "mesh-layer" ) );
  if ( doc.setContent( docString ) )
  {
    QDomElement domElem = doc.firstChildElement( QStringLiteral( "maplayer" ) );
    QgsReadWriteContext context;
    mMeshLayer->readLayerXml( domElem,  context );
  }

  QDir dir( dataPath );
  if ( dir.exists() )
  {
    meshProvider()->loadMeshFrame( dir.filePath( QStringLiteral( "meshFrame.nc" ) ), QStringLiteral( "Ugrid" ) );
    mMeshLayer->reload();
  }

  init();
}


void ReosMesh_p::init()
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

ReosEncodedElement ReosMesh_p::encode( const QString &dataPath ) const
{
  QDomDocument doc( QStringLiteral( "mesh-layer" ) );
  QDomElement elem = doc.createElement( QStringLiteral( "maplayer" ) );
  QgsReadWriteContext context;
  mMeshLayer->writeLayerXml( elem, doc, context );
  doc.appendChild( elem );

  ReosEncodedElement encodedElem( QStringLiteral( "reos-mesh" ) );
  encodedElem.addData( "qgis-mesh-layer", doc.toString() );

  QDir dir( dataPath );

  meshProvider()->setFilePath( dir.filePath( QStringLiteral( "meshFrame.nc" ) ) );
  meshProvider()->setMDALDriver( QStringLiteral( "Ugrid" ) );
  meshProvider()->saveMeshFrame( *mMeshLayer->nativeMesh() );

  return encodedElem;
}

bool ReosMesh_p::isValid() const
{
  if ( mMeshLayer )
    return mMeshLayer->isValid();

  return false;
}

void ReosMesh_p::addVertex( const QPointF pt, double z, double tolerance )
{

}

int ReosMesh_p::vertexCount() const
{
  return mMeshLayer->meshVertexCount();
}

int ReosMesh_p::faceCount() const
{
  return mMeshLayer->meshFaceCount();
}

void ReosMesh_p::render( QGraphicsView *canvas, QPainter *painter )
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( canvas );
  if ( mapCanvas )
  {
    QgsRenderContext renderContext = QgsRenderContext::fromMapSettings( mapCanvas->mapSettings() );
    renderContext.setPainter( painter );
    std::unique_ptr<QgsMapLayerRenderer> renderer;
    renderer.reset( mMeshLayer->createMapRenderer( renderContext ) );
    renderer->render();
  }
}

void ReosMesh_p::updateRendering( QGraphicsView *canvas )
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( canvas );
  if ( mapCanvas )
  {
    QgsRenderContext renderContext = QgsRenderContext::fromMapSettings( mapCanvas->mapSettings() );
    QImage img;
    QPainter *painter = new QPainter( &img );
    renderContext.setPainter( painter );
    std::unique_ptr<QgsMapLayerRenderer> renderer;
    renderer.reset( mMeshLayer->createMapRenderer( renderContext ) );
    renderer->render();
  }
}

QString ReosMesh_p::enableVertexElevationDataset( const QString &name )
{
  std::unique_ptr<QgsMeshDatasetGroup> group( new QgsMeshVerticesElevationDatasetGroup( name, mMeshLayer->nativeMesh() ) );

  mZVerticesDatasetGroup = group.get();
  QString id = addDatasetGroup( group.release() );

  int index = mDatasetGroupsIndex.value( id );
  mVerticesElevationDatasetIndex = index;

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( index );
  QgsColorRampShader colorRamp = scalarSettings.colorRampShader();
  colorRamp.setMinimumValue( 0 );
  colorRamp.setMaximumValue( 0 );
  scalarSettings.setClassificationMinimumMaximum( 0, 0 );
  scalarSettings.setColorRampShader( colorRamp );
  settings.setScalarSettings( index, scalarSettings );
  mMeshLayer->setRendererSettings( settings );

  return id;
}


QString ReosMesh_p::addDatasetGroup( QgsMeshDatasetGroup *group )
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

void ReosMesh_p::firstUpdateOfTerrainScalarSetting()
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


bool ReosMesh_p::activateDataset( const QString &id )
{
  int index = mDatasetGroupsIndex.value( id, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  settings.setActiveScalarDatasetGroup( index );
  mMeshLayer->setRendererSettings( settings );

  return true;
}

void ReosMesh_p::generateMesh( const ReosMeshFrameData &data )
{
  meshProvider()->generateMesh( data );
  mMeshLayer->reload();
  mMeshLayer->trigger3DUpdate();
}

QString ReosMesh_p::crs() const
{
  return mMeshLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
}

QObject *ReosMesh_p::data() const
{
  return mMeshLayer.get();
}

int ReosMesh_p::datasetGroupIndex( const QString &id ) const
{
  return mDatasetGroupsIndex.value( id, -1 );
}

void ReosMesh_p::applyTopographyOnVertices( ReosTopographyCollection *topographyCollection )
{
  meshProvider()->applyTopographyOnVertices( topographyCollection );
  mMeshLayer->reload();

  mZVerticesDatasetGroup->setStatisticObsolete();
  firstUpdateOfTerrainScalarSetting();

  emit repaintRequested();
  mMeshLayer->trigger3DUpdate();
}

ReosMeshDataProvider_p *ReosMesh_p::meshProvider() const
{
  return qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
}

