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

#include "reosmeshdataprovider_p.h"

ReosMesh_p::ReosMesh_p()
{
  mMeshLayer.reset( new QgsMeshLayer( "memory mesh", "some tries", QStringLiteral( "ReosMeshMemory" ) ) );
  mMeshLayer->setCrs( QgsProject::instance()->crs() );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererMeshSettings meshSettings = settings.nativeMeshSettings();
  meshSettings.setEnabled( true );
  settings.setNativeMeshSettings( meshSettings );
  mMeshLayer->setRendererSettings( settings );

  QgsCoordinateTransform transform( QgsProject::instance()->crs(), QgsProject::instance()->crs(), QgsProject::instance() );
  mMeshLayer->updateTriangularMesh( transform );
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

bool ReosMesh_p::generateMesh( const ReosMeshGenerator &generator )
{
  bool ok = meshProvider()->generateMesh( generator );
  if ( ok )
    mMeshLayer->reload();
  return ok;
}

ReosMeshDataProvider_p *ReosMesh_p::meshProvider()
{
  return qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
}

