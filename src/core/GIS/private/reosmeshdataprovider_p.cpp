/***************************************************************************
  reosmeshdataprovider.cpp - ReosMeshDataProvider

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
#include "reosmeshdataprovider_p.h"
#include "reosmeshgenerator.h"

int ReosMeshDataProvider_p::vertexCount() const
{
  return mCacheMesh.vertexCount();
}

int ReosMeshDataProvider_p::faceCount() const
{
  return mCacheMesh.faceCount();
}

void ReosMeshDataProvider_p::populateMesh( QgsMesh *mesh ) const
{
  *mesh = mCacheMesh;
}

bool ReosMeshDataProvider_p::generateMesh( const ReosMeshGenerator &generator )
{
  bool ok = true;
  ReosMeshFrameData data = generator.generatedMesh( &ok );
  if ( ok )
  {
    mCacheMesh = convertFrameFromReos( data );
    emit dataChanged();
  }
  return ok;
}

QgsMesh ReosMeshDataProvider_p::convertFrameFromReos( const ReosMeshFrameData &reosMesh )
{
  QgsMesh ret;

  ret.vertices.resize( reosMesh.vertexCoordinates.count() / 3 );
  for ( int i = 0; i < ret.vertices.size(); ++i )
    ret.vertices[i] = QgsMeshVertex( reosMesh.vertexCoordinates[i * 3],
                                     reosMesh.vertexCoordinates[i * 3 + 1],
                                     reosMesh.vertexCoordinates[i * 3 + 2] );

  ret.faces.resize( reosMesh.facesIndexes.size() );

  ret.faces = reosMesh.facesIndexes;
  return ret;
}
