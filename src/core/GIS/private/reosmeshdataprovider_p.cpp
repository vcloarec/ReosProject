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
#include <qgsproviderregistry.h>

#include "reosmeshdataprovider_p.h"
#include "reosmeshgenerator.h"
#include "reosdigitalelevationmodel.h"
#include "reostopographycollection_p.h"

int ReosMeshDataProvider_p::vertexCount() const
{
  return mMesh.vertexCount();
}

int ReosMeshDataProvider_p::faceCount() const
{
  return mMesh.faceCount();
}

void ReosMeshDataProvider_p::populateMesh( QgsMesh *mesh ) const
{
  *mesh = mMesh;
}

bool ReosMeshDataProvider_p::saveMeshFrameToFile( const QgsMesh &mesh )
{
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );
  if ( meta )
    return meta->createMeshData( mesh, mFilePath, mMDALDriverName, mCrs );
  else
    return false;
}

bool ReosMeshDataProvider_p::saveMeshFrame( const QgsMesh &mesh )
{
  mMesh = mesh;
  return true;
}

QgsRectangle ReosMeshDataProvider_p::extent() const
{
  return mExtent;
}

QgsMeshDriverMetadata ReosMeshDataProvider_p::driverMetadata() const
{
  return QgsMeshDriverMetadata( QStringLiteral( "ReosMeshMemory" ),
                                QStringLiteral( "reos mesh" ),
                                QgsMeshDriverMetadata::CanWriteMeshData, QString(),
                                QStringLiteral( "*.nc" ),
                                3 );
}

void ReosMeshDataProvider_p::loadMeshFrame( const QString &filePath, const QString &driverName )
{
  mMDALDriverName = driverName;
  mFilePath = filePath;
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );
  if ( meta )
  {
    QgsDataProvider::ProviderOptions options;
    std::unique_ptr<QgsMeshDataProvider> dataProvider( qobject_cast<QgsMeshDataProvider *>( meta->createProvider( filePath, options ) ) );
    dataProvider->populateMesh( &mMesh );
    mExtent = dataProvider->extent();
    emit dataChanged();
  }

}

void ReosMeshDataProvider_p::setFilePath( const QString &filePath )
{
  mFilePath = filePath;
}

void ReosMeshDataProvider_p::setMDALDriver( const QString &driverName )
{
  mMDALDriverName = driverName;
}

void ReosMeshDataProvider_p::generateMesh( const ReosMeshFrameData &data )
{
  mMesh = convertFrameFromReos( data );
  mExtent = data.extent;
  emit dataChanged();
}

void ReosMeshDataProvider_p::applyDemOnVertices( ReosDigitalElevationModel *dem )
{
  double noDataValue = dem->noDataValue();
  QString wktCrs = mCrs.toWkt( QgsCoordinateReferenceSystem::WKT2_2019_SIMPLIFIED );

  for ( int i = 0; i < mMesh.vertexCount(); ++i )
  {
    QPointF pt( mMesh.vertices.at( i ).toQPointF() );
    double value = dem->elevationAt( pt, wktCrs );
    if ( value != noDataValue )
      mMesh.vertices[i].setZ( value );
    else
      mMesh.vertices[i].setZ( std::numeric_limits<double>::quiet_NaN() );
  }
  emit dataChanged();
}

void ReosMeshDataProvider_p::applyTopographyOnVertices( ReosTopographyCollection *topographyCollection )
{
  ReosTopographyCollection_p *topoCollection = qobject_cast<ReosTopographyCollection_p *>( topographyCollection );

  if ( !topoCollection )
    return;

  topoCollection->prepare_p( mCrs );
  for ( int i = 0; i < mMesh.vertexCount(); ++i )
  {
    double value = topoCollection->elevationAt_p( mMesh.vertices.at( i ) );
    if ( !std::isnan( value ) )
      mMesh.vertices[i].setZ( value );
    else
      mMesh.vertices[i].setZ( std::numeric_limits<double>::quiet_NaN() );
  }
  topoCollection->clean_p();
  emit dataChanged();
}

void ReosMeshDataProvider_p::overrideCrs( const QgsCoordinateReferenceSystem &crs )
{
  mCrs = crs;
}

QgsMesh ReosMeshDataProvider_p::convertFrameFromReos( const ReosMeshFrameData &reosMesh )
{
  QgsMesh ret;

  ret.vertices.resize( reosMesh.vertexCoordinates.count() / 3 );
  bool hasZ = reosMesh.hasZ;
  for ( int i = 0; i < ret.vertices.size(); ++i )
  {
    ret.vertices[i] = QgsMeshVertex( reosMesh.vertexCoordinates[i * 3],
                                     reosMesh.vertexCoordinates[i * 3 + 1],
                                     reosMesh.vertexCoordinates[i * 3 + 2] );
    // if Z is NaN, constructor QgsMeshVertex create a 2D point, so need to set Z value to NaN after creation
    if ( !hasZ )
      ret.vertices[i].setZ( std::numeric_limits<double>::quiet_NaN() );
  }

  ret.faces.resize( reosMesh.facesIndexes.size() );

  ret.faces = reosMesh.facesIndexes;
  return ret;
}
