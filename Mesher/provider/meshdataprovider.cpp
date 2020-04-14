/***************************************************************************
                      meshdataprovider.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "meshdataprovider.h"


QgsMeshDatasetGroupMetadata TINProvider::datasetGroupMetadata( int groupIndex ) const
{
  if ( groupIndex != 0 )
    return QgsMeshDatasetGroupMetadata();

  QMap<QString, QString> extraOptions;

  extraOptions["By"] = "vcloarec";
  return QgsMeshDatasetGroupMetadata( tr( "Altitude terrain" ), true, QgsMeshDatasetGroupMetadata::DataOnVertices, -5, 20, 1, QDateTime(), false, extraOptions );
}

QgsMeshDatasetMetadata TINProvider::datasetMetadata( QgsMeshDatasetIndex index ) const
{
  if ( index.group() == 0 && index.dataset() == 0 )
  {
    return QgsMeshDatasetMetadata( 0, true, 0, 5, 0 );
  }

  return QgsMeshDatasetMetadata();
}

QgsMeshDatasetValue TINProvider::datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const {Q_UNUSED( index ); Q_UNUSED( valueIndex ); return QgsMeshDatasetValue();}

QgsMeshDataBlock TINProvider::datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const
{
  Q_UNUSED( valueIndex )

  if ( index.group() == 0 && index.dataset() == 0 )
  {
    QgsMeshDataBlock dataBlock( QgsMeshDataBlock::ScalarDouble, count );
    QVector<double> values( count );
    auto reader = mTin.getReader();
    int i = 0;
    while ( !reader->allVerticesReaden() )
    {
      double vert[3];
      reader->readOnlyVertex( vert );
      if ( i >= valueIndex && i < valueIndex + count )
      {
        values[i - valueIndex] = vert[2];
      }

      ++i;
    }
    dataBlock.setValues( values );
    dataBlock.setValid( true );
    return dataBlock;
  }
  else
    return QgsMeshDataBlock();
}

bool TINProvider::isFaceActive( QgsMeshDatasetIndex index, int faceIndex ) const
{
  Q_UNUSED( faceIndex );
  if ( index.group() == 0 && index.dataset() == 0 )
  {
    return true;
  }
  else
  {
    return false;
  }
}

QgsMeshDataBlock TINProvider::areFacesActive( QgsMeshDatasetIndex index, int faceIndex, int count ) const
{
  Q_UNUSED( index );
  Q_UNUSED( faceIndex );
  Q_UNUSED( count );

  QgsMeshDataBlock ret( QgsMeshDataBlock::ActiveFlagInteger, count );
  ret.setValid( true );
  return ret;

}

bool TINProvider::persistDatasetGroup( const QString &path, const QgsMeshDatasetGroupMetadata &meta, const QVector<QgsMeshDataBlock> &datasetValues, const QVector<QgsMeshDataBlock> &datasetActive, const QVector<double> &times )
{
  Q_UNUSED( path );
  Q_UNUSED( meta );
  Q_UNUSED( datasetValues );
  Q_UNUSED( datasetActive );
  Q_UNUSED( times );
  return false;
}

QgsMesh3dDataBlock TINProvider::dataset3dValues( QgsMeshDatasetIndex index, int faceIndex, int count ) const
{
  Q_UNUSED( index );
  Q_UNUSED( faceIndex );
  Q_UNUSED( count );

  return QgsMesh3dDataBlock();
}

int TINProvider::vertexCount() const
{
  return mTin.verticesCount();
}

int TINProvider::faceCount() const
{
  return mTin.facesCount();
}

int TINProvider::edgeCount() const
{
  return 0;
}

void TINProvider::populateMesh( QgsMesh *mesh ) const
{
  if ( !mesh )
    return;

  mesh->vertices.clear();
  mesh->faces.clear();

  std::unique_ptr<MeshIO> reader = mTin.getReader();
  int coordCount = reader->vertexCoordCount();

  if ( coordCount != 3 )
    return;

  while ( !reader->allVerticesReaden() )
  {
    QVector<double> vert( 3 );
    reader->readVertex( vert.data() );
    mesh->vertices.append( QgsMeshVertex( vert[0], vert[1], vert[2] ) );
  }

  while ( !reader->allFacesReaden() )
  {
    int verticesCount = reader->currentFaceVerticesCount();
    QVector<int> vert( verticesCount, 0 );
    reader->readFace( vert.data() );
    mesh->faces.append( vert );
  }


}

QgsCoordinateReferenceSystem TINProvider::crs() const
{
  return QgsCoordinateReferenceSystem( mTin.crs().c_str() );
}

QgsRectangle TINProvider::extent() const
{
  if ( faceCount() == 0 )
    return QgsRectangle();

  double xmin = 1e99;
  double ymin = 1e99;
  double xmax = -1e99;
  double ymax = -1e99;

  std::unique_ptr<MeshIO> reader = mTin.getReader();

  while ( !reader->allVerticesReaden() )
  {
    double vert[3];
    reader->readOnlyVertex( vert );
    if ( vert[0] >= xmax )
      xmax = vert[0];
    if ( vert[1] >= ymax )
      ymax = vert[1];
    if ( vert[0] <= xmin )
      xmin = vert[0];
    if ( vert[1] <= ymin )
      ymin = vert[1];
  }

  return QgsRectangle( xmin, ymin, xmax, ymax );
}



HdTinLayer::HdTinLayer( QString path ): QgsMeshLayer( path, "Editable mesh layer", "TIN" )
{
}
