/***************************************************************************
  reoseditabletindataprovider.cpp - ReosEditableTinDataProvider

 ---------------------
 begin                : 12.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoseditabletindataprovider_p.h"

ReosEditableTinDataProvider::ReosEditableTinDataProvider(): QgsMeshDataProvider( QString(),
      QgsDataProvider::ProviderOptions() )
{
  mTriangulation = new ReosTriangularIrregularNetworkQgsDualEdge_p( this );
}

QgsMeshDatasetGroupMetadata ReosEditableTinDataProvider::datasetGroupMetadata( int ) const
{
  return QgsMeshDatasetGroupMetadata( QObject::tr( "Elevation" ),
                                      QString(),
                                      true,
                                      QgsMeshDatasetGroupMetadata::DataOnVertices,
                                      minimumElevation(),
                                      maximumElevation(),
                                      0,
                                      QDateTime(),
                                      false,
                                      QMap<QString, QString>() );
}

QgsMeshDatasetMetadata ReosEditableTinDataProvider::datasetMetadata( QgsMeshDatasetIndex ) const
{
  return QgsMeshDatasetMetadata( 0, true, minimumElevation(), maximumElevation(), 0 );
}

QgsMeshDatasetValue ReosEditableTinDataProvider::datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const
{
  if ( mTriangulation )
    return mTriangulation->minimumElevation();
  else
    return QgsMeshDatasetValue();
}

QgsMeshDataBlock ReosEditableTinDataProvider::datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const
{
  if ( !mTriangulation )
    return QgsMeshDataBlock();

  QgsMeshDataBlock ret( QgsMeshDataBlock::ScalarDouble, vertexCount() );
  QVector<double> zValue( vertexCount() );
  QgsMesh mesh = mTriangulation->triangulatedMesh();
  for ( int i = 0; i < mesh.vertices.count(); ++i )
    zValue[i] = mesh.vertices.at( i ).z();

  ret.setValues( zValue );

  return ret;
}

QgsMeshDataBlock ReosEditableTinDataProvider::areFacesActive( QgsMeshDatasetIndex index, int faceIndex, int count ) const
{
  QgsMeshDataBlock ret( QgsMeshDataBlock::ActiveFlagInteger, count );
  ret.setValid( true );
  return ret;
}

int ReosEditableTinDataProvider::vertexCount() const
{
  if ( mTriangulation )
    return mTriangulation->vertexCount();
  else
    return 0;
}

int ReosEditableTinDataProvider::faceCount() const
{
  if ( mTriangulation )
    return mTriangulation->triangleCount();
  else
    return 0;
}

void ReosEditableTinDataProvider::populateMesh( QgsMesh *mesh ) const
{
  *mesh = mTriangulation->triangulatedMesh();
}

QgsRectangle ReosEditableTinDataProvider::extent() const
{
  if ( mTriangulation )
    return mTriangulation->qgsExtent();
  else
    return QgsRectangle();
}

ReosTriangularIrregularNetwork *ReosEditableTinDataProvider::triangulation()
{
  return mTriangulation;
}

double ReosEditableTinDataProvider::minimumElevation() const
{
  if ( mTriangulation )
    return mTriangulation->minimumElevation();
  else
    return std::numeric_limits<double>::quiet_NaN();
}

double ReosEditableTinDataProvider::maximumElevation() const
{
  if ( mTriangulation )
    return mTriangulation->maximumElevation();
  else
    return std::numeric_limits<double>::quiet_NaN();
}
