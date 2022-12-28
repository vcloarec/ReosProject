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

#include "reosduration.h"
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

int ReosMeshDataProvider_p::edgeCount() const {return 0;}

void ReosMeshDataProvider_p::populateMesh( QgsMesh *mesh ) const
{
  *mesh = mMesh;
}

bool ReosMeshDataProvider_p::saveMeshFrameToFile( const QgsMesh &mesh )
{
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );
  if ( meta )
    return meta->createMeshData( mesh, mFilePath, "Ugrid", mCrs );
  else
    return false;
}

void ReosMeshDataProvider_p::setDatasetSource( ReosMeshDatasetSource *datasetSource )
{
  mDatasetSource = datasetSource;

  mTemporalCapabilities->clear();

  if ( mDatasetSource && mDatasetSource->groupCount() > 0 )
  {
    mTemporalCapabilities->setHasTemporalCapabilities( true );
    for ( int i = 0; i < mDatasetSource->groupCount(); ++i )
    {
      mTemporalCapabilities->addGroupReferenceDateTime( i, mDatasetSource->groupReferenceTime( i ) );
      int datasetCount = mDatasetSource->datasetCount( i );
      for ( int j = 0; j < datasetCount; ++j )
        mTemporalCapabilities->addDatasetTimeInMilliseconds( i, mDatasetSource->datasetRelativeTime( i, j ).valueMilliSecond() );
    }

    emit datasetGroupsAdded( datasetSource->groupCount() );
  }
}

int ReosMeshDataProvider_p::datasetGroupCount() const
{
  if ( !mDatasetSource )
    return 0;

  return mDatasetSource->groupCount();
}

int ReosMeshDataProvider_p::datasetCount( int groupIndex ) const
{
  if ( !mDatasetSource )
    return 0;

  return mDatasetSource->datasetCount( groupIndex );
}

QgsMeshDatasetGroupMetadata ReosMeshDataProvider_p::datasetGroupMetadata( int groupIndex ) const
{
  if ( !mDatasetSource )
    return QgsMeshDatasetGroupMetadata();

  const QString name = mDatasetSource->groupName( groupIndex );
  const QString uri;
  bool isScalar = mDatasetSource->groupIsScalar( groupIndex );
  QgsMeshDatasetGroupMetadata::DataType dataType;
  switch ( mDatasetSource->groupLocation( groupIndex ) )
  {
    case ReosMeshDatasetSource::Location::Vertex:
      dataType = QgsMeshDatasetGroupMetadata::DataOnVertices;
      break;
    case ReosMeshDatasetSource::Location::Face:
      dataType = QgsMeshDatasetGroupMetadata::DataOnFaces;
      break;
  }

  double minimum;
  double maximum;
  mDatasetSource->groupMinMax( groupIndex, minimum, maximum );
  int maximumVerticalLevels = 0;
  const QDateTime referenceTime = mDatasetSource->groupReferenceTime( groupIndex );
  const QMap<QString, QString> extraOptions;

  return QgsMeshDatasetGroupMetadata( name,
                                      uri,
                                      isScalar,
                                      dataType,
                                      minimum,
                                      maximum,
                                      maximumVerticalLevels,
                                      referenceTime,
                                      true,
                                      extraOptions );
}

QgsMeshDatasetMetadata ReosMeshDataProvider_p::datasetMetadata( QgsMeshDatasetIndex index ) const
{
  if ( !mDatasetSource )
    return QgsMeshDatasetMetadata();

  ReosDuration time = mDatasetSource->datasetRelativeTime( index.group(), index.dataset() );
  bool isValid = mDatasetSource->datasetIsValid( index.group(), index.dataset() );
  double minimum;
  double maximum;
  mDatasetSource->datasetMinMax( index.group(), index.dataset(), minimum, maximum );
  int maximumVerticalLevels = 0;

  return QgsMeshDatasetMetadata( time.valueHour(), isValid, minimum, maximum, maximumVerticalLevels );
}

QgsMeshDatasetValue ReosMeshDataProvider_p::datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const
{
  if ( !mDatasetSource )
    return QgsMeshDatasetValue();

  QgsMeshDataBlock vals = datasetValues( index, valueIndex, 1 );
  return vals.value( 0 );
}

QgsMeshDataBlock ReosMeshDataProvider_p::datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const
{
  if ( !mDatasetSource )
    return QgsMeshDataBlock();

  bool isScalar = mDatasetSource->groupIsScalar( index.group() );

  const QVector<double> values = mDatasetSource->datasetValues( index.group(), index.dataset() );
  QgsMeshDataBlock ret( isScalar ? QgsMeshDataBlock::ScalarDouble : QgsMeshDataBlock::Vector2DDouble, count );
  QVector<double> buffer;
  int effectiveCount = count * ( isScalar ? 1 : 2 );
  if ( valueIndex == 0 && effectiveCount == values.count() )
  {
    buffer = values;
  }
  else
  {
    effectiveCount = std::min( count, values.count() - valueIndex );
    if ( !isScalar )
      effectiveCount = effectiveCount * 2;
    if ( effectiveCount > 0 )
    {
      buffer.resize( effectiveCount );
      memcpy( buffer.data(), &values[valueIndex], static_cast<size_t>( effectiveCount )*sizeof( double ) );
    }
  }
  ret.setValues( buffer );
  return ret;
}

QgsMesh3dDataBlock ReosMeshDataProvider_p::dataset3dValues( QgsMeshDatasetIndex, int, int ) const
{
  return QgsMesh3dDataBlock();
}

bool ReosMeshDataProvider_p::isFaceActive( QgsMeshDatasetIndex, int ) const {return true;}

QgsMeshDataBlock ReosMeshDataProvider_p::areFacesActive( QgsMeshDatasetIndex index, int valueIndex, int count ) const
{
  QgsMeshDataBlock ret( QgsMeshDataBlock::ActiveFlagInteger, count );

  if ( !mDatasetSource )
    return QgsMeshDataBlock();

  QVector<int> values = mDatasetSource->activeFaces( index.dataset() );

  if ( values.isEmpty() )
    return ret;

  ret.setValid( true );
  QVector<int> buffer;
  if ( valueIndex == 0 && count == values.count() )
  {
    buffer = values;
  }
  else
  {
    int effectiveCount = std::min( count, values.count() - valueIndex );
    if ( effectiveCount > 0 )
    {
      buffer.resize( effectiveCount );
      memcpy( buffer.data(), &values[valueIndex], static_cast<size_t>( effectiveCount )*sizeof( int ) );
    }
  }

  ret.setActive( buffer );
  return ret;
}

bool ReosMeshDataProvider_p::persistDatasetGroup( const QString &outputFilePath, const QString &outputDriver, const QgsMeshDatasetGroupMetadata &meta, const QVector<QgsMeshDataBlock> &datasetValues, const QVector<QgsMeshDataBlock> &datasetActive, const QVector<double> &times ) {return false;}

bool ReosMeshDataProvider_p::persistDatasetGroup( const QString &outputFilePath, const QString &outputDriver, QgsMeshDatasetSourceInterface *source, int datasetGroupIndex )
{return false;}

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

void ReosMeshDataProvider_p::loadMeshFrame( const QString &filePath, ReosModule::Message &message )
{
  mFilePath = filePath;
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );
  if ( meta )
  {
    QgsDataProvider::ProviderOptions options;
    std::unique_ptr<QgsMeshDataProvider> dataProvider( qobject_cast<QgsMeshDataProvider *>( meta->createProvider( filePath, options ) ) );
    dataProvider->populateMesh( &mMesh );
    mExtent = dataProvider->extent();
    overrideCrs( dataProvider->crs() );
    emit dataChanged();
  }
  else
  {
    message.type = ReosModule::Error;
    message.addText( tr( "MDAL not found, verify your installation." ) );
  }
}

ReosMeshDataProvider_p::ReosMeshDataProvider_p(): QgsMeshDataProvider( "mesh", QgsDataProvider::ProviderOptions() )
{
}

void ReosMeshDataProvider_p::setFilePath( const QString &filePath )
{
  mFilePath = filePath;
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

void ReosMeshDataProvider_p::applyTopographyOnVertices( ReosTopographyCollection_p *topographyCollection, ReosProcess *process )
{
  ReosTopographyCollection_p *topoCollection = qobject_cast<ReosTopographyCollection_p *>( topographyCollection );

  if ( !topoCollection )
    return;

  int processStep = mMesh.vertexCount() / 100;

  if ( process )
  {
    process->setMaxProgression( mMesh.vertexCount() );
    process->setCurrentProgression( 0 );
  }

  if ( process )
    process->setInformation( tr( "Apply topography on Mesh" ) );

  for ( int i = 0; i < mMesh.vertexCount(); ++i )
  {
    double value = topoCollection->elevationAt_p( mMesh.vertices.at( i ) );

    if ( !std::isnan( value ) )
      mMesh.vertices[i].setZ( value );
    else
      mMesh.vertices[i].setZ( std::numeric_limits<double>::quiet_NaN() );

    if ( process && i % processStep == 0 )
    {
      process->setCurrentProgression( i );
      if ( process->isStop() )
        break;
    }
  }

  emit dataChanged();
}

void ReosMeshDataProvider_p::overrideCrs( const QgsCoordinateReferenceSystem &crs )
{
  if ( crs.isValid() )
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

ReosMeshProviderMetaData::ReosMeshProviderMetaData() : QgsProviderMetadata( QStringLiteral( "ReosMesh" ), QStringLiteral( "reos mesh" ) ) {}

ReosMeshDataProvider_p *ReosMeshProviderMetaData::createProvider( const QString &, const QgsDataProvider::ProviderOptions &, QgsDataProvider::ReadFlags )
{
  return new ReosMeshDataProvider_p();
}
