/***************************************************************************
  reosgriddeddata.cpp - ReosGriddedData

 ---------------------
 begin                : 25.6.2024
 copyright            : (C) 2024 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosgriddeddata.h"

#include "reosgriddedrainfallprovider.h"
#include "reosgriddedrainfallrenderer_p.h"


ReosGriddedData::ReosGriddedData( QObject *parent )
  : ReosRenderedObject( parent )
{
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

ReosGriddedData::ReosGriddedData( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosRenderedObject( parent )
  , mProvider( qobject_cast<ReosGriddedDataProvider*>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) )
{
  if ( mProvider )
  {
    makeConnection();
    mProvider->setDataSource( dataSource );
  }
  // renderer factory must be created after set the datasource because, the factory needs the extent of the provider on creation
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

void ReosGriddedData::makeConnection()
{
  if ( mProvider )
  {
    connect( mProvider.get(), &ReosDataProvider::dataChanged, this, &ReosDataObject::dataChanged );
    connect( mProvider.get(), &ReosDataProvider::dataReset, this, &ReosDataObject::dataReset );
    connect( mProvider.get(), &ReosDataProvider::loadingFinished, this, &ReosGriddedData::loadingFinished );
  }

  connect( this, &ReosDataObject::dataChanged, this, &ReosRenderedObject::repaintRequested );
}

void ReosGriddedData::decodeProvider( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  ReosDataObject::decode( element );
  QString providerKey;
  element.getData( QStringLiteral( "provider-key" ), providerKey );
  mProvider.reset( qobject_cast<ReosGriddedDataProvider *>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) );
  if ( mProvider )
    mProvider->decode( element.getEncodedData( QStringLiteral( "provider" ) ), context );
}

void ReosGriddedData::setProvider( ReosGriddedDataProvider *provider )
{
  mProvider.reset( provider );
}

void ReosGriddedData::setRenderer( ReosGriddedRainfallRendererFactory *rendererFactory )
{
  mRendererFactory.reset( rendererFactory );
}

ReosGriddedRainfallRendererFactory *ReosGriddedData::renderer() const
{
  mRendererFactory.get();
}

QString ReosGriddedData::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;

  return rawKey + QStringLiteral( "::" ) + ReosGriddedData::staticType();
}

void ReosGriddedData::updateData() const
{
  if ( mProvider )
    mProvider->load();
}


QString ReosGriddedData::type() const {return staticType();}

ReosObjectRenderer *ReosGriddedData::createRenderer( ReosRendererSettings *settings )
{
  if ( mRendererFactory )
    return mRendererFactory->createRasterRenderer( settings );

  return nullptr;
}

ReosRendererObjectMapTimeStamp *ReosGriddedData::createMapTimeStamp( ReosRendererSettings *settings ) const
{
  return new ReosRendererGriddedRainfallMapTimeStamp_p( dataIndex( settings->mapTime() ) );
}

ReosMapExtent ReosGriddedData::extent() const
{
  return rasterExtent();
}

QString ReosGriddedData::staticType() {return QStringLiteral( "gridded-data" );}

bool ReosGriddedData::isValid() const
{
    return mProvider && mProvider->isValid();
}

int ReosGriddedData::gridCount() const
{
    if ( mProvider )
        return mProvider->count();
    else
        return 0;
}

const QVector<double> ReosGriddedData::values( int index ) const
{
  if ( mProvider )
    return mProvider->data( index );
  else
    return QVector<double>();
}

const QVector<double> ReosGriddedData::valuesInGridExtent( int index, int rowMin, int rowMax, int colMin, int colMax ) const
{
  if ( !mProvider )
    return QVector<double>();

  if ( index < 0 )
    return QVector<double>();

  if ( mProvider->hasCapability( ReosGriddedRainfallProvider::SubGridExtract ) )
  {
    return mProvider->dataInGridExtent( index, rowMin, rowMax, colMin, colMax );
  }

  const QVector<double> &data = mProvider->data( index );

  ReosRasterExtent extent = mProvider->extent();
  int colRawCount = extent.xCellCount();

  int rowCount = rowMax - rowMin + 1;
  int colCount = colMax - colMin + 1;
  QVector<double> ret;
  ret.resize( rowCount * colCount );
  for ( int r = 0; r < rowCount; ++r )
    for ( int c = 0; c < colCount; ++c )
    {
      int rawIndex = c + colMin + ( r + rowMin ) * colRawCount;
      int locIndex = c + r * colCount;
      ret[locIndex] = data.at( rawIndex );
    }

  return ret;
}

const QDateTime ReosGriddedData::startTime( int index ) const
{
  if ( mProvider )
    return mProvider->startTime( index );
  else
    return QDateTime();
}

const QDateTime ReosGriddedData::endTime( int index ) const
{
  if ( mProvider )
    return mProvider->endTime( index );
  else
    return QDateTime();
}

QPair<QDateTime, QDateTime> ReosGriddedData::timeExtent() const
{
  if ( !mProvider )
    return QPair<QDateTime, QDateTime>();

  int count = mProvider->count();

  if ( count == 0 )
    return QPair<QDateTime, QDateTime>();

  return {mProvider->startTime( 0 ), mProvider->endTime( count - 1 )};
}

ReosDuration ReosGriddedData::minimumTimeStep() const
{
  if ( !mProvider )
    return ReosDuration();

  int count = mProvider->count();

  if ( count == 0 )
    return ReosDuration();

  ReosDuration ret = ReosDuration( mProvider->startTime( 0 ), mProvider->endTime( 0 ) );

  for ( int i = 1; i < count; ++i )
  {
    ReosDuration dt( mProvider->startTime( i ), mProvider->endTime( i ) );
    if ( dt < ret )
      ret = dt;
  }

  return ret;
}

bool ReosGriddedData::supportExtractSubGrid() const
{
  if ( mProvider )
    return mProvider->hasCapability( ReosGriddedRainfallProvider::SubGridExtract );

  return false;
}


int ReosGriddedData::dataIndex( const QDateTime &time ) const
{
  if ( !mProvider )
    return  -1;

  return mProvider->dataIndex( time );
}

ReosRasterExtent ReosGriddedData::rasterExtent() const
{
  if ( mProvider )
  {
    if ( mOverridenCrs.isEmpty() )
      return mProvider->extent();
    ReosRasterExtent extent = mProvider->extent();
    extent.setCrs( mOverridenCrs );
    return extent;
  }
  else
    return ReosRasterExtent();
}

void ReosGriddedData::copyFrom( ReosGriddedData *other )
{
  copyFrom( other->mProvider.get() );
}

void ReosGriddedData::copyFrom( ReosGriddedDataProvider *provider )
{
  mProvider->copyFrom( provider );

  //! We need to reset the renderer factor tp take account of the new extent
  mRendererFactory.reset( new ReosGriddedRainfallRendererFactory_p( this ) );
}

QList<ReosColorShaderSettings *> ReosGriddedData::colorShaderSettings() const
{
  QList<ReosColorShaderSettings *> ret;
  ret << mRendererFactory->colorRampShaderSettings();
  return ret;
}

bool ReosGriddedData::getDirectMinMaxValue( double &min, double &max ) const
{
  if ( mProvider )
    return mProvider->getDirectMinMax( min, max );
  return false;
}

void ReosGriddedData::calculateMinMaxValue( double &min, double &max ) const
{
  if ( mProvider )
    mProvider->calculateMinMax( min, max );
}

