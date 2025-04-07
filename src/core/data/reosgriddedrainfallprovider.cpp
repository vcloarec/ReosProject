/***************************************************************************
  reosgriddedrainfallprovider.cpp - ReosGriddedRainfallProvider

 ---------------------
 begin                : 11.11.2022
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
#include "reosgriddedrainfallprovider.h"
#include "reosgriddedrainitem.h"

QString ReosGriddedDataProvider::dataSource() const
{
  return mDataSource;
}

void ReosGriddedDataProvider::setDataSource( const QString &uri )
{
  mDataSource = uri;
  load();
}

ReosDuration ReosGriddedDataProvider::intervalDuration( int index ) const
{
  return ReosDuration( startTime( index ), endTime( index ) );
}

bool ReosGriddedDataProvider::hasCapability( GridCapability )  const {return false;}

ReosDuration ReosGriddedDataProvider::minimumTimeStep() const
{

  int frameCount = count();

  if ( frameCount == 0 )
    return ReosDuration();

  ReosDuration ret = ReosDuration( startTime( 0 ), endTime( 0 ) );

  for ( int i = 1; i < frameCount; ++i )
  {
    ReosDuration dt( startTime( i ), endTime( i ) );
    if ( dt < ret )
      ret = dt;
  }

  return ret;
}

int ReosGriddedDataProvider::dataIndex( const QDateTime &time ) const
{
  int frameCount = count();

  if ( frameCount == 0 )
    return -1;

  if ( mLastFrameIndex != -1 )
  {
    if ( time >= startTime( mLastFrameIndex )
         && time < endTime( mLastFrameIndex ) )
      return  mLastFrameIndex;


    if ( mLastFrameIndex < ( frameCount - 1 ) && time >= startTime( mLastFrameIndex + 1 ) &&
         time < endTime( mLastFrameIndex + 1 ) )
    {
      mLastFrameIndex = mLastFrameIndex + 1;
      return  mLastFrameIndex;
    }
  }

  for ( int i = 0; i < frameCount; ++i )
  {
    if ( time >= startTime( i ) &&
         time < endTime( i ) )
    {
      mLastFrameIndex = i;
      return  i;
    }
  }

  if ( time == endTime( frameCount - 1 ) )
  {
    mLastFrameIndex = frameCount - 1;
    return  frameCount - 1;
  }

  return  -1;
}

bool ReosGriddedDataProvider::hasData( const QString &, const ReosTimeWindow & ) const {return false;}

QString ReosGriddedRainfallMemoryProvider::key() const
{
  return staticKey();
}

int ReosGriddedRainfallMemoryProvider::count() const
{
  return mRasters.count();
}

QDateTime ReosGriddedRainfallMemoryProvider::startTime( int index ) const
{
  return mRasters.at( index ).startTime;
}

QDateTime ReosGriddedRainfallMemoryProvider::endTime( int index ) const {return mRasters.at( index ).endTime;}

const QVector<double> ReosGriddedRainfallMemoryProvider::data( int index ) const
{
  return mRasters.at( index ).raster.values();
}

ReosRasterExtent ReosGriddedRainfallMemoryProvider::extent() const
{
  return mExtent;
}

ReosEncodedElement ReosGriddedRainfallMemoryProvider::encode( const ReosEncodeContext & ) const
{
  return ReosEncodedElement();
}

void ReosGriddedRainfallMemoryProvider::decode( const ReosEncodedElement &, const ReosEncodeContext & )
{

}

void ReosGriddedRainfallMemoryProvider::addFrame( const ReosRasterMemory<double> &raster, const QDateTime &startTime, const QDateTime &endTime )
{
  mRasters.append( {startTime, endTime, raster} );
}

QString ReosGriddedRainfallMemoryProvider::dataType()
{
  return ReosGriddedRainfall::staticType();
}

QString ReosGriddedRainfallMemoryProvider::staticKey()
{
  return  QString( QStringLiteral( "gridded-precipitation-memory" ) );
}

void ReosGriddedRainfallMemoryProvider::setExtent( const ReosRasterExtent &newExtent )
{
  mExtent = newExtent;
}

void ReosGriddedRainfallMemoryProvider::copyFrom( ReosGriddedDataProvider *other )
{
  mExtent = ReosRasterExtent();
  mRasters.clear();

  if ( !other )
    return;

  mExtent = other->extent();
  int count = other->count();

  mRasters.reserve( count );

  for ( int i = 0; i < count; ++i )
  {
    Frame frame;
    frame.startTime = other->startTime( i );
    frame.endTime = other->endTime( i );
    frame.raster = ReosRasterMemory<double>( mExtent.yCellCount(), mExtent.xCellCount() );
    frame.raster.reserveMemory();
    const QVector<double> valuesOther = other->data( i );
    memcpy( frame.raster.data(), valuesOther.constData(), sizeof( double ) * valuesOther.count() );
    mRasters.append( frame );
  }
}

QString ReosGriddedRainfallMemoryProviderFactory::key() const
{
  return ReosGriddedRainfallMemoryProvider::staticKey();
}

ReosGriddedRainfallProvider::~ReosGriddedRainfallProvider()
{}

ReosGriddedDataProvider::~ReosGriddedDataProvider()
{}

ReosGriddedRainfallProvider *ReosGriddedRainfallMemoryProvider::clone() const
{
  std::unique_ptr<ReosGriddedRainfallMemoryProvider> other = std::make_unique<ReosGriddedRainfallMemoryProvider>();

  other->mExtent = mExtent;
  other->mRasters = mRasters;
  other->mSourceValueType = mSourceValueType;
  other->setDataSource( dataSource() );
  return other.release();
}

ReosGriddedRainfallProvider *ReosGriddedRainfallMemoryProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosGriddedRainfallMemoryProvider::dataType() )
    return new ReosGriddedRainfallMemoryProvider;

  return nullptr;
}
