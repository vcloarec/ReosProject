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

ReosGriddedRainfallProvider::~ReosGriddedRainfallProvider()
{}

QString ReosGriddedRainfallProvider::dataSource() const
{
    return mDataSource;
}

void ReosGriddedRainfallProvider::setDataSource( const QString &uri )
{
    mDataSource = uri;
}

ReosDuration ReosGriddedRainfallProvider::intervalDuration( int index ) const
{
  return ReosDuration( startTime( index ), endTime( index ) );
}

void ReosGriddedRainfallProvider::setSourceValueType( ValueType valueType )
{
  mSourceValueType = valueType;
}


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

ReosEncodedElement ReosGriddedRainfallMemoryProvider::encode() const
{
    return ReosEncodedElement();
}

void ReosGriddedRainfallMemoryProvider::decode(const ReosEncodedElement &element)
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

void ReosGriddedRainfallMemoryProvider::copyFrom( ReosGriddedRainfallProvider *other )
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

ReosGriddedRainfallProvider *ReosGriddedRainfallMemoryProviderFactory::createProvider( const QString & ) const
{
  return new ReosGriddedRainfallMemoryProvider;
}

QString ReosGriddedRainfallMemoryProviderFactory::key() const
{
  return ReosGriddedRainfallMemoryProvider::staticKey();
}
