/***************************************************************************
  reosgriddedrainitem.cpp - ReosGriddedRainItem

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
#include "reosgriddedrainitem.h"

#include "reosgriddedrainfallprovider.h"
#include "reosmemoryraster.h"
#include "reosgisengine.h"

ReosGriddedRainfall::ReosGriddedRainfall( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosDataObject( parent )
  , mProvider( qobject_cast<ReosGriddedRainfallProvider*>( ReosDataProviderRegistery::instance()->createProvider( providerKey ) ) )
{
  if ( mProvider )
    mProvider->setDataSource( dataSource );
}

ReosGriddedRainfall::~ReosGriddedRainfall()
{

}

QString ReosGriddedRainfall::type() const {return staticType();}

QString ReosGriddedRainfall::staticType() {return QStringLiteral( "gridded-rainfall" );}

int ReosGriddedRainfall::gridCount() const
{
  if ( mProvider )
    return mProvider->count();
  else
    return 0;
}

const QDateTime ReosGriddedRainfall::startTime( int index ) const
{
  if ( mProvider )
    return mProvider->startTime( index );
  else
    return QDateTime();
}

const QDateTime ReosGriddedRainfall::endTime( int index ) const
{
  if ( mProvider )
    return mProvider->endTime( index );
  else
    return QDateTime();
}

const QVector<double> ReosGriddedRainfall::data( int index ) const
{
  if ( mProvider )
    return mProvider->data( index );
  else
    return QVector<double>();
}

ReosRasterExtent ReosGriddedRainfall::extent() const
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

bool ReosGriddedRainfall::isValid() const
{
  return mProvider && mProvider->isValid();
}

void ReosGriddedRainfall::overrideCrs( const QString &crs )
{
  mOverridenCrs = crs;
}

ReosGriddedRainfall *ReosGriddedRainfall::transform( const ReosMapExtent &destination, double resolX, double resolY, QObject *parent ) const
{
  std::unique_ptr<ReosGriddedRainfall> projectedRainfall( new ReosGriddedRainfall( parent ) );

  ReosRasterExtent destinationExtent;
  bool success;
  const ReosRasterExtent sourceExtent = extent();
  auto convertValues = ReosGisEngine::transformRasterExtent( sourceExtent, destination, resolX, resolY, destinationExtent, success );
  int sourceXCount = sourceExtent.xCellCount();

  std::unique_ptr<ReosGriddedRainfallMemoryProvider> newProvider( new ReosGriddedRainfallMemoryProvider );

  newProvider->setExtent( destinationExtent );

  int xCount = destinationExtent.xCellCount();
  int yCount = destinationExtent.yCellCount();

  for ( int i = 0; i < mProvider->count(); ++i )
  {
    const QVector<double> sourceValue = mProvider->data( i );
    ReosRasterMemory<double> raster( yCount, xCount );
    raster.reserveMemory();

    for ( int y = 0; y < yCount; ++y )
    {
      for ( int x = 0; x < xCount; ++x )
      {
        const QList<QPair<double, QPoint>> &pairs = convertValues.value( y, x );
        double value = 0;
        for ( const QPair<double, QPoint> &pair : pairs )
        {
          const QPoint &pt = pair.second;
          double coef = pair.first;
          int valIndex = pt.x() + pt.y() * sourceXCount;
          value += sourceValue.at( valIndex ) * coef;
        }
        raster.setValue( y, x, value );
      }
    }

    newProvider->addFrame( raster, mProvider->startTime( i ), mProvider->endTime( i ) );
  }

  projectedRainfall->mProvider.reset( newProvider.release() );
  return projectedRainfall.release();
}


ReosGriddedRainfall::ReosGriddedRainfall( QObject *parent )
  : ReosDataObject( parent )
{

}
