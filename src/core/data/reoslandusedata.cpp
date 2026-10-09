/***************************************************************************
  reoslandusedata.h - ReosLandUseData

 ---------------------
 begin                : 9.2.2021
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


#include "reoslandusedata.h"

#include "reosgeometryutils.h"
#include "reoswatershed.h"


ReosLandUseDataProvider::ReosLandUseDataProvider()
  : ReosDataProvider()
{}

QString ReosLandUseDataProvider::dataSource() const
{
  return mDataSource;
}

void ReosLandUseDataProvider::setDataSource( const QString &uri )
{
  mDataSource = uri;
  load();
}

ReosLandUseData::ReosLandUseData( const QString &dataSource, const QString &providerKey, QObject *parent )
  : ReosDataObject( parent )
  , mProvider( std::unique_ptr<ReosLandUseDataProvider>( qobject_cast<ReosLandUseDataProvider *>( ReosDataProviderRegistery::instance()->createProvider( formatKey( providerKey ) ) ) ) )
{
  mProvider->setDataSource( dataSource );
}

QString ReosLandUseData::formatKey( const QString &rawKey ) const
{
  if ( rawKey.contains( QStringLiteral( "::" ) ) )
    return rawKey;
  return rawKey + QStringLiteral( "::" ) + ReosLandUseData::staticType();
}

QString ReosLandUseData::staticType()
{
  return QStringLiteral( "land-use-data" );
}

QVector<int> ReosLandUseData::data() const
{
  if ( !mProvider )
    return QVector<int>();

  return mProvider->data();
}

QVector<int> ReosLandUseData::data( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const
{
  if ( !mProvider )
    return QVector<int>();

  return mProvider->data( requestedExent, outputExent );
}

ReosRasterMemory<int> ReosLandUseData::rasterData( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const
{
  const QVector<int> data = this->data( requestedExent, outputExent );

  ReosRasterMemory<int> raster( outputExent.yCellCount(), outputExent.xCellCount() );
  raster.setValues( data );

  return raster;
}

ReosRasterExtent ReosLandUseData::extent() const
{
  if ( !mProvider )
    return ReosRasterExtent();

  return mProvider->extent();
}

ReosLandUseDataOnWatershed::ReosLandUseDataOnWatershed( ReosLandUseData *landUseData, ReosWatershed *watershed )
  : mLandUseData( landUseData )
  , mWatershed( watershed )
{}

QMap<int, double> ReosLandUseDataOnWatershed::landUseDistribution() const
{
  ReosMapExtent watershedExtent( mWatershed->delineating(), mWatershed->crs() );

  ReosRasterExtent landUseExtent;
  ReosRasterMemory<int> landUseRaster = mLandUseData->rasterData( watershedExtent, landUseExtent );

  int xOri = -1;
  int yOri = -1;
  ReosRasterExtent rasterizedExtent;
  ReosRasterMemory<double> rasterizedWatershed = ReosGeometryUtils::rasterizePolygon( mWatershed->delineating(), landUseExtent, rasterizedExtent, xOri, yOri, true );

  Q_ASSERT( landUseRaster.rowCount() == rasterizedWatershed.rowCount() && landUseRaster.columnCount() == rasterizedWatershed.columnCount() );

  QMap<int, double> distribution;

  QVector<int> landUseValues = landUseRaster.values();
  QVector<double> cellAreas = rasterizedWatershed.values();

  double totalArea = 0;
  for ( qsizetype i = 0; i < landUseValues.size(); ++i )
  {
    int landUseValue = landUseValues.at( i );
    double cellArea = cellAreas.at( i );

    if ( cellArea > 0 )
    {
      distribution[landUseValue] += cellArea;
      totalArea += cellArea;
    }
  }

  for ( int i = 0; i < distribution.size(); ++i )
  {
    int landUseValue = distribution.keys().at( i );
    double area = distribution.value( landUseValue );

    distribution[landUseValue] = area / totalArea;
  }

  return distribution;
}
