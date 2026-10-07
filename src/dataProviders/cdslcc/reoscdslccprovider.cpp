/***************************************************************************
  reoscdslccprovider.cpp - ReosCdslccProvider

 ---------------------
 begin                : 5.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reoscdslccprovider.h"

#include "reosnetcdfutils.h"
#include "reosgisengine.h"
#include "reosgeometryutils.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosCdslccProviderFactory();
}

ReosCdslccProvider::ReosCdslccProvider()
  : ReosLandUseDataProvider()
{}

const QVector<int> ReosCdslccProvider::data() const
{
  int size = mExtent.xCellCount() * mExtent.yCellCount();
  const QVector<uchar> charData = mFile->getUcharArray( QStringLiteral( "lccs_class" ), size );

  QVector<int> ret;
  ret.resize( charData.size() );

  for ( qsizetype i = 0; i < charData.size(); ++i )
    ret[i] = static_cast<int>( charData.at( i ) );

  return ret;
}

const QVector<int> ReosCdslccProvider::data( const ReosMapExtent &requestedExent, ReosRasterExtent &outputExent ) const
{
  ReosRasterCellPos origin;
  ReosRasterExtent subExtent = ReosGeometryUtils::subRasterExtent( mExtent, requestedExent, origin );

  QVector<int> starts = { 0, origin.row(), origin.column() };
  QVector<int> sizes = { 1, subExtent.yCellCount(), subExtent.xCellCount() };

  const QVector<uchar> charData = mFile->getUcharArray( QStringLiteral( "lccs_class" ), starts, sizes );

  QVector<int> ret;
  ret.resize( charData.size() );

  for ( qsizetype i = 0; i < charData.size(); ++i )
    ret[i] = static_cast<int>( charData.at( i ) );

  outputExent = subExtent;

  return ret;
}

ReosRasterExtent ReosCdslccProvider::extent() const
{
  return mExtent;
}

bool ReosCdslccProvider::canReadUri( const QString &uri ) const
{
  ReosNetCdfFile file( uri );

  if ( !file.isValid() )
    return false;

  if ( !file.hasVariable( QStringLiteral( "lccs_class" ) ) )
    return false;

  QStringList dimensions = file.variableDimensionNames( QStringLiteral( "lccs_class" ) );

  if ( !dimensions.contains( QStringLiteral( "lat" ) ) || !dimensions.contains( QStringLiteral( "lon" ) ) || !dimensions.contains( QStringLiteral( "time" ) ) )
    return false;

  return true;
}

void ReosCdslccProvider::load()
{
  QString filePath = dataSource();
  mFile.reset( new ReosNetCdfFile( filePath ) );

  if ( !mFile->isValid() )
    return;
  int latCount = mFile->dimensionLength( QStringLiteral( "lat" ) );
  int lonCount = mFile->dimensionLength( QStringLiteral( "lon" ) );
  const QVector<double> latBounds = mFile->getDoubleArray( "lat_bounds", latCount * 2 );
  double maxLat = -std::numeric_limits<double>::max();
  double minLat = std::numeric_limits<double>::max();
  for ( double v : latBounds )
  {
    if ( v > maxLat )
      maxLat = v;
    if ( v < minLat )
      minLat = v;
  }

  const QVector<double> lonBouds = mFile->getDoubleArray( "lon_bounds", lonCount * 2 );
  double maxLon = -std::numeric_limits<double>::max();
  double minLon = std::numeric_limits<double>::max();
  for ( double v : lonBouds )
  {
    if ( v > maxLon )
      maxLon = v;
    if ( v < minLon )
      minLon = v;
  }

  ReosMapExtent extent( minLon, minLat, maxLon, maxLat );
  mExtent = ReosRasterExtent( extent, lonCount, latCount );

  QString wktCrs = mFile->stringAttributeValue( QStringLiteral( "crs" ), QStringLiteral( "wkt" ) );
  mExtent.setCrs( wktCrs );
}

ReosCdslccProvider *ReosCdslccProviderFactory::createProvider( const QString &dataType ) const
{
  if ( ReosCdslccProvider::dataType() == dataType )
    return new ReosCdslccProvider;

  return nullptr;
}
QVariantMap ReosCdslccProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
    ret.insert( QStringLiteral( "file-path" ), QObject::tr( "File where are stored the data" ) );


  return ret;
}
QString ReosCdslccProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType ) && parameters.contains( QStringLiteral( "file-path" ) ) )
  {
    QString uri = parameters.value( QStringLiteral( "file-path" ) ).toString();
    ok = true;
    return uri;
  }
  else
  {
    ok = false;
    return QString();
  }
}
