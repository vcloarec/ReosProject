/***************************************************************************
  reosgdalutils.cpp - ReosGdalUtils

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
#include "reosgdalutils.h"

#include <QStringList>
#include <QMap>
#include <reosmemoryraster.h>


bool ReosGdalDataset::sRegistered = false;

ReosGdalDataset::ReosGdalDataset( const QString &fileName, bool readOnly )
{
    if (!sRegistered)
    {
        GDALAllRegister();
        sRegistered = true;
    }

  mHDataset = GDALOpen( fileName.toStdString().c_str(), readOnly ? GA_ReadOnly : GA_Update );
}

ReosGdalDataset::~ReosGdalDataset()
{
  if ( mHDataset )
    GDALClose( mHDataset );
}

int ReosGdalDataset::bandCount()
{
  if ( mHDataset )
    return GDALGetRasterCount( mHDataset );
  else
    return 0;
}

static QMap<QString, QString> _metadata( GDALMajorObjectH mHMajorObject )
{
  QMap<QString, QString> meta;
  char **GDALmetadata = nullptr;
  GDALmetadata = GDALGetMetadata( mHMajorObject, nullptr );

  if ( GDALmetadata )
  {
    for ( int j = 0; GDALmetadata[j]; ++j )
    {
      const QString metadata_pair = GDALmetadata[j]; //KEY = VALUE
      QStringList metadata = metadata_pair.split( '=' );
      if ( metadata.size() > 1 )
        meta.insert( metadata.at( 0 ), metadata.at( 1 ) );
    }
  }

  return meta;
}


QMap<QString, QString> ReosGdalDataset::metadata() const
{
  if ( !mHDataset )
    return QMap<QString, QString>();

  return _metadata( mHDataset );
}

QMap<QString, QString> ReosGdalDataset::bandMetadata( int band ) const
{
  if ( !mHDataset )
    return QMap<QString, QString>();

  GDALRasterBandH hBand = GDALGetRasterBand( mHDataset, band );

  if ( !hBand )
    return QMap<QString, QString>();

  return _metadata( hBand );
}

ReosRasterExtent ReosGdalDataset::extent() const
{
  std::array<double, 6> geoTransform;
  GDALGetGeoTransform( mHDataset, geoTransform.data() );

  int xCount = GDALGetRasterXSize( mHDataset );
  int yCount = GDALGetRasterYSize( mHDataset );

  ReosRasterExtent extent( geoTransform.at( 0 ), geoTransform.at( 3 ), xCount, yCount, geoTransform.at( 1 ), geoTransform.at( 5 ) );

  char *proj = const_cast<char *>( GDALGetProjectionRef( mHDataset ) );
  if ( proj != nullptr )
  {
    extent.setCrs( QString( proj ) );
  }

  return extent;
}

bool ReosGdalDataset::isValid() const {return mHDataset != nullptr;}

ReosRasterMemory<double> ReosGdalDataset::values(int band)
{
    if ( !mHDataset )
        return ReosRasterMemory<double>();

    GDALRasterBandH hBand = GDALGetRasterBand( mHDataset, band );

    if ( !hBand )
        return ReosRasterMemory<double>();

    int xCount = GDALGetRasterXSize( mHDataset );
    int yCount = GDALGetRasterYSize( mHDataset );

    ReosRasterMemory<double> ret( yCount, xCount );

    if ( !ret.reserveMemory() )
        return ReosRasterMemory<double>();

    CPLErr err = GDALRasterIO( hBand, GF_Read, 0, 0, xCount, yCount, ret.data(), xCount, yCount, GDT_Float64, 0, 0 );

    if ( err != CE_None )
        return ReosRasterMemory<double>();

    return ret;
}
