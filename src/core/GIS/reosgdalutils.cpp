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
  if ( !sRegistered )
  {
    GDALAllRegister();
    sRegistered = true;
  }

  mHDataset = GDALOpen( fileName.toUtf8().constData(), readOnly ? GA_ReadOnly : GA_Update );
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

ReosRasterMemory<double> ReosGdalDataset::values( int band )
{
  if ( !mHDataset )
    return ReosRasterMemory<double>();

  GDALRasterBandH hBand = GDALGetRasterBand( mHDataset, band );

  if ( !hBand )
    return ReosRasterMemory<double>();

  int xCount = GDALGetRasterXSize( mHDataset );
  int yCount = GDALGetRasterYSize( mHDataset );

  ReosRasterMemory<double> ret( yCount, xCount );

  double no_data = static_cast<double>( GDALGetRasterNoDataValue( hBand, 0 ) );
  ret.setNodata( no_data );

  if ( !ret.reserveMemory() )
    return ReosRasterMemory<double>();

  CPLErr err = GDALRasterIO( hBand, GF_Read, 0, 0, xCount, yCount, ret.data(), xCount, yCount, GDT_Float64, 0, 0 );

  if ( err != CE_None )
    return ReosRasterMemory<double>();

  return ret;
}

ReosRasterMemory<int> ReosGdalDataset::valuesInt( int band )
{
  if ( !mHDataset )
    return ReosRasterMemory<int>();

  GDALRasterBandH hBand = GDALGetRasterBand( mHDataset, band );

  if ( !hBand )
    return ReosRasterMemory<int>();

  int xCount = GDALGetRasterXSize( mHDataset );
  int yCount = GDALGetRasterYSize( mHDataset );

  ReosRasterMemory<int> ret( yCount, xCount );

  int no_data = static_cast<int>( GDALGetRasterNoDataValue( hBand, 0 ) );
  ret.setNodata( no_data );

  if ( !ret.reserveMemory() )
    return ReosRasterMemory<int>();

  CPLErr err = GDALRasterIO( hBand, GF_Read, 0, 0, xCount, yCount, ret.data(), xCount, yCount, GDT_Int32, 0, 0 );

  if ( err != CE_None )
    return ReosRasterMemory<int>();

  return ret;
}

ReosRasterMemory<unsigned char> ReosGdalDataset::valuesBytes( int band )
{
  if ( !mHDataset )
    return ReosRasterMemory<unsigned char>();

  GDALRasterBandH hBand = GDALGetRasterBand( mHDataset, band );

  if ( !hBand )
    return ReosRasterMemory<unsigned char>();

  int xCount = GDALGetRasterXSize( mHDataset );
  int yCount = GDALGetRasterYSize( mHDataset );

  ReosRasterMemory<unsigned char> ret( yCount, xCount );

  unsigned char no_data = static_cast<unsigned char>( GDALGetRasterNoDataValue( hBand, 0 ) );
  ret.setNodata( no_data );

  if ( !ret.reserveMemory() )
    return ReosRasterMemory<unsigned char>();

  CPLErr err = GDALRasterIO( hBand, GF_Read, 0, 0, xCount, yCount, ret.data(), xCount, yCount, GDT_Byte, 0, 0 );

  if ( err != CE_None )
    return ReosRasterMemory<unsigned char>();

  return ret;
}

bool ReosGdalDataset::writeByteRasterToFile( const QString &fileName, ReosRasterMemory<unsigned char> raster, const ReosRasterExtent &extent )
{
  GDALDriver *driver = GetGDALDriverManager()->GetDriverByName( "GTiff" );

  if ( !driver )
    return false;

  char *papszOptions[] =
  {
    const_cast<char *>( "COMPRESS=DEFLATE" ),
    const_cast<char *>( "PREDICTOR=2" ),
    nullptr
  };

  GDALDataset *dataSet = driver->Create( fileName.toStdString().c_str(), raster.columnCount(), raster.rowCount(), 1, GDALDataType::GDT_Byte, papszOptions );
  if ( !dataSet )
    return false;

  GDALRasterBand *band = dataSet->GetRasterBand( 1 );

  CPLErr err = band->RasterIO( GF_Write, 0, 0, raster.columnCount(), raster.rowCount(), raster.data(), raster.columnCount(), raster.rowCount(), GDALDataType::GDT_Byte, 0, 0 );
  if ( err )
    return false;

  band->SetNoDataValue( raster.noData() );

  double geoTrans[6] = { extent.xMapOrigin(), extent.xCellSize(), 0, extent.yMapOrigin(), 0, extent.yCellSize() };
  dataSet->SetGeoTransform( geoTrans );

  char *proj_WKT = nullptr;
  QString wktCrs = extent.crs();
  dataSet->SetProjection( extent.crs().toStdString().c_str() );
  CPLFree( proj_WKT );

  GDALClose( static_cast<GDALDatasetH>( dataSet ) );

  return true;
}
