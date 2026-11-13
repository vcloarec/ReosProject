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
#include "gdal_alg.h"
#include "cpl_conv.h"
#include "gdalwarper.h"
#include "gdal_utils.h"
#include <iostream>
#include <algorithm>
#include <limits>

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
    GDALReleaseDataset( mHDataset );
}

int ReosGdalDataset::bandCount() const
{
  if ( mHDataset )
    return GDALGetRasterCount( mHDataset );
  else
    return 0;
}

static QMap<QString, QString> _metadata( GDALMajorObjectH mHMajorObject )
{
  QMap<QString, QString> meta;
  CSLConstList GDALmetadata = nullptr;
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

ReosRasterExtent ReosGdalDataset::extent( int i ) const
{
  Q_UNUSED( i );
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

ReosRasterMemory<double> ReosGdalDataset::values( int frameIndex ) const
{
  return valuesFromBand( frameIndex + 1 );
}

bool ReosGdalDataset::isValid() const
{
  return mHDataset != nullptr;
}

int ReosGdalDataset::frameCount() const
{
  return bandCount();
}

ReosRasterMemory<double> ReosGdalDataset::valuesFromBand( int band ) const
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

ReosRasterMemory<int> ReosGdalDataset::valuesInt( int band ) const
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

ReosRasterMemory<unsigned char> ReosGdalDataset::valuesBytes( int band ) const
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

void ReosGdalDataset::resample( const ReosRasterExtent &newExtent, const QString &alg )
{
  double pixelSizeX = newExtent.xCellSize();
  double pixelSizeY = newExtent.yCellSize();
  double minX = newExtent.xMapMin();
  double maxY = newExtent.yMapMax();
  int outWidth = newExtent.xCellCount();
  int outHeight = newExtent.yCellCount();

  GDALDriverH hMemDriver = GDALGetDriverByName( "MEM" );

  GDALDatasetH hDstDS = GDALCreate( hMemDriver, "", outWidth, outHeight, GDALGetRasterCount( mHDataset ), GDALGetRasterDataType( GDALGetRasterBand( mHDataset, 1 ) ), NULL );
  double dstGeoTransform[6] = { minX, pixelSizeX, 0, maxY, 0, pixelSizeY };
  GDALSetGeoTransform( hDstDS, dstGeoTransform );
  const char *proj = GDALGetProjectionRef( mHDataset );
  GDALSetProjection( hDstDS, newExtent.crs().toUtf8() );

  char **papszOptions = NULL;
  papszOptions = CSLAddString( papszOptions, "-r" );
  papszOptions = CSLAddString( papszOptions, alg.toUtf8() ); // or "near", "cubic", etc.

  int usageError;
  GDALWarpAppOptions *psWarpOptions = GDALWarpAppOptionsNew( papszOptions, NULL );

  GDALDatasetH ahSrcDS[1] = { mHDataset };

  GDALDatasetH hWarped = GDALWarp( "", hDstDS, 1, ahSrcDS, psWarpOptions, &usageError );

  if ( hWarped )
  {
    GDALReleaseDataset( mHDataset );
    mHDataset = hWarped;
  }

  GDALWarpAppOptionsFree( psWarpOptions );
  CSLDestroy( papszOptions );
}

QVector<ReosRasterMemory<double>> ReosGdalDataset::classFractions( int band, int classCount, const ReosRasterExtent &newExtent ) const
{
  QVector<ReosRasterMemory<double>> ret;

  if ( !mHDataset || classCount <= 0 )
    return ret;

  GDALRasterBandH hBand = GDALGetRasterBand( mHDataset, band );
  if ( !hBand )
    return ret;

  int xCount = GDALGetRasterXSize( mHDataset );
  int yCount = GDALGetRasterYSize( mHDataset );

  QVector<unsigned char> values( static_cast<qsizetype>( xCount ) * yCount );
  if ( GDALRasterIO( hBand, GF_Read, 0, 0, xCount, yCount, values.data(), xCount, yCount, GDT_Byte, 0, 0 ) != CE_None )
    return ret;

  float noData = -1.0f;
  GDALDriverH hMemDriver = GDALGetDriverByName( "MEM" );

  // One binary mask per class, the average of a mask over a cell is the fraction of the cell belonging to the class
  GDALDatasetH hSrcDS = GDALCreate( hMemDriver, "", xCount, yCount, classCount, GDT_Float32, nullptr );
  if ( !hSrcDS )
    return ret;

  double srcGeoTransform[6];
  GDALGetGeoTransform( mHDataset, srcGeoTransform );
  GDALSetGeoTransform( hSrcDS, srcGeoTransform );
  GDALSetProjection( hSrcDS, GDALGetProjectionRef( mHDataset ) );

  QVector<float> mask( values.size() );
  for ( int k = 0; k < classCount; ++k )
  {
    for ( qsizetype i = 0; i < values.size(); ++i )
    {
      int v = values.at( i );
      if ( v == 0 )
        mask[i] = noData;
      else
        mask[i] = std::min( classCount - 1, v * classCount / 256 ) == k ? 1.0f : 0.0f;
    }

    GDALRasterBandH hSrcBand = GDALGetRasterBand( hSrcDS, k + 1 );
    GDALSetRasterNoDataValue( hSrcBand, noData );
    GDALRasterIO( hSrcBand, GF_Write, 0, 0, xCount, yCount, mask.data(), xCount, yCount, GDT_Float32, 0, 0 );
  }

  int outWidth = newExtent.xCellCount();
  int outHeight = newExtent.yCellCount();

  GDALDatasetH hDstDS = GDALCreate( hMemDriver, "", outWidth, outHeight, classCount, GDT_Float32, nullptr );
  if ( !hDstDS )
  {
    GDALClose( hSrcDS );
    return ret;
  }

  double dstGeoTransform[6] = { newExtent.xMapMin(), newExtent.xCellSize(), 0, newExtent.yMapMax(), 0, newExtent.yCellSize() };
  GDALSetGeoTransform( hDstDS, dstGeoTransform );
  GDALSetProjection( hDstDS, newExtent.crs().toUtf8() );
  for ( int k = 0; k < classCount; ++k )
  {
    GDALRasterBandH hDstBand = GDALGetRasterBand( hDstDS, k + 1 );
    GDALSetRasterNoDataValue( hDstBand, noData );
    GDALFillRaster( hDstBand, noData, 0 );
  }

  char **papszOptions = nullptr;
  papszOptions = CSLAddString( papszOptions, "-r" );
  papszOptions = CSLAddString( papszOptions, "average" );

  int usageError = 0;
  GDALWarpAppOptions *psWarpOptions = GDALWarpAppOptionsNew( papszOptions, nullptr );
  GDALDatasetH ahSrcDS[1] = { hSrcDS };
  GDALDatasetH hWarped = GDALWarp( nullptr, hDstDS, 1, ahSrcDS, psWarpOptions, &usageError );
  GDALWarpAppOptionsFree( psWarpOptions );
  CSLDestroy( papszOptions );

  if ( hWarped )
  {
    for ( int k = 0; k < classCount; ++k )
    {
      ReosRasterMemory<double> fractions( outHeight, outWidth );
      if ( !fractions.reserveMemory() )
        break;

      GDALRasterBandH hDstBand = GDALGetRasterBand( hDstDS, k + 1 );
      if ( GDALRasterIO( hDstBand, GF_Read, 0, 0, outWidth, outHeight, fractions.data(), outWidth, outHeight, GDT_Float64, 0, 0 ) != CE_None )
        break;

      for ( int row = 0; row < outHeight; ++row )
      {
        for ( int col = 0; col < outWidth; ++col )
        {
          if ( fractions.value( row, col ) < 0 )
            fractions.setValue( row, col, std::numeric_limits<double>::quiet_NaN() );
        }
      }
      ret.append( fractions );
    }

    if ( ret.count() != classCount )
      ret.clear();
  }

  GDALClose( hDstDS );
  GDALClose( hSrcDS );

  return ret;
}

bool ReosGdalDataset::writeDoubleToFile( int bandNo, const QString &fileName ) const
{
  return writeDoubleRasterToFile( fileName, valuesFromBand( bandNo ), extent() );
}

bool ReosGdalDataset::writeByteRasterToCOGFile( const QString &fileName, ReosRasterMemory<unsigned char> raster, const ReosRasterExtent &extent )
{
  GDALDriverH hMemDriver = GDALGetDriverByName( "MEM" );
  if ( !hMemDriver )
  {
    std::cout << ( stderr, "MEM driver not available.\n" );
    return false;
  }

  int width = raster.columnCount();
  int height = raster.rowCount();


  GDALDatasetH hTempDS = GDALCreate( hMemDriver, "", width, height, 1, GDT_Byte, NULL );
  if ( !hTempDS )
  {
    std::cout << ( stderr, "Failed to create in-memory dataset.\n" );
    return false;
  }

  double geoTrans[6] = { extent.xMapOrigin(), extent.xCellSize(), 0, extent.yMapOrigin(), 0, extent.yCellSize() };
  GDALSetGeoTransform( hTempDS, geoTrans );

  QString wktCrs = extent.crs();
  GDALSetProjection( hTempDS, wktCrs.toUtf8() );

  GDALRasterBandH hBand = GDALGetRasterBand( hTempDS, 1 );
  GDALSetRasterNoDataValue( hBand, 255 );
  CPLErr err = GDALRasterIO( hBand, GF_Write, 0, 0, width, height, raster.data(), width, height, GDT_Byte, 0, 0 );

  if ( err != CPLErr::CE_None )
  {
    std::cout << ( stderr, "Failed to write to file.\n" );
    return false;
  }

  GDALDriverH hCOGDriver = GDALGetDriverByName( "COG" );
  if ( !hCOGDriver )
  {
    std::cout << ( stderr, "COG driver not found.\n" );
    GDALClose( hTempDS );
    return false;
  }

  char *papszOptions[] = { ( char * ) "COMPRESS=DEFLATE", ( char * ) "TILING_SCHEME=GoogleMapsCompatible", NULL };

  GDALDatasetH hCOG = GDALCreateCopy( hCOGDriver, fileName.toUtf8(), hTempDS, FALSE, papszOptions, NULL, NULL );
  if ( !hCOG )
  {
    fprintf( stderr, "Failed to create COG dataset.\n" );
    GDALClose( hTempDS );
    return false;
  }

  // Cleanup
  GDALClose( hCOG );
  GDALClose( hTempDS );

  return true;
}


bool ReosGdalDataset::writeByteRasterToFile( const QString &fileName, ReosRasterMemory<unsigned char> raster, const ReosRasterExtent &extent )
{
  GDALDriver *driver = GetGDALDriverManager()->GetDriverByName( "GTiff" );

  if ( !driver )
    return false;

  char **papszOptions = nullptr;

  papszOptions = CSLSetNameValue( papszOptions, "TILED", "YES" );
  papszOptions = CSLSetNameValue( papszOptions, "COPY_SRC_OVERVIEWS", "YES" );
  papszOptions = CSLSetNameValue( papszOptions, "BLOCKXSIZE", "256" );
  papszOptions = CSLSetNameValue( papszOptions, "BLOCKYSIZE", "256" );
  papszOptions = CSLSetNameValue( papszOptions, "COMPRESS", "DEFLATE" );
  papszOptions = CSLSetNameValue( papszOptions, "PREDICTOR", "2" );
  papszOptions = CSLSetNameValue( papszOptions, "BIGTIFF", "IF_SAFER" );

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

  QString wktCrs = extent.crs();
  dataSet->SetProjection( wktCrs.toStdString().c_str() );

  GDALClose( static_cast<GDALDatasetH>( dataSet ) );

  return true;
}

bool ReosGdalDataset::writeIntRasterToFile( const QString &fileName, ReosRasterMemory<int> raster, const ReosRasterExtent &extent )
{
  GDALDriver *driver = GetGDALDriverManager()->GetDriverByName( "GTiff" );

  if ( !driver )
    return false;

  char *papszOptions[] = { const_cast<char *>( "COMPRESS=DEFLATE" ), const_cast<char *>( "PREDICTOR=2" ), nullptr };

  GDALDataset *dataSet = driver->Create( fileName.toStdString().c_str(), raster.columnCount(), raster.rowCount(), 1, GDALDataType::GDT_Int32, papszOptions );
  if ( !dataSet )
    return false;

  GDALRasterBand *band = dataSet->GetRasterBand( 1 );

  CPLErr err = band->RasterIO( GF_Write, 0, 0, raster.columnCount(), raster.rowCount(), raster.data(), raster.columnCount(), raster.rowCount(), GDALDataType::GDT_Int32, 0, 0 );
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

bool ReosGdalDataset::writeDoubleRasterToFile( const QString &fileName, ReosRasterMemory<double> raster, const ReosRasterExtent &extent )
{
  GDALDriver *driver = GetGDALDriverManager()->GetDriverByName( "GTiff" );

  if ( !driver )
    return false;

  char *papszOptions[] = { const_cast<char *>( "COMPRESS=DEFLATE" ), const_cast<char *>( "PREDICTOR=1" ), nullptr };

  GDALDataset *dataSet = driver->Create( fileName.toStdString().c_str(), raster.columnCount(), raster.rowCount(), 1, GDALDataType::GDT_Float64, papszOptions );
  if ( !dataSet )
    return false;

  GDALRasterBand *band = dataSet->GetRasterBand( 1 );

  CPLErr err = band->RasterIO( GF_Write, 0, 0, raster.columnCount(), raster.rowCount(), raster.data(), raster.columnCount(), raster.rowCount(), GDALDataType::GDT_Float64, 0, 0 );
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

ReosGriddedDataSource::~ReosGriddedDataSource()
{}
