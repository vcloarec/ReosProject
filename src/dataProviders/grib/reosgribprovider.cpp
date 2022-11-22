/***************************************************************************
  reosgribprovider.cpp - ReosGribProvider

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

#include <QFileInfo>
#include <QDir>

#include "reosgribprovider.h"
#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosGribProviderFactory();
}

ReosGribGriddedRainfallProvider::ReosGribGriddedRainfallProvider()
{
  mCache.setMaxCost( 20000000 );
}

void ReosGribGriddedRainfallProvider::setDataSource( const QString &dataSource )
{
  QString source = sourcePathFromUri( dataSource );
  QString varName = variableFromUri( dataSource );
  mSourceValueType = valueTypeFromUri( dataSource );
  mIsValid = false;

  QMap<qint64, GribFrame> pathes;

  if ( !sourceIsValid( source, mLastMessage ) )
    return;
  QDir dir;
  QFileInfo fileInfo( source );
  if ( fileInfo.isDir() )
  {
    QStringList filters;
    dir = QDir( source );
    filters << QStringLiteral( "*.grib2" );
    const QStringList files = dir.entryList( filters, QDir::Files );
    for ( const QString &file : files )
    {
      parseFile( dir.filePath( file ), varName, mReferenceTime, pathes, mExtent );
    }

  }
  else if ( fileInfo.isFile() )
  {
    parseFile( source, varName, mReferenceTime, pathes, mExtent );
  }

  mFrames = pathes.values();

  mIsValid = true;
}

ReosGriddedRainfallProvider::Details ReosGribGriddedRainfallProvider::details( const QString &source, ReosModule::Message &message ) const
{
  Details ret;

  if ( !sourceIsValid( source, message ) )
    return ret;

  QStringList files;
  QDir dir;
  QFileInfo fileInfo( source );
  if ( fileInfo.isDir() )
  {
    QStringList filters;
    dir = QDir( source );
    filters << QStringLiteral( "*.grib2" );
    filters << QStringLiteral( "*.grb2" );
    files = dir.entryList( filters, QDir::Files );
  }
  else if ( fileInfo.isFile() )
  {
    files << source;
    dir = fileInfo.dir();
  }

  if ( files.isEmpty() )
  {
    message.type = ReosModule::Error;
    message.text = tr( "No Grib2 source found in \"%1\"." ).arg( source );
  }

  bool hasExtent = false;

  for ( const QString &file : std::as_const( files ) )
  {
    ReosGdalDataset dataset( dir.filePath( file ) );

    int bandCount = dataset.bandCount();

    if ( bandCount == 0 )
    {
      message.type = ReosModule::Error;
      message.text = tr( "No data found in \"%1\"." ).arg( source );
      return ret;
    }

    for ( int i = 1; i <= bandCount; ++i )
    {
      QMap<QString, QString> metadata = dataset.bandMetadata( i );
      auto it = metadata.find( QStringLiteral( "GRIB_COMMENT" ) );
      if ( it == metadata.end() )
        continue;

      if ( ret.availableVariables.contains( it.value() ) )
        continue;

      ret.availableVariables.append( it.value() );
      if ( !hasExtent )
        ret.extent = dataset.extent();
    }
  }

  if ( ret.availableVariables.isEmpty() )
  {
    message.type = ReosModule::Error;
    message.text = tr( "No data found in \"%1\"." ).arg( source );
  }

  ret.files = files;

  return ret;
}

bool ReosGribGriddedRainfallProvider::isValid() const
{
  return mIsValid;
}

int ReosGribGriddedRainfallProvider::count() const
{
  return mFrames.count();

}

QDateTime ReosGribGriddedRainfallProvider::startTime( int index ) const
{
  switch ( mSourceValueType )
  {
    case ValueType::CumulativeHeight:
      if ( index == 0 )
        return QDateTime::fromSecsSinceEpoch( mReferenceTime, Qt::UTC );
      else
        return QDateTime::fromSecsSinceEpoch( mFrames.at( index - 1 ).validTime, Qt::UTC );
      break;
    case ValueType::Height:
    case ValueType::Intensity:
      return QDateTime();
      break;
  }

  return QDateTime();
}

QDateTime ReosGribGriddedRainfallProvider::endTime( int index ) const
{
  switch ( mSourceValueType )
  {
    case ValueType::CumulativeHeight:
      return QDateTime::fromSecsSinceEpoch( mFrames.at( index ).validTime, Qt::UTC );
      break;
    case ValueType::Height:
    case ValueType::Intensity:
      return QDateTime();
      break;
  }

  return QDateTime();;
}

const QVector<double> ReosGribGriddedRainfallProvider::data( int index ) const
{
  if ( index < 0 )
    return QVector<double>();

  CacheValues *cache = mCache.object( index );

  if ( cache && cache->typeCalculatedFrom == mSourceValueType )
    return cache->values;

  if ( cache )
    mCache.remove( index );

  ReosGdalDataset dataset( mFrames.at( index ).file );
  if ( !dataset.isValid() )
    return QVector<double>();

  ReosRasterMemory<double> raster = dataset.values( mFrames.at( index ).bandNo );
  ReosDuration duration = intervalDuration( index );

  switch ( mSourceValueType )
  {
    case ValueType::CumulativeHeight:
    {
      QVector<double> ret( raster.values().count() );

      if ( index == 0 )
      {
        for ( int i = 0; i < ret.count(); ++i )
        {
          ret[i] = raster.values().at( i ) / duration.valueHour();
        }
        return ret;;
      }

      ReosGdalDataset prevDataset( mFrames.at( index - 1 ).file );
      ReosRasterMemory<double> prevRaster = prevDataset.values( mFrames.at( index - 1 ).bandNo );
      const QVector<double> &prevIndex = prevRaster.values();
      const QVector<double> &currentIndex = raster.values();
      Q_ASSERT( prevIndex.count() == currentIndex.count() );


      for ( int i = 0; i < ret.count(); ++i )
      {
        ret[i] = ( currentIndex.at( i ) - prevIndex.at( i ) ) / duration.valueHour();
      }

      return ret;
    }
    break;
    case ValueType::Height:
    case ValueType::Intensity:
      return raster.values();
      break;
  }

  return QVector<double>();
}

ReosRasterExtent ReosGribGriddedRainfallProvider::extent() const
{
  return mExtent;
}

QString ReosGribGriddedRainfallProvider::dataType() {return ReosGriddedRainfall::staticType();}

QString ReosGribGriddedRainfallProvider::staticKey()
{
  return GRIB_KEY + QString( "::" ) + dataType();
}

QString ReosGribGriddedRainfallProvider::uri( const QString &sourcePath, const QString &variable, ValueType valueType )
{
  QString stringValueType;

  switch ( valueType )
  {
    case ValueType::CumulativeHeight:
      stringValueType = QStringLiteral( "cumulative" );
      break;
    case ValueType::Height:
      stringValueType = QStringLiteral( "height" );
    case ValueType::Intensity:
      stringValueType = QStringLiteral( "intensity" );
      break;
  }
  return QStringLiteral( "\"%1\"::%2::%3" ).arg( sourcePath, variable, stringValueType );
}

QString ReosGribGriddedRainfallProvider::sourcePathFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() == 0 )
    return QString();

  QString source = part.at( 0 );
  source.remove( QStringLiteral( "\"" ) );
  return source;
}

QString ReosGribGriddedRainfallProvider::variableFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 2 )
    return QString();

  return part.at( 1 );
}

ReosGriddedRainfallProvider::ValueType ReosGribGriddedRainfallProvider::valueTypeFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 3 )
    return ValueType::Height;

  if ( part.at( 2 ) == QStringLiteral( "cumulative" ) )
    return ValueType::CumulativeHeight;

  if ( part.at( 2 ) == QStringLiteral( "intensity" ) )
    return ValueType::Intensity;

  if ( part.at( 2 ) == QStringLiteral( "height" ) )
    return ValueType::Height;

  return ValueType::Height;
}

bool ReosGribGriddedRainfallProvider::sourceIsValid( const QString &source, ReosModule::Message &message ) const
{
  QFileInfo fileInfo( source );
  if ( fileInfo.isDir() )
  {
    QDir dir( source );
    if ( !dir.exists() )
    {
      message.type = ReosModule::Error;
      message.text = tr( "Folder \"%1\" does not exist." ).arg( source );
      return false;
    }
  }
  else if ( fileInfo.isFile() )
  {
    if ( !fileInfo.exists() )
    {
      message.type = ReosModule::Error;
      message.text = tr( "File \"%1\" does not exist." ).arg( source );
      return false;
    }
  }
  else
  {
    message.type = ReosModule::Error;
    message.text = tr( "Unknow source \"%1\"." ).arg( source );
    return false;
  }

  return true;

}

void ReosGribGriddedRainfallProvider::parseFile(
  const QString &fileName,
  const QString &varName,
  qint64 &referenceTime,
  QMap<qint64, GribFrame> &pathes,
  ReosRasterExtent &extent ) const
{
  ReosGdalDataset dataset( fileName );
  if ( !dataset.isValid() )
    return;

  int bandCount = dataset.bandCount();
  for ( int bi = 1; bi <= bandCount; ++bi )
  {
    const QMap<QString, QString> metadata = dataset.bandMetadata( bi );

    if ( metadata.value( QStringLiteral( "GRIB_COMMENT" ) ) == varName )
    {
      if ( pathes.isEmpty() )
        extent = dataset.extent();
      else
      {
        if ( extent != dataset.extent() )
          continue;
      }

      GribFrame path;
      path.file = fileName;
      path.bandNo = bi;

      QString strRefTime = metadata.value( QStringLiteral( "GRIB_REF_TIME" ) );
      if ( strRefTime.isEmpty() )
        continue;
      strRefTime = strRefTime.split( ' ', Qt::SplitBehaviorFlags::SkipEmptyParts ).at( 0 );
      bool ok = false;
      qint64 refTime = strRefTime.toInt( &ok );
      if ( !ok )
        continue;
      if ( mReferenceTime == -1 )
        referenceTime = refTime;
      else if ( refTime != mReferenceTime )
        continue;

      QString strValidTime = metadata.value( QStringLiteral( "GRIB_VALID_TIME" ) );
      if ( strValidTime.isEmpty() )
        continue;
      strValidTime = strValidTime.split( ' ', Qt::SplitBehaviorFlags::SkipEmptyParts ).at( 0 );
      ok = false;
      path.validTime = strValidTime.toInt( &ok );
      if ( !ok )
        continue;
      pathes.insert( path.validTime, path );
    }
  }
}

ReosGriddedRainfallProvider *ReosGribProviderFactory::createProvider( const QString & ) const
{
  return new ReosGribGriddedRainfallProvider;
}

QString ReosGribProviderFactory::key() const
{
  return GRIB_KEY;
}
