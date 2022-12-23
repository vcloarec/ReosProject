/***************************************************************************
  reoscomephoresprovider.cpp - ReosComephoresProvider

 ---------------------
 begin                : 22.12.2022
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
#include "reoscomephoresprovider.h"

#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"

ReosComephoresProvider::ReosComephoresProvider()
{
  mCache.setMaxCost( 20000000 );
}

ReosGriddedRainfallProvider *ReosComephoresProvider::clone() const
{
  std::unique_ptr<ReosComephoresProvider> other = std::make_unique<ReosComephoresProvider>();

  other->mIsValid = mIsValid;
  other->mFileReader.reset( mFileReader->clone() );
  other->mExtent = mExtent;

  return other.release();
}

ReosComephoresProvider::~ReosComephoresProvider() = default;

void ReosComephoresProvider::setDataSource( const QString &dataSource )
{
  mIsValid = false;
  ReosGriddedRainfallProvider::setDataSource( dataSource );
  QFileInfo sourceInfo( dataSource );

  if ( sourceInfo.isDir() )
  {
    mFileReader.reset( new ReosComephoresTiffFilesReader( dataSource ) );
  }

  if ( mFileReader )
  {
    mIsValid = mFileReader->frameCount() > 0;
    mExtent = mFileReader->extent();
  }
}

bool ReosComephoresProvider::isValid() const
{
  return mIsValid;
}

int ReosComephoresProvider::count() const
{
  if ( mFileReader )
    return mFileReader->frameCount();
  return 0;
}

QDateTime ReosComephoresProvider::startTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index );

  return QDateTime();
}

QDateTime ReosComephoresProvider::endTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index ).addSecs( 3600 );

  return QDateTime();
}

const QVector<double> ReosComephoresProvider::data( int index ) const
{
  if ( mCache.contains( index ) )
    return *mCache.object( index );

  if ( mFileReader )
  {
    QVector<double> rawValues = mFileReader->data( index );
    std::unique_ptr<QVector<double>> values = std::make_unique<QVector<double>>();
    values->resize( rawValues.count() );

    for ( int i = 0; i < rawValues.count(); ++i )
    {
      if ( rawValues.at( i ) == 65535 )
        ( *values )[i] = std::numeric_limits<double>::quiet_NaN();
      else
        ( *values )[i] = rawValues.at( i ) / 10.0;
    }

    QVector<double> ret = *values;
    mCache.insert( index, values.release(), rawValues.size() );

    return ret;
  }

  return QVector<double>();
}

ReosRasterExtent ReosComephoresProvider::extent() const
{
  return mExtent;
}

ReosEncodedElement ReosComephoresProvider::encode( const ReosEncodeContext &context ) const
{

}

void ReosComephoresProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{

}

QString ReosComephoresProvider::dataType() {return ReosGriddedRainfall::staticType();}

QString ReosComephoresProvider::staticKey()
{
  return COMEPHORES_KEY + QString( "::" ) + dataType();
}

ReosComephoresTiffFilesReader::ReosComephoresTiffFilesReader( const QString &folderPath )
{
  QFileInfo sourceInfo( folderPath );
  if ( !sourceInfo.isDir() )
    return;

  QDir sourceDir( folderPath );

  if ( !sourceDir.cd( QStringLiteral( "RR" ) ) )
    return;

  QStringList filters;
  filters << QStringLiteral( "*_RR.gtif" );
  filters << QStringLiteral( "*_RR.tif" );
  filters << QStringLiteral( "*_RR.tiff" );
  const QFileInfoList fileInfoList = sourceDir.entryInfoList( filters, QDir::Files );

  for ( const QFileInfo &fi : fileInfoList )
  {
    const QString timeString = fi.baseName().remove( QStringLiteral( "_RR" ) );
    QDateTime time = QDateTime::fromString( timeString, QStringLiteral( "yyyyMMddHH" ) );
    time.setTimeSpec( Qt::UTC );
    mFilesNames.insert( time, fi.filePath() );
    mTimes.append( time );
  }
}

ReosComephoresFilesReader *ReosComephoresTiffFilesReader::clone() const
{
  std::unique_ptr<ReosComephoresTiffFilesReader> other( new ReosComephoresTiffFilesReader );
  other->mFilesNames = mFilesNames;
  other->mTimes = mTimes;
  return other.release();
}

int ReosComephoresTiffFilesReader::frameCount() const
{
  return mFilesNames.count();
}

QDateTime ReosComephoresTiffFilesReader::time( int i ) const
{
  return mTimes.at( i );
}

QVector<double> ReosComephoresTiffFilesReader::data( int index ) const
{
  const QDateTime &time = mTimes.at( index );
  QString fileName = mFilesNames.value( time );
  ReosGdalDataset dataset( fileName );

  ReosRasterMemory<double> values = dataset.values( 1 );

  return values.values();
}

ReosRasterExtent ReosComephoresTiffFilesReader::extent() const
{
  if ( mFilesNames.isEmpty() )
    return ReosMapExtent();

  ReosGdalDataset dataset( mFilesNames.begin().value() );

  return dataset.extent();
}

ReosComephoresTiffFilesReader::~ReosComephoresTiffFilesReader() = default;
