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
#include "reoscomephoreprovider.h"

#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosComephoresProviderFactory();
}

ReosComephoreProvider::ReosComephoreProvider()
{
  mCache.setMaxCost( 20000000 );
}

ReosGriddedRainfallProvider *ReosComephoreProvider::clone() const
{
  std::unique_ptr<ReosComephoreProvider> other = std::make_unique<ReosComephoreProvider>();

  other->mIsValid = mIsValid;
  other->mFileReader.reset( mFileReader->clone() );
  other->mExtent = mExtent;

  return other.release();
}

ReosComephoreProvider::~ReosComephoreProvider() = default;

void ReosComephoreProvider::setDataSource( const QString &dataSource )
{
  mIsValid = false;
  ReosGriddedRainfallProvider::setDataSource( dataSource );
  QFileInfo sourceInfo( dataSource );

  if ( sourceInfo.isDir() )
  {
    mFileReader.reset( new ReosComephoreTiffFilesReader( dataSource ) );
  }

  if ( mFileReader )
  {
    mIsValid = mFileReader->frameCount() > 0;
    mExtent = mFileReader->extent();
  }
}

bool ReosComephoreProvider::isValid() const
{
  return mIsValid;
}

int ReosComephoreProvider::count() const
{
  if ( mFileReader )
    return mFileReader->frameCount();
  return 0;
}

QDateTime ReosComephoreProvider::startTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index );

  return QDateTime();
}

QDateTime ReosComephoreProvider::endTime( int index ) const
{
  if ( mFileReader )
    return mFileReader->time( index ).addSecs( 3600 );

  return QDateTime();
}

const QVector<double> ReosComephoreProvider::data( int index ) const
{
  if ( index < 0 )
    return QVector<double>();

  if ( mCache.contains( index ) )
    return *mCache.object( index );

  if ( mFileReader )
  {
    QVector<double> rawValues = mFileReader->data( index );
    std::unique_ptr<QVector<double>> values = std::make_unique<QVector<double>>();
    values->resize( rawValues.count() );

    for ( int i = 0; i < rawValues.count(); ++i )
    {
      if ( rawValues.at( i ) == 65535 || rawValues.at( i ) == 0 )
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

ReosRasterExtent ReosComephoreProvider::extent() const
{
  return mExtent;
}

bool ReosComephoreProvider::canReadUri( const QString &uri ) const
{
  if ( ReosComephoreTiffFilesReader::canReadFile( uri ) )
    return true;

  return false;
}

static QFileInfoList tiffFiles( const QString &folderPath )
{
  QFileInfoList ret;
  QFileInfo sourceInfo( folderPath );
  if ( !sourceInfo.isDir() )
    return ret;

  QDir sourceDir( folderPath );

  if ( !sourceDir.cd( QStringLiteral( "RR" ) ) )
    return ret;

  QStringList filters;
  filters << QStringLiteral( "*_RR.gtif" );
  filters << QStringLiteral( "*_RR.tif" );
  filters << QStringLiteral( "*_RR.tiff" );
  const QFileInfoList fileInfoList = sourceDir.entryInfoList( filters, QDir::Files );

  return fileInfoList;
}

ReosGriddedRainfallProvider::Details ReosComephoreProvider::details( const QString &source, ReosModule::Message &message ) const
{
  Details ret;

  bool ok = false;
  ret = ReosComephoreTiffFilesReader::details( source, &ok );
  if ( ok )
    return ret;

  return Details();
}

ReosEncodedElement ReosComephoreProvider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "comephores-gridded-precipitation" ) );

  QString sourcePath = dataSource();
  sourcePath = context.pathToEncode( sourcePath );
  element.addData( QStringLiteral( "data-source" ), sourcePath );

  return element;
}

void ReosComephoreProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != QStringLiteral( "comephores-gridded-precipitation" ) )
    return;
  QString source;
  if ( element.getData( QStringLiteral( "data-source" ), source ) )
  {
    const QString sourcePath =  context.resolvePath( source );
    setDataSource( sourcePath );
  }
}

bool ReosComephoreProvider::getDirectMinMax( double &min, double &max ) const
{
  if ( mFileReader )
    return mFileReader->getDirectMinMax( min, max );
  return false;
}

void ReosComephoreProvider::calculateMinMax( double &min, double &max ) const
{
  min = std::numeric_limits<double>::max();
  max = -std::numeric_limits<double>::max();

  int gridCount = count();

  for ( int i = 0; i < gridCount; ++i )
  {
    const QVector<double> vals = data( i );
    for ( const double &v : vals )
    {
      if ( v < min )
        min = v;
      if ( v > max )
        max = v;
    }
  }
}

QString ReosComephoreProvider::dataType() {return ReosGriddedRainfall::staticType();}

QString ReosComephoreProvider::staticKey()
{
  return COMEPHORES_KEY + QString( "::" ) + dataType();
}

ReosComephoreTiffFilesReader::ReosComephoreTiffFilesReader( const QString &folderPath )
{
  const QFileInfoList fileInfoList = tiffFiles( folderPath );

  for ( const QFileInfo &fi : fileInfoList )
  {
    const QString timeString = fi.baseName().remove( QStringLiteral( "_RR" ) );
    QDateTime time = QDateTime::fromString( timeString, QStringLiteral( "yyyyMMddHH" ) );
    time.setTimeSpec( Qt::UTC );
    mFilesNames.insert( time, fi.filePath() );
    mTimes.append( time );
  }
}

ReosComephoreFilesReader *ReosComephoreTiffFilesReader::clone() const
{
  std::unique_ptr<ReosComephoreTiffFilesReader> other( new ReosComephoreTiffFilesReader );
  other->mFilesNames = mFilesNames;
  other->mTimes = mTimes;
  return other.release();
}

int ReosComephoreTiffFilesReader::frameCount() const
{
  return mFilesNames.count();
}

QDateTime ReosComephoreTiffFilesReader::time( int i ) const
{
  return mTimes.at( i );
}

QVector<double> ReosComephoreTiffFilesReader::data( int index ) const
{
  const QDateTime &time = mTimes.at( index );
  QString fileName = mFilesNames.value( time );
  ReosGdalDataset dataset( fileName );

  ReosRasterMemory<double> values = dataset.values( 1 );

  return values.values();
}

ReosRasterExtent ReosComephoreTiffFilesReader::extent() const
{
  if ( mFilesNames.isEmpty() )
    return ReosMapExtent();

  ReosGdalDataset dataset( mFilesNames.begin().value() );

  return dataset.extent();
}

bool ReosComephoreTiffFilesReader::getDirectMinMax( double &, double & ) const
{
  return false;
}

bool ReosComephoreTiffFilesReader::canReadFile( const QString &uri )
{
  return !tiffFiles( uri ).isEmpty();
}

ReosGriddedRainfallProvider::Details ReosComephoreTiffFilesReader::details( const QString &source, bool *ok )
{
  ReosGriddedRainfallProvider::Details ret;

  ReosComephoreTiffFilesReader fileReader( source );
  if ( fileReader.frameCount() == 0 )
  {
    *ok = false;
    return ret;
  }

  ret.extent = fileReader.extent();
  ret.files = fileReader.mFilesNames.values();

  QDir dir( source );
  ret.deducedName = dir.dirName();

  *ok = true;

  return ret;
}

ReosComephoreTiffFilesReader::~ReosComephoreTiffFilesReader() = default;

ReosGriddedRainfallProvider *ReosComephoresProviderFactory::createProvider( const QString &dataType ) const
{
  return new ReosComephoreProvider;
}

QString ReosComephoresProviderFactory::key() const
{
  return COMEPHORES_KEY;
}
