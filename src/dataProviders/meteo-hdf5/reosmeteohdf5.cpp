/***************************************************************************
  reosmeteohdf5.cpp - ReosMeteoHdf5Provider

 ---------------------
 begin                : 14.03.2025
 copyright            : (C) 2025 by Vincent Cloarec
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
#include <QLocale>
#include <QDirIterator>

#include "reoshdf5.h"
#include "reosmeteohdf5.h"
#include "reosgriddedrainitem.h"
#include "reosgdalutils.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosMeteoHdf5ProviderFactory();
}

ReosMeteoHdf5Provider::ReosMeteoHdf5Provider()
{
  mCache.setMaxCost( 500000000 );
}

ReosMeteoHdf5Provider *ReosMeteoHdf5Provider::clone() const
{
  std::unique_ptr<ReosMeteoHdf5Provider> other = std::make_unique<ReosMeteoHdf5Provider>();
  other->setDataSource( dataSource() );

  return other.release();
}

void ReosMeteoHdf5Provider::load()
{
  QString filePathSource = sourcePathFromUri( dataSource() );
  mIsValid = false;

  ReosModule::Message message;
  if ( !sourceIsValid( filePathSource, message ) )
    return;
  const QStringList &files = getFiles( filePathSource );

  QMap<QDateTime, Frame> frames;

  int xSize = -1;
  int ySize = -1;

  for ( const QString &filePath : files )
  {
    ReosHdf5File file( filePath );
    if ( file.isValid() )
    {
      ReosHdf5Group dataGroup = file.createGroup( QStringLiteral( "/dataset1/data1/what" ) );
      QString dataAttr = dataGroup.attribute( QStringLiteral( "quantity" ) ).readString();
      if ( dataAttr != "ACRR" )
        continue;

      // Check the size
      ReosHdf5Group whereGroup = file.createGroup( QStringLiteral( "/where" ) );
      bool ok;
      int currentXSize = whereGroup.attribute( QStringLiteral( "xsize" ) ).readInt( ok );
      if ( !ok )
        continue;

      int currentYSize = whereGroup.attribute( QStringLiteral( "ysize" ) ).readInt( ok );
      if ( !ok )
        continue;

      if ( xSize < 0 || ySize == 0 )
      {
        xSize = currentXSize;
        ySize = currentYSize;
      }
      else
      {
        if ( xSize != currentXSize or ySize != currentYSize )
          continue;
      }

      ReosHdf5Group group = file.createGroup( QStringLiteral( "/dataset1/what" ) );
      ReosHdf5Attribute attr = group.attribute( QStringLiteral( "startdate" ) );
      QString dateStr = attr.readString();
      const QDate startDate( QDate::fromString( dateStr, QStringLiteral( "yyyyMMdd" ) ) );
      if ( !startDate.isValid() )
        continue;

      attr = group.attribute( QStringLiteral( "enddate" ) );
      dateStr = attr.readString();
      const QDate endDate( QDate::fromString( dateStr, QStringLiteral( "yyyyMMdd" ) ) );
      if ( !endDate.isValid() )
        continue;

      attr = group.attribute( QStringLiteral( "starttime" ) );
      QString timeStr = attr.readString();
      const QTime startTime( QTime::fromString( timeStr, QStringLiteral( "hhmmss" ) ) );
      if ( !startTime.isValid() )
        continue;

      attr = group.attribute( QStringLiteral( "endtime" ) );
      timeStr = attr.readString();
      const QTime endTime( QTime::fromString( timeStr, QStringLiteral( "hhmmss" ) ) );
      if ( !endTime.isValid() )
        continue;

      Frame frame;
      frame.gdalUri = QStringLiteral( "HDF5:\"%1\"://dataset1/data1/data" ).arg( filePath );
      frame.startTime = QDateTime( startDate, startTime, Qt::UTC );
      frame.endTime = QDateTime( endDate, endTime, Qt::UTC );
      frames.insert( frame.startTime, frame );
    }
  }

  mFrames = frames.values();

  if ( mFrames.count() > 0 )
  {
    // We open the first frame to extract the extent
    ReosGdalDataset dataset( mFrames.at( 0 ).gdalUri );
    mExtent = dataset.extent();
  }

  mIsValid = mFrames.count() > 0;
}

QStringList ReosMeteoHdf5Provider::fileSuffixes() const
{
  QStringList ret;
  ret << QStringLiteral( "h5" );

  return ret;
}

ReosGriddedRainfallProvider::FileDetails ReosMeteoHdf5Provider::details( const QString &source, ReosModule::Message &message ) const
{
  return FileDetails();
}

bool ReosMeteoHdf5Provider::isValid() const
{
  return mIsValid;
}

int ReosMeteoHdf5Provider::count() const
{
  return mFrames.count();
}

bool ReosMeteoHdf5Provider::canReadUri( const QString &uri ) const
{
  QDir dir;
  ReosModule::Message message;
  if ( !sourceIsValid( sourcePathFromUri( uri ), message ) )
    return false;

  QStringList files = getFiles( sourcePathFromUri( uri ) );

  return !files.empty() && dir.exists();
}

QDateTime ReosMeteoHdf5Provider::startTime( int index ) const
{
  return mFrames.at( index ).startTime;
}

QDateTime ReosMeteoHdf5Provider::endTime( int index ) const
{
  return mFrames.at( index ).endTime;
}

const QVector<double> ReosMeteoHdf5Provider::data( int index ) const
{
  if ( index < 0 )
    return QVector<double>();

  QVector<double> *cache = mCache.object( index );

  if ( cache )
    return ( *cache );

  ReosGdalDataset dataset( mFrames.at( index ).gdalUri );
  if ( !dataset.isValid() )
    return QVector<double>();

  ReosRasterMemory<double> raster = dataset.values( 1 );
  QVector<double> rawValues = raster.values();

  std::unique_ptr<QVector<double>> values = std::make_unique<QVector<double>>( raster.values().count(),  std::numeric_limits<double>::quiet_NaN() );
  for ( int i = 0; i < rawValues.count(); ++i )
  {
    if ( raster.values().at( i ) != 65535 )
      ( *values )[i] = raster.values().at( i ) / 100;
  }

  QVector<double> ret = *values;
  mCache.insert( index, values.release(), rawValues.size() * 16 );

  return ret;

}

bool ReosMeteoHdf5Provider::getDirectMinMax( double &min, double &max ) const
{
  if ( mHasMinMaxCalculated )
  {
    min = mMin;
    max = mMax;
  }
  return mHasMinMaxCalculated;
}

void ReosMeteoHdf5Provider::calculateMinMax( double &min, double &max ) const
{
  mMin = std::numeric_limits<double>::max();
  mMax = -std::numeric_limits<double>::max();

  int gridCount = count();

  for ( int i = 0; i < gridCount; ++i )
  {
    const QVector<double> vals = data( i );
    for ( const double &v : vals )
    {
      if ( v < mMin )
        mMin = v;
      if ( v > mMax )
        mMax = v;
    }
  }
  min = mMin;
  max = mMax;
  mHasMinMaxCalculated = true;
}

QString ReosMeteoHdf5Provider::htmlDescription() const
{
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );

  htmlText += QStringLiteral( "<h2>" ) + tr( "Gridded Precipitation" ) + QStringLiteral( "</h2>\n<hr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Format" ) ) + QStringLiteral( "</td><td>" )
              + QStringLiteral( "GRIB 2" ) + QStringLiteral( "</td></tr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Source" ) ) + QStringLiteral( "</td><td>" )
              + sourcePathFromUri( dataSource() ) + QStringLiteral( "</td></tr>\n" );

  if ( count() > 0 )
  {
    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                +  QStringLiteral( "<b>%1</b>" ).arg( tr( "Start date" ) ) + QStringLiteral( "</td><td>" )
                + startTime( 0 ).toString( QLocale().dateTimeFormat() )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                +  QStringLiteral( "<b>%1</b>" ).arg( tr( "End date" ) ) + QStringLiteral( "</td><td>" )
                + endTime( count() - 1 ).toString( QLocale().dateTimeFormat() )
                + QStringLiteral( "</td></tr>\n" );
  }

  return htmlText;
}

ReosRasterExtent ReosMeteoHdf5Provider::extent() const
{
  return mExtent;
}

QString ReosMeteoHdf5Provider::dataType() {return ReosGriddedData::staticType();}

QString ReosMeteoHdf5Provider::staticKey()
{
  return METEO_HDF5_KEY + QString( "::" ) + dataType();
}

QString ReosMeteoHdf5Provider::uri( const QString &sourcePath )
{
  return QStringLiteral( "\"%1\"" ).arg( sourcePath );
}

QString ReosMeteoHdf5Provider::sourcePathFromUri( const QString &uri )
{
  QString source = uri;
  source.remove( QStringLiteral( "\"" ) );
  return source;
}


bool ReosMeteoHdf5Provider::sourceIsValid( const QString &source, ReosModule::Message &message ) const
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

ReosEncodedElement ReosMeteoHdf5Provider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "grib-gridded-precipitation" ) );

  QString uriToEncode = dataSource();

  QString sourcePath = sourcePathFromUri( uriToEncode );
  sourcePath = context.pathToEncode( sourcePath );
  uriToEncode = uri( sourcePath );
  element.addData( QStringLiteral( "data-source" ), uriToEncode );

  return element;
}

void ReosMeteoHdf5Provider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != QStringLiteral( "grib-gridded-precipitation" ) )
    return;
  QString source;
  if ( element.getData( QStringLiteral( "data-source" ), source ) )
  {
    QString sourcePath = sourcePathFromUri( source );
    sourcePath = context.resolvePath( sourcePath );
    source = uri( sourcePath );
    setDataSource( source );
  }

}


QStringList ReosMeteoHdf5Provider::getFiles( const QString &path ) const
{
  QStringList files;
  QFileInfo fileInfo( path );
  if ( fileInfo.isDir() )
  {
    QStringList filters;
    filters << QStringLiteral( "*.h5" );

    // Use QDirIterator for recursive search
    QDirIterator it( path, filters, QDir::Files, QDirIterator::Subdirectories );
    while ( it.hasNext() )
      files << it.next();
  }
  else if ( fileInfo.isFile() &&
            ( fileInfo.suffix() == QStringLiteral( "h5" ) ) )
  {
    files << path;
  }

  return files;
}

void ReosMeteoHdf5Provider::giveName( FileDetails &details )
{
  return;
}

ReosMeteoHdf5Provider *ReosMeteoHdf5ProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosMeteoHdf5Provider::dataType() )
    return new ReosMeteoHdf5Provider;

  return nullptr;
}

QString ReosMeteoHdf5ProviderFactory::key() const
{
  return METEO_HDF5_KEY;
}

bool ReosMeteoHdf5ProviderFactory::supportType( const QString &dataType ) const
{
  return dataType.contains( ReosGriddedRainfall::staticType() ) or dataType.contains( ReosGriddedData::staticType() );
}

QVariantMap ReosMeteoHdf5ProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
  {
    ret.insert( QStringLiteral( "file-or-dir-path" ), QObject::tr( "File or directory where are stored the data" ) );
  }

  return ret;
}

QString ReosMeteoHdf5ProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType ) )
  {
    const QString path = parameters.value( QStringLiteral( "file-or-dir-path" ) ).toString();
    ok = true;
    return ReosMeteoHdf5Provider::uri( path );
  }
  ok = false;
  return QString();
}
