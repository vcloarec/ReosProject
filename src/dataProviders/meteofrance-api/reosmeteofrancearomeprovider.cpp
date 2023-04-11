/***************************************************************************
  reosmeteofrancearomeprovider.cpp - ReosMeteoFranceAromeProvider

 ---------------------
 begin                : 31.3.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmeteofrancearomeprovider.h"

#include <QTemporaryFile>

#include "reosgriddedrainitem.h"
#include "reosgisengine.h"
#include "reosgdalutils.h"

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosMeteoFranceAromeApiProviderFactory();
}


ReosMeteoFranceAromeApiProvider::ReosMeteoFranceAromeApiProvider()
{
}

QString ReosMeteoFranceAromeApiProvider::dataType() {return ReosGriddedRainfall::staticType();}

QString ReosMeteoFranceAromeApiProvider::staticKey()
{
  return AROME_KEY + QString( "::" ) + dataType();
}

QString ReosMeteoFranceAromeApiProvider::uri( const QString &apiKeyFileName, const QString &zone, const QString &resol, const ReosMapExtent &extent, int runIndex )
{
  QStringList extentList = ReosMeteoFranceApiArome::extentToLonLatList( extent );
  QString ret = QStringLiteral( "\"%1\"::%2::%3::%4::%5" ).arg( apiKeyFileName, zone, resol, extentList.join( ',' ), QString::number( runIndex ) );

  return ret;
}

QString ReosMeteoFranceAromeApiProvider::apiKeyFileNamefromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() == 0 )
    return QString();

  QString keyFile = part.at( 0 );
  keyFile.remove( QStringLiteral( "\"" ) );
  return keyFile;
}

QString ReosMeteoFranceAromeApiProvider::zoneFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 2 )
    return QString();

  return part.at( 1 );
}

QString ReosMeteoFranceAromeApiProvider::resolFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 3 )
    return QString();

  return part.at( 2 );
}

ReosMapExtent ReosMeteoFranceAromeApiProvider::extentFromUri( const QString &uri )
{
  QList<double> extentNumber = extentListFromUri( uri );
  QString crs = ReosGisEngine::crsFromEPSG( 4326 );
  return ReosMapExtent( ReosSpatialPosition( extentNumber.at( 0 ), extentNumber.at( 2 ), crs ), ReosSpatialPosition( extentNumber.at( 1 ), extentNumber.at( 3 ), crs ) );
}

QList<double> ReosMeteoFranceAromeApiProvider::extentListFromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 4 )
    return QList<double>();

  QStringList numberParts = part.at( 3 ).split( "," );
  if ( numberParts.count() != 4 )
    return QList<double>();

  QList<double> ret;
  for ( int i = 0; i < 4; ++i )
    ret << numberParts.at( i ).toDouble();

  return ret;
}

int ReosMeteoFranceAromeApiProvider::runIndexfromUri( const QString &uri )
{
  const QStringList part = uri.split( QStringLiteral( "::" ) );
  if ( part.count() < 5 )
    return -1;

  return part.at( 4 ).toInt();
}

void ReosMeteoFranceAromeApiProvider::onConnected( const QString &connectError )
{
  if ( !connectError.isEmpty() )
    return;

  int runIndex = runIndexfromUri( dataSource() );

  QList<QDateTime> runs = mService->availableRuns();
  if ( runs.count() <= runIndex || runIndex < 0 )
    return;

  mRun = runs.at( runs.count() - 1 - runIndex );

  mRunInfo = ReosMeteoFranceApiArome::RunInfo();
  QString error;
  connect( mService.get(), &ReosMeteoFranceApiArome::runInfoReady, this, &ReosMeteoFranceAromeApiProvider::receiveRunInfo );
  mService->requestRunInfo( mRun, error );
}

void ReosMeteoFranceAromeApiProvider::receiveRunInfo( const QByteArray &data, const QDateTime &run )
{
  if ( mRun != run )
    return;

  QString error;
  mRunInfo = ReosMeteoFranceApiArome::decodeRunInfo( data, error );

  mValues.clear();
  mValues.reserve( mRunInfo.frameCount );

  for ( int i = 0; i < mRunInfo.frameCount; ++i )
    mValues.append( QVector<double>() );

  connect( mService.get(), &ReosMeteoFranceApiArome::valuesReady, this, &ReosMeteoFranceAromeApiProvider::receiveData );

  for ( int i = 0; i < mRunInfo.frameCount; ++i )
    mService->requestFrame( mRequestedExtent, mRun, i );

  mIsValid = true;

  emit dataReset();
}

void ReosMeteoFranceAromeApiProvider::receiveData( const QByteArray &data, int frameIndex )
{
  QTemporaryFile file( QDir::tempPath() + QStringLiteral( "/XXXXXX.grb2" ) );
  file.open();
  QDataStream stream( &file );
  stream << data;
  file.close();

  mReceivedFrames.insert( frameIndex );

  ReosGdalDataset dataset( file.fileName(), true );
  if ( !dataset.isValid() )
    return;

  ReosRasterMemory<double> datasetValues = dataset.values( 1 );
  mExtent = dataset.extent();

  if ( frameIndex < mValues.count() )
  {
    mValues[frameIndex] = datasetValues.values();
  }

  if ( mReceivedFrames.count() == mRunInfo.frameCount )
  {
    mIsLoading = false;
    emit loadingFinished();
  }
  else
    emit dataChanged();
}

void ReosMeteoFranceAromeApiProvider::loadFrame()
{
  mIsValid = false;
  mIsLoading = true;
  mReceivedFrames.clear();
  mService.reset( new ReosMeteoFranceApiArome( apiKeyFileNamefromUri( dataSource() ) ) );
  mModel.zone = zoneFromUri( dataSource() );
  mModel.resol = resolFromUri( dataSource() );
  mRequestedExtent = extentFromUri( dataSource() );
  mExtent = mRequestedExtent;

  QString error;
  connect( mService.get(), &ReosMeteoFranceApiArome::connected, this, &ReosMeteoFranceAromeApiProvider::onConnected );
  mService->connectToService( mModel, error );

  emit dataReset();
}

ReosGriddedRainfallProvider *ReosMeteoFranceAromeApiProvider::clone() const
{
  return nullptr;
}

void ReosMeteoFranceAromeApiProvider::setDataSource( const QString &dataSource )
{
  ReosGriddedRainfallProvider::setDataSource( dataSource );
  loadFrame();
}

QString ReosMeteoFranceAromeApiProvider::htmlDescription() const
{
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Format" ) ) + QStringLiteral( "</td><td>" )
              + QStringLiteral( "Météo France Arome API" ) + QStringLiteral( "</td></tr>\n" );

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Zone" ) ) + QStringLiteral( "</td><td>" )
              + mModel.zone + QStringLiteral( "</td></tr>\n" );;

  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Resolution" ) ) + QStringLiteral( "</td><td>" )
              + mModel.resol + QStringLiteral( "</td></tr>\n" );;

  int runIndex = runIndexfromUri( dataSource() );
  htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
              + QStringLiteral( "<b>%1</b>" ).arg( tr( "Run" ) ) + QStringLiteral( "</td><td>" )
              + ( runIndex == 0 ? tr( "Last" ) : tr( "%1 before last" ).arg( runIndex ) )
              + QStringLiteral( " (%1)" ).arg( mRun.toString( QLocale().dateTimeFormat() ) ) + QStringLiteral( "</td></tr>\n" );;

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

ReosGriddedRainfallProvider::FileDetails ReosMeteoFranceAromeApiProvider::details( const QString &, ReosModule::Message & ) const {return FileDetails();}

bool ReosMeteoFranceAromeApiProvider::isValid() const {return mIsValid;}

int ReosMeteoFranceAromeApiProvider::count() const { return mRunInfo.frameCount;}

bool ReosMeteoFranceAromeApiProvider::canReadUri( const QString &path ) const {return false;}

QDateTime ReosMeteoFranceAromeApiProvider::startTime( int index ) const
{
  return mRunInfo.startTime.addSecs( ( index - 1 ) * 3600 );
}

QDateTime ReosMeteoFranceAromeApiProvider::endTime( int index ) const
{
  return mRunInfo.startTime.addSecs( ( index ) * 3600 );
}

ReosRasterExtent ReosMeteoFranceAromeApiProvider::extent() const {return mExtent;}

const QVector<double> ReosMeteoFranceAromeApiProvider::data( int index ) const
{
  if ( index < 0 || index >= mValues.count() )
    return QVector<double>();

  return mValues.at( index );
}

bool ReosMeteoFranceAromeApiProvider::getDirectMinMax( double &, double & ) const {return false;}

void ReosMeteoFranceAromeApiProvider::calculateMinMax( double &min, double &max ) const
{
  min = std::numeric_limits<double>::max();
  max = -std::numeric_limits<double>::max();
  for ( const QVector<double> &frame : mValues )
  {
    for ( double val : frame )
    {
      if ( val < min )
        min = val;
      if ( val > max )
        max = val;
    }
  }
}

ReosEncodedElement ReosMeteoFranceAromeApiProvider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "arome-api-gridded-precipitation" ) );

  QString uriToEncode = dataSource();
  QString keyFilePath = apiKeyFileNamefromUri( uriToEncode );
  keyFilePath = context.pathToEncode( keyFilePath );
  uriToEncode = uri( keyFilePath,
                     zoneFromUri( uriToEncode ), resolFromUri( uriToEncode ), extentFromUri( uriToEncode ), runIndexfromUri( uriToEncode ) );
  element.addData( QStringLiteral( "data-source" ), uriToEncode );

  return element;
}

void ReosMeteoFranceAromeApiProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != QStringLiteral( "arome-api-gridded-precipitation" ) )
    return;
  QString source;
  if ( element.getData( QStringLiteral( "data-source" ), source ) )
  {
    QString keyFilePath = apiKeyFileNamefromUri( source );
    keyFilePath = context.resolvePath( keyFilePath );
    source = uri( keyFilePath, zoneFromUri( source ), resolFromUri( source ), extentFromUri( source ), runIndexfromUri( source ) );
    setDataSource( source );
  }
}


ReosGriddedRainfallProvider *ReosMeteoFranceAromeApiProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosMeteoFranceAromeApiProvider::dataType() )
    return new ReosMeteoFranceAromeApiProvider;

  return nullptr;
}

QString ReosMeteoFranceAromeApiProviderFactory::key() const
{
  return AROME_KEY;
}

bool ReosMeteoFranceAromeApiProviderFactory::hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const
{
  if ( dataType.contains( ReosGriddedRainfall::staticType() ) )
    return ( mCapabilities & capabilities ) == capabilities;

  return false;
}
