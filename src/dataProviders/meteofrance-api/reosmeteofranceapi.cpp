/***************************************************************************
  reosmeteofranceapi.cpp - ReosMeteoFranceApi

 ---------------------
 begin                : 28.3.2023
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
#include "reosmeteofranceapi.h"

#include <QFileInfo>
#include <QTextStream>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QEventLoop>
#include <QElapsedTimer>
#include <QDomDocument>
#include <QDateTime>
#include <QThread>
#include <QTimer>

#include "reosnetworkaccessmanager.h"
#include "reosgisengine.h"

ReosMeteoFranceApi::ReosMeteoFranceApi( const QString &keyFileName )
{
  mAromeService.reset( new  ReosMeteoFranceApiArome( keyFileName ) );
}

QString ReosMeteoFranceApi::name() const
{
  return QStringLiteral( "API Météo France" );
}

ReosMeteoFranceApiArome *ReosMeteoFranceApi::aromeService() const
{
  return mAromeService.get();
}


QList<ReosMeteoFranceApiArome::Model>  ReosMeteoFranceApiArome::sAvailableModels =
{
  Model( {QStringLiteral( "FRANCE" ), QStringLiteral( "001" )} ),
  Model( {QStringLiteral( "FRANCE" ), QStringLiteral( "0025" )} ),
  Model( {QStringLiteral( "NCALED" ), QStringLiteral( "0025" )} ),
  Model( {QStringLiteral( "INDIEN" ), QStringLiteral( "0025" )} ),
  Model( {QStringLiteral( "GUYANE" ), QStringLiteral( "0025" )} ),
  Model( {QStringLiteral( "ANTIL" ), QStringLiteral( "0025" )} ),
  Model( {QStringLiteral( "POLYN" ), QStringLiteral( "0025" )} )
};

bool ReosMeteoFranceApiArome::hasKey() const
{
    return !mKeyApi.isEmpty();
}

ReosMeteoFranceApiArome::ReosMeteoFranceApiArome( const QString &keyFileName )
    : mKeyFileName( keyFileName )
{
  setKey( keyFileName );
}

void ReosMeteoFranceApiArome::connectToService( const Model &model, QString &error )
{
  mCoverageIds.clear();
  mModel = model;
  QString strUrl = baseUrl( model ) + capabilitiesRequest();

  QByteArray bytes = networkRequestBlocking( strUrl, error );
  if ( !error.isEmpty() )
    return;

  QDomDocument domDoc( QStringLiteral( "reply" ) );
  domDoc.setContent( bytes );

  QDomElement rootElem = domDoc.firstChildElement( QStringLiteral( "wcs:Capabilities" ) );
  if ( rootElem.isNull() )
  {
    error = QObject::tr( "Invalid reply of the server." );
    return;
  }

  QDomElement contentElement = rootElem.firstChildElement( QStringLiteral( "wcs:Contents" ) );

  if ( contentElement.isNull() )
  {
    error = QObject::tr( "Invalid reply of the server." );
    return;
  }

  QDomNode coverageSummary = contentElement.firstChildElement( QStringLiteral( "wcs:CoverageSummary" ) );
  while ( !coverageSummary.isNull() )
  {
    const QDomElement coverageIdElem = coverageSummary.firstChildElement( QStringLiteral( "wcs:CoverageId" ) );
    QString idText = coverageIdElem.text();
    if ( idText.startsWith( QStringLiteral( "TOTAL_PRECIPITATION_RATE__GROUND_OR_WATER_SURFACE___" ) ) )
    {
      QString isoDate = idText;
      isoDate.remove( QStringLiteral( "TOTAL_PRECIPITATION_RATE__GROUND_OR_WATER_SURFACE___" ) );
      isoDate.replace( '.', ':' );
      const QDateTime time = QDateTime::fromString( isoDate, Qt::ISODate );
      if ( time.isValid() )
      {
        mCoverageIds.insert( time, idText );
      }
    }
    coverageSummary = coverageSummary.nextSiblingElement( QStringLiteral( "wcs:CoverageSummary" ) );
  }
}


QList<QDateTime> ReosMeteoFranceApiArome::availableRuns() const
{
  return mCoverageIds.keys();
}

ReosMeteoFranceApiArome::RunInfo ReosMeteoFranceApiArome::runInfo( const QDateTime &run ) const
{
  QString strUrl = baseUrl( mModel ) + describeCoverageRequest( run );

  RunInfo runInfo;
  QString error;
  QByteArray bytes = networkRequestBlocking( strUrl, error );
  if ( !error.isEmpty() )
    return runInfo;

  QDomDocument domDoc( QStringLiteral( "reply" ) );
  domDoc.setContent( bytes );

  const QDomElement rootElem = domDoc.firstChildElement( QStringLiteral( "wcs:CoverageDescriptions" ) );
  if ( rootElem.isNull() )
  {
    error = QObject::tr( "Invalid reply of the server." );
    return runInfo;
  }

  const QDomElement coverageElement = rootElem.firstChildElement( QStringLiteral( "wcs:CoverageDescription" ) );
  if ( coverageElement.isNull() )
  {
    error = QObject::tr( "Invalid reply of the server." );
    return runInfo;
  }

  const QDomElement boundedElement = coverageElement.firstChildElement( QStringLiteral( "gml:boundedBy" ) );
  if ( boundedElement.isNull() )
  {
    error = QObject::tr( "Invalid reply of the server." );
    return runInfo;
  }

  const QDomElement envelopElement = boundedElement.firstChildElement( QStringLiteral( "gml:EnvelopeWithTimePeriod" ) );
  if ( envelopElement.isNull() )
  {
    error = QObject::tr( "Invalid reply of the server." );
    return runInfo;
  }

  QPointF lowerCorner;
  QPointF upperCorner;
  const QDomElement lowerCornerElem = envelopElement.firstChildElement( QStringLiteral( "gml:lowerCorner" ) );
  QString str = lowerCornerElem.text();
  QStringList parts = str.split( ' ' );
  if ( parts.count() == 2 )
  {
    lowerCorner = QPointF( parts.at( 0 ).toDouble(), parts.at( 1 ).toDouble() );

    const QDomElement upperCornerElem = envelopElement.firstChildElement( QStringLiteral( "gml:upperCorner" ) );
    str = upperCornerElem.text();
    parts = str.split( ' ' );
    if ( parts.count() == 2 )
    {
      upperCorner = QPointF( parts.at( 0 ).toDouble(), parts.at( 1 ).toDouble() );
    }
    else
    {
      error = QObject::tr( "Invalid reply of the server." );
      return runInfo;
    }
  }
  else
  {
    error = QObject::tr( "Invalid reply of the server." );
    return runInfo;
  }

  QString crs = ReosGisEngine::crsFromEPSG( 4326 );
  runInfo.extent = ReosMapExtent( ReosSpatialPosition( lowerCorner, crs ), ReosSpatialPosition( upperCorner, crs ) );

  const QDomElement beginPositionElem = envelopElement.firstChildElement( QStringLiteral( "gml:beginPosition" ) );
  const QDomElement endPositionElem = envelopElement.firstChildElement( QStringLiteral( "gml:endPosition" ) );
  const QString strBegin = beginPositionElem.text();
  const QString strEnd = endPositionElem.text();
  runInfo.startTime = QDateTime::fromString( strBegin, Qt::ISODate );
  runInfo.endTime = QDateTime::fromString( strEnd, Qt::ISODate );
  runInfo.frameCount = static_cast<int>( std::round( static_cast<double>( runInfo.startTime.secsTo( runInfo.endTime ) ) / 3600.0 ) ) + 1;

  return runInfo;
}


QByteArray ReosMeteoFranceApiArome::requestFrameBlocking( const ReosMapExtent &extent, const QDateTime &run, int frameIndex ) const
{
  QString strUrl = baseUrl( mModel ) + coverageRequest( run, frameIndex, extent );

  QString error;
  return networkRequestBlocking( strUrl, error );
}

void ReosMeteoFranceApiArome::requestFrame( const ReosMapExtent &extent, const QDateTime &run, int frameIndex )
{
  QString strUrl = baseUrl( mModel ) + coverageRequest( run, frameIndex, extent );

  QString error;
  QNetworkReply *reply = networkRequest( strUrl, error );

  connect( reply, &QNetworkReply::finished, this, [ this, reply, frameIndex, extent, run ]
  {
    bool throttled = false;
    QByteArray bytes = preventThrottling( reply, throttled );
    if ( !throttled )
      emit valuesReady( bytes, frameIndex );
    else
    {
      qDebug() << "Waiting 60 s before retrying.";
      requestFrameDeffered( extent, run, frameIndex, 60000 );
    }
    reply->deleteLater();
  } );
}

void ReosMeteoFranceApiArome::requestFrameDeffered( const ReosMapExtent &extent, const QDateTime &run, int frameIndex, int delay )
{
  QTimer::singleShot( delay, this, [this, extent, run, frameIndex]
  {
    requestFrame( extent, run, frameIndex );
  } );
}

QStringList ReosMeteoFranceApiArome::extentToLonLatList( const ReosMapExtent &extent )
{
  ReosMapExtent lonLat = ReosGisEngine::transformExtent( extent, ReosGisEngine::crsFromEPSG( 4326 ) );
  QString latMin = QString::number( lonLat.yMapMin(), 'f', 16 );
  QString latMax = QString::number( lonLat.yMapMax(), 'f', 16 );
  QString lonMin = QString::number( lonLat.xMapMin(), 'f', 16 );
  QString lonMax = QString::number( lonLat.xMapMax(), 'f', 16 );

  QStringList ret;
  ret  << lonMin << lonMax << latMin << latMax;

  return ret;
}

QByteArray ReosMeteoFranceApiArome::preventThrottling( QNetworkReply *reply, bool &throttled )
{
  throttled = false;

  if ( reply->header( QNetworkRequest::ContentTypeHeader ).toString().contains( "application/xml" ) )
  {
    const QByteArray ret = reply->readAll();
    throttled = QString( ret ).contains( QStringLiteral( "Message throttled out" ) );
    if ( throttled )
      qDebug() << tr( "Meteo France API throttled" );
    return ret;
  }

  return reply->readAll();
}

QString ReosMeteoFranceApiArome::capabilitiesRequest() const
{
  return QStringLiteral( "GetCapabilities?service=%1&version=%2&language=%3&apikey=%4" ).arg(
           mVersion.service.toUpper(), mVersion.version, mVersion.language, QString::fromUtf8( QUrl::toPercentEncoding( mKeyApi ) ) );
}

QString ReosMeteoFranceApiArome::describeCoverageRequest( const QDateTime &run ) const
{
  return QStringLiteral( "DescribeCoverage?service=%1&version=%2&language=%3&coverageID=%4&apikey=%5" ).arg(
           mVersion.service.toUpper(), mVersion.version, mVersion.language, mCoverageIds.value( run ), QString::fromUtf8( QUrl::toPercentEncoding( mKeyApi ) ) );
}

QString ReosMeteoFranceApiArome::coverageRequest( const QDateTime &run, int frameIndex, const ReosMapExtent &extent ) const
{
  QDateTime time = run.addSecs( ( 1 + static_cast<qint64>( frameIndex ) ) * 3600 );
  QString isoDate = time.toString( Qt::ISODate );
  QStringList extentString = extentToLonLatList( extent );


  return QStringLiteral( "GetCoverage?service=%1&version=%2&coverageid=%3&subset=time(%4)&subset=lat(%5,%6)&subset=long(%7,%8)&format=" )
         .arg( mVersion.service.toUpper(),
               mVersion.version,
               mCoverageIds.value( run ),
               isoDate,
               extentString.at( 2 ),
               extentString.at( 3 ),
               extentString.at( 0 ),
               extentString.at( 1 ) )
         + QString::fromUtf8( QUrl::toPercentEncoding( QStringLiteral( "application/wmo-grib" ) ) )
         + QStringLiteral( "&apikey=%1" ).arg( QString::fromUtf8( QUrl::toPercentEncoding( mKeyApi ) ) );
}

QByteArray ReosMeteoFranceApiArome::networkRequestBlocking( const QString &stringRequest, QString &error ) const
{
  QUrl url( stringRequest );

  QNetworkRequest request;
  request.setUrl( url );
  request.setRawHeader( QByteArray( "User-Agent" ), QByteArray( "Lekan/2.3" ) );

  ReosNetworkAccessManager *networkManager = ReosNetworkAccessManager::instance();

  QNetworkReply *reply = nullptr;
  if ( networkManager )
  {
    reply = networkManager->getBlocking( request, error, mTimeOutDelay );
    bool throttled = false;
    QByteArray ret = preventThrottling( reply, throttled );
    while ( throttled )
    {
      qDebug() << "Waitint 60 s...";
      QThread::sleep( 60 );
      reply->deleteLater();
      reply = networkManager->getBlocking( request, error, mTimeOutDelay );
      ret = preventThrottling( reply, throttled );
    }
    return ret;
  }
  else
  {
    error = QObject::tr( "Unable to have a network connection" );
    return QByteArray();
  }
}

QNetworkReply *ReosMeteoFranceApiArome::networkRequest( const QString &stringRequest, QString &error ) const
{
  QUrl url( stringRequest );

  QNetworkRequest request;
  request.setUrl( url );
  request.setRawHeader( QByteArray( "User-Agent" ), QByteArray( "Lekan/2.3" ) );

  ReosNetworkAccessManager *networkManager = ReosNetworkAccessManager::instance();

  QNetworkReply *reply = nullptr;
  if ( networkManager )
  {
    reply = networkManager->get( request );
  }
  else
  {
    error = QObject::tr( "Unable to have a network connection" );
  }

  return reply;
}

bool ReosMeteoFranceApiArome::setKey( const QString &fileName )
{
  QFileInfo fileInfo( fileName );

  if ( !fileInfo.exists() )
    return false;

  QFile file( fileName );

  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QTextStream stream( &file );
  mKeyApi = stream.readAll();

  return !mKeyApi.isEmpty();
}

QString ReosMeteoFranceApiArome::baseUrl( const Model &model ) const
{
  return mServerUrl + QStringLiteral( "/%1/MF-NWP-HIGHRES-AROME%5-%2-%3-%4/" ).
         arg( mVersion.service.toLower(), model.resol, model.zone, mVersion.service,
              model.zone != QStringLiteral( "FRANCE" ) ? QStringLiteral( "-OM" ) : QString() );
}


