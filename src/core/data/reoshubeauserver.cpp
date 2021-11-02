/***************************************************************************
  reoshubeauserver.cpp - ReosHubEauServer

 ---------------------
 begin                : 31.10.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshubeauserver.h"

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QElapsedTimer>
#include <QThread>
#include <QEventLoop>
#include <QJsonDocument>
#include <QCborValue>
#include <qjsondocument.h>
#include <QTimer>

#include "reosmapextent.h"


ReosHubEauConnection::ReosHubEauConnection( QObject *parent )
  : QObject( parent )
  , mNetworkAccessManager( new QNetworkAccessManager( this ) )
{
  mBaseUri = "https://hubeau.eaufrance.fr/api/v1/hydrometrie/";
  connect( mNetworkAccessManager, &QNetworkAccessManager::finished, this, &ReosHubEauConnection::onReplied );
}

void ReosHubEauConnection::launchRequest()
{
  if ( mWaitedReply )
    mWaitedReply->deleteLater();

  mWaitedReply = mNetworkAccessManager->get( QNetworkRequest( mRequest ) );
  mRequestInProgress = true;
}

void ReosHubEauConnection::request( const QString &string )
{
  mRequest = mBaseUri + string;
  QTimer::singleShot( 1, this, &ReosHubEauConnection::launchRequest );
}

void ReosHubEauConnection::requestByUrl( const QString &Url )
{
  mRequest = Url;
  QTimer::singleShot( 1, this, &ReosHubEauConnection::launchRequest );
}

void ReosHubEauConnection::onReplied( QNetworkReply *reply )
{
  if ( reply != mWaitedReply )
    return;

  mErrorCode = reply->error();

  QTextStream textStream( reply );
  QJsonDocument mJsonResult = QJsonDocument::fromJson( textStream.readAll().toUtf8() );
  QVariant var = mJsonResult.toVariant();
  if ( var.type() != QVariant::Map )
    return;

  mResult = var.toMap();

  mRequestInProgress = false;
  emit repliedReady();
  mWaitedReply->deleteLater();
  mWaitedReply = nullptr;
}

int ReosHubEauConnection::errorCode() const
{
  return mErrorCode;
}

QVariantMap ReosHubEauConnection::result() const
{
  return mResult;
}


ReosHubEauConnectionControler::ReosHubEauConnectionControler( QObject *parent ): QObject( parent )
{
  mConnection = new ReosHubEauConnection();
  mThread = new QThread( this );
  mConnection->moveToThread( mThread );
  mThread->start();
  connect( mThread, &QThread::finished, mConnection, &QObject::deleteLater );
  connect( mConnection, &ReosHubEauConnection::repliedReady, this, &ReosHubEauConnectionControler::onReplied );
}


ReosHubEauAccess::ReosHubEauAccess( QObject *parent )
  : QObject( parent )
{
  mStationsRequestControler = new ReosHubEauConnectionControler( this );
  connect( mStationsRequestControler, &ReosHubEauConnectionControler::resultReady, this, &ReosHubEauAccess::addStations );
}

ReosHubEauConnectionControler::~ReosHubEauConnectionControler()
{
  mThread->quit();
  mThread->wait();
}

void ReosHubEauConnectionControler::request( const QString &requestString )
{
  mConnection->request( requestString );
}


void ReosHubEauConnectionControler::requestAndWait( const QString &requestString )
{
  QEventLoop loop;
  connect( this, &ReosHubEauConnectionControler::requestFinished, &loop, &QEventLoop::quit );
  mConnection->request( requestString );
  loop.exec();
}

int ReosHubEauConnectionControler::lastError() const
{
  return mConnection->errorCode();
}

void ReosHubEauConnectionControler::onReplied()
{
  QVariantMap result = mConnection->result();
  if ( result.contains( QStringLiteral( "next" ) ) )
    mNextURL = result.value( QStringLiteral( "next" ) ).toString();
  else
    mNextURL = QString();

  mError = mConnection->errorCode();

  emit resultReady( result );

  if ( !mNextURL.isEmpty() )
    mConnection->requestByUrl( mNextURL );
  else
    emit requestFinished();
}

bool ReosHubEauAccess::testConnection()
{
  ReosHubEauConnectionControler controler;
  controler.requestAndWait( QStringLiteral( "referentiel/stations?bbox=0,0,1,1&format=json&pretty&page=1&size=20" ) );

  return controler.lastError() == 0;
}

void ReosHubEauAccess::setExtent( const ReosMapExtent &extent )
{
  mExtent = extent;

  mStations.clear();

  double lontMin = extent.xMapMin();
  double lontMax = extent.xMapMax();
  double latMin = extent.yMapMin();
  double latMax = extent.yMapMax();

  const QString request = QStringLiteral( "referentiel/stations?bbox=%1,%2,%3,%4&format=json&pretty&page=1&size=2000" ).arg( lontMin ).arg( latMin ).arg( lontMax ).arg( latMax );
  mStationsRequestControler->request( request );
}

void ReosHubEauAccess::addStations( const QVariantMap &requestResult )
{
  if ( !requestResult.contains( QStringLiteral( "data" ) ) )
    return;

  const QVariantList stations = requestResult.value( QStringLiteral( "data" ) ).toList();

  for ( const QVariant &stationVariant : stations )
  {
    const QVariantMap mapStation = stationVariant.toMap();

    ReosHubEauStation station;
    if ( mapStation.contains( QStringLiteral( "libelle_station" ) ) )
      station.name = mapStation.value( QStringLiteral( "libelle_station" ) ).toString();
    if ( mapStation.contains( QStringLiteral( "code_station" ) ) )
      station.id = mapStation.value( QStringLiteral( "code_station" ) ).toString();
    if ( mapStation.contains( QStringLiteral( "longitude_station" ) ) )
      station.longitude = mapStation.value( QStringLiteral( "longitude_station" ) ).toDouble();
    if ( mapStation.contains( QStringLiteral( "latitude_station" ) ) )
      station.latitude = mapStation.value( QStringLiteral( "latitude_station" ) ).toDouble();

    mStations.append( station );
  }

  emit stationsUpdated();
}

QList<ReosHubEauStation> ReosHubEauAccess::stations() const
{
  return mStations;
}


//QStringList ReosHubEauAccess::stationsInExtent( const ReosMapExtent &extent )
//{
//  double lontMin = extent.xMapMin();
//  double lontMax = extent.xMapMax();
//  double latMin = extent.yMapMin();
//  double latMax = extent.yMapMax();
//  QString request = QStringLiteral( "referentiel/stations?bbox=%1,%2,%3,%4&format=json&pretty&page=1&size=1" ).arg( lontMin ).arg( latMin ).arg( lontMax ).arg( latMax );
//  requestAndWait( request );

//  QVariantMap variantMap = mConnection->result();

//  if ( !variantMap.contains( QStringLiteral( "count" ) ) )
//    return QStringList();

//  int stationCount = variantMap.value( QStringLiteral( "count" ) ).toInt();
//  request = QStringLiteral( "referentiel/stations?bbox=%1,%2,%3,%4&format=json&pretty&page=1&size=%5" ).arg( lontMin ).arg( latMin ).arg( lontMax ).arg( latMax ).arg( stationCount );
//  requestAndWait( request );

//  variantMap =  mConnection->result();

//  if ( !variantMap.contains( QStringLiteral( "data" ) ) )
//    return QStringList();

//  QVariant varData = variantMap.value( QStringLiteral( "data" ) );

//  if ( varData.type() != QVariant::List )
//    return QStringList();

//  QVariantList stations = varData.toList();

//  QStringList ret;
//  for ( const QVariant &station : stations )
//  {
//    QVariantMap stationMap = station.toMap();
//    if ( !stationMap.contains( QStringLiteral( "code_station" ) ) )
//      continue;
//    request = QStringLiteral( "observations_tr?code_entite=%1&format=json&pretty&page=1&size=1" ).arg( stationMap.value( QStringLiteral( "code_site" ) ).toString() );
//    requestAndWait( request );

//    variantMap = mConnection->result();

//    if ( !variantMap.contains( QStringLiteral( "count" ) ) )
//      continue;

//    if ( variantMap.value( QStringLiteral( "count" ) ).toInt() != 0 )
//      ret.append( stationMap.value( QStringLiteral( "libelle_site" ) ).toString() );
//  }

//  return ret;
//}
