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
#include "reoshydrograph.h"
#include "reoshubeauhydrographprovider.h"


ReosHubEauConnection::ReosHubEauConnection( QObject *parent )
  : QObject( parent )
  , mNetworkAccessManager( new QNetworkAccessManager( this ) )
{
  mBaseUri = QStringLiteral( "https://hubeau.eaufrance.fr/api/v1/hydrometrie/" );
  connect( mNetworkAccessManager, &QNetworkAccessManager::finished, this, &ReosHubEauConnection::onReplied );
}

void ReosHubEauConnection::launchRequest()
{
  if ( mWaitedReply )
    mWaitedReply->deleteLater();

  mWaitedReply = mNetworkAccessManager->get( QNetworkRequest( mRequest ) );
  mRequestInProgress = true;
}

void ReosHubEauConnection::request( const QString &operation )
{
  mRequest = mBaseUri + operation;
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

  if ( mErrorCode != 0 )
  {
    mErrorString = reply->attribute( QNetworkRequest::HttpReasonPhraseAttribute ).toString();
    if ( mErrorString.isEmpty() )
      mErrorString = reply->errorString();
  }

  QTextStream textStream( reply );
  QJsonDocument mJsonResult = QJsonDocument::fromJson( textStream.readAll().toUtf8() );
  QVariant var = mJsonResult.toVariant();
  if ( var.type() != QVariant::Map )
  {
    emit repliedReady();
    return;
  }

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

QString ReosHubEauConnection::errorString() const
{
  return mErrorString;
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


ReosHubEauServer::ReosHubEauServer( QObject *parent )
  : QObject( parent )
{
  mStationsRequestControler = new ReosHubEauConnectionControler( this );
  connect( mStationsRequestControler, &ReosHubEauConnectionControler::resultReady, this, &ReosHubEauServer::addStations );
  connect( mStationsRequestControler, &ReosHubEauConnectionControler::errorOccured, this, &ReosHubEauServer::onErrorOccured );
}

ReosHubEauConnectionControler::~ReosHubEauConnectionControler()
{
  mThread->quit();
  mThread->wait();
}

void ReosHubEauConnectionControler::request( const QString &operation )
{
  mConnection->request( operation );
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

QString ReosHubEauConnectionControler::lastErrorReason() const
{
  return mConnection->errorString();
}

void ReosHubEauConnectionControler::onReplied()
{
  QVariantMap result = mConnection->result();
  if ( result.contains( QStringLiteral( "next" ) ) )
    mNextURL = result.value( QStringLiteral( "next" ) ).toString();
  else
    mNextURL = QString();

  mError = mConnection->errorCode();

  if ( mError != 0 )
  {
    emit errorOccured();
    return;
  }

  emit resultReady( result );

  if ( !mNextURL.isEmpty() )
    mConnection->requestByUrl( mNextURL );
  else
    emit requestFinished();
}

bool ReosHubEauServer::testConnection()
{
  ReosHubEauConnectionControler controler;
  controler.requestAndWait( QStringLiteral( "referentiel/stations?bbox=0,0,1,1&format=json&pretty&page=1&size=20" ) );

  return controler.lastError() == 0;
}

void ReosHubEauServer::setExtent( const ReosMapExtent &extent )
{
  mExtent = extent;

  mStations.clear();

  double lontMin = extent.xMapMin();
  double lontMax = extent.xMapMax();
  double latMin = extent.yMapMin();
  double latMax = extent.yMapMax();

  const QString request = QStringLiteral( "referentiel/stations?bbox=%1,%2,%3,%4&en_service=true&fields=code_station,libelle_station,type_station,longitude_station,latitude_station,en_service,date_ouverture_station,date_fermeture_station,influence_locale_station,commentaire_influence_locale_station,commentaire_station&format=json&pretty&page=1&size=2000" ).arg( lontMin ).arg( latMin ).arg( lontMax ).arg( latMax );
  mStationsRequestControler->request( request );
}

void ReosHubEauServer::addStations( const QVariantMap &requestResult )
{
  mLastMessage = ReosModule::Message();

  if ( !requestResult.contains( QStringLiteral( "data" ) ) )
  {
    onErrorOccured();
    return;
  }

  mLastMessage = ReosModule::Message();

  const QVariantList stations = requestResult.value( QStringLiteral( "data" ) ).toList();

  for ( const QVariant &stationVariant : stations )
  {
    const QVariantMap mapStation = stationVariant.toMap();

    ReosHubEauStation station;
    station.meta = mapStation;
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

void ReosHubEauServer::onErrorOccured()
{
  if ( mStationsRequestControler )
  {
    mLastMessage = ReosModule::Message();
    mLastMessage.type = ReosModule::Error;
    mLastMessage.text = tr( "Following error occured with Hubeau server: %1" ).arg( mStationsRequestControler->lastErrorReason() );

    emit errorOccured();
  }
}

QList<ReosHubEauStation> ReosHubEauServer::stations() const
{
  return mStations;
}

ReosHydrograph *ReosHubEauServer::createHydrograph( const QString &stationId, const QVariantMap &meta, QObject *parent ) const
{
  std::unique_ptr<ReosHydrograph> hyd = std::make_unique<ReosHydrograph>( parent, ReosHubEauHydrographProvider::staticKey(), stationId );
  hyd->setColor( QColor( 12, 114, 185 ) );
  hyd->setName( meta.value( QStringLiteral( "libelle_station" ) ).toString() );

  ReosHubEauHydrographProvider *provider = qobject_cast<ReosHubEauHydrographProvider *>( hyd->dataProvider() );
  Q_ASSERT( provider );
  provider->setMetadata( meta );
  return hyd.release();
}

ReosModule::Message ReosHubEauServer::lastMessage() const
{
  return mLastMessage;
}
