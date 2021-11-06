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


ReosHubEauServer::ReosHubEauServer( QObject *parent )
  : QObject( parent )
{
  mStationsRequestControler = new ReosHubEauConnectionControler( this );
  connect( mStationsRequestControler, &ReosHubEauConnectionControler::resultReady, this, &ReosHubEauServer::addStations );
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

  const QString request = QStringLiteral( "referentiel/stations?bbox=%1,%2,%3,%4&format=json&pretty&page=1&size=2000" ).arg( lontMin ).arg( latMin ).arg( lontMax ).arg( latMax );
  mStationsRequestControler->request( request );
}

void ReosHubEauServer::addStations( const QVariantMap &requestResult )
{
  if ( !requestResult.contains( QStringLiteral( "data" ) ) )
    return;

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

QList<ReosHubEauStation> ReosHubEauServer::stations() const
{
  return mStations;
}

ReosHydrograph *ReosHubEauServer::createHydrograph( const QString &stationId ) const
{
  std::unique_ptr<ReosHydrograph> hyd = std::make_unique<ReosHydrograph>( nullptr, QStringLiteral( "hub-eau-hydrograph" ), stationId );
  hyd->setColor( QColor( 12, 114, 185 ) );
  return hyd.release();
}

QString ReosHubEauHydrographProvider::key() const {return QStringLiteral( "hub-eau-hydrograph" );}

QDateTime ReosHubEauHydrographProvider::referenceTime() const {return mReferenceTime;}

void ReosHubEauHydrographProvider::setReferenceTime( const QDateTime &referenceTime )
{
  mReferenceTime = referenceTime;
}

QString ReosHubEauHydrographProvider::valueUnit() const {return QString();}

int ReosHubEauHydrographProvider::valueCount() const {return mCachedValues.count();}

double ReosHubEauHydrographProvider::value( int i ) const  {return mCachedValues.at( i );}

double ReosHubEauHydrographProvider::firstValue() const {return mCachedValues.first();}

double ReosHubEauHydrographProvider::lastValue() const {return mCachedValues.last();}

void ReosHubEauHydrographProvider::load()
{
  mCachedTimeValues.clear();
  mCachedValues.clear();
  if ( !mFlowRequestControler )
  {
    mFlowRequestControler = new ReosHubEauConnectionControler( this );
    connect( mFlowRequestControler, &ReosHubEauConnectionControler::resultReady, this, &ReosHubEauHydrographProvider::onResultReady );
    connect( mFlowRequestControler, &ReosHubEauConnectionControler::requestFinished, this, &ReosHubEauHydrographProvider::onLoadingFinished );
  }

  mFlowRequestControler->request( QStringLiteral( "observations_tr?code_entite=%1&size=20000&pretty&grandeur_hydro=Q&fields=code_station,date_obs,resultat_obs&sort=asc" ).arg( dataSource() ) );
  mStatus = Status::Loading;
}

double *ReosHubEauHydrographProvider::data() {return mCachedValues.data();}

const QVector<double> &ReosHubEauHydrographProvider::constData() const {return mCachedValues;}

ReosEncodedElement ReosHubEauHydrographProvider::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hub-eau-hydrograph" ) );
  element.addData( QStringLiteral( "source" ), dataSource() );

  return element;
}

void ReosHubEauHydrographProvider::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "hub-eau-hydrograph" ) )
    return;

  QString source;
  element.getData( QStringLiteral( "source" ), source );
  setDataSource( source );
}

ReosDuration ReosHubEauHydrographProvider::relativeTimeAt( int i ) const {return mCachedTimeValues.at( i );}

ReosDuration ReosHubEauHydrographProvider::lastRelativeTime() const {return mCachedTimeValues.last();}

void ReosHubEauHydrographProvider::onResultReady( const QVariantMap &result )
{
  if ( !result.contains( QStringLiteral( "data" ) ) )
    return;

  QVariant data = result.value( QStringLiteral( "data" ) );

  if ( data.type() != QVariant::List )
    return;

  const QVariantList dataList = data.toList();

  if ( dataList.count() == 0 )
  {
    mStatus = Status::NoData;
    return;
  }

  if ( !mReferenceTime.isValid() )
  {
    QVariantMap firstData =   dataList.at( 0 ).toMap();
    QString dateString = firstData.value( QStringLiteral( "date_obs" ) ).toString();
    mReferenceTime = QDateTime::fromString( dateString, Qt::ISODate );
  }

  for ( const QVariant &varDat : dataList )
  {
    QVariantMap mapVar = varDat.toMap();
    const QDateTime time = QDateTime::fromString( mapVar.value( QStringLiteral( "date_obs" ) ).toString(), Qt::ISODate );
    const ReosDuration relativeTime = ReosDuration( mReferenceTime.msecsTo( time ), ReosDuration::millisecond );
    double value = mapVar.value( QStringLiteral( "resultat_obs" ) ).toDouble() / 1000.0; // server gives valu in l/s
    mCachedValues.append( value );
    mCachedTimeValues.append( relativeTime );
  }

  emit dataChanged();
}

void ReosHubEauHydrographProvider::onLoadingFinished()
{
  if ( mCachedValues.count() == 0 )
    mStatus = Status::NoData;
  else
    mStatus = Status::Loaded;

  emit dataChanged();
}

ReosHubEauHydrographProvider::Status ReosHubEauHydrographProvider::status() const
{
  return mStatus;
}
