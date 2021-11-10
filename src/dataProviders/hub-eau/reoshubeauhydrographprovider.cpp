/***************************************************************************
  reoshubeauhydrographprovider.cpp - ReosHubEauHydrographProvider

 ---------------------
 begin                : 6.11.2021
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
#include "reoshubeauhydrographprovider.h"

#include "reoshubeauserver.h"
#include "reoscore.h"

QString ReosHubEauHydrographProvider::key() const {return ReosHubEauHydrographProvider::staticKey();}

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
  emit dataChanged();
}

double *ReosHubEauHydrographProvider::data() {return mCachedValues.data();}

const QVector<double> &ReosHubEauHydrographProvider::constData() const {return mCachedValues;}

ReosEncodedElement ReosHubEauHydrographProvider::encode() const
{
  ReosEncodedElement element( ReosHubEauHydrographProvider::staticKey() );
  element.addData( QStringLiteral( "source" ), dataSource() );

  element.addData( QStringLiteral( "metadata" ), mMetaData );

  return element;
}

void ReosHubEauHydrographProvider::decode( const ReosEncodedElement &element )
{
  if ( element.description() != ReosHubEauHydrographProvider::staticKey() )
    return;

  QString source;
  element.getData( QStringLiteral( "source" ), source );
  element.getData( QStringLiteral( "metadata" ), mMetaData );
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

QVariantMap ReosHubEauHydrographProvider::metaData() const
{
  return mMetaData;
}

void ReosHubEauHydrographProvider::setMetaData( const QVariantMap &metaData )
{
  mMetaData = metaData;
}

ReosHubEauHydrographProvider::Status ReosHubEauHydrographProvider::status() const
{
  return mStatus;
}

QString ReosHubEauHydrographProvider::staticKey()
{
  return QStringLiteral( "hub-eau-hydrometry" );
}

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosHubEauHydrographProviderFactory();
}

QString ReosHubEauHydrographProviderFactory::key() const {return ReosHubEauHydrographProvider::staticKey();}
