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
#include "reoshydrograph.h"

#include <QLocale>

QString ReosHubEauHydrographProvider::key() const {return ReosHubEauHydrographProvider::staticKey();}

QStringList ReosHubEauHydrographProvider::fileSuffixes() const
{
  return QStringList();
}

QDateTime ReosHubEauHydrographProvider::referenceTime() const {return mReferenceTime;}

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
    connect( mFlowRequestControler, &ReosHubEauConnectionControler::errorOccured, this, &ReosHubEauHydrographProvider::onErrorOccured );
    connect( mFlowRequestControler, &ReosHubEauConnectionControler::requestFinished, this, &ReosHubEauHydrographProvider::onLoadingFinished );
  }

  mFlowRequestControler->request( QStringLiteral( "observations_tr?code_entite=%1&size=20000&pretty&grandeur_hydro=Q&fields=code_station,date_obs,resultat_obs&sort=asc" ).arg( dataSource() ) );
  mStatus = Status::Loading;
  emit dataChanged();
}

double *ReosHubEauHydrographProvider::data() {return mCachedValues.data();}

const QVector<ReosDuration> &ReosHubEauHydrographProvider::constTimeData() const
{
  return mCachedTimeValues;
}

const QVector<double> &ReosHubEauHydrographProvider::constData() const {return mCachedValues;}

ReosEncodedElement ReosHubEauHydrographProvider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( ReosHubEauHydrographProvider::staticKey() );
  element.addData( QStringLiteral( "source" ), dataSource() );

  element.addData( QStringLiteral( "metadata" ), mMetadata );

  return element;
}

void ReosHubEauHydrographProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext & )
{
  if ( element.description() != ReosHubEauHydrographProvider::staticKey() )
    return;

  QString source;
  element.getData( QStringLiteral( "source" ), source );
  element.getData( QStringLiteral( "metadata" ), mMetadata );
  setDataSource( source );

  // update the metadata with server
  mMetadataRequestControler = new ReosHubEauConnectionControler( this );
  connect( mMetadataRequestControler, &ReosHubEauConnectionControler::resultReady, this, &ReosHubEauHydrographProvider::onMetadataReady );
  connect( mMetadataRequestControler, &ReosHubEauConnectionControler::errorOccured, this, &ReosHubEauHydrographProvider::onErrorOccured );
  mMetadataRequestControler->request( QStringLiteral( "referentiel/stations?code_entite=%1&fields=code_station,libelle_station,type_station,longitude_station,latitude_station,en_service,date_ouverture_station,date_fermeture_station,influence_locale_station,commentaire_influence_locale_station,commentaire_station&format=json&pretty&page=1&size=1" ).arg( source ) );
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
  emit dataReset();
}

void ReosHubEauHydrographProvider::onLoadingFinished()
{
  if ( mCachedValues.count() == 0 )
    mStatus = Status::NoData;
  else
    mStatus = Status::Loaded;

  emit dataChanged();
}

void ReosHubEauHydrographProvider::onMetadataReady( const QVariantMap &result )
{
  if ( result.contains( QStringLiteral( "data" ) ) )
  {
    const QVariant varData = result.value( QStringLiteral( "data" ) );
    if ( varData.type() == QVariant::Map )
    {
      mMetadata = varData.toMap();
      mMetadataRequestControler->deleteLater();
      mMetadataRequestControler = nullptr;
      emit dataChanged();
    }
  }
}

void ReosHubEauHydrographProvider::onErrorOccured()
{
  mLastMessage = ReosModule::Message();
  mLastMessage.type = ReosModule::Error;
  mStatus = Status::NoData;

  if ( mFlowRequestControler && mFlowRequestControler->lastError() != 0 )
    mLastMessage.text = tr( "Following error occured with Hubeau server: %1" ).arg( mFlowRequestControler->lastErrorReason() );

  if ( mMetadataRequestControler && mMetadataRequestControler->lastError() != 0 )
    mLastMessage.text = tr( "Following error occured with Hubeau server: %1" ).arg( mMetadataRequestControler->lastErrorReason() );

  emit errorOccured();
}

QVariantMap ReosHubEauHydrographProvider::metadata() const
{
  return mMetadata;
}

void ReosHubEauHydrographProvider::setMetadata( const QVariantMap &metadata )
{
  mMetadata = metadata;
}

ReosModule::Message ReosHubEauHydrographProvider::lastMessage() const
{
  return mLastMessage;
}

QString ReosHubEauHydrographProvider::htmlDescription() const
{
  return htmlDescriptionFromMeta( mMetadata );
}

QString ReosHubEauHydrographProvider::htmlDescriptionFromMeta( const QVariantMap &metadata )
{
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );

  if ( metadata.isEmpty() )
  {
    htmlText += QStringLiteral( "<h2>" ) + tr( "No station selected" ) + QStringLiteral( "</h2>\n<hr>\n" );
  }
  else
  {
    htmlText += QStringLiteral( "<h2>" ) + metadata.value( QStringLiteral( "libelle_station" ) ).toString() + QStringLiteral( "</h2>\n<hr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>On duty</b>" ) + QStringLiteral( "</td><td>" )
                + ( metadata.value( QStringLiteral( "en_service" ) ).toBool() ? tr( "yes" ) : tr( "no" ) )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Station type</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "type_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Opening date</b>" ) + QStringLiteral( "</td><td>" )
                + QLocale().toString( QDateTime::fromString( metadata.value( QStringLiteral( "date_ouverture_station" ) ).toString(), Qt::ISODate ) )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Closing date</b>" ) + QStringLiteral( "</td><td>" )
                + QLocale().toString( QDateTime::fromString( metadata.value( QStringLiteral( "date_fermeture_station" ) ).toString(), Qt::ISODate ) )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Local influence</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "influence_locale_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Local influence comments</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "commentaire_influence_locale_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Comments</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "commentaire_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );
  }

  htmlText += QLatin1String( "\n</body>\n</html>\n" );

  return htmlText;
}

ReosHubEauHydrographProvider::Status ReosHubEauHydrographProvider::status() const
{
  return mStatus;
}

QString ReosHubEauHydrographProvider::staticKey()
{
  return QStringLiteral( "hub-eau-hydrometry" );
}

QString ReosHubEauHydrographProvider::dataType()
{
  return ReosHydrograph::staticType();
}

ReosTimeSerieProvider *ReosHubEauHydrographProviderFactory::createProvider( const QString &dataType ) const
{
  if ( ReosHubEauHydrographProvider::dataType() == dataType )
    return new ReosHubEauHydrographProvider;

  return nullptr;
}

QString ReosHubEauHydrographProviderFactory::key() const
{
  return ReosHubEauHydrographProvider::staticKey();
}

bool ReosHubEauHydrographProviderFactory::hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const
{
  return ( capabilities & mCapabilities ) == mCapabilities;
}

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosHubEauHydrographProviderFactory();
}
