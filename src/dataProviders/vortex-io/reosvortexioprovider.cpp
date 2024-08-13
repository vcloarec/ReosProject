/***************************************************************************
  reoseaufranceprovider.cpp - ReosEauFranceProvider

 ---------------------
 begin                : 13.6.2024
 copyright            : (C) 2024 by vorteX-io
 email                : v.cloarec at vortex-io dot fr
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosvortexioprovider.h"
#include <QTextStream>
#include <QDebug>
#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QEventLoop>
#include <QJsonDocument>

#include "reoshydrograph.h"

#define VORTEXIO_KEY QStringLiteral("vortexio")


REOSEXTERN ReosVortexIoProviderFactory *providerFactory()
{
  return new ReosVortexIoProviderFactory;
}

ReosVortexIoProvider::ReosVortexIoProvider()
  : mNetworkManager( new QNetworkAccessManager( this ) )
{
  mBaseUri = QStringLiteral( "https://www.hydro.eaufrance.fr/stationhydro/ajax/%1/series?hydro_series[startAt]=%2&hydro_series[endAt]=%3&hydro_series[variableType]=simple_and_interpolated_and_hourly_variable&hydro_series[simpleAndInterpolatedAndHourlyVariable]=Q&hydro_series[statusData]=most_valid" );
}

QString ReosVortexIoProvider::key() const {return staticKey();}

QStringList ReosVortexIoProvider::fileSuffixes() const
{
  return QStringList();
}

void ReosVortexIoProvider::load()
{
  const QString uri = dataSource();

  bool ok = false;
  QVariantMap params = decodeUri( uri, ok );

  if ( !ok )
  {
    mIsValid = false;
    return;
  }

  QString stationId = params.value( QStringLiteral( "station-id" ) ).toString();
  QStringList parts = stationId.split( '-' );
  if ( parts.count() > 1 )
    stationId = parts.at( 1 );
  QString startStr = params.value( QStringLiteral( "start" ) ).toString();
  QString endStr = params.value( QStringLiteral( "end" ) ).toString();

  const QDate start = QDate::fromString( startStr, Qt::ISODate );
  const QDate end = QDate::fromString( endStr, Qt::ISODate );

  QDate pageStart = start;
  QDate pageEnd;

  mValues.clear();
  mTimes.clear();
  mReferenceTime = QDateTime();

  int valIndex = 0;

  do
  {
    pageEnd = pageStart.addYears( 2 ).addDays( -1 );
    if ( pageEnd > end )
      pageEnd = end;

    const QString url = mBaseUri.arg( stationId, pageStart.toString( "dd/MM/yyyy" ), pageEnd.toString( "dd/MM/yyyy" ) );

    QEventLoop *loop = new QEventLoop( this );

    QNetworkReply *waitedReply  = mNetworkManager->get( QNetworkRequest( url ) );
    connect( waitedReply, &QNetworkReply::finished, this, [ = ]()
    {
      loop->quit();
    } );

    loop->exec();
    loop->deleteLater();

    QNetworkReply::NetworkError er = waitedReply->error();
    int statusCode = waitedReply->attribute( QNetworkRequest::HttpStatusCodeAttribute ).toInt();
    mMeta.insert( QStringLiteral( "request_status" ), statusCode );

    switch ( er )
    {
      case QNetworkReply::NoError:
        break;
      default:
        mIsValid = false;
        return;
        break;
    }

    const QJsonDocument mJsonResult = QJsonDocument::fromJson( waitedReply->readAll() );
    waitedReply->deleteLater();
    const QVariant var = mJsonResult.toVariant();

    QMap map = var.toMap();

    if ( map.size() < 2 )
    {
      mIsValid = false;
      if ( map.size() == 1 )
      {
        qDebug() << QString( "Invalid response: %1" ).arg( map[0].toString() );
      }
      else
      {
        qDebug() << QString( "Void response." );
      }
      mMeta.insert( QStringLiteral( "request_status" ) )
      return;
    }

    double unitFactor = 1;
    if ( map.contains( QStringLiteral( "unitQ" ) ) )
      if ( map.value( QStringLiteral( "unitQ" ) ).toString() == QStringLiteral( "l" ) )
        unitFactor = 0.001;

    if ( map.contains( QStringLiteral( "series" ) ) )
    {
      const QMap series = map.value( QStringLiteral( "series" ) ).toMap();
      QVariantList data = series.value( QStringLiteral( "data" ) ).toList();
      if ( data.size() != 0 )
      {

        mValues.resize( mValues.size() + data.size() );
        mTimes.resize( mTimes.size() + data.size() );


        if ( !mReferenceTime.isValid() )
        {
          QVariantMap firstTimeVar = data.at( 0 ).toMap();
          QVariant timeVar = firstTimeVar.value( "t" );

          mReferenceTime = QDateTime::fromString( timeVar.toString(), Qt::ISODate );
        }

        for ( int i = 0; i < data.size(); ++i )
        {
          QVariantMap step = data.at( i ).toMap();
          QVariant continuity = step.value( QString( 'c' ) );
          QVariant tv = step.value( QString( 't' ) );
          QDateTime time = QDateTime::fromString( tv.toString(), Qt::ISODate );
          if ( !tv.isValid() )
          {
            mIsValid = false;
            qDebug() << QString( "Bad time format" ).arg( tv.toString() );
            return;
          }

          QVariant v = step.value( QString( 'v' ) );
          ok = false;
          double value = v.toDouble( &ok );
          if ( !ok )
            value = std::numeric_limits<double>::quiet_NaN();

          const ReosDuration relTime = ReosDuration( mReferenceTime.secsTo( time ), ReosDuration::second );

          if ( continuity.toInt( &ok ) == 1 && ok && valIndex > 0 )
          {
            mValues.resize( mValues.size() + 1 );
            mTimes.resize( mTimes.size() + 1 );
            mValues[valIndex] = std::numeric_limits<double>::quiet_NaN();
            ReosDuration( static_cast<double>( mReferenceTime.secsTo( time ) ), ReosDuration::second );
            mTimes[valIndex] = ( relTime + mTimes[valIndex - 1] ) / 2.0;
            valIndex += 1;
          }

          mValues[valIndex] = value * unitFactor ;
          mTimes[valIndex] = relTime;
          valIndex += 1;
        }
      }
    }
    pageStart = pageEnd.addDays( 1 );
  }
  while ( pageStart < end );
}

QDateTime ReosVortexIoProvider::referenceTime() const {return mReferenceTime;}

int ReosVortexIoProvider::valueCount() const
{
  return mValues.count();
}

double ReosVortexIoProvider::value( int i ) const
{
  return mValues.at( i );
}

double ReosVortexIoProvider::firstValue() const
{
  return mValues.first();
}

double ReosVortexIoProvider::lastValue() const
{
  return mValues.last();
}

double *ReosVortexIoProvider::data()
{
  return mValues.data();
}

const QVector<double> &ReosVortexIoProvider::constData() const
{
  return mValues;
}

ReosEncodedElement ReosVortexIoProvider::encode( const ReosEncodeContext & ) const
{
  ReosEncodedElement element( ReosVortexIoProvider::staticKey() );
  const QString uri = dataSource();
  element.addData( QStringLiteral( "source" ), uri );
  return element;
}

void ReosVortexIoProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != ReosVortexIoProvider::staticKey() )
    return;

  QString uri;
  element.getData( QStringLiteral( "source" ), uri );
  setDataSource( uri );
}

ReosDuration ReosVortexIoProvider::relativeTimeAt( int i ) const
{
  return mTimes.at( i );
}

ReosDuration ReosVortexIoProvider::lastRelativeTime() const
{
  return mTimes.last();
}

const QVector<ReosDuration> &ReosVortexIoProvider::constTimeData() const
{
  return mTimes;
}

bool ReosVortexIoProvider::canReadUri( const QString &uri ) const
{
  bool ret = false;
  decodeUri( uri, ret );
  return ret;
}

QVariantMap ReosVortexIoProvider::decodeUri( const QString &uri, bool &ok )
{
  const QStringList params = uri.split( QStringLiteral( "::" ) );
  if ( params.size() != 4 )
  {
    ok = false;
    return QVariantMap();
  }

  QVariantMap ret;
  ret[QStringLiteral( "maelstrom-key" )] = params.at( 0 );
  ret[QStringLiteral( "station-id" )] = params.at( 1 );
  ret[QStringLiteral( "start" )] = params.at( 2 );
  ret[QStringLiteral( "end" )] = params.at( 3 );

  ok = true;

  return ret;

}


QString ReosVortexIoProvider::staticKey()
{
  return VORTEXIO_KEY + QString( "::" ) + dataType();
}

QString ReosVortexIoProvider::dataType()
{
  return ReosHydrograph::staticType();
}

ReosDataProvider *ReosVortexIoProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosHydrograph::staticType() )
    return new ReosVortexIoProvider();

  return nullptr;
}

QString ReosVortexIoProviderFactory::key() const
{
  return VORTEXIO_KEY;
}

bool ReosVortexIoProviderFactory::supportType( const QString &dataType ) const
{
  return dataType == ReosHydrograph::staticType();
}

QVariantMap ReosVortexIoProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
  {
    ret.insert( QStringLiteral( "maelstrom-key" ), QObject::tr( "Maelstrom API key" ) );
    ret.insert( QStringLiteral( "station-id" ), QObject::tr( "Station id" ) );
    ret.insert( QStringLiteral( "start" ), QObject::tr( "Start date/time of the requested data (ISO format)" ) );
    ret.insert( QStringLiteral( "end" ), QObject::tr( "End date/time of the requested data (ISO format)" ) );
  }

  return ret;
}

QString ReosVortexIoProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType )
       && parameters.contains( QStringLiteral( "maelstrom-key" ) )
       && parameters.contains( QStringLiteral( "station-id" ) )
       && parameters.contains( QStringLiteral( "start" ) )
       && parameters.contains( QStringLiteral( "end" ) ) )
  {
    ok = true;
    return QString( "%1::%2::%3::%4" ).
           arg( parameters.value( QStringLiteral( "maelstrom-key" ) ).toString(),
                parameters.value( QStringLiteral( "station-id" ) ).toString(),
                parameters.value( QStringLiteral( "start" ) ).toString(),
                parameters.value( QStringLiteral( "end" ) ).toString() ) ;
  }
  ok = false;
  return QString();
}
