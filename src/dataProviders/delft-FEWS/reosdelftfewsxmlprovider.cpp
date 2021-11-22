/***************************************************************************
  reosdelftfewsxmlprovider.cpp - ReosDelftFewsXMLProvider

 ---------------------
 begin                : 10.11.2021
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
#include "reosdelftfewsxmlprovider.h"
#include "reosencodedelement.h"

#include <QDomElement>
#include <QFile>
#include <QVariantMap>
#include <QLocale>

#include "reoshydrograph.h"

ReosDelftFewsXMLProvider::ReosDelftFewsXMLProvider()
{}

QDateTime ReosDelftFewsXMLProvider::timefromElement( const QDomElement &element )
{
  if ( element.hasAttribute( QStringLiteral( "date" ) ) && element.hasAttribute( QStringLiteral( "time" ) ) )
  {
    const QDate date = QDate::fromString( element.attribute( QStringLiteral( "date" ) ), Qt::ISODate );
    const QTime time = QTime::fromString( element.attribute( QStringLiteral( "time" ) ), QStringLiteral( "hh:mm:ss" ) );

    return QDateTime( date, time, Qt::UTC );
  }
  else
    return QDateTime();
}

double ReosDelftFewsXMLProvider::doubleValueFromElement( const QDomElement &element, const QString &noData )
{
  if ( element.hasAttribute( QStringLiteral( "value" ) ) )
  {
    QString doubleString = element.attribute( "value" );
    if ( doubleString != noData )
    {
      bool ok = false;
      double ret = doubleString.toDouble( &ok );
      if ( ok )
        return ret;
    }
  }
  return std::numeric_limits<double>::quiet_NaN();
}

QString ReosDelftFewsXMLProvider::valueStringFromElement( const QDomElement &element )
{
  return element.firstChild().nodeValue();
}

QString ReosDelftFewsXMLProvider::htmlDescriptionFromMetada( const QVariantMap &metadata )
{
  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );

  if ( metadata.isEmpty() )
  {
    htmlText += QStringLiteral( "<h2>" ) + QObject::tr( "No station selected" ) + QStringLiteral( "</h2>\n<hr>\n" );
  }
  else
  {

    htmlText += QStringLiteral( "<h2>" ) + metadata.value( QStringLiteral( "name" ) ).toString() + QStringLiteral( "</h2>\n<hr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + QObject::tr( "<b>Location Id</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "location-id" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + QObject::tr( "<b>Start date</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "start-time" ) ).toDateTime().toString( QLocale().dateTimeFormat() )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + QObject::tr( "<b>End date</b>" ) + QStringLiteral( "</td><td>" )
                + metadata.value( QStringLiteral( "end-time" ) ).toDateTime().toString( QLocale().dateTimeFormat() )
                + QStringLiteral( "</td></tr>\n" );
  }

  return htmlText;
}

QDomElement ReosDelftFewsXMLProvider::seriesElement( const QString &uri, QDomDocument &xmlDoc, QString &noData ) const
{
  const QString filePath = fileNameFromUri( uri );
  const QString stationId = stationIdFromUri( uri );
  const QDateTime startTime = startTimeFromUri( uri );
  const QDateTime endTime = endTimeFromUri( uri );

  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return QDomElement();
  if ( !xmlDoc.setContent( &file ) )
  {
    file.close();
    return QDomElement();
  }
  file.close();

  const QDomElement rootNode = xmlDoc.firstChildElement( QStringLiteral( "TimeSeries" ) );
  if ( rootNode.isNull() )
    return QDomElement();

  QString noDataValue;

  QDomElement serieElement = rootNode.firstChildElement( QStringLiteral( "series" ) );
  bool found = false;
  while ( !serieElement.isNull() )
  {
    const QDomElement header = serieElement.firstChildElement( QStringLiteral( "header" ) );
    if ( !header.isNull() )
    {
      const QDomElement locationIdElement = header.firstChildElement( QStringLiteral( "locationId" ) );
      const QDomElement startTimeElement = header.firstChildElement( QStringLiteral( "startDate" ) );
      const QDomElement endTimeElement = header.firstChildElement( QStringLiteral( "endDate" ) );

      if ( !locationIdElement.isNull() && !startTimeElement.isNull() && !endTimeElement.isNull() )
      {

        const QString locationId = valueStringFromElement( locationIdElement );
        const QDateTime canditateStartTime = timefromElement( startTimeElement );
        const QDateTime candidateEndTime = timefromElement( endTimeElement );

        if ( stationId == locationId && canditateStartTime == startTime && candidateEndTime == endTime )
        {
          found = true;
          noDataValue = valueStringFromElement( header.firstChildElement( QStringLiteral( "missVal" ) ) );
          break;
        }
      }
    }
    serieElement = serieElement.nextSiblingElement( QStringLiteral( "series" ) );
  }

  if ( !found )
    return QDomElement();
  else
    return serieElement;
}

QVariantMap ReosDelftFewsXMLProvider::metadata() const
{
  return mMeta;
}

void ReosDelftFewsXMLProvider::setMetadata( const QVariantMap &meta )
{
  mMeta = meta;
}

QString ReosDelftFewsXMLHydrographProvider::key() const {return ReosDelftFewsXMLProvider::staticKey() + ':' + dataType();}

QString ReosDelftFewsXMLHydrographProvider::htmlDescription() const
{
  return htmlDescriptionFromMetada( metadata() );
}

void ReosDelftFewsXMLHydrographProvider::load()
{
  const QString uri = dataSource();

  QDomDocument xmlDoc( "delft-document" );
  QString noDataValue;

  QDomElement serieElement = seriesElement( uri, xmlDoc, noDataValue );

  QDomElement valueElement = serieElement.firstChildElement( QStringLiteral( "event" ) );
  while ( !valueElement.isNull() )
  {
    double value = doubleValueFromElement( valueElement, noDataValue );
    if ( !std::isnan( value ) )
    {
      const QDateTime dateTime = timefromElement( valueElement );
      if ( !dateTime.isValid() )
        return;
      ReosDuration relativetime;
      if ( mReferenceTime.isValid() )
      {
        relativetime = ReosDuration( mReferenceTime.msecsTo( dateTime ), ReosDuration::millisecond );
      }
      else
      {
        mReferenceTime = dateTime;
      }

      mCacheTimeValues.append( relativetime );
      mCacheValues.append( value );
    }
    valueElement = valueElement.nextSiblingElement( QStringLiteral( "event" ) );
  }

  emit dataChanged();
}

QDateTime ReosDelftFewsXMLHydrographProvider::referenceTime() const {return mReferenceTime;}

QString ReosDelftFewsXMLHydrographProvider::valueUnit() const {return QString();}

int ReosDelftFewsXMLHydrographProvider::valueCount() const {return mCacheValues.count();}

double ReosDelftFewsXMLHydrographProvider::value( int i ) const {return mCacheValues.at( i );}

double ReosDelftFewsXMLHydrographProvider::firstValue() const {return mCacheValues.first();}

double ReosDelftFewsXMLHydrographProvider::lastValue() const {return mCacheValues.last();}

double *ReosDelftFewsXMLHydrographProvider::data() {return mCacheValues.data();}

const QVector<double> &ReosDelftFewsXMLHydrographProvider::constData() const {return mCacheValues;}

QString ReosDelftFewsXMLRainfallProvider::key() const {return ReosDelftFewsXMLProvider::staticKey() + ':' + dataType();}

QString ReosDelftFewsXMLRainfallProvider::htmlDescription() const
{
  return htmlDescriptionFromMetada( metadata() );
}

void ReosDelftFewsXMLRainfallProvider::load()
{
  const QString uri = dataSource();

  QDomDocument xmlDoc( "delft-document" );
  QString noDataValue;

  QDomElement serieElement = seriesElement( uri, xmlDoc, noDataValue );

  bool isIntensity = false;

  const QDomElement headerElement = serieElement.firstChildElement( QStringLiteral( "header" ) );
  const QDomElement typeElement = headerElement.firstChildElement( QStringLiteral( "type" ) );
  QString type = ReosDelftFewsXMLProvider::valueStringFromElement( typeElement );
  isIntensity = type == QStringLiteral( "instantaneous" );

  QDomElement timeStepElement = headerElement.firstChildElement( QStringLiteral( "timeStep" ) );
  if ( timeStepElement.isNull() )
    return;

  QString timeStepUnitString = timeStepElement.attribute( QStringLiteral( "unit" ) );
  QString timeStepStringValue = timeStepElement.attribute( QStringLiteral( "multiplier" ) );
  bool ok = false;
  double timeStepValue = timeStepStringValue.toDouble( &ok );
  if ( !ok )
    return;

  ReosDuration::Unit timeStepUnit;
  if ( timeStepUnitString == QStringLiteral( "second" ) )
    timeStepUnit = ReosDuration::second;
  else if ( timeStepUnitString == QStringLiteral( "minute" ) )
    timeStepUnit = ReosDuration::minute;
  else if ( timeStepUnitString == QStringLiteral( "hour" ) )
    timeStepUnit = ReosDuration::hour;
  else if ( timeStepUnitString == QStringLiteral( "day" ) )
    timeStepUnit = ReosDuration::day;
  else
    return;

  mTimeStep = ReosDuration( timeStepValue, timeStepUnit );

  QDomElement valueElement = serieElement.firstChildElement( QStringLiteral( "event" ) );

  QDateTime prevTime;

  while ( !valueElement.isNull() )
  {
    const QDateTime dateTime = timefromElement( valueElement );
    if ( !dateTime.isValid() )
      return;

    ReosDuration step;
    if ( !mReferenceTime.isValid() )
      mReferenceTime = dateTime;
    else
      step = ReosDuration( prevTime.msecsTo( dateTime ), ReosDuration::millisecond );
    prevTime = dateTime;

    if ( step > mTimeStep )
    {
      int count = step.numberOfFullyContainedIntervals( mTimeStep ) - 1;
      for ( int i = 0; i < count ; ++i )
        mCacheValues.append( 0 );
    }

    double value = doubleValueFromElement( valueElement, noDataValue );
    if ( !std::isnan( value ) )
    {
      if ( isIntensity )
        mCacheValues.append( value * mTimeStep.valueHour() );
      else
        mCacheValues.append( value );
    }
    else
      mCacheValues.append( 0 );

    valueElement = valueElement.nextSiblingElement( QStringLiteral( "event" ) );
  }

  emit dataChanged();
}

QDateTime ReosDelftFewsXMLRainfallProvider::referenceTime() const {return mReferenceTime;}

QString ReosDelftFewsXMLRainfallProvider::valueUnit() const {return QString();}

int ReosDelftFewsXMLRainfallProvider::valueCount() const {return mCacheValues.count();}

double ReosDelftFewsXMLRainfallProvider::value( int i ) const {return mCacheValues.at( i );}

double ReosDelftFewsXMLRainfallProvider::firstValue() const {return mCacheValues.first();}

double ReosDelftFewsXMLRainfallProvider::lastValue() const {return mCacheValues.last();}

double *ReosDelftFewsXMLRainfallProvider::data() {return mCacheValues.data();}

const QVector<double> &ReosDelftFewsXMLRainfallProvider::constData() const {return mCacheValues;}

ReosEncodedElement ReosDelftFewsXMLRainfallProvider::encode() const
{
  ReosEncodedElement element( ReosDelftFewsXMLRainfallProvider::staticKey() );

  element.addData( QStringLiteral( "source" ), dataSource() );
  element.addData( QStringLiteral( "metadata" ), metadata() );

  return element;
}

void ReosDelftFewsXMLRainfallProvider::decode( const ReosEncodedElement &element )
{
  if ( element.description() != ReosDelftFewsXMLRainfallProvider::staticKey() )
    return;

  QString source;
  element.getData( QStringLiteral( "source" ), source );
  setDataSource( source );

  QVariantMap meta;
  element.getData( QStringLiteral( "metadata" ), meta );
  setMetadata( meta );
}

ReosDuration ReosDelftFewsXMLRainfallProvider::timeStep() const {return mTimeStep;}

QString ReosDelftFewsXMLRainfallProvider::dataType() {return QStringLiteral( "rainfall" );}

ReosEncodedElement ReosDelftFewsXMLHydrographProvider::encode() const
{
  ReosEncodedElement element( ReosDelftFewsXMLRainfallProvider::staticKey() );

  element.addData( QStringLiteral( "source" ), dataSource() );
  element.addData( QStringLiteral( "metadata" ), metadata() );

  return element;
}

void ReosDelftFewsXMLHydrographProvider::decode( const ReosEncodedElement &element )
{
  if ( element.description() != ReosDelftFewsXMLRainfallProvider::staticKey() )
    return;

  QString source;
  element.getData( QStringLiteral( "source" ), source );
  setDataSource( source );

  QVariantMap meta;
  element.getData( QStringLiteral( "metadata" ), meta );
  setMetadata( meta );
}

ReosDuration ReosDelftFewsXMLHydrographProvider::relativeTimeAt( int i ) const {return mCacheTimeValues.at( i );}

ReosDuration ReosDelftFewsXMLHydrographProvider::lastRelativeTime() const {return mCacheTimeValues.last();}

QString ReosDelftFewsXMLHydrographProvider::dataType() {return ReosHydrograph::staticType();}

#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
#define skipEmptyPart QString::SkipEmptyParts
#else
#define skipEmptyPart Qt::SplitBehaviorFlags::SkipEmptyParts
#endif

QString ReosDelftFewsXMLProvider::fileNameFromUri( const QString &uri )
{
  if ( !uri.contains( QStringLiteral( "\"" ) ) )
    return QString();

  QStringList split = uri.split( QStringLiteral( "\"" ), skipEmptyPart );
  return split.at( 0 );
}

QString ReosDelftFewsXMLProvider::stationIdFromUri( const QString &uri )
{
  QStringList splitUri = uri.split( QStringLiteral( "\"" ), skipEmptyPart );
  if ( splitUri.count() < 2 )
    return QString();

  QStringList splitSecondPart = splitUri.at( 1 ).split( QStringLiteral( "::" ), skipEmptyPart );
  return splitSecondPart.at( 0 );

}

QDateTime ReosDelftFewsXMLProvider::startTimeFromUri( const QString &uri )
{
  QStringList splitUri = uri.split( QStringLiteral( "\"" ), skipEmptyPart );
  if ( splitUri.count() < 2 )
    return QDateTime();

  QStringList splitSecondPart = splitUri.at( 1 ).split( QStringLiteral( "::" ), skipEmptyPart );
  if ( splitSecondPart.count() < 3 )
    return QDateTime();

  return QDateTime::fromString( splitSecondPart.at( 1 ), Qt::ISODate );
}

QDateTime ReosDelftFewsXMLProvider::endTimeFromUri( const QString &uri )
{
  QStringList splitUri = uri.split( QStringLiteral( "\"" ), skipEmptyPart );
  if ( splitUri.count() < 2 )
    return QDateTime();

  QStringList splitSecondPart = splitUri.at( 1 ).split( QStringLiteral( "::" ), skipEmptyPart );
  if ( splitSecondPart.count() < 3 )
    return QDateTime();

  return QDateTime::fromString( splitSecondPart.at( 2 ), Qt::ISODate );
}

ReosTimeSerieProvider *ReosDelftFewsXMLProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosDelftFewsXMLRainfallProvider::dataType() )
    return new ReosDelftFewsXMLRainfallProvider;

  if ( dataType == ReosDelftFewsXMLHydrographProvider::dataType() )
    return new ReosDelftFewsXMLHydrographProvider;

  return nullptr;
}

QString ReosDelftFewsXMLProviderFactory::key() const
{
  return ReosDelftFewsXMLProvider::staticKey();
}

REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosDelftFewsXMLProviderFactory();
}
