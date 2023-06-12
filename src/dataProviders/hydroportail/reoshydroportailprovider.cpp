/***************************************************************************
  reoshydroportailprovider.cpp - ReosHydroportailProvider

 ---------------------
 begin                : 10.6.2023
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
#include "reoshydroportailprovider.h"
#include <QTextStream>

#include "reoshydrograph.h"

#define HYDROPORTAIL_KEY QStringLiteral("hydroportail")


REOSEXTERN ReosDataProviderFactory *providerFactory()
{
  return new ReosHydroportailProviderFactory;
}

ReosHydroportailProvider::ReosHydroportailProvider()
{

}

QString ReosHydroportailProvider::key() const {return staticKey();}

QStringList ReosHydroportailProvider::fileSuffixes() const
{
  return QStringList() << QStringLiteral( "csv" );
}

void ReosHydroportailProvider::load()
{
  QFile file( dataSource() );
  if ( !file.open( QIODevice::ReadOnly ) )
    return;

  QTextStream stream( &file );
  QString headerLine = stream.readLine();
  double unitFactor = 1;

  if ( headerLine.contains( QString::fromUtf8( "l/s" ) ) )
    unitFactor = 0.001;
  else if ( headerLine.contains( QString::fromUtf8( "mm³/s" ) ) )
    unitFactor = 1;

  const QStringList headers = headerLine.split( "," );

  int timeColumn = 0;
  int valueColumn = 0;

  for ( int i = 0; i < headers.count(); ++i )
  {
    if ( headers.at( i ).contains( "Date" ) )
      timeColumn = i;
    if ( headers.at( i ).contains( "Valeur" ) )
      valueColumn = i;
  }

  while ( !stream.atEnd() )
  {
    QString line = stream.readLine();
    QStringList splittedLine = line.split( ',' );
    if ( splittedLine.count() - 1 < timeColumn || splittedLine.count() - 1 < valueColumn )
      continue;

    const QDateTime time = QDateTime::fromString( splittedLine.at( timeColumn ), Qt::ISODate );
    bool ok = false;
    QString strValue = splittedLine.at( valueColumn );
    strValue.remove( QStringLiteral( "\"" ) );
    double value = strValue.toDouble( &ok );
    if ( !ok )
      continue;

    if ( mReferenceTime.isValid() )
      mTimes.append( ReosDuration( mReferenceTime.msecsTo( time ) ) );
    else
    {
      mReferenceTime = time;
      mTimes.append( ReosDuration( qint64( 0 ) ) );
    }

    mValues.append( value * unitFactor );
  }
}

QDateTime ReosHydroportailProvider::referenceTime() const {return mReferenceTime;}

int ReosHydroportailProvider::valueCount() const
{
  return mValues.count();
}

double ReosHydroportailProvider::value( int i ) const
{
  return mValues.at( i );
}

double ReosHydroportailProvider::firstValue() const
{
  return mValues.first();
}

double ReosHydroportailProvider::lastValue() const
{
  return mValues.last();
}

double *ReosHydroportailProvider::data()
{
  return mValues.data();
}

const QVector<double> &ReosHydroportailProvider::constData() const
{
  return mValues;
}

ReosEncodedElement ReosHydroportailProvider::encode( const ReosEncodeContext &context ) const
{
  ReosEncodedElement element( ReosHydroportailProvider::staticKey() );
  const QString fileName = context.pathToEncode( dataSource() );
  element.addData( QStringLiteral( "source" ), fileName );
  return element;
}

void ReosHydroportailProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext &context )
{
  if ( element.description() != ReosHydroportailProvider::staticKey() )
    return;

  QString fileName;
  element.getData( QStringLiteral( "source" ), fileName );
  fileName = context.resolvePath( fileName );
  setDataSource( fileName );
}

ReosDuration ReosHydroportailProvider::relativeTimeAt( int i ) const
{
  return mTimes.at( i );
}

ReosDuration ReosHydroportailProvider::lastRelativeTime() const
{
  return mTimes.last();
}

const QVector<ReosDuration> &ReosHydroportailProvider::constTimeData() const
{
  return mTimes;
}

bool ReosHydroportailProvider::canReadUri( const QString &uri ) const
{
  QFile file( uri );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QTextStream stream( &file );
  QString headerLine = stream.readLine();

  return headerLine.contains( QStringLiteral( "Date (TU)" ) ) && headerLine.contains( QStringLiteral( "Valeur (en" ) );

}

QString ReosHydroportailProvider::staticKey()
{
  return HYDROPORTAIL_KEY + QString( "::" ) + dataType();
}

QString ReosHydroportailProvider::dataType()
{
  return ReosHydrograph::staticType();
}

ReosDataProvider *ReosHydroportailProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType == ReosHydrograph::staticType() )
    return new ReosHydroportailProvider();

  return nullptr;
}

QString ReosHydroportailProviderFactory::key() const
{
  return HYDROPORTAIL_KEY;
}

bool ReosHydroportailProviderFactory::supportType( const QString &dataType ) const
{
  return dataType == ReosHydrograph::staticType();
}

QVariantMap ReosHydroportailProviderFactory::uriParameters( const QString &dataType ) const
{
  QVariantMap ret;

  if ( supportType( dataType ) )
  {
    ret.insert( QStringLiteral( "file-path" ), QObject::tr( "File where are stored the data" ) );
  }

  return ret;
}

QString ReosHydroportailProviderFactory::buildUri( const QString &dataType, const QVariantMap &parameters, bool &ok ) const
{
  if ( supportType( dataType ) && parameters.contains( QStringLiteral( "file-path" ) ) )
  {
    ok = true;
    return parameters.value( QStringLiteral( "file-path" ) ).toString();
  }
  ok = false;
  return QString();
}
