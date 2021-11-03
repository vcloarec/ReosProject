/***************************************************************************
  reostimeserieprovider.cpp - ReosTimeSerieProvider

 ---------------------
 begin                : 2.11.2021
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
#include "reostimeserieprovider.h"

ReosTimeSerieProviderRegistery *ReosTimeSerieProviderRegistery::sInstance = nullptr;

ReosTimeSerieProviderRegistery::ReosTimeSerieProviderRegistery()
{
  registerProviderFactory( new ReosTimeSerieConstantTimeStepMemoryProviderFactory );
  registerProviderFactory( new ReosTimeSerieVariableTimeStepMemoryProviderFactory );
}

void ReosTimeSerieProviderRegistery::registerProviderFactory( ReosTimeSerieProviderFactory *factory )
{
  mFactories[factory->key()] = std::unique_ptr<ReosTimeSerieProviderFactory>( factory );
}

ReosTimeSerieProviderRegistery *ReosTimeSerieProviderRegistery::instance()
{
  if ( !sInstance )
    sInstance = new ReosTimeSerieProviderRegistery();

  return sInstance;
}

ReosTimeSerieProvider *ReosTimeSerieProviderRegistery::createProvider( const QString &key )
{
  auto it = mFactories.find( key );
  if ( it != mFactories.end() )
    return it->second->createProvider();
  else
    return nullptr;
}

ReosTimeSerieProvider::~ReosTimeSerieProvider() {}

QString ReosTimeSerieProvider::dataSource() const
{
  return mDataSource;
}

void ReosTimeSerieProvider::setDataSource( const QString &dataSource )
{
  mDataSource = dataSource;
}

ReosTimeSerieConstantTimeStepProvider::~ReosTimeSerieConstantTimeStepProvider() {}

ReosTimeSerieConstantTimeStepMemoryProvider::ReosTimeSerieConstantTimeStepMemoryProvider( const QVector<double> &values )
  : mValues( values )
{}

QString ReosTimeSerieConstantTimeStepMemoryProvider::key() const
{
  return QStringLiteral( "constant-time-step-memory" );
}

QDateTime ReosTimeSerieConstantTimeStepMemoryProvider::referenceTime() const
{
  return QDateTime();
}

QString ReosTimeSerieConstantTimeStepMemoryProvider::valueUnit() const
{
  return QString();
}

int ReosTimeSerieConstantTimeStepMemoryProvider::valueCount() const
{
  return mValues.count();
}

void ReosTimeSerieConstantTimeStepMemoryProvider::resize( int size )
{
  mValues.resize( size );
}

double ReosTimeSerieConstantTimeStepMemoryProvider::value( int i ) const
{
  return mValues.at( i );
}

double ReosTimeSerieConstantTimeStepMemoryProvider::firstValue() const
{
  return mValues.first();
}

double ReosTimeSerieConstantTimeStepMemoryProvider::lastValue() const
{
  return  mValues.last();
}

void ReosTimeSerieConstantTimeStepMemoryProvider::setValue( int i, double v )
{
  mValues[i] = v;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::appendValue( double v )
{
  mValues.append( v );
}

void ReosTimeSerieConstantTimeStepMemoryProvider::prependValue( double v )
{
  mValues.prepend( v );
}

void ReosTimeSerieConstantTimeStepMemoryProvider::insertValue( int fromPos, double v )
{
  mValues.insert( fromPos, v );
}

bool ReosTimeSerieConstantTimeStepMemoryProvider::isEditable() const {return true;}

ReosDuration ReosTimeSerieConstantTimeStepMemoryProvider::timeStep() const
{
  return ReosDuration();
}

double *ReosTimeSerieConstantTimeStepMemoryProvider::data()
{
  return mValues.data();
}

const QVector<double> &ReosTimeSerieConstantTimeStepMemoryProvider::constData() const
{
  return mValues;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::removeValues( int fromPos, int count )
{
  int maxCount = std::min( count, mValues.count() - fromPos );
  QVector<double>::iterator itStart = mValues.begin() + fromPos;
  QVector<double>::iterator itEnd = itStart + maxCount;
  mValues.erase( itStart, itEnd );
}

void ReosTimeSerieConstantTimeStepMemoryProvider::clear()
{
  mValues.clear();
}

ReosEncodedElement ReosTimeSerieConstantTimeStepMemoryProvider::encode() const
{
  ReosEncodedElement element( QStringLiteral( "constant-time-step-serie-memory-provider" ) );
  element.addData( QStringLiteral( "values" ), mValues );

  return element;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "constant-time-step-serie-memory-provider" ) )
    return;

  mValues.clear();
  element.getData( QStringLiteral( "values" ), mValues );
}

ReosTimeSerieVariableTimeStepProvider::~ReosTimeSerieVariableTimeStepProvider()
{

}

ReosTimeSerieVariableTimeStepMemoryProvider::ReosTimeSerieVariableTimeStepMemoryProvider( const QVector<double> &values, const QVector<ReosDuration> &timeValues )
  : mValues( values )
  , mTimeValues( timeValues )
{}

QString ReosTimeSerieVariableTimeStepMemoryProvider::key() const
{
  return QStringLiteral( "variable-time-step-memory" );
}

QDateTime ReosTimeSerieVariableTimeStepMemoryProvider::referenceTime() const {return QDateTime();}

QString ReosTimeSerieVariableTimeStepMemoryProvider::valueUnit() const {return QString();}

int ReosTimeSerieVariableTimeStepMemoryProvider::valueCount() const {return mValues.count();}

double ReosTimeSerieVariableTimeStepMemoryProvider::value( int i ) const {return mValues.at( i );}

double ReosTimeSerieVariableTimeStepMemoryProvider::firstValue() const {return mValues.first();}

double ReosTimeSerieVariableTimeStepMemoryProvider::lastValue() const {return mValues.last();}

void ReosTimeSerieVariableTimeStepMemoryProvider::setValue( int i, double v )
{
  mValues[i] = v;
}

ReosDuration ReosTimeSerieVariableTimeStepMemoryProvider::relativeTimeAt( int i ) const {return mTimeValues.at( i );}

ReosDuration ReosTimeSerieVariableTimeStepMemoryProvider::lastRelativeTime() const {return mTimeValues.last();}

void ReosTimeSerieVariableTimeStepMemoryProvider::setRelativeTimeAt( int i, const ReosDuration &relativeTime )
{
  mTimeValues[i] = relativeTime;
}

void ReosTimeSerieVariableTimeStepMemoryProvider::appendValue( const ReosDuration &relativeTime, double v )
{
  mValues.append( v );
  mTimeValues.append( relativeTime );
}

void ReosTimeSerieVariableTimeStepMemoryProvider::prependValue( const ReosDuration &relativeTime, double v )
{
  mValues.prepend( v );
  mTimeValues.prepend( relativeTime );
}

void ReosTimeSerieVariableTimeStepMemoryProvider::insertValue( int fromPos, const ReosDuration &relativeTime, double v )
{
  mValues.insert( fromPos, v );
  mTimeValues.insert( fromPos, relativeTime );
}

void ReosTimeSerieVariableTimeStepMemoryProvider::removeValues( int fromPos, int count )
{
  mValues.remove( fromPos, count );
  mTimeValues.remove( fromPos, count );
}

void ReosTimeSerieVariableTimeStepMemoryProvider::clear()
{
  mValues.clear();
  mTimeValues.clear();
}

ReosEncodedElement ReosTimeSerieVariableTimeStepMemoryProvider::encode() const
{
  ReosEncodedElement element( QStringLiteral( "variable-time-step-serie-memory-provider" ) );
  element.addData( QStringLiteral( "values" ), mValues );
  QList<ReosEncodedElement> encodedTimeValues;
  encodedTimeValues.reserve( mTimeValues.count() );

  for ( const ReosDuration &time : mTimeValues )
    encodedTimeValues.append( time.encode() );

  element.addListEncodedData( QStringLiteral( "timeValues" ), encodedTimeValues );

  return element;
}

void ReosTimeSerieVariableTimeStepMemoryProvider::decode( const ReosEncodedElement &element )
{
  if ( element.description() != QStringLiteral( "variable-time-step-serie-memory-provider" ) )
    return;

  mValues.clear();
  element.getData( QStringLiteral( "values" ), mValues );

  QList<ReosEncodedElement> encodedTimeValues = element.getListEncodedData( QStringLiteral( "timeValues" ) );
  mTimeValues.clear();
  mTimeValues.reserve( encodedTimeValues.size() );
  for ( const ReosEncodedElement &elem : std::as_const( encodedTimeValues ) )
    mTimeValues.append( ReosDuration::decode( elem ) );
}
