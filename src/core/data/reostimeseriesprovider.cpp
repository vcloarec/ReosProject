/***************************************************************************
  reostimeseriesprovider.cpp - ReosTimeSeriesProvider

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
#include "reostimeseriesprovider.h"
#include "reostimeseries.h"

ReosTimeSerieProvider::~ReosTimeSerieProvider() {}

void ReosTimeSerieProvider::setReferenceTime( const QDateTime & ) {}

void ReosTimeSerieProvider::setValue( int, double ) {}

void ReosTimeSerieProvider::removeValues( int, int ) {}

void ReosTimeSerieProvider::clear() {}

QString ReosTimeSerieProvider::dataSource() const
{
  return mDataSource;
}

void ReosTimeSerieProvider::setDataSource( const QString &dataSource, bool loadAfter )
{
  mDataSource = dataSource;
  if ( loadAfter )
    load();
}

ReosTimeSerieConstantTimeStepProvider::~ReosTimeSerieConstantTimeStepProvider() {}

void ReosTimeSerieConstantTimeStepProvider::resize( int ) {}

void ReosTimeSerieConstantTimeStepProvider::appendValue( double ) {}

void ReosTimeSerieConstantTimeStepProvider::prependValue( double ) {}

void ReosTimeSerieConstantTimeStepProvider::insertValue( int, double ) {}

void ReosTimeSerieConstantTimeStepProvider::setTimeStep( const ReosDuration & ) {}

void ReosTimeSerieConstantTimeStepProvider::copy( ReosTimeSerieConstantTimeStepProvider * ) {}

void ReosTimeSerieConstantTimeStepProvider::setValues( const QVector<double> &vals )
{}

ReosTimeSerieConstantTimeStepMemoryProvider::ReosTimeSerieConstantTimeStepMemoryProvider( const QVector<double> &values )
  : mValues( values )
{}

QString ReosTimeSerieConstantTimeStepMemoryProvider::key() const
{
  return QStringLiteral( "constant-time-step-memory" );
}

QDateTime ReosTimeSerieConstantTimeStepMemoryProvider::referenceTime() const
{
  return mReferenceTime;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::setReferenceTime( const QDateTime &referenceTime )
{
  mReferenceTime = referenceTime;
  emit dataChanged();
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
  return mTimeStep;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::setTimeStep( const ReosDuration &timeStep )
{
  mTimeStep = timeStep;
  emit dataChanged();
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

ReosEncodedElement ReosTimeSerieConstantTimeStepMemoryProvider::encode( const ReosEncodeContext & ) const
{
  ReosEncodedElement element( QStringLiteral( "constant-time-step-serie-memory-provider" ) );
  element.addData( QStringLiteral( "values" ), mValues );

  element.addData( QStringLiteral( "reference-time" ), mReferenceTime );
  element.addEncodedData( QStringLiteral( "time-step" ), mTimeStep.encode() );

  return element;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext & )
{
  if ( element.description() != QStringLiteral( "constant-time-step-serie-memory-provider" ) )
    return;

  mValues.clear();
  element.getData( QStringLiteral( "values" ), mValues );

  element.getData( QStringLiteral( "reference-time" ), mReferenceTime );
  mTimeStep = ReosDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ) );
}

void ReosTimeSerieConstantTimeStepMemoryProvider::copy( ReosTimeSerieConstantTimeStepProvider *other )
{
  mReferenceTime = other->referenceTime();
  mTimeStep = other->timeStep();
  mValues = other->constData();

  emit dataChanged();
}

void ReosTimeSerieConstantTimeStepMemoryProvider::setValues( const QVector<double> &vals )
{
  mValues = vals;
}

QString ReosTimeSerieConstantTimeStepMemoryProvider::staticType()
{
  return ReosTimeSeriesConstantInterval::staticType();
}

ReosTimeSerieVariableTimeStepProvider::~ReosTimeSerieVariableTimeStepProvider()
{

}

void ReosTimeSerieVariableTimeStepProvider::setRelativeTimeAt( int, const ReosDuration & ) {}

void ReosTimeSerieVariableTimeStepProvider::appendValue( const ReosDuration &, double ) {}

void ReosTimeSerieVariableTimeStepProvider::prependValue( const ReosDuration &, double ) {}

void ReosTimeSerieVariableTimeStepProvider::insertValue( int, const ReosDuration &, double ) {}

void ReosTimeSerieVariableTimeStepProvider::copy( ReosTimeSerieVariableTimeStepProvider * ) {}

bool ReosTimeSerieVariableTimeStepProvider::writeSeries( ReosTimeSeriesVariableTimeStep *, const QString & ) {return false;}

int ReosTimeSerieVariableTimeStepProvider::timeValueIndex( const ReosDuration &time, bool &exact ) const
{

  if ( valueCount() == 0 || time < relativeTimeAt( 0 ) )
  {
    exact = false;
    return -1;
  }

  if ( time > lastRelativeTime() )
  {
    exact = false;
    return valueCount() - 1;
  }

  int i1 = 0;
  int i2 = valueCount() - 1;
  while ( true )
  {
    if ( relativeTimeAt( i1 ) == time )
    {
      exact = true;
      return i1;
    }
    if ( relativeTimeAt( i2 ) == time )
    {
      exact = true;
      return i2;
    }

    if ( i1 == i2 || i1 + 1 == i2 )
    {
      exact = false;
      return i1;
    }

    int inter = ( i1 + i2 ) / 2;
    if ( time < relativeTimeAt( inter ) )
      i2 = inter;
    else
      i1 = inter;
  }
}

double ReosTimeSerieVariableTimeStepProvider::valueAtTime( const ReosDuration &relativeTime ) const
{
  bool exact = false;
  int index = timeValueIndex( relativeTime, exact );

  if ( exact )
    return value( index );

  if ( index < 0 || index >= valueCount() - 1 )
    return 0;

  const ReosDuration time1 = relativeTimeAt( index );
  const ReosDuration time2 = relativeTimeAt( index + 1 );

  double ratio = ( relativeTime - time1 ) / ( time2 - time1 );

  return ( value( index + 1 ) - value( index ) ) * ratio + value( index );
}


ReosTimeSerieVariableTimeStepMemoryProvider::ReosTimeSerieVariableTimeStepMemoryProvider( const QVector<double> &values, const QVector<ReosDuration> &timeValues )
  : mValues( values )
  , mTimeValues( timeValues )
{}

QString ReosTimeSerieVariableTimeStepMemoryProvider::key() const
{
  return QStringLiteral( "variable-time-step-memory" );
}

QDateTime ReosTimeSerieVariableTimeStepMemoryProvider::referenceTime() const
{
  return mReferenceTime;
}

void ReosTimeSerieVariableTimeStepMemoryProvider::setReferenceTime( const QDateTime &referenceTime )
{
  mReferenceTime = referenceTime;
  emit dataChanged();
}

QString ReosTimeSerieVariableTimeStepMemoryProvider::valueUnit() const {return QString();}

int ReosTimeSerieVariableTimeStepMemoryProvider::valueCount() const {return mValues.count();}

double ReosTimeSerieVariableTimeStepMemoryProvider::value( int i ) const
{
  return mValues.at( i );
}

double ReosTimeSerieVariableTimeStepMemoryProvider::firstValue() const {return mValues.first();}

double ReosTimeSerieVariableTimeStepMemoryProvider::lastValue() const {return mValues.last();}

void ReosTimeSerieVariableTimeStepMemoryProvider::setValue( int i, double v )
{
  mValues[i] = v;
}

ReosDuration ReosTimeSerieVariableTimeStepMemoryProvider::relativeTimeAt( int i ) const
{
  return mTimeValues.at( i );
}

ReosDuration ReosTimeSerieVariableTimeStepMemoryProvider::lastRelativeTime() const {return mTimeValues.last();}

void ReosTimeSerieVariableTimeStepMemoryProvider::setRelativeTimeAt( int i, const ReosDuration &relativeTime )
{
  mTimeValues[i] = relativeTime;
}

const QVector<ReosDuration> &ReosTimeSerieVariableTimeStepMemoryProvider::constTimeData() const
{
  return mTimeValues;
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
  int effCount = std::min( mValues.count() - fromPos, count );
  mValues.remove( fromPos, effCount );
  mTimeValues.remove( fromPos, effCount );
}

void ReosTimeSerieVariableTimeStepMemoryProvider::clear()
{
  mValues.clear();
  mTimeValues.clear();
  mReferenceTime = QDateTime();
}

void ReosTimeSerieVariableTimeStepMemoryProvider::copy( ReosTimeSerieVariableTimeStepProvider *other )
{
  mReferenceTime = other->referenceTime();
  mValues = other->constData();
  mTimeValues = other->constTimeData();

  emit dataChanged();
}

ReosEncodedElement ReosTimeSerieVariableTimeStepMemoryProvider::encode( const ReosEncodeContext & ) const
{
  ReosEncodedElement element( QStringLiteral( "variable-time-step-serie-memory-provider" ) );

  element.addData( QStringLiteral( "reference-time" ), mReferenceTime );
  element.addData( QStringLiteral( "store-duration-millisecond" ), true );

  element.addData( QStringLiteral( "values" ), mValues );
  QList<qint64> timeValuesMs;
  timeValuesMs.reserve( mTimeValues.count() );

  for ( const ReosDuration &time : mTimeValues )
  {
    time.valueMilliSecond();
    timeValuesMs.append( time.valueMilliSecond() );
  }

  element.addData( QStringLiteral( "timeValuesMs" ), timeValuesMs );

  return element;
}

void ReosTimeSerieVariableTimeStepMemoryProvider::decode( const ReosEncodedElement &element, const ReosEncodeContext & )
{
  if ( element.description() != QStringLiteral( "variable-time-step-serie-memory-provider" ) )
    return;

  element.getData( QStringLiteral( "reference-time" ), mReferenceTime );

  mValues.clear();
  element.getData( QStringLiteral( "values" ), mValues );

  if ( element.hasEncodedData( QStringLiteral( "store-duration-millisecond" ) ) )
  {
    bool storedMs = false;
    element.getData( QStringLiteral( "store-duration-millisecond" ), storedMs );
    if ( storedMs )
    {
      QList<qint64> timeValuesMs;
      if ( element.getData( QStringLiteral( "timeValuesMs" ), timeValuesMs ) )
      {
        mTimeValues.clear();
        mTimeValues.reserve( timeValuesMs.size() );
        for ( const qint64 tv : std::as_const( timeValuesMs ) )
          mTimeValues.append( ReosDuration( qint64( tv ) ) );

        return;
      }
    }
  }

  // if we are here, this is surely because that was encoded with version <= 2.2.95
  // we keep it for backward compatibility

  QList<ReosEncodedElement> encodedTimeValues = element.getListEncodedData( QStringLiteral( "timeValues" ) );
  mTimeValues.clear();
  mTimeValues.reserve( encodedTimeValues.size() );
  for ( const ReosEncodedElement &elem : std::as_const( encodedTimeValues ) )
    mTimeValues.append( ReosDuration::decode( elem ) );
}

QString ReosTimeSerieVariableTimeStepMemoryProvider::staticType()
{
  return ReosTimeSeriesVariableTimeStep::staticType();
}

ReosDataProvider *ReosTimeSerieConstantTimeStepMemoryProviderFactory::createProvider( const QString &dataType ) const
{
  if ( dataType.contains( ReosTimeSerieConstantTimeStepMemoryProvider::staticType() ) )
    return new ReosTimeSerieConstantTimeStepMemoryProvider;

  return nullptr;
}

bool ReosTimeSerieConstantTimeStepMemoryProviderFactory::hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const
{
  if ( dataType.contains( ReosTimeSeriesConstantInterval::staticType() ) )
    return ( capabilities & mCapabilities ) == capabilities;

  return false;
}

bool ReosTimeSerieConstantTimeStepMemoryProviderFactory::supportType( const QString &dataType ) const
{
  return dataType.contains( ReosTimeSerieConstantTimeStepMemoryProvider::staticType() );
}

bool ReosTimeSerieVariableTimeStepMemoryProviderFactory::hasCapabilities( const QString &dataType, ReosDataProvider::Capabilities capabilities ) const
{
  if ( dataType.contains( ReosTimeSeriesVariableTimeStep::staticType() ) )
    return ( capabilities & mCapabilities ) == capabilities;

  return false;
}

bool ReosTimeSerieVariableTimeStepMemoryProviderFactory::supportType( const QString &dataType ) const
{
  return dataType.contains( ReosTimeSerieVariableTimeStepMemoryProvider::staticType() );
}
