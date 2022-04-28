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

ReosTimeSerieProvider::~ReosTimeSerieProvider() {}

void ReosTimeSerieProvider::setReferenceTime( const QDateTime & ) {}

void ReosTimeSerieProvider::setValue( int, double ) {}

void ReosTimeSerieProvider::removeValues( int, int ) {}

void ReosTimeSerieProvider::clear() {}

QString ReosTimeSerieProvider::dataSource() const
{
  return mDataSource;
}

void ReosTimeSerieProvider::setDataSource( const QString &dataSource )
{
  mDataSource = dataSource;
  load();
}

ReosTimeSerieConstantTimeStepProvider::~ReosTimeSerieConstantTimeStepProvider() {}

void ReosTimeSerieConstantTimeStepProvider::resize( int ) {}

void ReosTimeSerieConstantTimeStepProvider::appendValue( double ) {}

void ReosTimeSerieConstantTimeStepProvider::prependValue( double ) {}

void ReosTimeSerieConstantTimeStepProvider::insertValue( int, double ) {}

void ReosTimeSerieConstantTimeStepProvider::setTimeStep( const ReosDuration & ) {}

void ReosTimeSerieConstantTimeStepProvider::copy( ReosTimeSerieConstantTimeStepProvider * ) {}

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

ReosEncodedElement ReosTimeSerieConstantTimeStepMemoryProvider::encode() const
{
  ReosEncodedElement element( QStringLiteral( "constant-time-step-serie-memory-provider" ) );
  element.addData( QStringLiteral( "values" ), mValues );

  element.addData( QStringLiteral( "reference-time" ), mReferenceTime );
  element.addEncodedData( QStringLiteral( "time-step" ), mTimeStep.encode() );

  return element;
}

void ReosTimeSerieConstantTimeStepMemoryProvider::decode( const ReosEncodedElement &element )
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

ReosTimeSerieVariableTimeStepProvider::~ReosTimeSerieVariableTimeStepProvider()
{

}

void ReosTimeSerieVariableTimeStepProvider::setRelativeTimeAt( int, const ReosDuration & ) {}

void ReosTimeSerieVariableTimeStepProvider::appendValue( const ReosDuration &, double ) {}

void ReosTimeSerieVariableTimeStepProvider::prependValue( const ReosDuration &, double ) {}

void ReosTimeSerieVariableTimeStepProvider::insertValue( int, const ReosDuration &, double ) {}

void ReosTimeSerieVariableTimeStepProvider::copy( ReosTimeSerieVariableTimeStepProvider * ) {}


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
}

void ReosTimeSerieVariableTimeStepMemoryProvider::copy( ReosTimeSerieVariableTimeStepProvider *other )
{
  mReferenceTime = other->referenceTime();
  mValues = other->constData();
  mTimeValues = other->constTimeData();

  emit dataChanged();
}

ReosEncodedElement ReosTimeSerieVariableTimeStepMemoryProvider::encode() const
{
  ReosEncodedElement element( QStringLiteral( "variable-time-step-serie-memory-provider" ) );

  element.addData( QStringLiteral( "reference-time" ), mReferenceTime );

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

  element.getData( QStringLiteral( "reference-time" ), mReferenceTime );

  mValues.clear();
  element.getData( QStringLiteral( "values" ), mValues );

  QList<ReosEncodedElement> encodedTimeValues = element.getListEncodedData( QStringLiteral( "timeValues" ) );
  mTimeValues.clear();
  mTimeValues.reserve( encodedTimeValues.size() );
  for ( const ReosEncodedElement &elem : std::as_const( encodedTimeValues ) )
    mTimeValues.append( ReosDuration::decode( elem ) );
}
