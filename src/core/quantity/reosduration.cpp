/***************************************************************************
                      reosduration.cpp
                     --------------------------------------
Date                 : 21-08-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                :   projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosduration.h"
#include "reosparameter.h"


static qint64 SECOND_IN_MILLISECONDS = 1000;
static qint64 MINUTE_IN_MILLISECONDS = SECOND_IN_MILLISECONDS * 60;
static qint64 HOUR_IN_MILLISECONDS = MINUTE_IN_MILLISECONDS * 60;
static qint64 DAY_IN_MILLISECONDS = HOUR_IN_MILLISECONDS * 24;
static qint64 WEEK_IN_MILLISECOND = DAY_IN_MILLISECONDS * 7;
static qint64 MONTH_IN_MILLISECOND = DAY_IN_MILLISECONDS * 30;
static qint64 YEAR_IN_MILLISECOND = DAY_IN_MILLISECONDS * 365;


ReosDuration::ReosDuration( qint64 milliseconds )
{
  mUnit = millisecond;
  mValue = milliseconds;
}

ReosDuration::ReosDuration( double value ): mValue( value )
{}

ReosDuration::ReosDuration( double value, ReosDuration::Unit un )
{
  mUnit = un;

  switch ( mUnit )
  {
    case millisecond:
      mValue = value;
      break;
    case second:
      mValue = value * SECOND_IN_MILLISECONDS;
      break;
    case minute:
      mValue = value * MINUTE_IN_MILLISECONDS;
      break;
    case hour:
      mValue = value * HOUR_IN_MILLISECONDS;
      break;
    case day:
      mValue = value * DAY_IN_MILLISECONDS;
      break;
    case week:
      mValue = value * WEEK_IN_MILLISECOND;
      break;
    case month:
      mValue = value * MONTH_IN_MILLISECOND;
      break;
    case year:
      mValue = value * YEAR_IN_MILLISECOND;
      break;
  }
}


QString ReosDuration::toString( int precision ) const
{
  return toString( mUnit, precision );
}

QString ReosDuration::toString( ReosDuration::Unit unit, int precision ) const
{
  QString returnValue = ReosParameter::doubleToString( valueUnit( unit ), precision );
  returnValue.append( ' ' + unitToString( unit ) );

  return returnValue;
}

QString ReosDuration::unitToString( ReosDuration::Unit unit ) const
{
  double val;
  switch ( unit )
  {
    case millisecond:
      return QObject::tr( "ms" ) ;
      break;
    case second:
      return QObject::tr( "s" ) ;
      break;
    case minute:
      return QObject::tr( "mn" );
      break;
    case hour:
      return QObject::tr( "h" );
      break;
    case day:
      return QObject::tr( "d" );
      break;
    case week:
      val = valueWeek();
      if ( val > 1 )
        return  QObject::tr( "weeks" );
      else
        return QObject::tr( "week" );
      break;
    case month:
      val = valueMonth();
      if ( val > 1 )
        return QObject::tr( "monthes" );
      else
        return  QObject::tr( "month" );
      break;
    case year:
      val = valueYear();
      if ( val > 1 )
        return  QObject::tr( "years" );
      else
        return QObject::tr( "year" );
      break;
  }

  return QString();
}

QString ReosDuration::unitToString() const
{
  return unitToString( mUnit );
}

ReosDuration ReosDuration::operator+( const ReosDuration &other ) const
{
  ReosDuration ret( 0, this->mUnit );
  ret.mValue = this->mValue + other.mValue;

  return ret;
}

ReosDuration ReosDuration::operator-( const ReosDuration &other ) const
{
  ReosDuration ret( 0, this->mUnit );
  ret.mValue = this->mValue - other.mValue;

  return ret;
}

ReosDuration ReosDuration::operator*( const double k ) const
{
  ReosDuration ret( 0, this->mUnit );
  ret.mValue = k * this->mValue;

  return ret;
}

ReosDuration ReosDuration::operator*( const int i ) const
{
  ReosDuration ret( 0, this->mUnit );
  ret.mValue = i * this->mValue;

  return ret;
}

ReosDuration ReosDuration::operator/( const double k ) const
{
  ReosDuration ret( 0, this->mUnit );
  ret.mValue = this->mValue / k;
  return ret;
}

double ReosDuration::operator/( const ReosDuration &other ) const
{
  return mValue / static_cast<double>( other.mValue );
}

bool ReosDuration::operator>( const ReosDuration &other ) const
{
  return this->mValue > other.mValue;
}

bool ReosDuration::operator>=( const ReosDuration &other ) const
{
  return this->mValue >= other.mValue;
}

bool ReosDuration::operator<( const ReosDuration &other ) const
{
  return this->mValue < other.mValue;
}

bool ReosDuration::operator<=( const ReosDuration &other ) const
{
  return this->mValue <= other.mValue;
}

bool ReosDuration::operator==( const ReosDuration &other ) const
{
  return mValue == other.mValue;
}

bool ReosDuration::operator!=( const ReosDuration &other ) const
{
  return !operator==( other );
}

qint64 ReosDuration::valueMilliSecond() const
{
  return mValue;
}

double ReosDuration::valueSecond() const {return mValue / static_cast<double>( SECOND_IN_MILLISECONDS );}

double ReosDuration::valueMinute() const {return mValue / static_cast<double>( MINUTE_IN_MILLISECONDS );}

double ReosDuration::valueHour() const {return mValue / static_cast<double>( HOUR_IN_MILLISECONDS );}

double ReosDuration::valueDay() const {return mValue / static_cast<double>( DAY_IN_MILLISECONDS );}

double ReosDuration::valueWeek() const {return mValue / static_cast<double>( WEEK_IN_MILLISECOND );}

double ReosDuration::valueMonth() const {return mValue / static_cast<double>( MONTH_IN_MILLISECOND );}

double ReosDuration::valueYear() const {return mValue / static_cast<double>( YEAR_IN_MILLISECOND );}

double ReosDuration::valueUnit() const
{
  return valueUnit( mUnit );
}

double ReosDuration::valueUnit( ReosDuration::Unit un ) const
{
  double val;
  switch ( un )
  {
    case second:
      val = valueSecond();
      break;
    case minute:
      val = valueMinute();
      break;
    case hour:
      val = valueHour();
      break;
    case day:
      val = valueDay();
      break;
    case week:
      val = valueWeek();
      break;
    case month:
      val = valueMonth();
      break;
    case year:
      val = valueYear();
      break;
    default:
      val = mValue;
      break;
  }

  return val;
}

ReosDuration::Unit ReosDuration::unit() const {return mUnit;}

void ReosDuration::setUnit( ReosDuration::Unit u )
{
  mUnit = u;
}

unsigned ReosDuration::numberOfFullyContainedIntervals( const ReosDuration &other ) const
{
  if ( other.mValue == 0 || other.mValue > mValue )
    return 0;

  return unsigned( mValue / other.mValue );
}

ReosEncodedElement ReosDuration::encode() const
{
  ReosEncodedElement element( QStringLiteral( "duration" ) );

  element.addData( QStringLiteral( "value" ), mValue );
  element.addData( QStringLiteral( "unit" ), static_cast<int>( mUnit ) );

  return element;
}

ReosDuration ReosDuration::decode( const ReosEncodedElement &element )
{
  ReosDuration ret;

  if ( element.description() != QStringLiteral( "duration" ) )
    return ret;

  if ( !element.getData( QStringLiteral( "value" ), ret.mValue ) )
    return ret;

  int intUnit;
  if ( !element.getData( QStringLiteral( "unit" ), intUnit ) )
    return ret;
  ret.mUnit = static_cast<Unit>( intUnit );

  return ret;
}

void ReosDuration::setAdaptedUnit()
{
  if ( mValue > SECOND_IN_MILLISECONDS )
    mUnit = ReosDuration::second;

  if ( mValue > MINUTE_IN_MILLISECONDS )
    mUnit = ReosDuration::minute;

  if ( mValue > HOUR_IN_MILLISECONDS )
    mUnit = ReosDuration::hour;

  if ( mValue > DAY_IN_MILLISECONDS )
    mUnit = ReosDuration::day;

  if ( mValue > WEEK_IN_MILLISECOND )
    mUnit = ReosDuration::week;

  if ( mValue > MONTH_IN_MILLISECOND )
    mUnit = ReosDuration::month;

  if ( mValue > YEAR_IN_MILLISECOND )
    mUnit = ReosDuration::year;
}
