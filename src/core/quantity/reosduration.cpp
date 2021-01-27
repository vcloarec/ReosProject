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
      mValue = value * 1000;
      break;
    case minute:
      mValue = value * 60 * 1000;
      break;
    case hour:
      mValue = value * 3600 * 1000;
      break;
    case day:
      mValue = value * 86400 * 1000;
      break;
    case week:
      mValue = value * 604800 * 1000;
      break;
    case month:
      mValue = value * 2592000 * 1000;
      break;
    case year:
      mValue = value * 31536000 * 1000;
      break;
  }
}


QString ReosDuration::toString( int precision )
{
  return toString( mUnit, precision );
}

QString ReosDuration::toString( ReosDuration::Unit unit, int precision )
{
  QString returnValue = QString::number( valueUnit( unit ), 'f', precision );
  double val = valueUnit();
  switch ( unit )
  {
    case second:
      returnValue.append( QObject::tr( " s" ) );
      break;
    case minute:
      returnValue.append( QObject::tr( " mn" ) );
      break;
    case hour:
      returnValue.append( QObject::tr( " h" ) );
      break;
    case day:
      returnValue.append( QObject::tr( " d" ) );
      break;
    case week:
      val = valueWeek();
      if ( val > 1 )
        returnValue.append( QObject::tr( " weeks" ) );
      else
        returnValue.append( QObject::tr( " week" ) );
      break;
    case month:
      if ( val > 1 )
        returnValue.append( QObject::tr( "monthes" ) );
      else
        returnValue.append( QObject::tr( " month" ) );
      break;
    case year:
      val = valueYear();
      if ( val > 1 )
        returnValue.append( QObject::tr( " year" ) );
      else
        returnValue.append( QObject::tr( " years" ) );
      break;
  }

  return returnValue;
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

double ReosDuration::operator/( ReosDuration &other ) const
{
  return mValue / other.mValue;
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

double ReosDuration::valueSeconde() const {return mValue / 1000.0;}

double ReosDuration::valueMinute() const {return mValue / 1000.0 / 60;}

double ReosDuration::valueHour() const {return mValue / 1000.0 / 3600;}

double ReosDuration::valueDay() const {return mValue / 1000.0 / 86400;}

double ReosDuration::valueWeek() const {return mValue / 1000.0 / 604800;}

double ReosDuration::valueMonth() const {return mValue / 1000.0 / 2592000;}

double ReosDuration::valueYear() const {return mValue / 1000.0 / 31536000;}

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
      val = valueSeconde();
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
  if ( other.mValue > mValue )
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
