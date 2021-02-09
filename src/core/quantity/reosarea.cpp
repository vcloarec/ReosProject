/***************************************************************************
                      reosarea.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosarea.h"


ReosArea::ReosArea( double value ): mValueM2( value )
{
  setUnitAuto();
}

ReosArea::ReosArea( double value, Unit unit ): mUnit( unit )
{
  switch ( mUnit )
  {
    case ReosArea::m2:
      mValueM2 = value;
      break;
    case ReosArea::a:
      mValueM2 = value * 100;
      break;
    case ReosArea::ha:
      mValueM2 = value * 10000;
      break;
    case ReosArea::km2:
      mValueM2 = value * 1000000;
      break;
  }
}

ReosArea::ReosArea( const QPolygonF polygon, ReosArea::Unit unit ): mUnit( unit )
{
  double cumul = 0;
  int pointCount = polygon.count();

  if ( pointCount < 3 )
  {
    mValueM2 = 0;
    return;
  }

  for ( int i = 0; i < pointCount; ++i )
  {
    const QPointF &p1 = polygon.at( i );
    const QPointF &p2 = polygon.at( ( i + 1 ) % ( pointCount ) );

    cumul += ( p1.x() * p2.y() - p1.y() * p2.x() );
  }

  mValueM2 = fabs( cumul / 2 );
}

ReosArea ReosArea::operator+( const ReosArea &other ) const
{
  ReosArea returnValue( 0, mUnit );
  returnValue.mValueM2 = mValueM2 + other.mValueM2;

  return returnValue;
}

ReosArea ReosArea::operator-( const ReosArea &other ) const
{
  ReosArea returnValue( 0, mUnit );
  returnValue.mValueM2 = mValueM2 - other.mValueM2;

  return returnValue;
}

ReosArea ReosArea::operator*( double k ) const
{
  ReosArea returnValue( 0, mUnit );
  returnValue.mValueM2 = mValueM2 * k;

  return returnValue;
}

ReosArea ReosArea::operator*( int i ) const
{
  ReosArea returnValue( 0, mUnit );
  returnValue.mValueM2 = mValueM2 * i;

  return returnValue;
}

ReosArea ReosArea::operator/( double k ) const
{
  ReosArea returnValue( 0, mUnit );
  returnValue.mValueM2 = mValueM2 / k;

  return returnValue;
}

bool ReosArea::operator>( const ReosArea &other ) const
{
  return mValueM2 > other.mValueM2;
}

bool ReosArea::operator>=( const ReosArea &other ) const
{
  return this->mValueM2 >= other.mValueM2;
}

bool ReosArea::operator<( const ReosArea &other ) const
{
  return mValueM2 < other.mValueM2;
}

bool ReosArea::operator<=( const ReosArea &other ) const
{
  return mValueM2 <= other.mValueM2;
}

bool ReosArea::operator==( const ReosArea &other ) const
{
  return fabs( mValueM2 - other.mValueM2 ) < std::numeric_limits<double>::epsilon() * fabs( mValueM2 + other.mValueM2 );
}

bool ReosArea::operator!=( const ReosArea &other ) const
{
  return !( operator==( other ) );
}

double ReosArea::valueM2() const
{
  return mValueM2;
}

double ReosArea::valueA() const
{
  return mValueM2 / 100;
}

double ReosArea::valueHa() const
{
  return mValueM2 / 10000;
}

double ReosArea::valueKm2() const
{
  return mValueM2 / 1000000;
}

double ReosArea::valueInUnit( ReosArea::Unit unit ) const
{
  double returnValue;
  switch ( unit )
  {
    case ReosArea::m2:
      returnValue = valueM2();
      break;
    case ReosArea::a:
      returnValue = valueA();
      break;
    case ReosArea::ha:
      returnValue = valueHa();
      break;
    case ReosArea::km2:
      returnValue = valueKm2();
      break;

  }

  return returnValue;
}

double ReosArea::valueInUnit() const
{
  return valueInUnit( mUnit );
}

ReosArea::Unit ReosArea::unit() const
{
  return mUnit;
}

void ReosArea::setUnitAuto()
{
  if ( mValueM2 < 100 )
  {
    mUnit = m2;
    return;
  }

  if ( mValueM2 < 1000 )
  {
    mUnit = a;
    return;
  }

  if ( mValueM2 < 1000000 )
  {
    mUnit = ha;
    return;
  }

  mUnit = km2;
}

void ReosArea::setUnit( ReosArea::Unit u )
{
  mUnit = u;
}

QString ReosArea::toString( ReosArea::Unit u, int precision ) const
{
  double v = valueInUnit( u );
  return QString::number( v, 'f', precision ) + " " + unitToString( u );
}

QString ReosArea::toString( int precision ) const
{
  return toString( mUnit, precision );
}

QString ReosArea::unitToString() const
{
  return unitToString( ( mUnit ) );
}

QString ReosArea::unitToString( ReosArea::Unit u )
{
  switch ( u )
  {
    case ReosArea::m2:
      return QString( 'm' ).append( QChar( 0x00B2 ) );
      break;
    case ReosArea::a:
      return QObject::tr( "a" );
      break;
    case ReosArea::ha:
      return QObject::tr( "ha" );
      break;
    case ReosArea::km2:
      return QStringLiteral( "km" ).append( QChar( 0x00B2 ) );
      break;

  }
  return QString();
}

ReosEncodedElement ReosArea::encode() const
{
  ReosEncodedElement element( QStringLiteral( "area" ) );

  element.addData( QStringLiteral( "value" ), mValueM2 );
  element.addData( QStringLiteral( "unit" ), mUnit );

  return element;
}

ReosArea ReosArea::decode( const ReosEncodedElement &element )
{
  ReosArea ret;
  if ( element.description() != QStringLiteral( "area" ) )
    return ret;

  if ( !element.getData( QStringLiteral( "value" ), ret.mValueM2 ) )
    return ret;

  int unit;
  if ( !element.getData( QStringLiteral( "unit" ), unit ) )
    return ret;
  ret.mUnit = static_cast<ReosArea::Unit>( unit );

  return ret;
}

