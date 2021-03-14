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


ReosArea::ReosArea(double value):mValueM2(value)
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

ReosArea::ReosArea(const QPolygonF polygon, ReosArea::Unit unit ): mUnit( unit )
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

ReosArea::ReosArea( const ReosEncodedElement &encodedElem )
{
  encodedElem.getData( QStringLiteral( "Value_m2"), mValueM2 );
  int u;
  encodedElem.getData( QStringLiteral( "Unit" ), u );
  mUnit = static_cast<Unit>( u );
}



QByteArray ReosArea::encode() const
{
  ReosEncodedElement encodedArea( QStringLiteral( "Area" ) );
  encodedArea.addData( QStringLiteral( "Value_m2" ), mValueM2 );
  encodedArea.addData( QStringLiteral( "Unit" ), int( mUnit ) );
  return encodedArea.encode();
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

