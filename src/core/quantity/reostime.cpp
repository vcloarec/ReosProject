/***************************************************************************
                      HdTemps.cpp
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

#include "reostime.h"


ReosTime::ReosTime()
{
  mDateTime = QDateTime( QDate::currentDate(), QTime( 0, 0, 0, 0 ) );
}

ReosTime::ReosTime( QDate date, QTime time ): mDateTime( date, time )
{}

ReosTime::ReosTime( QDateTime dateTime ): mDateTime( dateTime )
{}

ReosTime::ReosTime( const ReosEncodedElement &encoded )
{
  encoded.getData( QStringLiteral( "Value" ), mDateTime );
}

ReosTime ReosTime::operator+( const ReosDuration &duree ) const
{
  return ReosTime( mDateTime.addSecs( qint64( duree.getValueSeconde() ) ) );
}

ReosDuration ReosTime::operator-( const ReosTime &other ) const
{
  return ReosDuration( mDateTime.toSecsSinceEpoch() - other.mDateTime.toSecsSinceEpoch(), ReosDuration::seconde );
}

bool ReosTime::operator>( const ReosTime &other ) const
{
  return mDateTime > other.mDateTime;
}

bool ReosTime::operator>=( const ReosTime &other ) const
{
  return mDateTime >= other.mDateTime;
}

bool ReosTime::operator<( const ReosTime &other ) const
{
  return mDateTime < other.mDateTime;
}

bool ReosTime::operator<=( const ReosTime &other ) const
{
  return mDateTime <= other.mDateTime;
}

bool ReosTime::operator==( const ReosTime &other ) const
{
  return mDateTime == other.mDateTime;
}

bool ReosTime::operator!=( const ReosTime &other ) const
{
  return mDateTime != other.mDateTime;
}

ReosTime ReosTime::operator-( const ReosDuration &duree ) const
{
  return ReosTime( mDateTime.addSecs( 0 - qint64( duree.getValueSeconde() ) ) );
}

QString ReosTime::toString(const QString &format) const
{
  QString retour = mDateTime.toString( format );
  return retour;
}

QDateTime ReosTime::getDateTime() const
{
  return mDateTime;
}

QByteArray ReosTime::encode() const
{
  ReosEncodedElement encoded( QStringLiteral( "Time" ) );
  encoded.addData( QStringLiteral( "Value" ), mDateTime );

  return encoded.encode();
}

