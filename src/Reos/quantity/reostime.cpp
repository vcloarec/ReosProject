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
  dateTime_ = QDateTime( QDate::currentDate(), QTime( 0, 0, 0, 0 ) );
}

ReosTime::ReosTime( QDate date, QTime time ): dateTime_( date, time )
{

}

ReosTime::ReosTime( QDateTime dateTime ): dateTime_( dateTime )
{

}

ReosTime::ReosTime( const ReosEncodedElement &encoded )
{
  encoded.getData( QStringLiteral( "Value" ), dateTime_ );
}

ReosTime ReosTime::operator+( const ReosDuration &duree ) const
{
  return ReosTime( dateTime_.addSecs( qint64( duree.getValueSeconde() ) ) );
}

ReosDuration ReosTime::operator-( const ReosTime &other ) const
{
  return ReosDuration( dateTime_.toSecsSinceEpoch() - other.dateTime_.toSecsSinceEpoch(), ReosDuration::seconde );
}

bool ReosTime::operator>( const ReosTime &other ) const
{
  return dateTime_ > other.dateTime_;
}

bool ReosTime::operator>=( const ReosTime &other ) const
{
  return dateTime_ >= other.dateTime_;
}

bool ReosTime::operator<( const ReosTime &other ) const
{
  return dateTime_ < other.dateTime_;
}

bool ReosTime::operator<=( const ReosTime &other ) const
{
  return dateTime_ <= other.dateTime_;
}

bool ReosTime::operator==( const ReosTime &other ) const
{
  return dateTime_ == other.dateTime_;
}

bool ReosTime::operator!=( const ReosTime &other ) const
{
  return dateTime_ != other.dateTime_;
}

ReosTime ReosTime::operator-( const ReosDuration &duree ) const
{
  return ReosTime( dateTime_.addSecs( 0 - qint64( duree.getValueSeconde() ) ) );
}

QString ReosTime::getString() const
{
  QString retour = dateTime_.toString( "dd/MM/yyyy hh:mm" );

  return retour;
}

QDateTime ReosTime::getDateTime() const
{
  return dateTime_;
}

QByteArray ReosTime::encodage() const
{
  //******************************
  // "Temps"
  // dateTime_ (QDateTime)
  // suite (qint8)


  QByteArray byteArray;
  QDataStream fluxLocal( &byteArray, QIODevice::WriteOnly );
  fluxLocal << QString( "Temps" );
  fluxLocal << dateTime_;
  fluxLocal << qint8( 0 );

  return byteArray;
}

QByteArray ReosTime::encode() const
{
  ReosEncodedElement encoded( QStringLiteral( "Time" ) );
  encoded.addData( QStringLiteral( "Value" ), dateTime_ );

  return encoded.encode();
}

ReosTime decodeTemps( QByteArray &byteArray )
{

  //******************************
  // "Temps"
  // dateTime_ (QDateTime)
  // suite (qint8)
  QDataStream stream( byteArray );
  QString dataType;
  stream >> dataType;

  if ( dataType != "Temps" )
    return ReosTime();

  QDateTime dt;
  stream >> dt;

  qint8 suite;
  stream >> suite;

  return ReosTime( dt );
}
