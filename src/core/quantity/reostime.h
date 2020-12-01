/***************************************************************************
                      HdTemps.h
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

#ifndef REOSTIME_H
#define REOSTIME_H
#include <QDate>
#include <QTime>
#include <QDebug>

#include "reosduration.h"

class ReosTime
{
  public:
    //! Default constructor, set the dateto the current date and the time 00:00
    ReosTime();
    ReosTime( QDate date, QTime time );
    ReosTime( QDateTime dateTime );
    ReosTime( const ReosEncodedElement &encoded );

    ReosTime operator+( const ReosDuration &duree ) const;
    ReosTime operator-( const ReosDuration &duree ) const;
    ReosDuration operator-( const ReosTime &other ) const;
    bool operator>( const ReosTime &other ) const;
    bool operator>=( const ReosTime &other ) const;
    bool operator<( const ReosTime &other ) const;
    bool operator<=( const ReosTime &other ) const;
    bool operator==( const ReosTime &other ) const;
    bool operator!=( const ReosTime &other ) const;

    QString toString(const QString &format="dd/MM/yyyy hh:mm") const;

    QDateTime getDateTime() const;

    QByteArray encode() const;

  private:
    QDateTime mDateTime;

};

#endif //REOSTIME_H
