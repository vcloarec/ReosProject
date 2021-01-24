/***************************************************************************
                      reosduration.h
                     --------------------------------------
Date                 : 21-08-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                :   vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#ifndef REOSDURATION_H
#define REOSDURATION_H

#include <string>
#include <sstream>
#include <iostream>
#include <math.h>

#include "reosencodedelement.h"

class REOSCORE_EXPORT ReosDuration
{
  public:

    enum Unit {millisecond, second, minute, hour, day, week, month, year};

    ReosDuration( double value = 0 );
    ReosDuration( double value, Unit mUnit );
    ReosDuration( bool defined ): mDefined( defined )
    {}


    ReosDuration operator+( const ReosDuration & ) const;
    ReosDuration operator-( const ReosDuration & ) const;
    ReosDuration operator*( double k ) const;
    ReosDuration operator*( int i ) const;
    ReosDuration operator/( double k ) const;
    double operator/( ReosDuration & ) const;
    bool operator>( const ReosDuration & ) const;
    bool operator>=( const ReosDuration & ) const;
    bool operator<( const ReosDuration & ) const;
    bool operator<=( const ReosDuration & ) const;
    bool operator==( const ReosDuration & ) const;
    bool operator!=( const ReosDuration & ) const;

    double valueSeconde() const;
    double valueMinute() const;
    double valueHour() const;
    double valueDay() const;
    double valueWeek() const;
    double valueMonth() const;
    double valueYear() const;
    double valueUnit() const;
    double valueUnit( Unit un ) const;

    Unit unit() const;
    void setUnit( Unit u );
    QByteArray encode() const;

    QString toString( int precision = 1 );
    QString toString( ReosDuration::Unit unit, int precision = 1 );

    unsigned numberOfFullyContainedIntervals( const ReosDuration &other ) const;

    bool isDefined() const
    {
      return mDefined;
    }

  private:
    Unit mUnit = second;
    qint64 mValue = 0; //milliseconds
    bool mDefined = true;
};

#endif //REOSDURATION_H
