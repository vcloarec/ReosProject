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

    explicit ReosDuration( double value = 0.0 );
    explicit ReosDuration( qint64 milliseconds );
    explicit ReosDuration( double value, Unit mUnit );

    ReosDuration operator+( const ReosDuration & ) const;
    ReosDuration operator-( const ReosDuration & ) const;
    ReosDuration operator*( double k ) const;
    ReosDuration operator*( int i ) const;
    ReosDuration operator/( double k ) const;
    double operator/( const ReosDuration & ) const;
    bool operator>( const ReosDuration & ) const;
    bool operator>=( const ReosDuration & ) const;
    bool operator<( const ReosDuration & ) const;
    bool operator<=( const ReosDuration & ) const;
    bool operator==( const ReosDuration & ) const;
    bool operator!=( const ReosDuration & ) const;

    qint64 valueMilliSecond() const;
    double valueSecond() const;
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

    QString toString( int precision = 1 ) const;
    QString toString( ReosDuration::Unit unit, int precision = 1 ) const;

    QString unitToString( ReosDuration::Unit unit ) const;
    QString unitToString() const;

    unsigned numberOfFullyContainedIntervals( const ReosDuration &other ) const;

    ReosEncodedElement encode() const;

    static ReosDuration decode( const ReosEncodedElement &element );

    void setAdaptedUnit();

  private:
    Unit mUnit = second;
    qint64 mValue = 0; //milliseconds

};

#endif //REOSDURATION_H
