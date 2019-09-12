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
    ReosTime();
    ReosTime(QDate date, QTime time);
    ReosTime(QDateTime dateTime);

    ReosTime(const ReosEncodedElement &encoded);

    ReosTime operator+(const ReosDuration &duree) const;
    ReosTime operator-(const ReosDuration &duree) const;
    ReosDuration operator-(const ReosTime &other) const;
    bool operator>(const ReosTime& other) const;
    bool operator>=(const ReosTime& other) const;
    bool operator<(const ReosTime& other) const;
    bool operator<=(const ReosTime& other) const;
    bool operator==(const ReosTime& other) const;
    bool operator!=(const ReosTime& other) const;

    QString getString() const;

    QDateTime getDateTime() const;

    QByteArray encodage() const;
    QByteArray encode() const;

private:
    QDateTime dateTime_;

};


ReosTime decodeTemps(QByteArray &byteArray);

#endif //REOSTIME_H
