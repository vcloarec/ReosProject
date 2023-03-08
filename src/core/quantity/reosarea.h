/***************************************************************************
                      reosarea.h
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

#define SIP_NO_FILE

#ifndef REOSAREA_H
#define REOSAREA_H

#include <math.h>

#include <QByteArray>
#include <QDataStream>
#include <QPolygonF>

#include "reoscore.h"
#include "reosencodedelement.h"

class REOSCORE_EXPORT ReosArea
{
  public:
    enum Unit {m2, a, ha, km2};
    ReosArea( double value = 0 );
    ReosArea( double value, Unit unit );
    ReosArea( const QPolygonF polygon, Unit unit );

    ReosArea operator+( const ReosArea & ) const;
    ReosArea operator-( const ReosArea & ) const;
    ReosArea operator*( double k ) const;
    ReosArea operator*( int i ) const;
    ReosArea operator/( double k ) const;
    bool operator>( const ReosArea & ) const;
    bool operator>=( const ReosArea & ) const;
    bool operator<( const ReosArea & ) const;
    bool operator<=( const ReosArea & ) const;
    bool operator==( const ReosArea & ) const;
    bool operator!=( const ReosArea & ) const;

    double valueM2() const;
    double valueA() const;
    double valueHa() const;
    double valueKm2() const;
    double valueInUnit( Unit unit ) const;
    double valueInUnit() const;

    Unit unit() const;

    void setUnitAuto();
    void setUnit( ReosArea::Unit u );

    QString toString( ReosArea::Unit u, int precision = 2 ) const;
    QString toString( int precision = 2 ) const;
    QString unitToString() const;

    static QString unitToString( ReosArea::Unit u );

    ReosEncodedElement encode() const;
    static ReosArea decode( const ReosEncodedElement &element );

  private:
    double mValueM2 = 0;
    Unit mUnit = m2;
};

#endif // REOSAREA_H
