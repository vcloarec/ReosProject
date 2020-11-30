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

#ifndef REOSAREA_H
#define REOSAREA_H

#include <math.h>

#include <QByteArray>
#include <QDataStream>
#include <QPolygonF>
#include <QDebug>

#include "../../Reos/reosencodedelement.h"

class ReosArea
{
  public:
    enum Unit {m2, a, ha, km2};
    ReosArea( double value=0);
    ReosArea( double value, Unit unit );
    ReosArea( const QPolygonF polygon, Unit unit);
    ReosArea( const ReosEncodedElement &encodedElem );

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

    QByteArray encode() const;

    void setUnitAuto();
    void setUnit( ReosArea::Unit u )
    {
      mUnit = u;
    }

  private:
    double mValueM2=0;
    Unit mUnit=m2;


};

#endif // REOSAREA_H
