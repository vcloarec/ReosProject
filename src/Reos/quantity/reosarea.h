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
    enum Unit {m2, a, ha, km2, autoUnit};

    ReosArea( double value = 0, Unit unit = m2 );
    ReosArea( QPolygonF polygon, Unit unit = m2 );
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

    double getValueM2() const;
    double getValueA() const;
    double getValueHa() const;
    double getValuerKm2() const;
    double getValueInUnit( Unit unit ) const;
    double getValueInUnit() const;

    Unit unit() const;

    [[deprecated]] QByteArray encodage() const;

    QByteArray encode() const;

    void setUnitAuto();
    void setUnit( ReosArea::Unit u )
    {
      unit_ = u;
    }

  private:
    double valueM2_;
    Unit unit_;


};



[[deprecated]] ReosArea decodeArea( const QByteArray &byteArray );

#endif // REOSAREA_H
