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

#ifndef HDAREA_H
#define HDAREA_H

#include <math.h>

#include <QByteArray>
#include <QDataStream>
#include <QPolygonF>
#include <QDebug>

#include "../../Reos/reosencodedelement.h"

class HdArea
{
public:
    enum Unit {m2,a,ha,km2,autoUnit};

    HdArea(double value=0,Unit unit=m2);
    HdArea(QPolygonF polygon,Unit unit=m2);
    HdArea(const ReosEncodedElement &encodedElem);

    HdArea operator+(const HdArea&) const;
    HdArea operator-(const HdArea&) const;
    HdArea operator*(double k) const;
    HdArea operator*(int i) const;
    HdArea operator/(double k) const;
    bool operator>(const HdArea&) const;
    bool operator>=(const HdArea&) const;
    bool operator<(const HdArea&) const;
    bool operator<=(const HdArea&) const;
    bool operator==(const HdArea&) const;
    bool operator!=(const HdArea&) const;

    double getValeurM2() const;
    double getValeurA() const;
    double getValeurHa() const;
    double getValeurKm2() const;
    double getValeurInUnit(Unit unit) const;
    double getValeurInUnit() const;

    Unit getUnit() const;

    QByteArray encodage() const;

    QByteArray encode() const;

    void setUnitAuto();
    void setUnit(HdArea::Unit u)
    {
        unit_=u;
    }

private:
    double valueM2_;
    Unit unit_;


};



HdArea decodeArea(const QByteArray &byteArray);

#endif // HDAREA_H
