/***************************************************************************
                      hdarea.cpp
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

#include "reosarea.h"


HdArea::HdArea(double value, Unit unit):unit_(unit)
{

    switch (unit_) {
    case HdArea::m2:
        valueM2_=value;
        break;
    case HdArea::a:
        valueM2_=value*100;
        break;
    case HdArea::ha:
        valueM2_=value*10000;
        break;
    case HdArea::km2:
        valueM2_=value*1000000;
        break;
    case HdArea::autoUnit:
        valueM2_=value;
        setUnitAuto();
        break;
    }
}

HdArea::HdArea(QPolygonF polygon, HdArea::Unit unit):unit_(unit)
{
    double cumul=0;
    int pointCount=polygon.count();

    if (pointCount<3)
    {
     valueM2_=0;
     return;
    }

    for (int i=0;i<pointCount;++i)
    {
        const QPointF& p1=polygon.at(i);
        const QPointF& p2=polygon.at((i+1)%(pointCount));

        cumul+=(p1.x()*p2.y()-p1.y()*p2.x());
    }

    valueM2_=fabs(cumul/2);
}

HdArea::HdArea(const ReosEncodedElement &encodedElem)
{
    encodedElem.getData(QStringLiteral("value m2"),valueM2_);
    int u;
    encodedElem.getData(QStringLiteral("Unit"),u);
    unit_=static_cast<Unit>(u);
}

HdArea HdArea::operator+(const HdArea &other) const
{
    HdArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_+other.valueM2_;

    return returnValue;
}

HdArea HdArea::operator-(const HdArea &other) const
{
    HdArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_-other.valueM2_;

    return returnValue;
}

HdArea HdArea::operator*(double k) const
{
    HdArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_*k;

    return returnValue;
}

HdArea HdArea::operator*(int i) const
{
    HdArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_*i;

    return returnValue;
}

HdArea HdArea::operator/(double k) const
{
    HdArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_/k;

    return returnValue;
}

bool HdArea::operator>(const HdArea &other) const
{
    return valueM2_>other.valueM2_;
}

bool HdArea::operator>=(const HdArea &other) const
{
    return this->valueM2_>=other.valueM2_;
}

bool HdArea::operator<(const HdArea &other) const
{
    return valueM2_<other.valueM2_;
}

bool HdArea::operator<=(const HdArea &other) const
{
    return valueM2_<=other.valueM2_;
}

bool HdArea::operator==(const HdArea &other) const
{
    return fabs(valueM2_-other.valueM2_)<std::numeric_limits<double>::epsilon()*fabs(valueM2_+other.valueM2_);
}

bool HdArea::operator!=(const HdArea &other) const
{
    return !(operator==(other));
}

double HdArea::getValeurM2() const
{
    return valueM2_;
}

double HdArea::getValeurA() const
{
    return valueM2_/100;
}

double HdArea::getValeurHa() const
{
    return valueM2_/10000;
}

double HdArea::getValeurKm2() const
{
    return valueM2_/1000000;
}

double HdArea::getValeurInUnit(HdArea::Unit unit) const
{
    double returnValue;
    switch (unit) {
    case HdArea::m2:
        returnValue=getValeurM2();
        break;
    case HdArea::a:
        returnValue=getValeurA();
        break;
    case HdArea::ha:
        returnValue=getValeurHa();
        break;
    case HdArea::km2:
        returnValue=getValeurKm2();
        break;

    }

    return returnValue;
}

double HdArea::getValeurInUnit() const
{
    return getValeurInUnit(unit_);
}

HdArea::Unit HdArea::getUnit() const
{
    return unit_;
}

QByteArray HdArea::encodage() const
{
    QByteArray byteArray;
    QDataStream fluxLocal(&byteArray,QIODevice::WriteOnly);
    fluxLocal<<QString("Area");
    fluxLocal<<qint8(unit_);
    fluxLocal<<qreal(valueM2_);
    fluxLocal<<qint8(0);
    return byteArray;


    //******************************
    // "Area"
    // unite(qint8)
    // valeur en unite (qreal)
    // 0 (qint8)
}

QByteArray HdArea::encode() const
{
    ReosEncodedElement encodedArea(QStringLiteral("Area"));
    encodedArea.addData(QStringLiteral("value m2"),valueM2_);
    encodedArea.addData(QStringLiteral("Unit"),int(unit_));
    return encodedArea.encode();
}

void HdArea::setUnitAuto()
{
    if (valueM2_<100)
    {
        unit_=m2;
        return;
    }

    if (valueM2_<1000)
    {
        unit_=a;
        return;
    }

    if (valueM2_<1000000)
    {
        unit_=ha;
        return;
    }

    unit_=km2;

}

HdArea decodeArea(const QByteArray &byteArray)
{
    //******************************
    // "Area"
    // unit(qint8)
    // valeur en unite (qreal)
    // 0 (qint8)

    QDataStream stream (byteArray);
    QString dataType;
    stream>>dataType;
    if (dataType!="Area")
        return HdArea();

    qint8 un;
    stream>>un;
    qreal val;
    stream>>val;

    qint8 suite;
    stream>>suite;

    return HdArea(val,static_cast<HdArea::Unit>(un));
}
