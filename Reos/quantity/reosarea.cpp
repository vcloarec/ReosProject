/***************************************************************************
                      reosarea.cpp
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


ReosArea::ReosArea(double value, Unit unit):unit_(unit)
{

    switch (unit_) {
    case ReosArea::m2:
        valueM2_=value;
        break;
    case ReosArea::a:
        valueM2_=value*100;
        break;
    case ReosArea::ha:
        valueM2_=value*10000;
        break;
    case ReosArea::km2:
        valueM2_=value*1000000;
        break;
    case ReosArea::autoUnit:
        valueM2_=value;
        setUnitAuto();
        break;
    }
}

ReosArea::ReosArea(QPolygonF polygon, ReosArea::Unit unit):unit_(unit)
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

ReosArea::ReosArea(const ReosEncodedElement &encodedElem)
{
    encodedElem.getData(QStringLiteral("value m2"),valueM2_);
    int u;
    encodedElem.getData(QStringLiteral("Unit"),u);
    unit_=static_cast<Unit>(u);
}

ReosArea ReosArea::operator+(const ReosArea &other) const
{
    ReosArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_+other.valueM2_;

    return returnValue;
}

ReosArea ReosArea::operator-(const ReosArea &other) const
{
    ReosArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_-other.valueM2_;

    return returnValue;
}

ReosArea ReosArea::operator*(double k) const
{
    ReosArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_*k;

    return returnValue;
}

ReosArea ReosArea::operator*(int i) const
{
    ReosArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_*i;

    return returnValue;
}

ReosArea ReosArea::operator/(double k) const
{
    ReosArea returnValue(0,unit_);
    returnValue.valueM2_=valueM2_/k;

    return returnValue;
}

bool ReosArea::operator>(const ReosArea &other) const
{
    return valueM2_>other.valueM2_;
}

bool ReosArea::operator>=(const ReosArea &other) const
{
    return this->valueM2_>=other.valueM2_;
}

bool ReosArea::operator<(const ReosArea &other) const
{
    return valueM2_<other.valueM2_;
}

bool ReosArea::operator<=(const ReosArea &other) const
{
    return valueM2_<=other.valueM2_;
}

bool ReosArea::operator==(const ReosArea &other) const
{
    return fabs(valueM2_-other.valueM2_)<std::numeric_limits<double>::epsilon()*fabs(valueM2_+other.valueM2_);
}

bool ReosArea::operator!=(const ReosArea &other) const
{
    return !(operator==(other));
}

double ReosArea::getValueM2() const
{
    return valueM2_;
}

double ReosArea::getValueA() const
{
    return valueM2_/100;
}

double ReosArea::getValueHa() const
{
    return valueM2_/10000;
}

double ReosArea::getValuerKm2() const
{
    return valueM2_/1000000;
}

double ReosArea::getValueInUnit(ReosArea::Unit unit) const
{
    double returnValue;
    switch (unit) {
    case ReosArea::m2:
        returnValue=getValueM2();
        break;
    case ReosArea::a:
        returnValue=getValueA();
        break;
    case ReosArea::ha:
        returnValue=getValueHa();
        break;
    case ReosArea::km2:
        returnValue=getValuerKm2();
        break;

    }

    return returnValue;
}

double ReosArea::getValueInUnit() const
{
    return getValueInUnit(unit_);
}

ReosArea::Unit ReosArea::unit() const
{
    return unit_;
}

QByteArray ReosArea::encodage() const
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

QByteArray ReosArea::encode() const
{
    ReosEncodedElement encodedArea(QStringLiteral("Area"));
    encodedArea.addData(QStringLiteral("value m2"),valueM2_);
    encodedArea.addData(QStringLiteral("Unit"),int(unit_));
    return encodedArea.encode();
}

void ReosArea::setUnitAuto()
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

ReosArea decodeArea(const QByteArray &byteArray)
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
        return ReosArea();

    qint8 un;
    stream>>un;
    qreal val;
    stream>>val;

    qint8 suite;
    stream>>suite;

    return ReosArea(val,static_cast<ReosArea::Unit>(un));
}
