/***************************************************************************
                      reosduration.cpp
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

#include "reosduration.h"


ReosDuration::ReosDuration(double valeur, ReosDuration::Unit un)
{
    mUnit=un;

    switch (mUnit) {
    case seconde:
        mValueInSeconde=valeur;
        break;
    case minute:
       mValueInSeconde=valeur*60;
        break;
    case heure:
        mValueInSeconde=valeur*3600;
        break;
    case jour:
        mValueInSeconde=valeur*86400;
        break;
    case semaine:
        mValueInSeconde=valeur*604800;
        break;
    case mois:
        mValueInSeconde=valeur*2592000;
        break;
    case annee:
        mValueInSeconde=valeur*31536000;
        break;
    }
}

std::string ReosDuration::toString(int precision) const
{

    std::ostringstream retour;

    retour.setf(std::ios_base::fixed);
    retour.precision(precision);

    double val;

    std::string uniteStr;
    switch (mUnit) {
    case seconde:
        val=mValueInSeconde;
        uniteStr="s";
        break;
    case minute:
        val=getValueMinute();
        uniteStr="mn";
        break;
    case heure:
        val=getValueHour();
        uniteStr="h";
        break;
    case jour:
        val=getValueDay();
        uniteStr="j";
        break;
    case semaine:
        val=getValueWeek();
        if (val>1)
           uniteStr="semaines";
        else
            uniteStr="semaine";
        break;
    case mois:
        val=getValueMonth();
        uniteStr="mois";
        break;
    case annee:
        val=getValueYear();
        if (val>1)
           uniteStr="années";
        else
            uniteStr="annnée";
        break;
    }

    retour<<val<<" "<<uniteStr;

    return retour.str();
}

QString ReosDuration::getString(ReosDuration::Unit unit,int precision)
{
    QString returnValue=QString::number(getValueUnit(unit),'f',precision);
    double val;
    switch (unit) {
    case seconde:
        returnValue.append(QObject::tr(" s"));
        break;
    case minute:
        returnValue.append(QObject::tr(" mn"));
        break;
    case heure:
        returnValue.append(QObject::tr(" h"));
        break;
    case jour:
        returnValue.append(QObject::tr(" j"));
        break;
    case semaine:
        val=getValueWeek();
        if (val>1)
           returnValue.append(QObject::tr(" semaines"));
        else
            returnValue.append(QObject::tr(" semaine"));
        break;
    case mois:
        returnValue.append(QObject::tr(" mois"));
        break;
    case annee:
        val=getValueYear();
        if (val>1)
           returnValue.append(QObject::tr(" années"));
        else
            returnValue.append(QObject::tr(" annnée"));
        break;
    }


    return returnValue;
}




ReosDuration ReosDuration::operator+(const ReosDuration &other) const
{
  ReosDuration retour(0,this->mUnit);
  retour.mValueInSeconde=this->mValueInSeconde+other.mValueInSeconde;

  return retour;
}

ReosDuration ReosDuration::operator-(const ReosDuration &other) const
{
    ReosDuration retour(0,this->mUnit);
    retour.mValueInSeconde=this->mValueInSeconde-other.mValueInSeconde;

    return retour;
}

ReosDuration ReosDuration::operator*(const double k) const
{
    ReosDuration retour(0,this->mUnit);
    retour.mValueInSeconde=k*this->mValueInSeconde;

    return retour;
}

ReosDuration ReosDuration::operator*(const int i) const
{
    ReosDuration retour(0,this->mUnit);
    retour.mValueInSeconde=i*this->mValueInSeconde;

    return retour;
}

ReosDuration ReosDuration::operator/(const double k) const
{
    ReosDuration retour(0,this->mUnit);
    retour.mValueInSeconde=this->mValueInSeconde/k;
    return retour;
}

double ReosDuration::operator/(ReosDuration &other) const
{
    return mValueInSeconde/other.mValueInSeconde;
}

bool ReosDuration::operator>(const ReosDuration &other) const
{
    return this->mValueInSeconde>other.mValueInSeconde;
}

bool ReosDuration::operator>=(const ReosDuration &other) const
{
    return this->mValueInSeconde>=other.mValueInSeconde;
}

bool ReosDuration::operator<(const ReosDuration &other) const
{
    return this->mValueInSeconde<other.mValueInSeconde;
}

bool ReosDuration::operator<=(const ReosDuration &other) const
{
    return this->mValueInSeconde<=other.mValueInSeconde;
}

bool ReosDuration::operator==(const ReosDuration &other) const
{
    return fabs(mValueInSeconde-other.mValueInSeconde)<std::numeric_limits<double>::epsilon()*fabs(mValueInSeconde+other.mValueInSeconde);
}

bool ReosDuration::operator!=(const ReosDuration &other) const
{
    return !operator==(other);
}

double ReosDuration::getValueSeconde() const {return mValueInSeconde;}

double ReosDuration::getValueMinute() const {return mValueInSeconde/60;}

double ReosDuration::getValueHour() const {return mValueInSeconde/3600;}

double ReosDuration::getValueDay() const {return mValueInSeconde/86400;}

double ReosDuration::getValueWeek() const {return mValueInSeconde/604800;}

double ReosDuration::getValueMonth() const {return mValueInSeconde/2592000;}

double ReosDuration::getValueYear() const {return mValueInSeconde/31536000;}

double ReosDuration::getValueUnit() const
{
    return getValueUnit(mUnit);
}

double ReosDuration::getValueUnit(ReosDuration::Unit un) const
{
    double val;
    switch (un) {
    case seconde:
        val=getValueSeconde();
        break;
    case minute:
        val=getValueMinute();
        break;
    case heure:
        val=getValueHour();
        break;
    case jour:
        val=getValueDay();
        break;
    case semaine:
        val=getValueWeek();
        break;
    case mois:
        val=getValueMonth();
        break;
    case annee:
        val=getValueYear();
        break;
    }

    return val;
}

ReosDuration::Unit ReosDuration::unit() const {return mUnit;}

void ReosDuration::setUnit(ReosDuration::Unit u)
{
    mUnit=u;
}

QByteArray ReosDuration::encodage() const
{
    QByteArray byteArray;
    QDataStream fluxLocal(&byteArray,QIODevice::WriteOnly);
    fluxLocal<<QString("Duree");
    fluxLocal<<qint8(mUnit);
    fluxLocal<<qreal(getValueUnit());
    fluxLocal<<qint8(0);
    return byteArray;


    //******************************
    // "Duree"
    // unite(qint8)
    // valeur en unite (qreal)
    // 0 (qint8)
}

QByteArray ReosDuration::encode() const
{
    ReosEncodedElement encoded(QStringLiteral("Duration"));
    encoded.addData(QStringLiteral("Unit"),qint8(mUnit));
    encoded.addData(QStringLiteral("Value"),qreal(getValueSeconde()));

    return encoded.encode();
}


unsigned ReosDuration::numberOfFullyContainedIntervals(const ReosDuration &other) const
{
    if (other.getValueSeconde()>mValueInSeconde)
        return 0;

    return unsigned(mValueInSeconde/other.getValueSeconde());
}


ReosDuration decodeDuration(const QByteArray &byteArray)
{
    //******************************
    // "Duree"
    // unite(qint8)
    // valeur en unite (qreal)
    // 0 (qint8)

    QDataStream stream (byteArray);
    QString dataType;
    stream>>dataType;
    if (dataType!="Duree")
        return ReosDuration();

    qint8 un;
    stream>>un;
    qreal val;
    stream>>val;

    qint8 suite;
    stream>>suite;

    return ReosDuration(val,static_cast<ReosDuration::Unit>(un));
}
