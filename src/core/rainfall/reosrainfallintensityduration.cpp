/***************************************************************************
                      hlgidf.cpp
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

#include "reosrainfallintensityduration.h"
#include "HlgCoefMontana.h"


QByteArray ReosRainfallIntensityDuration::encode() const
{
    ReosEncodedElement encoded(QStringLiteral("IDF curve"));
    encoded.addData(QStringLiteral("Type"),qint8(getType()));
    encoded.addData(QStringLiteral("Return period"),mReturnPeriod.encode());
    encoded.addData(QStringLiteral("Time unit"),qint8(uniteTemps));

    QList<QVector<double>> coefficoentList;
    for (auto couple:coefficient)
    {
        QVector<double> cp(2);
        cp[0]=couple.at(0);
        cp[1]=couple.at(1);
        coefficoentList.append(cp);
    }
    encoded.addData(QStringLiteral("Coefficients"),coefficoentList);

    QList<QByteArray> timeStepList;
    for (auto dur:pasTemps)
    {
        timeStepList.append(dur.encode());
    }
    encoded.addData(QStringLiteral("Time step"),timeStepList);


    return encoded.encode();
}



double ReosRainfallIntensityDuration::getCoefA(unsigned int i) const {
    if (i<coefficient.size())
        return coefficient.at(i).at(0);
    else return 0;
}

double ReosRainfallIntensityDuration::getCoefB(unsigned int i) const {
    if (i<coefficient.size())
        return coefficient.at(i).at(1);
    else return 0;
}

std::vector<ReosDuration> ReosRainfallIntensityDuration::getIntervalle(unsigned int i) const
{
    std::vector<ReosDuration> retour;

    if (i<pasTemps.size()-1)
    {

        retour.push_back(pasTemps.at(i));
        retour.push_back(pasTemps.at(i+1));
    }

    return retour;

}

ReosDuration ReosRainfallIntensityDuration::getDureeMin() const
{
    if (pasTemps.size()>0)
         return pasTemps.at(0);
    else
        return ReosDuration();
}

ReosDuration ReosRainfallIntensityDuration::getDureeMax() const
{
    if (pasTemps.size()>0)
             return pasTemps.at(pasTemps.size()-1);
        else
            return ReosDuration();
}



unsigned int ReosRainfallIntensityDuration::getIndex(ReosDuration &duree) const
{
    if (duree<pasTemps.at(0))
        return 0;

    unsigned int i=0;
    bool trouve=false;
    while ((!trouve)&&((i+1)<pasTemps.size()))
    {
        trouve=(duree>=pasTemps.at(i))&&(duree<=pasTemps.at(i+1));
        if (!trouve)
            ++i;
    }

    if(trouve)
        return i;
    else
        return i-1;


}


bool ReosRainfallIntensityDuration::setCoef(unsigned int i,double a,double b)
{
    if (i<coefficient.size())
        coefficient[i]=std::vector<double>({a,b});
    return true;
}

bool ReosRainfallIntensityDuration::setCoef(unsigned int i, unsigned int j, double v)
{
    if ((i<coefficient.size())&&(j<2))
        coefficient[i][j]=v;
    return true;
}

void ReosRainfallIntensityDuration::insertDuree(ReosDuration duree)
{
    if (pasTemps.empty())
    {
        pasTemps.push_back(duree);
        return;
    }

    if (duree<pasTemps.at(0))
    {
        pasTemps.insert(pasTemps.begin(),duree);
        coefficient.insert(coefficient.begin(),std::vector<double>(2,0));
        return;
    }

    if (duree>pasTemps.back())
    {
        pasTemps.push_back(duree);
        coefficient.push_back(std::vector<double>(2,0));
        return;
    }



    std::vector<ReosDuration>::iterator it=pasTemps.begin();
    std::vector<std::vector<double>>::iterator itCoef=coefficient.begin();
    bool trouve=false;
    while ((it!=pasTemps.end())&&(!trouve))
    {
        trouve=duree<(*it);
        if (!trouve)
        {
            ++it;
            if (it!=pasTemps.end())
                ++itCoef;
        }
    }

    if (trouve)
    {
        pasTemps.insert(it,duree);
        (*(itCoef-1))=std::vector<double>(2,0);
        coefficient.insert(itCoef,std::vector<double>(2,0));
    }

}

void ReosRainfallIntensityDuration::removeDuree(unsigned int i)
{
    if (i>=pasTemps.size())
        return;

    pasTemps.erase(pasTemps.begin()+i);

    if (i==0)
    {
        coefficient.erase(coefficient.begin());
        return;
    }

    if (i==coefficient.size())
    {
        coefficient.erase(coefficient.end()-1);

        return;
    }

    coefficient.erase(coefficient.begin()+(i-1));
    coefficient[i-1]=std::vector<double>(2,0);

}

void ReosRainfallIntensityDuration::clear()
{
    pasTemps.clear();
    coefficient.clear();
}

void ReosRainfallIntensityDuration::setReturnPeriod(const ReosDuration &retour) {mReturnPeriod=retour;}

void ReosRainfallIntensityDuration::setUnitTime(const ReosRainfallIntensityDuration::UnitTime &unite) {mUnitTime=unite;}

ReosRainfallIntensityDuration &ReosRainfallIntensityDuration::operator<<(ReosDuration duree)
{
    insertDuree(duree);
    return (*this);
}


ReosDuration ReosRainfallIntensityDuration::returnPeriod() const {return mReturnPeriod;}

ReosRainfallIntensityDuration::UnitTime ReosRainfallIntensityDuration::untiTime() const {return mUnitTime;}


ReosRainfallIntensityDuration::ReosRainfallIntensityDuration(HdmReturnPeriod periode, HlgIntensiteDuree::UniteTemps un):
    mReturnPeriod(periode),uniteTemps(un)    {}

ReosRainfallIntensityDurationReosRainfallIntensityDuration::HlgIntensiteDuree(const HlgIntensiteDuree *other):
    mReturnPeriod(other->mReturnPeriod),
    uniteTemps(other->uniteTemps),
    coefficient(other->coefficient),
    pasTemps(other->pasTemps) {}

ReosRainfallIntensityDurationReosRainfallIntensityDuration::~HlgIntensiteDuree() {}

double ReosRainfallIntensityDuration::getHauteur(ReosDuration duree) const
{
    double intensite=getIntensite(duree);

    if (intensite<0)
        return intensite;

    double hauteur;

    switch (uniteTemps) {
    case minute:
        hauteur=intensite*duree.getValueMinute();
        break;
    case heure:
        hauteur=intensite*duree.getValueHour();
        break;
    }
    return hauteur;
}


unsigned int ReosRainfallIntensityDuration::getNumberCoef() const {return static_cast<unsigned int>(coefficient.size());}



ReosRainfallIntensityDuration *decodeIDF(const QByteArray &byteArray)
{
    //******************************
    // "IDF"
    // type equation (qint8)
    // période de retour (encodage)
    // unite de temp (qint8)
    // nombre de couple de coefficient (qint32)
    // n x (a,b) (qreal)
    // nombre de pas de temps (qint32)
    // n x pas de temps (encodage)
    // 0 (qint8)

    QDataStream stream(byteArray);
    HlgIntensiteDuree *retour=nullptr;

    QString dataType;
    stream>>dataType;
    if (dataType!="IDF")
        return retour;

    qint8 typeEquation;
    stream>>typeEquation;

    QByteArray mReturnPeriod;
    stream>>mReturnPeriod;

    qint8 uniteTemps;
    stream>>uniteTemps;


    switch (typeEquation) {
    case 0:
        retour=new HlgCoefMontana(decodemReturnPeriod(mReturnPeriod),static_cast<HlgIntensiteDuree::UniteTemps>(uniteTemps));
        break;
    default:
        retour=nullptr;
        break;
    }

    if (!retour)
        return retour;

    std::vector<std::vector<double>> coefficient;
    qint32 nbCoef;

    stream>>nbCoef;

    for (qint32 i=0;i<nbCoef;++i)
    {
        qreal a,b;
        stream>>a;
        stream>>b;

        std::vector<double> coef={a,b};

        coefficient.push_back(coef);
    }

    qint32 nbpdt;
    std::vector<ReosDuration> pasTemps;

    stream>>nbpdt;
    for (qint32 i=0;i<nbpdt;++i)
    {
        QByteArray pdt;
        stream>>pdt;
        pasTemps.push_back(decodeDuration(pdt));
    }

    retour->setCoefficients(coefficient);
    retour->setPasTemps(pasTemps);

    return retour;

}

void ReosRainfallIntensityDuration::setA(unsigned int i, double a)
{
    if (i<coefficient.size())
        coefficient[i][0]=a;
}

void ReosRainfallIntensityDuration::setB(unsigned int i, double b)
{
    if (i<coefficient.size())
        coefficient[i][1]=b;
}

void ReosRainfallIntensityDuration::setCoefficients(const std::vector<std::vector<double> > &coef) {coefficient=coef;}

void ReosRainfallIntensityDuration::setPasTemps(const std::vector<ReosDuration> &pdt) {pasTemps=pdt;}
