/***************************************************************************
                      hlgidf.h
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

#ifndef HlgIntensiteDuree_H
#define HlgIntensiteDuree_H

#include <string>
#include <vector>

#include "HdPeriodeRetour.h"
#include "../quantity/reosduration.h"
#include "../../cmn/Fichier/hddatastream.h"
#include "../../ReosProject/Reos/reosencodedelement.h"

class ReosRainfallIntensityDuration
{
public:
    enum UnitTime{minute,hour};
    enum EquationType {montana,talbot};

    ReosRainfallIntensityDuration(ReosDuration periode=ReosDuration(10,ReosDuration::year),UniteTemps un=hour);

    ReosRainfallIntensityDuration(const ReosIntensityDuration* other);


    virtual ReosRainfallIntensityDuration* clone() const =0;

    virtual ~ReosRainfallIntensityDuration();

    virtual double intensity(const ReosDuration &time) const =0;
    virtual EquationType type() const =0;
    virtual double getHauteur(ReosDuration temps) const;

    void setReturnPeriod(const ReosDuration &returnPeriod);
    ReosDuration returnPeriod() const;

    UnitTime unitTime() const;
    void setUnitTime(const UnitTime &unite)

    unsigned int getNumberCoef() const;
    double getCoefA(unsigned int i) const;
    double getCoefB(unsigned int i) const;
    std::vector<ReosDuration> getIntervalle(unsigned int i) const;
    ReosDuration getDureeMin() const;
    ReosDuration getDureeMax() const;
    HlgIntensiteDuree& operator<<(ReosDuration duree);

    void insertDuree(ReosDuration duree);
    void removeDuree(unsigned int i);
    void clear();
;
    bool setCoef(unsigned int i, double a, double b);
    bool setCoef(unsigned int i, unsigned int j,double v);
    void setA(unsigned int i, double a);
    void setB(unsigned int i, double b);
    void setCoefficients(const std::vector<std::vector<double>> &coef);
    void setPasTemps(const std::vector<ReosDuration> &pdt);

    QByteArray encodage() const;

    QByteArray encode() const;

protected:

    unsigned int getIndex(ReosDuration &duree) const;
    std::vector<std::vector<double>> coefficient;
    std::vector<ReosDuration> pasTemps;

private:

    ReosDuration mReturnPeriod;
    UnitTime mUnitTime;


};

HlgIntensiteDuree* decodeIDF(const QByteArray &byteArray);






#endif // HlgIntensiteDuree_H
