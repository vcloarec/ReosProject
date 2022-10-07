/***************************************************************************
  reoshecrassimulation.h - ReosHecRasSimulation

 ---------------------
 begin                : 06.10.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHECRASSIMULATION_H
#define REOSHECRASSIMULATION_H

#include "reoshydraulicsimulation.h"

class ReosHecRasSimulation
{
public:
    static QString staticKey() { return QStringLiteral("hecras"); }
};


class ReosTelemac2DSimulationEngineFactory : public ReosSimulationEngineFactory
{
public:

    virtual ReosHydraulicSimulation* createSimulation(QObject* parent) const override 
    { return nullptr; }
    virtual ReosHydraulicSimulation* createSimulation(const ReosEncodedElement& element, QObject* parent) const override 
    { return nullptr; }

    virtual QString key() const { return ReosHecRasSimulation::staticKey(); }
    QString displayName() const { return QObject::tr("HEC-RAS Simulation"); }

    void initializeSettings() override;
};


#endif REOSHECRASSIMULATION_H
