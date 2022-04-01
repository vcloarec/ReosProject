/***************************************************************************
  reossimulationinitialcondition.h - ReosSimulationInitialCondition

 ---------------------
 begin                : 29.3.2022
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
#ifndef REOSSIMULATIONINITIALCONDITION_H
#define REOSSIMULATIONINITIALCONDITION_H

#include "reosdataobject.h"

class ReosParameterDouble;
class ReosEncodedElement;

class ReosSimulationInitialConditions : public ReosDataObject
{
  public:
    enum class Type
    {
      FromFile,
      ConstantLevelNoVelocity,
      HightLevelEmptying
    };

    ReosSimulationInitialConditions( QObject *parent = nullptr );
    ReosSimulationInitialConditions( const ReosEncodedElement &element, QObject *parent = nullptr );

    ReosEncodedElement encode() const;

    ReosParameterDouble *initialWaterLevel() const;
  private:

    ReosParameterDouble *mInitialWaterLevel = nullptr;


};

#endif // REOSSIMULATIONINITIALCONDITION_H
