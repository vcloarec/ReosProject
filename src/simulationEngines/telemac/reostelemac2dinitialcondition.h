/***************************************************************************
  reostelemac2dinitialcondition.h - ReosTelemac2DInitialCondition

 ---------------------
 begin                : 28.4.2022
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
#ifndef REOSTELEMAC2DINITIALCONDITION_H
#define REOSTELEMAC2DINITIALCONDITION_H

#include "reosdataobject.h"

class ReosParameterDouble;

class ReosTelemac2DInitialCondition: public ReosDataObject
{
  public:
    enum class Type
    {
      FromFile,
      ConstantLevelNoVelocity
    };

    ReosTelemac2DInitialCondition( QObject *parent = nullptr );
    ReosTelemac2DInitialCondition( const ReosEncodedElement &element, QObject *parent = nullptr );

    virtual Type initialConditionType() const = 0;

    virtual ReosEncodedElement encode() const = 0;

};


class ReosTelemac2DInitialConstantWaterLevel: public ReosTelemac2DInitialCondition
{
    Q_OBJECT
  public:
    ReosTelemac2DInitialConstantWaterLevel( QObject *parent = nullptr );
    ReosTelemac2DInitialConstantWaterLevel( const ReosEncodedElement &element, QObject *parent = nullptr );

    Type initialConditionType() const override {return Type::ConstantLevelNoVelocity;}

    ReosEncodedElement encode() const override;

    ReosParameterDouble *initialWaterLevel() const;

  private:

    ReosParameterDouble *mInitialWaterLevel = nullptr;
};

#endif // REOSTELEMAC2DINITIALCONDITION_H
