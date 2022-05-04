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
class ReosHydraulicScheme;

class ReosTelemac2DInitialCondition: public ReosDataObject
{
  public:
    enum class Type
    {
      FromOtherSimulation,
      ConstantLevelNoVelocity
    };

    ReosTelemac2DInitialCondition( QObject *parent = nullptr );
    ReosTelemac2DInitialCondition( const ReosEncodedElement &element, QObject *parent = nullptr );

    virtual Type initialConditionType() const = 0;
    virtual ReosEncodedElement encode() const = 0;

    virtual void saveConfiguration( ReosHydraulicScheme *scheme ) const = 0;
    virtual void restoreConfiguration( ReosHydraulicScheme *scheme ) = 0;

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

    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

  private:
    ReosParameterDouble *mInitialWaterLevel = nullptr;
};

class ReosTelemac2DInitialConditionFromSimulation: public ReosTelemac2DInitialCondition
{
    Q_OBJECT
  public:
    ReosTelemac2DInitialConditionFromSimulation( QObject *parent = nullptr );
    ReosTelemac2DInitialConditionFromSimulation( const ReosEncodedElement &element, QObject *parent = nullptr );

    Type initialConditionType() const override {return Type::FromOtherSimulation;}
    ReosEncodedElement encode() const override;
    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

    QString otherSchemeId() const;
    void setOtherSchemeId( const QString &otherSchemeId );

    int timeStepIndex() const;
    void setTimeStepIndex( int timeStepIndex );

  private:
    QString mOtherSchemeId;
    int mTimeStepIndex = -1;
};


#endif // REOSTELEMAC2DINITIALCONDITION_H
