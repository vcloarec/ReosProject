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

#include <QPolygonF>

#include "reosdataobject.h"

class ReosParameterDouble;
class ReosHydraulicScheme;

class ReosTelemac2DInitialCondition: public ReosDataObject
{
  public:
    enum class Type
    {
      FromOtherSimulation,
      ConstantLevelNoVelocity,
      Interpolation,
      LastTimeStep
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
    explicit ReosTelemac2DInitialConditionFromSimulation( const ReosEncodedElement &element, QObject *parent = nullptr );

    Type initialConditionType() const override {return Type::FromOtherSimulation;}
    ReosEncodedElement encode() const override;
    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

    QString otherSchemeId() const;
    void setOtherSchemeId( const QString &otherSchemeId );

    int timeStepIndex() const;
    void setTimeStepIndex( int timeStepIndex );

    bool useLastTimeStep() const;
    void setUseLastTimeStep( bool useLastTimeStep );

  private:
    QString mOtherSchemeId;
    int mTimeStepIndex = -1;
    bool mUseLastTimeStep = false;
};

class ReosTelemac2DInitialConditionFromInterpolation: public ReosTelemac2DInitialCondition
{
    Q_OBJECT
  public:
    ReosTelemac2DInitialConditionFromInterpolation( QObject *parent = nullptr );
    ReosTelemac2DInitialConditionFromInterpolation( const ReosEncodedElement &element, QObject *parent = nullptr );

    Type initialConditionType() const override {return Type::Interpolation;}
    ReosEncodedElement encode() const override;
    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;

    ReosParameterDouble *firstValue() const;
    ReosParameterDouble *secondValue() const;

    void setLine( const QPolygonF &line, const QString &crs );
    QPolygonF line() const;
    QString crs() const;

  private:
    ReosParameterDouble *mFirstValue = nullptr;
    ReosParameterDouble *mSecondValue = nullptr;
    QPolygonF mLine;
    QString mCrs;
};

class ReosTelemac2DInitialConditionUseLastTimeStep: public ReosTelemac2DInitialCondition
{
    Q_OBJECT
  public:
    ReosTelemac2DInitialConditionUseLastTimeStep( QObject *parent = nullptr );
    ReosTelemac2DInitialConditionUseLastTimeStep( const ReosEncodedElement &element, QObject *parent = nullptr );

    Type initialConditionType() const override {return Type::LastTimeStep;}
    ReosEncodedElement encode() const override;

    void saveConfiguration( ReosHydraulicScheme *scheme ) const override;
    void restoreConfiguration( ReosHydraulicScheme *scheme ) override;
};


#endif // REOSTELEMAC2DINITIALCONDITION_H
