/***************************************************************************
  reosmuskingumclassicroutine.h - ReosMuskingumClassicRoutine

 ---------------------
 begin                : 19.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSMUSKINGUMCLASSICROUTINE_H
#define REOSMUSKINGUMCLASSICROUTINE_H

#include "reoshydrographtransfer.h"
#include "reosparameter.h"

class ReosMuskingumClassicRoutine : public ReosHydrographRoutingMethod
{
    Q_OBJECT
  public:
    ReosMuskingumClassicRoutine( ReosHydrographRoutingLink *parent = nullptr );
    ReosMuskingumClassicRoutine( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *parent = nullptr );

    void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) override;
    ReosHydrographCalculation *calculationProcess( ReosHydrograph *inputHydrograph, const ReosCalculationContext &context ) override;

    QString type() const override {return staticType();}
    static QString staticType() {return ReosHydrographRoutingMethod::staticType() + QString( ':' ) + QStringLiteral( "muskingumClassic" );}

    ReosParameterDuration *kParameter() const;
    ReosParameterDouble *xParameter() const;

    ReosEncodedElement encode() const override;

    static void calculate( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosDuration &K, double x, ReosProcess *process = nullptr );

  private:
    class Calculation: public ReosHydrographCalculation
    {
      public:
        Calculation( ReosHydrograph *inputHydrograph, const ReosDuration &K, double X )
          : mK( K )
          , mX( X )
        {
          mInputHydrograph = std::make_unique<ReosHydrograph>();
          mInputHydrograph->copyFrom( inputHydrograph );
          mHydrograph = std::make_unique<ReosHydrograph>();
        }

        void start() override
        {
          calculate( mInputHydrograph.get(), mHydrograph.get(), mK, mX );
          mIsSuccessful = true;
        }
      private:
        std::unique_ptr<ReosHydrograph> mInputHydrograph;
        ReosDuration mK;
        double mX = 0;
    };

    ReosParameterDuration *mKParameter = nullptr;
    ReosParameterDouble *mXParameter = nullptr;
};

class ReosMuskingumClassicRoutineFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRoutingLink *routingLink ) const override;
    ReosHydrographRoutingMethod *createRoutingMethod( const ReosEncodedElement &encodedElement, ReosHydrographRoutingLink *routingLink ) const override;
    QString type() const override;
    QString displayName() const override {return QObject::tr( "Muskingum classic" );}
};


#endif // REOSMUSKINGUMCLASSICROUTINE_H
