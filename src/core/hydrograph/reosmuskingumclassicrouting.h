/***************************************************************************
  reosmuskingumclassicrouting.h - ReosMuskingumClassicRouting

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
#ifndef REOSMUSKINGUMCLASSICROUTING_H
#define REOSMUSKINGUMCLASSICROUTING_H

#include "reoshydrographtransfer.h"
#include "reosparameter.h"

class ReosMuskingumClassicRouting : public ReosHydrographRoutingMethod
{
    Q_OBJECT
  public:
    ReosMuskingumClassicRouting( ReosHydrographRouting *parent = nullptr );

    void calculateOutputHydrograph( ReosHydrograph *inputHydrograph, ReosHydrograph *outputHydrograph, const ReosCalculationContext &context ) override;

    QString type() const override {return typeString();}
    static QString typeString() {return ReosHydrographRoutingMethod::typeString() + QString( ':' ) + QStringLiteral( "muskingumClassic" );}

    ReosParameterDuration *kParameter() const;
    ReosParameterDouble *xParameter() const;

  private:
    ReosParameterDuration *mKParameter = nullptr;
    ReosParameterDouble *mXParameter = nullptr;
};

class ReosMuskingumClassicRoutingFactory : public ReosHydrographRoutingMethodFactory
{
  public:
    ReosHydrographRoutingMethod *createRoutingMethod( ReosHydrographRouting *routingLink ) const override    {return new ReosMuskingumClassicRouting( routingLink );}
    virtual QString type() const override    {return ReosMuskingumClassicRouting::typeString();}
};


#endif // REOSMUSKINGUMCLASSICROUTING_H
