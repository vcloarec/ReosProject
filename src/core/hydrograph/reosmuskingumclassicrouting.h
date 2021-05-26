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

class ReosMuskingumClassicRouting : public ReosHydrographRouting
{
  public:
    ReosMuskingumClassicRouting( ReosHydraulicNetwork *parent = nullptr );

    ReosHydrograph *outputHydrograph( const ReosCalculationContext &context ) override;

  private:
    ReosParameterDuration *mKParameter = nullptr;
    ReosParameterDouble *mXParameter = nullptr;

    mutable ReosHydrograph *mResultHydrograph = nullptr;

    void calculate( const ReosCalculationContext &context ) const;
};

#endif // REOSMUSKINGUMCLASSICROUTING_H
