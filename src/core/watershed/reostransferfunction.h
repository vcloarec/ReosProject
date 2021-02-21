/***************************************************************************
  reostransferfunction.h - ReosTransferFunction

 ---------------------
 begin                : 19.2.2021
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
#ifndef REOSTRANSFERFUNCTION_H
#define REOSTRANSFERFUNCTION_H

#include "reostimeserie.h"

class ReosRunoff;

class ReosHydrograph : public ReosTimeSerieVariableTimeStep
{
  public:
    ReosHydrograph( QObject *parent = nullptr ): ReosTimeSerieVariableTimeStep( parent ) {}

    QString type() const override {return QStringLiteral( "hydrograph" );}
};

class ReosTransferFunction
{
  public:
    ReosTransferFunction();

    ReosHydrograph *calculateHydrograph( ReosRunoff *runoff );

};

#endif // REOSTRANSFERFUNCTION_H
