/***************************************************************************
  reoscoremodule.cpp - ReosCoreModule

 ---------------------
 begin                : 11.3.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoscoremodule.h"

#include "reoswatershedmodule.h"
#include "reosgisengine.h"
#include "reoshydraulicnetwork.h"
#include "reosstyleregistery.h"
#include "reosrainfallregistery.h"
#include "reosrunoffmodel.h"


ReosCoreModule::ReosCoreModule( QObject *parent )
  : ReosModule( "core-module", parent )
{
  new ReosGisEngine( this );
  new ReosWatershedModule( this, gisEngine() );
  new ReosHydraulicNetwork( this, gisEngine(), watershedModule() );

  ReosStyleRegistery::instantiate( this );
  ReosRainfallRegistery::instantiate( this );
  ReosRunoffModelRegistery::instantiate( this );
}

ReosGisEngine *ReosCoreModule::gisEngine() const
{
  return module<ReosGisEngine *>();
}

ReosWatershedModule *ReosCoreModule::watershedModule() const
{
  return module<ReosWatershedModule *>();
}

ReosHydraulicNetwork *ReosCoreModule::hydraulicNetwork() const
{
  return module<ReosHydraulicNetwork *>();
}
