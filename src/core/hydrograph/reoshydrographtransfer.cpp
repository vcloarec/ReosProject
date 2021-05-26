/***************************************************************************
  reoshydrographtransfer.cpp - ReosHydrographTransfer

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
#include "reoshydrographtransfer.h"
#include "reoshydrograph.h"

ReosHydrographRouting::ReosHydrographRouting( ReosHydraulicNetwork *parent ):
  ReosHydraulicLink( parent )
{}

ReosHydrographRouting::ReosHydrographRouting( ReosHydrographSource *hydrographSource, ReosHydrographNode *destination, ReosHydraulicNetwork *parent ):
  ReosHydraulicLink( parent )
{
  attachOnSide1( hydrographSource );
  attachOnSide2( destination );
}

void ReosHydrographRouting::setInputHydrographSource( ReosHydrographSource *hydrographSource )
{
  attachOnSide1( hydrographSource );
}

ReosHydrographSource *ReosHydrographRouting::inputHydrographSource() const
{
  if ( mNode_1.isNull() )
    return nullptr;
  else
    return qobject_cast<ReosHydrographSource *>( mNode_1 );
}

void ReosHydrographRouting::setHydrographDestination( ReosHydrographNode *destination )
{
  attachOnSide2( destination );
}

