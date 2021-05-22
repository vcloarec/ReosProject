/***************************************************************************
  reoshydraulicnetworkmapitemfactory.cpp - ReosHydraulicNetworkMapItemFactory

 ---------------------
 begin                : 20.5.2021
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
#include "reoshydraulicnetworkmapitemfactory.h"

ReosHydraulicNetworkMapItemFactory::ReosHydraulicNetworkMapItemFactory()
{

}

ReosMapItem *ReosHydraulicNetworkMapItemFactory::createMapItem( ReosHydraulicNetworkElement *element, ReosMap *map )
{
  auto it = mCreationFunctions.find( element->type() );
  if ( it != mCreationFunctions.constEnd() )
    return it.value()( element, map );

  return nullptr;
}

void ReosHydraulicNetworkMapItemFactory::updateMapItem( ReosHydraulicNetworkElement *element, ReosMapItem *item )
{
  auto it = mUpdateFunctions.find( element->type() );
  if ( it != mUpdateFunctions.constEnd() )
    return it.value()( element, item );
}

void ReosHydraulicNetworkMapItemFactory::addCreationFunction( QString type, ReosHydraulicNetworkMapItemCreationFunction function )
{
  mCreationFunctions.insert( type, function );
}

void ReosHydraulicNetworkMapItemFactory::addUpdateFunction( QString type, ReosHydraulicNetworkMapItemUpdateFunction function )
{
  mUpdateFunctions.insert( type, function );
}
