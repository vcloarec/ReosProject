/***************************************************************************
  reoshydraulicnetworkmapitemfactory.h - ReosHydraulicNetworkMapItemFactory

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
#ifndef REOSHYDRAULICNETWORKMAPITEMFACTORY_H
#define REOSHYDRAULICNETWORKMAPITEMFACTORY_H

#include <QHash>
#include "reosmap.h"
#include "reosmapitem.h"
#include "reoshydraulicnetwork.h"

typedef ReosMapItem *( *ReosHydraulicNetworkMapItemCreationFunction )( ReosHydraulicNetworkElement *, ReosMap * );
typedef void ( *ReosHydraulicNetworkMapItemUpdateFunction )( ReosHydraulicNetworkElement *, ReosMapItem * );

class ReosHydraulicNetworkMapItemFactory
{
  public:
    ReosHydraulicNetworkMapItemFactory();

    ReosMapItem *createMapItem( ReosHydraulicNetworkElement *element, ReosMap *map );
    void updateMapItem( ReosHydraulicNetworkElement *element, ReosMapItem *item );

    void addCreationFunction( QString type, ReosHydraulicNetworkMapItemCreationFunction function );
    void addUpdateFunction( QString type, ReosHydraulicNetworkMapItemUpdateFunction function );

  private:
    QHash<QString, ReosHydraulicNetworkMapItemCreationFunction> mCreationFunctions;
    QHash<QString, ReosHydraulicNetworkMapItemUpdateFunction> mUpdateFunctions;
};

#endif // REOSHYDRAULICNETWORKMAPITEMFACTORY_H
