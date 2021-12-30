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
typedef void ( *ReosHydraulicNetworkMapItemSelectFunction )( ReosHydraulicNetworkElement *, ReosMapItem * );
typedef void ( *ReosHydraulicNetworkMapItemUnselectFunction )( ReosHydraulicNetworkElement *, ReosMapItem * );

typedef ReosMapItem *( *ReosMapExtraItemSelectedFunction )( ReosHydraulicNetworkElement *,  ReosMap * );

class ReosHydraulicNetworkMapItemFactory
{
  public:
    ReosHydraulicNetworkMapItemFactory();

    ReosMapItem *createMapItem( ReosHydraulicNetworkElement *element, ReosMap *map );
    void updateMapItem( ReosHydraulicNetworkElement *element, ReosMapItem *item );
    void selectItem( ReosHydraulicNetworkElement *element, ReosMapItem *item );
    void unselectItem( ReosHydraulicNetworkElement *element, ReosMapItem *item );

    ReosMapItem *createExtraItemSelected( ReosHydraulicNetworkElement *element, ReosMap *map );

  private:
    QHash<QString, ReosHydraulicNetworkMapItemCreationFunction> mCreationFunctions;
    QHash<QString, ReosHydraulicNetworkMapItemUpdateFunction> mUpdateFunctions;
    QHash<QString, ReosHydraulicNetworkMapItemSelectFunction> mSelectFunctions;
    QHash<QString, ReosHydraulicNetworkMapItemUnselectFunction> mUnselectFunctions;

    QHash<QString, ReosMapExtraItemSelectedFunction> mExtraItemSelectedFunctions;
};

#endif // REOSHYDRAULICNETWORKMAPITEMFACTORY_H
