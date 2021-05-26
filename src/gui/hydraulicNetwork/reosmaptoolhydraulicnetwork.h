/***************************************************************************
  reosmaptoolhydraulicnetwork.h - %{Cpp:License:ClassName}

 ---------------------
 begin                : 24.5.2021
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
#ifndef REOSMAPTOOLHYDRAULICNETWORK_H
#define REOSMAPTOOLHYDRAULICNETWORK_H

#include "reosmaptool.h"
#include "reosmap.h"
#include "reoshydraulicnetwork.h"
#include "reoshydrographsource.h"

class ReosMapToolDrawHydraulicNetworkLink_p;

class ReosMapToolDrawHydraulicNetworkLink: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosMapToolDrawHydraulicNetworkLink( ReosMap *map );
    ~ReosMapToolDrawHydraulicNetworkLink();

    int itemsCount() const;

    QList<ReosMapItem *> linkedItems() const;

  signals:
    void itemAdded( ReosMapItem *item );
    void finished();

  private slots:
    void onItemSuggested( ReosMapItem_p *itemPrivate );

  protected:
    //! This method can be reimplemented in derived class to select items that can be linked or not. Default implementation returns true.
    virtual bool acceptItem( ReosMapItem * ) {return true;}

    //! This method can be reimplemented in derived class to test if the drawing is finished. Default implementation returns false.
    virtual bool isFinished() const {return false;}

  private:
    ReosMapToolDrawHydraulicNetworkLink_p *d;
    ReosMapTool_p *tool_p() const;
};

class ReosMapToolDrawHydrographRouting: public ReosMapToolDrawHydraulicNetworkLink
{
    Q_OBJECT
  public:
    ReosMapToolDrawHydrographRouting( ReosHydraulicNetwork *network, ReosMap *map ):
      ReosMapToolDrawHydraulicNetworkLink( map ),
      mNetwork( network )
    {
    }

  protected:
    bool acceptItem( ReosMapItem *item ) override
    {
      if ( !item )
        return false;

      if ( itemsCount() == 0 && item->description().contains( ReosHydrographSource::hydrographSourceType() ) )
      {
        ReosHydrographSource *source = qobject_cast<ReosHydrographSource *>( mNetwork->getElement( item->description() ) );
        if ( !source || source->outputHydrographTransfer() )
          return false;

        return true;
      }
      if ( itemsCount() == 1 && item->description().contains( ReosHydrographJunction::hydrographJunctionType() ) )
        return true;

      return false;
    }

    bool isFinished() const override
    {
      return itemsCount() == 2;
    }

  private:
    ReosHydraulicNetwork *mNetwork;
};


#endif // REOSMAPTOOLHYDRAULICNETWORK_H
