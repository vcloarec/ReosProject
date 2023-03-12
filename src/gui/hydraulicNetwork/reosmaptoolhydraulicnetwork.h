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
class ReosMapToolMoveHydraulicNetworkNode_p;


class ReosMapToolHydraulicElement
{
  public:
    ReosMapToolHydraulicElement( ReosHydraulicNetwork *network );

  protected:
    ReosHydraulicNetwork *mNetwork = nullptr;
};

class ReosMapToolDrawHydraulicNetworkLink: public ReosMapTool, public ReosMapToolHydraulicElement
{
    Q_OBJECT
  public:
    ReosMapToolDrawHydraulicNetworkLink( ReosHydraulicNetwork *network, ReosMap *map );
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
    QPointer<ReosMapToolDrawHydraulicNetworkLink_p> d;
    ReosMapTool_p *tool_p() const;
};

class ReosMapToolDrawHydrographRouting: public ReosMapToolDrawHydraulicNetworkLink
{
    Q_OBJECT
  public:
    ReosMapToolDrawHydrographRouting( ReosHydraulicNetwork *network, ReosMap *map );

  protected:
    bool acceptItem( ReosMapItem *item ) override;
    bool isFinished() const override;

  private:
};

class ReosMapToolMoveHydraulicNetworkElement : public ReosMapTool, public ReosMapToolHydraulicElement
{
    Q_OBJECT
  public:
    ReosMapToolMoveHydraulicNetworkElement( ReosHydraulicNetwork *network, ReosMap *map );
    ~ReosMapToolMoveHydraulicNetworkElement();

  private:
    QPointer<ReosMapToolMoveHydraulicNetworkNode_p> d;
    ReosMapTool_p *tool_p() const;
};


class ReosMapToolNewStructure2D: public ReosMapToolDrawPolygon, public ReosMapToolHydraulicElement
{
    Q_OBJECT
  public:
    ReosMapToolNewStructure2D( ReosHydraulicNetwork *network, ReosMap *map );

  private slots:
    void onDomainDrawn( const QPolygonF &polygon );
};


#endif // REOSMAPTOOLHYDRAULICNETWORK_H
