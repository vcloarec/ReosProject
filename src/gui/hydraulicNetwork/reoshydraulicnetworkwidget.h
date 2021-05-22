/***************************************************************************
  reoshydraulicnetworkwidget.h - ReosHydraulicNetworkWidget

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
#ifndef REOSHYDRAULICNETWORKWIDGET_H
#define REOSHYDRAULICNETWORKWIDGET_H

#include <QWidget>

#include "reosmap.h"
#include "reosmapitem.h"
#include "reoshydraulicnetwork.h"
#include "reoshydraulicnetworkmapitemfactory.h"

class ReosMapToolDrawPoint;


namespace Ui
{
  class ReosHydraulicNetworkWidget;
}

class ReosHydraulicNetworkWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosMap *map, QWidget *parent = nullptr );
    ~ReosHydraulicNetworkWidget();

  private slots:
    void onElementAdded( ReosHydraulicNetworkElement *elem );
    void onElementChanged( ReosHydraulicNetworkElement *elem );

  private:
    Ui::ReosHydraulicNetworkWidget *ui;
    ReosHydraulicNetwork *mHydraulicNetwork = nullptr;
    ReosMap *mMap = nullptr;

    typedef std::shared_ptr<ReosMapItem> NetworkItem ;

    QMap<ReosHydraulicNetworkElement *, NetworkItem> mMapItems;
    ReosHydraulicNetworkMapItemFactory mMapItemFactory;

    QAction *mActionAddHydrographJunction = nullptr;
    ReosMapToolDrawPoint *mMapToolAddHydrographJunction = nullptr;

};

#endif // REOSHYDRAULICNETWORKWIDGET_H
