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
#include "reosformwidget.h"
#include "reosmaptoolhydraulicnetwork.h"

namespace Ui
{
  class ReosHydraulicNetworkWidget;
}

class ReosMapToolDrawPoint;
class ReosHydraulicElementPropertiesWidget;
class ReosMeteorologicModelsCollection;
class ReosWatershedModule;

class ReosHydraulicElementWidget : public QWidget
{
  public:
    ReosHydraulicElementWidget( QWidget *parent = nullptr ):  QWidget( parent )
    {}

    virtual void setCurrentCalculationContext( const ReosCalculationContext & ) {}
};


class ReosHydraulicNetworkWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network,
                                         ReosMap *map,
                                         ReosWatershedModule *watershedModule,
                                         QWidget *parent = nullptr );
    ~ReosHydraulicNetworkWidget();

    // Temporary methods used for implementation
    //*******
    ReosCalculationContext currentContext() const;
    void setMeteoModelCollection( ReosMeteorologicModelsCollection *meteoCollection );
    ReosMeteorologicModelsCollection *meteoModelCollection = nullptr;
    //***

  private slots:
    void onElementAdded( ReosHydraulicNetworkElement *elem );
    void onElementRemoved( ReosHydraulicNetworkElement *elem );
    void onElementChanged( ReosHydraulicNetworkElement *elem );

    void onDrawHydrographRoutingFinish();
    void onElementSelected( ReosMapItem *item );
    void onSelectedElementRemoved();

  private:
    Ui::ReosHydraulicNetworkWidget *ui;
    ReosHydraulicNetwork *mHydraulicNetwork = nullptr;
    ReosMap *mMap = nullptr;
    ReosHydraulicNetworkElement *mCurrentSelectedElement = nullptr;

    typedef std::shared_ptr<ReosMapItem> NetworkItem ;

    QMap<ReosHydraulicNetworkElement *, NetworkItem> mMapItems;
    ReosHydraulicNetworkMapItemFactory mMapItemFactory;

    QAction *mActionSelectNetworkElement = nullptr;
    ReosMapToolSelectMapItem *mMapToolSelectNetworkElement = nullptr;

    QAction *mActionAddHydrographJunction = nullptr;
    ReosMapToolDrawPoint *mMapToolAddHydrographJunction = nullptr;

    QAction *mActionAddHydrographRouting = nullptr;
    ReosMapToolDrawHydrographRouting *mMapToolAddHydrographRouting = nullptr;

    QAction *mActionHydraulicNetworkProperties = nullptr;
    ReosHydraulicElementPropertiesWidget *mElementPropertiesWidget = nullptr;

    QAction *mActionMoveHydrographJunction = nullptr;
    ReosMapToolMoveHydraulicNetworkElement *mMapToolMoveHydrographJunction = nullptr;

    QAction *mActionRemoveElement = nullptr;
};


#endif // REOSHYDRAULICNETWORKWIDGET_H