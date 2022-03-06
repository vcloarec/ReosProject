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
#include "reosdockwidget.h"

namespace Ui
{
  class ReosHydraulicNetworkWidget;
}

class ReosMapToolDrawPoint;
class ReosHydraulicElementPropertiesActionWidget;
class ReosMeteorologicModelsCollection;
class ReosWatershedModule;
class ReosGeometryStructure;

class ReosHydraulicElementWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosHydraulicElementWidget( QWidget *parent = nullptr );

    virtual void setCurrentCalculationContext( const ReosCalculationContext & ) {}

  signals:
    void stackedPageWidgetOpened( ReosStackedPageWidget *widget );
};


class ReosHydraulicNetworkWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network,
                                         ReosWatershedModule *watershedModule,
                                         const ReosGuiContext &context );
    ~ReosHydraulicNetworkWidget();

  private slots:
    void onElementAdded( ReosHydraulicNetworkElement *elem );
    void onElementRemoved( ReosHydraulicNetworkElement *elem );
    void onElementChanged( ReosHydraulicNetworkElement *elem );

    void onDrawHydrographRoutingFinish();
    void onElementSelected( ReosMapItem *item );
    void onSelectedElementRemoved();

    void onModuleReset();

    void onClosed();
    void onOpened();

  private:
    Ui::ReosHydraulicNetworkWidget *ui;
    ReosHydraulicNetwork *mHydraulicNetwork = nullptr;
    ReosMap *mMap = nullptr;
    ReosHydraulicNetworkElement *mCurrentSelectedElement = nullptr;

    typedef std::shared_ptr<ReosMapItem> NetworkItem ;

    QMap<ReosHydraulicNetworkElement *, NetworkItem> mMapItems;
    std::unique_ptr<ReosMapItem> mExtraItemSelection;

    ReosHydraulicNetworkMapItemFactory mMapItemFactory;

    QAction *mActionSelectNetworkElement = nullptr;
    ReosMapToolSelectMapItem *mMapToolSelectNetworkElement = nullptr;

    QAction *mActionAddHydrographJunction = nullptr;
    ReosMapToolDrawPoint *mMapToolAddHydrographJunction = nullptr;

    QAction *mActionAddHydrographRouting = nullptr;
    ReosMapToolDrawHydrographRouting *mMapToolAddHydrographRouting = nullptr;

    QAction *mActionHydraulicNetworkProperties = nullptr;
    ReosHydraulicElementPropertiesActionWidget *mElementPropertiesWidget = nullptr;

    QAction *mActionMoveHydrographJunction = nullptr;
    ReosMapToolMoveHydraulicNetworkElement *mMapToolMoveHydrographJunction = nullptr;

    QAction *mActionNewStructure2D = nullptr;
    ReosMapToolNewStructure2D *mMapToolNewStructure2D = nullptr;

    QAction *mActionRemoveElement = nullptr;

    void setMapItemVisible( bool visible );


    QList<ReosGeometryStructure *> mGeometryStructures;
    void addGeometryStructure( ReosHydraulicNetworkElement *elem );
    void removeGeometryStructure( ReosHydraulicNetworkElement *elem );

    ReosGuiContext createContext();
};

class REOSGUI_EXPORT ReosHydraulicNetworkDockWidget: public ReosDockWidget
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkDockWidget( ReosHydraulicNetwork *network,
                                    ReosWatershedModule *watershedModule,
                                    const ReosGuiContext &context );
};


#endif // REOSHYDRAULICNETWORKWIDGET_H
