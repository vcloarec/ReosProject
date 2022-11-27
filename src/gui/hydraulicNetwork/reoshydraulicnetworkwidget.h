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
    virtual void setTime( const QDateTime & ) {}

  signals:
    void stackedPageWidgetOpened( ReosStackedPageWidget *widget );
    void askForShow();
};


class ReosHydraulicNetworkWidget : public QWidget
{
    Q_OBJECT
  public:
    explicit ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network,
                                         ReosWatershedModule *watershedModule,
                                         const ReosGuiContext &context );
    ~ReosHydraulicNetworkWidget();

    void closePropertiesWidget();

    ReosTimeWindow timeWindow() const;
    ReosDuration mapTimeStep() const;

  signals:
    void timeWindowChanged();
    void mapTimeStepChanged();

  private slots:
    void onElementAdded( ReosHydraulicNetworkElement *elem, bool select );
    void onElementRemoved( ReosHydraulicNetworkElement *elem );
    void onElementChanged( ReosHydraulicNetworkElement *elem );

    void onDrawHydrographRoutingFinish();
    void onElementSelected( ReosMapItem *item );
    void onSelectedElementRemoved();
    void onZoomToNetworkExtent();

    void onAddHydraulicScheme();
    void onRemoveHydraulicScheme();
    void onNetworkLoaded();

    void onModuleReset();

    void onClosed();
    void onOpened();

    void onMapCrsChanged();

    void updateSchemeInfo();

  private:
    Ui::ReosHydraulicNetworkWidget *ui;
    ReosGuiContext mGuiContext;
    ReosHydraulicNetwork *mHydraulicNetwork = nullptr;
    ReosMap *mMap = nullptr;
    ReosHydraulicNetworkElement *mCurrentSelectedElement = nullptr;
    ReosHydraulicScheme *mCurrentHydraulicScheme = nullptr;

    typedef std::shared_ptr<ReosMapItem> NetworkItem ;

    QHash<ReosHydraulicNetworkElement *, NetworkItem> mMapItems;
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
    QAction *mActionImportStructure2D = nullptr;

    QAction *mActionRemoveElement = nullptr;

    QAction *mActionZoomToNetworkExtent = nullptr;

    void unselectCurrentElement();
    void setMapItemVisible( bool visible );
    QList<ReosGeometryStructure *> mGeometryStructures;
    void addGeometryStructure( ReosHydraulicNetworkElement *elem );
    void removeGeometryStructure( ReosHydraulicNetworkElement *elem );

    void changeCurrentScheme( ReosHydraulicScheme *scheme );
};

class REOSGUI_EXPORT ReosHydraulicNetworkDockWidget: public ReosDockWidget
{
    Q_OBJECT
  public:
    ReosHydraulicNetworkDockWidget( ReosHydraulicNetwork *network,
                                    ReosWatershedModule *watershedModule,
                                    const ReosGuiContext &context );

    void closePropertieWidget();

    ReosHydraulicNetworkWidget *hydraulicNetworkWidget() const;

  private:
    ReosHydraulicNetworkWidget *mHydraulicNetworkWidget = nullptr;
};


#endif // REOSHYDRAULICNETWORKWIDGET_H
