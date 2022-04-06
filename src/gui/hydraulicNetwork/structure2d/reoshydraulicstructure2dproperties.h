/***************************************************************************
  reoshydraulicstructrure2dproperties.h - ReosHydraulicStructrure2DProperties

 ---------------------
 begin                : 10.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSHYDRAULICSTRUCTURE2DPROPERTIES_H
#define REOSHYDRAULICSTRUCTURE2DPROPERTIES_H

#include <QWidget>
#include <QPointer>

#include "reoshydraulicelementpropertieswidget.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicnetworkwidget.h"
#include "reoscalculationcontext.h"


class Reos3dView;

namespace Ui
{
  class ReosHydraulicStructure2DProperties;
}

class ReosHydraulicStructure2DProperties : public ReosHydraulicElementWidget
{
    Q_OBJECT

  public:
    explicit ReosHydraulicStructure2DProperties( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context = ReosGuiContext() );
    ~ReosHydraulicStructure2DProperties();

    void setCurrentCalculationContext( const ReosCalculationContext &context ) override;

  private slots:
    void requestMapRefresh();
    void onLaunchCalculation();
    void updateDatasetMenu();

  private:
    Ui::ReosHydraulicStructure2DProperties *ui;
    ReosHydraulicStructure2D *mStructure2D = nullptr;
    QPointer<ReosMap> mMap = nullptr;
    QAction *mActionEditStructure = nullptr;
    QAction *mActionRunSimulation = nullptr;
    QAction *mAction3DView = nullptr;
    QMenu *mScalarDatasetMenu = nullptr;
    QActionGroup *mScalarDatasetActions = nullptr;
    QPointer<Reos3dView> mView3D;
    ReosGuiContext mGuiContext;
    ReosCalculationContext mCalculationContext;
};


class ReosHydraulicStructure2DPropertiesWidgetFactory : public ReosHydraulicElementWidgetFactory
{
  public:
    ReosHydraulicStructure2DPropertiesWidgetFactory( QObject *parent = nullptr ): ReosHydraulicElementWidgetFactory( parent ) {}
    virtual ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString elementType() {return ReosHydraulicStructure2D::staticType();}
};


#endif // REOSHYDRAULICSTRUCTURE2DPROPERTIES_H
