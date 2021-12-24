/***************************************************************************
  reoshydrographpropertieswidget.h - ReosHydrographPropertiesWidget

 ---------------------
 begin                : 28.5.2021
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
#ifndef REOSHYDROGRAPHPROPERTIESWIDGET_H
#define REOSHYDROGRAPHPROPERTIESWIDGET_H

#include <QWidget>
#include "reoshydraulicelementpropertieswidget.h"
#include "reosformwidget.h"
#include "reoshydrographrouting.h"
#include "reoshydraulicnetworkwidget.h"

namespace Ui
{
  class ReosHydrographRoutingPropertiesWidget;
}

class ReosHydrographRoutingLink;
class ReosPlotTimeSerieVariableStep;
class ReosVariableTimeStepPlotListButton;

class ReosHydrographRoutingPropertiesWidget : public ReosHydraulicElementWidget
{
    Q_OBJECT

  public:
    explicit ReosHydrographRoutingPropertiesWidget( ReosHydrographRoutingLink *hydrographRouting, QWidget *parent = nullptr );
    ~ReosHydrographRoutingPropertiesWidget();

    virtual void setCurrentCalculationContext( const ReosCalculationContext &context );

  private slots:
    void onCurrentMethodChange();
    void updateInformation();
    void onMethodDescription();

  private:
    Ui::ReosHydrographRoutingPropertiesWidget *ui;
    QPointer<ReosHydrographRoutingLink> mRouting = nullptr;
    ReosCalculationContext calculationContext;
    QWidget *mRoutingWidget = nullptr;
    ReosHydrauylicNetworkElementCalculationControler *mProgressControler = nullptr;
    ReosVariableTimeStepPlotListButton *mHydrographPlotButton = nullptr;

    ReosPlotTimeSerieVariableStep *mInputHydrographCurve = nullptr;
    ReosPlotTimeSerieVariableStep *mOutputtHydrographCurve = nullptr;

    void populateHydrographs();
};


class ReosHydrographRoutingPropertiesWidgetFactory : public ReosHydraulicElementWidgetFactory
{
  public:
    ReosHydrographRoutingPropertiesWidgetFactory( QObject *parent = nullptr ): ReosHydraulicElementWidgetFactory( parent ) {}

    ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, QWidget *parent = nullptr );
    virtual QString elementType();
};

class ReosFormMuskingumClassicRoutingWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const;
};


#endif // REOSHYDROGRAPHPROPERTIESWIDGET_H
