/***************************************************************************
  reoshydraulichydrographnodepropertieswidget.h - ReosHydraulicHydrographNodePropertiesWidget

 ---------------------
 begin                : 8.12.2021
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
#ifndef REOSHYDRAULICHYDROGRAPHJUNCTIONPROPERTIESWIDGET_H
#define REOSHYDRAULICHYDROGRAPHJUNCTIONPROPERTIESWIDGET_H

#include <QWidget>

#include "reoshydraulicelementpropertieswidget.h"
#include "reosformwidget.h"
#include "reoshydraulicnetworkwidget.h"

class ReosHydrographJunction;
class ReosPlotTimeSerieVariableStep;
class ReosVariableTimeStepPlotListButton;

namespace Ui
{
  class ReosHydraulicHydrographJunctionPropertiesWidget;
}

class ReosHydraulicHydrographJunctionPropertiesWidget : public ReosHydraulicElementWidget
{
    Q_OBJECT
  public:
    explicit ReosHydraulicHydrographJunctionPropertiesWidget( ReosHydrographJunction *junctionNode, QWidget *parent = nullptr );
    ~ReosHydraulicHydrographJunctionPropertiesWidget();

    void setCurrentCalculationContext( const ReosCalculationContext &calculationContext ) override;

  private slots:
    void populateHydrographs();
    void updateInformation();

  private:
    Ui::ReosHydraulicHydrographJunctionPropertiesWidget *ui;
    ReosHydrographJunction *mJunctionNode = nullptr;
    ReosPlotTimeSerieVariableStep *mOutputCurve = nullptr;
    ReosVariableTimeStepPlotListButton *mHydrographPlotButton = nullptr;
    ReosHydrauylicNetworkElementCalculationControler *mProgressControler = nullptr;
};


class ReosHydraulicHydrographNodePropertiesWidgetFactory : public ReosHydraulicElementWidgetFactory
{
  public:
    ReosHydraulicHydrographNodePropertiesWidgetFactory( QObject *parent = nullptr );

    ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, QWidget *parent = nullptr );
    virtual QString elementType();
};

class ReosFormJunctionNodeWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const;
};

class ReosFormJunctionNodeWidget: public ReosFormWidget
{
    Q_OBJECT
  public:
    ReosFormJunctionNodeWidget( ReosHydrographJunction *junction, QWidget *parent = nullptr );
};

class ReosFormWatershedNodeWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent );
    virtual QString datatype() const;
};

class ReosFormWatershedNodeWidget: public ReosFormJunctionNodeWidget
{
    Q_OBJECT
  public:
    ReosFormWatershedNodeWidget( ReosHydrographNodeWatershed *watershedNode, QWidget *parent = nullptr );

  private slots:
    void originChange();

  private:
    ReosHydrographNodeWatershed *mNode = nullptr;
    QComboBox *mOriginCombo = nullptr;
    QComboBox *mGaugedHydrographCombo = nullptr;
    QLabel *mGaugedLabel = nullptr;

    void updateGaugedHydrograph();
};


#endif // REOSHYDRAULICHYDROGRAPHJUNCTIONPROPERTIESWIDGET_H
