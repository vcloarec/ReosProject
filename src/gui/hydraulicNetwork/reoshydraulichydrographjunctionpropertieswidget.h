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
#include "reosguicontext.h"

class ReosHydrographJunction;
class ReosPlotTimeSerieVariableStep;
class ReosVariableTimeStepPlotListButton;
class ReosHydraulicStructureBoundaryCondition;

namespace Ui
{
  class ReosHydraulicHydrographJunctionPropertiesWidget;
}

class ReosHydraulicHydrographJunctionPropertiesWidget : public ReosHydraulicElementWidget
{
    Q_OBJECT
  public:
    explicit ReosHydraulicHydrographJunctionPropertiesWidget( ReosHydrographJunction *junctionNode, const ReosGuiContext &context = ReosGuiContext() );
    ~ReosHydraulicHydrographJunctionPropertiesWidget();

    void setCurrentCalculationContext( const ReosCalculationContext &calculationContext ) override;

  private slots:
    void populateHydrographs();
    void updateInformation();
    void updateGaugedHydrograph();

  private:
    Ui::ReosHydraulicHydrographJunctionPropertiesWidget *ui;
    ReosHydrographJunction *mJunctionNode = nullptr;
    ReosPlotTimeSerieVariableStep *mOutputCurve = nullptr;
    ReosVariableTimeStepPlotListButton *mHydrographPlotButton = nullptr;
    ReosVariableTimeStepPlotListButton *mGaugedHydrographPlotButton = nullptr;
    ReosHydrauylicNetworkElementCalculationControler *mProgressControler = nullptr;
};

class ReosHydraulicHydrographNodePropertiesWidgetFactory : public ReosHydraulicElementWidgetFactory
{
  public:
    ReosHydraulicHydrographNodePropertiesWidgetFactory( QObject *parent = nullptr );

    ReosHydraulicElementWidget *createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString elementType();
};

class ReosFormBaseJunctionNodeWidget: public ReosFormWidget
{
    Q_OBJECT
  public:
    ReosFormBaseJunctionNodeWidget( ReosHydrographJunction *junction, const ReosGuiContext &context );

  protected slots:
    void updateGaugedHydrograph();

  protected:
    QComboBox *mGaugedHydrographCombo = nullptr;
    QLabel *mGaugedLabel = nullptr;

    virtual ReosHydrographJunction *node() const = 0;
};

class ReosFormJunctionNodeWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString datatype() const;
};

class ReosFormJunctionNodeWidget: public ReosFormBaseJunctionNodeWidget
{
    Q_OBJECT
  public:
    ReosFormJunctionNodeWidget( ReosHydrographJunction *junction, const ReosGuiContext &context = ReosGuiContext() );
  protected:
    ReosHydrographJunction *node() const override;

  private slots:
    void syncToNode();

  private:
    ReosHydrographJunction *mJunctioNode = nullptr;
    QCheckBox *mCheckBoxGauged = nullptr;
};

class ReosFormWatershedNodeWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString datatype() const;
};

class ReosFormWatershedNodeWidget: public ReosFormBaseJunctionNodeWidget
{
    Q_OBJECT
  public:
    ReosFormWatershedNodeWidget( ReosHydrographNodeWatershed *watershedNode, const ReosGuiContext &context = ReosGuiContext() );

  protected:
    ReosHydrographJunction *node() const override;

  private slots:
    void originChange();
    void syncToNode();

  private:
    ReosHydrographNodeWatershed *mNode = nullptr;
    QComboBox *mOriginCombo = nullptr;
};


class ReosFormJunctionBoundaryConditionWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    virtual ReosFormWidget *createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context = ReosGuiContext() );
    virtual QString datatype() const;
};


class ReosFormJunctionBoundaryConditionWidget: public ReosFormBaseJunctionNodeWidget
{
    Q_OBJECT
  public:
    enum Type
    {
      WaterLevel,
      FlowRate
    };

    ReosFormJunctionBoundaryConditionWidget( ReosHydraulicStructureBoundaryCondition *boundary, const ReosGuiContext &context = ReosGuiContext() );

    //! Constructor with predefined type, type could be change
    ReosFormJunctionBoundaryConditionWidget( ReosHydraulicStructureBoundaryCondition *boundary, Type type, const ReosGuiContext &context = ReosGuiContext() );

  private slots:
    void syncToNode();
    void updateWidgetsDisplaying();

  private:
    ReosHydraulicStructureBoundaryCondition *mNode = nullptr;
    QComboBox *mTypeCombo = nullptr;
    QComboBox *mWaterLevelCombo = nullptr;
    QWidget *mWaterLevelWidget = nullptr;
    ReosParameterBooleanWidget *mIsElevationConstant = nullptr;
    ReosParameterDoubleWidget *mConstantLevel = nullptr;
    QToolButton *mButtonWaterLevelSeries = nullptr;
    QWidget *mWaterLevelSeriesWidget = nullptr;

    QWidget *mHydrographComboWidget = nullptr;
    QComboBox *mHydrographCombo = nullptr;
    QToolButton *mButtonHydrograph = nullptr;

    ReosHydrographJunction *node() const;
};


#endif // REOSHYDRAULICHYDROGRAPHJUNCTIONPROPERTIESWIDGET_H
