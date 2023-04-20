/***************************************************************************
  reoshydrographeditingwidget.h - ReosHydrographEditingWidget

 ---------------------
 begin                : 25.10.2021
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
#ifndef REOSHYDROGRAPHEDITINGWIDGET_H
#define REOSHYDROGRAPHEDITINGWIDGET_H

#include <QWidget>

#include "reosformwidget.h"
#include "reosplotwidget.h"

class ReosParameterBoolean;
class ReosParameterDuration;
class ReosTimeSeriesVariableTimeStepModel;
class ReosTimeSeriesVariableTimeStep;
class ReosHydrograph;

class ReosHydrographEditingWidget : public ReosFormWidget
{
    Q_OBJECT
  public:
    explicit ReosHydrographEditingWidget( ReosHydrograph *hydrograph, QWidget *parent = nullptr );
    ~ReosHydrographEditingWidget();

  private:
    ReosParameterBoolean *mIsUseConstantTimeStepForNewEntry = nullptr;
    ReosParameterDuration *mConstantTimeStepForNewEntry = nullptr;
    QWidget *mConstantTimeStepForNewEntryWidget = nullptr;
    ReosTimeSeriesVariableTimeStepModel *mDataModel;
    ReosDurationUnitComboBox *mTimeStepUnitCombo;
};


class ReosHydrographEditingWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    QString datatype() const override;
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context ) override;
};


class ReosHydrographPlotFactory : public ReosDataPlotItemFactory
{
  public:
    virtual QString datatype() const override;

    //! Do nothing
    void buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data ) override;

    //! Creates a new plot hydrograph in the plot widget \a plotWidget with the hydrograph \a data
    ReosPlotItem *buildPlotItem( ReosPlotWidget *plotWidget, ReosDataObject *data ) override;
};

#endif // REOSHYDROGRAPHEDITINGWIDGET_H
