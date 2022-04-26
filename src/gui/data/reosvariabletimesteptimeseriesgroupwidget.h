/***************************************************************************
  reosvariabletimesteptimeseriesgroupwidget.h - ReosVariableTimeStepTimeSeriesGroupWidget

 ---------------------
 begin                : 11.4.2022
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
#ifndef REOSVARIABLETIMESTEPTIMESERIESGROUPWIDGET_H
#define REOSVARIABLETIMESTEPTIMESERIESGROUPWIDGET_H

#include <QWidget>
#include <QPointer>
#include "reosactionwidget.h"
#include "reosformwidget.h"

class QToolBar;

class ReosTimeSeriesVariableTimeStepGroup;
class ReosTimeSerieVariableTimeStepModel;
class ReosPlotTimeSerieVariableStep;
class ReosTimeSerieVariableTimeStep;
class ReosGuiContext;
class ReosMap;

namespace Ui
{
  class ReosTimeSeriesVariableTimeStepWidget;
}

class ReosVariableTimeStepTimeSeriesGroupWidget: public ReosStackedPageWidget
{
    Q_OBJECT
  public:
    explicit ReosVariableTimeStepTimeSeriesGroupWidget( const ReosGuiContext &guiContext, const QString genericName, const QString &unitString, int currentIndex = -1 );
    ~ReosVariableTimeStepTimeSeriesGroupWidget();

    void setTimeSeriesGroup( ReosTimeSeriesVariableTimeStepGroup *group );

    virtual void showBackButton();

  private slots:
    void onAddSeries();
    void onRemoveSeries();
    void onRenameSeries();
    void onGroupChanged();
    void onCurrentSeriesChanged();
    void updatePlotExtent();

  private:
    Ui::ReosTimeSeriesVariableTimeStepWidget *ui;
    ReosMap *mMap = nullptr;
    ReosTimeSeriesVariableTimeStepGroup *mGroup = nullptr;
    QString mGenericSeriesName;
    QString mUnitString;
    QAction *mActionAddSeries = nullptr;
    QAction *mActionDeleteSeries = nullptr;
    QAction *mActionRenameSeries = nullptr;
    QWidget *mCurrentProviderSettingsWidget = nullptr;
    QWidget *mCurrenEditingWidget = nullptr;
//    QMap<QAction *, QString> mProvidersActionToKeys;
    QPointer<ReosTimeSerieVariableTimeStep> mCurrentSeries = nullptr;
    ReosPlotTimeSerieVariableStep *mSeriesPlot = nullptr;
    QToolBar *mToolBarProvider;

    bool mIsDatasetSelected = false;
    bool mIsDataReady = false;

//    void populateProviderActions();
//    void showProviderSelector( const QString &providerKey );
};


class ReosVariableTimeStepSeriesEditingWidget : public ReosFormWidget
{
    Q_OBJECT
  public:
    explicit ReosVariableTimeStepSeriesEditingWidget( ReosTimeSerieVariableTimeStep *timeSeries, QWidget *parent = nullptr );
    ~ReosVariableTimeStepSeriesEditingWidget();

  private:
    ReosParameterBoolean *mIsUseConstantTimeStepForNewEntry = nullptr;
    ReosParameterDuration *mConstantTimeStepForNewEntry = nullptr;
    QWidget *mConstantTimeStepForNewEntryWidget = nullptr;
    ReosTimeSerieVariableTimeStepModel *mDataModel;
    ReosDurationUnitComboBox *mTimeStepUnitCombo;
};


class ReosVariableTimeStepSeriesEditingWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    QString datatype() const override;
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, const ReosGuiContext &context ) override;
};

#endif // REOSVARIABLETIMESTEPTIMESERIESGROUPWIDGET_H
