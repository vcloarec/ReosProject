/***************************************************************************
  reosgaugedhydrographwidget.h - ReosGaugedHydrographWidget

 ---------------------
 begin                : 24.10.2021
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
#ifndef REOSGAUGEDHYDROGRAPHWIDGET_H
#define REOSGAUGEDHYDROGRAPHWIDGET_H

#include <QPointer>
#include <QMap>

#include "reosactionwidget.h"

namespace Ui
{
  class ReosGaugedHydrographWidget;
}

class ReosWatershed;
class ReosHydrographsStore;
class ReosTimeSerieVariableTimeStepModel;
class ReosPlotTimeSerieVariableStep;
class ReosHydrographEditingWidget;
class ReosHydrograph;
class ReosMap;
class ReosDataProviderSelectorWidget;
class ReosGuiContext;

class QToolBar;

class ReosHubEauWidget;


class ReosGaugedHydrographWidget : public ReosStackedPageWidget
{
    Q_OBJECT
  public:
    explicit ReosGaugedHydrographWidget( const ReosGuiContext &guiContext );
    ~ReosGaugedHydrographWidget();

    void setHydrographStore( ReosHydrographsStore *store );

    virtual void showBackButton();

  private slots:
    void onAddHydrograph();
    void onRemoveHydrograph();
    void onRenameHydrograph();
    void onStoreChanged();
    void onCurrentHydrographChanged();
    void updatePlotExtent();

  private:
    Ui::ReosGaugedHydrographWidget *ui;
    ReosMap *mMap = nullptr;
    ReosHydrographsStore *mHydrographStore = nullptr;
    QAction *mActionAddHydrograph = nullptr;
    QAction *mActionDeleteHydrograph = nullptr;
    QAction *mActionRenameHydrograph = nullptr;
    QWidget *mCurrentProviderSettingsWidget = nullptr;
    QWidget *mCurrenEditingWidget = nullptr;
    QMap<QAction *, QString> mProvidersActionToKeys;
    QPointer<ReosHydrograph> mCurrentHydrograph = nullptr;
    ReosPlotTimeSerieVariableStep *mHydrographPlot = nullptr;
    QToolBar *mToolBarProvider;

    bool mIsDatasetSelected = false;
    bool mIsDataReady = false;

    void populateProviderActions();
    void showProviderSelector( const QString &providerKey );
};


class ReosWatershedGaugedHydrographWidget : public ReosActionStackedWidget
{
    Q_OBJECT
  public:
    ReosWatershedGaugedHydrographWidget( const ReosGuiContext  &guiContext );

  public slots:
    void setCurrentWatershed( ReosWatershed *watershed );

  private:
    ReosGaugedHydrographWidget *mGaugedHydrographWidget = nullptr;
};

#endif // REOSGAUGEDHYDROGRAPHWIDGET_H
