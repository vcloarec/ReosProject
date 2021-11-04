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

#include "reosactionwidget.h"

namespace Ui
{
  class ReosGaugedHydrographWidget;
}

class ReosWatershed;
class ReosHydrographStore;
class ReosTimeSerieVariableTimeStepModel;
class ReosPlotTimeSerieVariableStep;
class ReosHydrographEditingWidget;
class ReosHydrograph;
class ReosMap;

class ReosHubEauWidget;

class ReosGaugedHydrographWidget : public ReosActionWidget
{
    Q_OBJECT
  public:
    explicit ReosGaugedHydrographWidget( ReosMap *map, QWidget *parent = nullptr );
    ~ReosGaugedHydrographWidget();

  public slots:
    void setCurrentWatershed( ReosWatershed *watershed );

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
    ReosWatershed *mCurrentWatershed = nullptr;
    ReosHydrographStore *mHydrographStore = nullptr;
    ReosTimeSerieVariableTimeStepModel *mTableModel = nullptr;
    QAction *mActionHubEau = nullptr;
    QAction *mActionAddHydrograph = nullptr;
    QAction *mActionDeleteHydrograph = nullptr;
    QAction *mActionRenameHydrograph = nullptr;
    QWidget *mCurrenEditingWidget = nullptr;
    QPointer<ReosHydrograph> mCurrentHydrograph = nullptr;
    ReosPlotTimeSerieVariableStep *mHydrographPlot = nullptr;

    ReosHubEauWidget *mHubEauWidget = nullptr;
};

#endif // REOSGAUGEDHYDROGRAPHWIDGET_H
