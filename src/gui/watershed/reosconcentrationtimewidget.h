/***************************************************************************
  reosconcentrationtimewidget.h - ReosConcentrationTimeWidget

 ---------------------
 begin                : 14.2.2021
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
#ifndef REOSCONCENTRATIONTIMEWIDGET_H
#define REOSCONCENTRATIONTIMEWIDGET_H

#include <QWidget>
#include <QPointer>

#include "reosactionwidget.h"
#include "reoswatershed.h"
#include "reosconcentrationtimecalculation.h"

class ReosParameterDoubleWidget;
class ReosParameterSlopeWidget;
class ReosParameterAreaWidget;
class QAction;

namespace Ui
{
  class ReosConcentrationTimeWidget;
}

class ReosConcentrationTimeWidget : public ReosActionWidget
{
    Q_OBJECT

  public:
    explicit ReosConcentrationTimeWidget( QWidget *parent = nullptr );
    ~ReosConcentrationTimeWidget();

  public slots:
    void setCurrentWatershed( ReosWatershed *ws );
    void updateFormulas();

  private slots:
    void applyCalculation();
    void onMethodChanged();
    void updateCalcultedValue();

    void onSelectAll();
    void onDeselectAll();
    void onSelectValid();
    void onCopy();

    void onViewDoubleClicked( const QModelIndex &index );

    void onFormulaDisplaying();

  private:
    Ui::ReosConcentrationTimeWidget *ui;
    ReosConcentrationTimeFormulasModel *mFormulasModel = nullptr;
    QPointer<ReosWatershed> mCurrentWatershed;

    ReosParameterDoubleWidget *mLengthParameterWidget = nullptr;
    ReosParameterDoubleWidget *mDropParameterWidget = nullptr;
    ReosParameterSlopeWidget *mSlopeParameterWidget = nullptr;
    ReosParameterAreaWidget *mAreaParameterWidget = nullptr;

    QAction *mActionSelectAll = nullptr;
    QAction *mActionDeselectAll = nullptr;
    QAction *mActionnSelectValid = nullptr;
    QAction *mActionCopy = nullptr;

    ReosConcentrationTimeCalculation::UsedMethod usedMethod() const;
    void setUsedMethod( ReosConcentrationTimeCalculation::UsedMethod method );
};

#endif // REOSCONCENTRATIONTIMEWIDGET_H
