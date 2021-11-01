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

class ReosParameterBoolean;
class ReosParameterDuration;
class ReosTimeSerieVariableTimeStepModel;
class ReosTimeSerieVariableTimeStep;
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
    ReosTimeSerieVariableTimeStepModel *mDataModel;
    ReosDurationUnitComboBox *mTimeStepUnitCombo;
};


class ReosHydrographEditingWidgetFactory: public ReosFormWidgetDataFactory
{
  public:
    QString datatype() const override {return QStringLiteral( "hydrograph" );}
    ReosFormWidget *createDataWidget( ReosDataObject *dataObject, QWidget *parent ) override;
};

#endif // REOSHYDROGRAPHEDITINGWIDGET_H
