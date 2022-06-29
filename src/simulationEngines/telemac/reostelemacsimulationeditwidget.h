/***************************************************************************
  reostelemacsimulationeditwidget.h - ReosTelemacSimulationEditWidget

 ---------------------
 begin                : 31.3.2022
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
#ifndef REOSTELEMACSIMULATIONEDITWIDGET_H
#define REOSTELEMACSIMULATIONEDITWIDGET_H

#include <QWidget>
#include <QDialog>
#include "reostelemac2dsimulation.h"
#include "reoshydraulic2dsimulationwidget.h"

class QComboBox;

class ReosTelemac2DInitialConditionFromSimulation;

namespace Ui
{
  class ReosTelemacSimulationEditWidget;
  class ReosTelemacEngineConfigurationDialog;
}

class ReosTelemacSimulationEditWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosTelemacSimulationEditWidget( ReosHydraulicStructure2D *structure, ReosTelemac2DSimulation *simulation, QWidget *parent = nullptr );
    ~ReosTelemacSimulationEditWidget();

  private slots:
    void onInitialConditionChanged();

  private:
    Ui::ReosTelemacSimulationEditWidget *ui;
    ReosTelemac2DSimulation *mSimulation = nullptr;
    ReosHydraulicStructure2D *mStructure = nullptr;
    QWidget *mCurrentInitialConditionWidget = nullptr;
};

class ReosTelemacEngineConfigurationDialog : public QDialog
{
    Q_OBJECT

  public:
    explicit ReosTelemacEngineConfigurationDialog( QWidget *parent = nullptr );

  private slots:
    void onAccepted();

  private:
    Ui::ReosTelemacEngineConfigurationDialog *ui;
};

class ReosTelemacSimulationEditWidgetFactory : public ReosHydraulicSimulationWidgetFactory
{
  public:
    QString key() const override {return ReosTelemac2DSimulation::staticKey();}

    QWidget *simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, QWidget *parent ) const override;
    QDialog *engineConfigurationDialog( QWidget *parent ) const override;
    QWidget *simulationEngineDescription( QWidget *parent ) const override;
};


class ReosTelemac2DInitialConditionWidgetFactory
{
  public:

    static QWidget *createWidget( ReosHydraulicStructure2D *structure, ReosTelemac2DInitialCondition *initialCondition, QWidget *parent );
};

class ReosTelemac2DInititalConditionFromOtherSimulationWidget : public QWidget
{
    Q_OBJECT
  public:
    ReosTelemac2DInititalConditionFromOtherSimulationWidget( ReosTelemac2DInitialConditionFromSimulation *initialCondition, ReosHydraulicStructure2D *structure );

  private slots:
    void onSchemeChange();

  private:
    QPointer<ReosTelemac2DInitialConditionFromSimulation> mInitialCondition;
    QPointer<ReosHydraulicStructure2D> mStructure;
    QComboBox *mSchemeCombo = nullptr;
    QComboBox *mTimeStepCombo = nullptr;


};

#endif // REOSTELEMACSIMULATIONEDITWIDGET_H
