/***************************************************************************
  reostelemacsimulationeditwidget.cpp - ReosTelemacSimulationEditWidget

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
#include "reostelemacsimulationeditwidget.h"
#include "ui_reostelemacsimulationeditwidget.h"

#include "reossimulationinitialcondition.h"

ReosTelemacSimulationEditWidget::ReosTelemacSimulationEditWidget( ReosTelemac2DSimulation *simulation, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosTelemacSimulationEditWidget )
{
  ui->setupUi( this );
  ui->mTimeStepWidget->setDuration( simulation->timeStep() );
  ui->mOutputPeriodWidget->setInteger( simulation->outputResultPeriod() );
  ui->mInitialWaterLevelWidget->setDouble( simulation->initialCondition()->initialWaterLevel() );

  ui->mEquationCombo->addItem( tr( "Finite Element" ), int( ReosTelemac2DSimulation::Equation::FiniteElement ) );
  ui->mEquationCombo->addItem( tr( "Finite Volume" ), int( ReosTelemac2DSimulation::Equation::FiniteVolume ) );
  ui->mEquationCombo->setCurrentIndex( ui->mEquationCombo->findData( static_cast<int>( simulation->equation() ) ) );
  connect( ui->mEquationCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), simulation, [this, simulation]
  {
    simulation->setEquation( static_cast<ReosTelemac2DSimulation::Equation>( ui->mEquationCombo->currentData().toInt() ) );
  } );
}

ReosTelemacSimulationEditWidget::~ReosTelemacSimulationEditWidget()
{
  delete ui;
}

REOSEXTERN ReosHydraulicSimulationWidgetFactory *simulationWidgetFactory()
{
  return new ReosTelemacSimulationEditWidgetFactory;
}

QWidget *ReosTelemacSimulationEditWidgetFactory::simulationSettingsWidget( ReosHydraulicSimulation *simulation, QWidget *parent )
{
  return new ReosTelemacSimulationEditWidget( qobject_cast<ReosTelemac2DSimulation *>( simulation ), parent );
}
