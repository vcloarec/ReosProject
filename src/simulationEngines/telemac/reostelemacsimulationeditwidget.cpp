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
#include "ui_reostelemacengineconfigurationdialog.h"

#include <QFileDialog>

#include "reossimulationinitialcondition.h"
#include "reossettings.h"

ReosTelemacSimulationEditWidget::ReosTelemacSimulationEditWidget( ReosTelemac2DSimulation *simulation, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosTelemacSimulationEditWidget )
{
  ui->setupUi( this );
  ui->mTimeStepWidget->setDuration( simulation->timeStep() );
  ui->mOutputPeriod2DWidget->setInteger( simulation->outputPeriodResult2D() );
  ui->mOutputPeriodHydWidget->setInteger( simulation->outputPeriodResultHydrograph() );
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

QWidget *ReosTelemacSimulationEditWidgetFactory::simulationSettingsWidget( ReosHydraulicSimulation *simulation, QWidget *parent ) const
{
  return new ReosTelemacSimulationEditWidget( qobject_cast<ReosTelemac2DSimulation *>( simulation ), parent );
}

QDialog *ReosTelemacSimulationEditWidgetFactory::engineConfigurationDialog( QWidget *parent ) const
{
  return new ReosTelemacEngineConfigurationDialog( parent );
}

ReosTelemacEngineConfigurationDialog::ReosTelemacEngineConfigurationDialog( QWidget *parent ):
  QDialog( parent ),
  ui( new Ui::ReosTelemacEngineConfigurationDialog )
{
  ui->setupUi( this );
  ReosSettings settings;

  ui->mTelemac2DPythonScriptLineEdit->setText(
    settings.value( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ) ).toString() );

  ui->mTelemacConfigFileLineEdit->setText(
    settings.value( QStringLiteral( "/engine/telemac/telemac-config-file" ) ).toString() );

  ui->mLineEditConfig->setText(
    settings.value( QStringLiteral( "/engine/telemac/telemac-configuration" ) ).toString() );

  if ( settings.contains( QStringLiteral( "/engine/telemac/cpu-usage-count" ) ) )
    ui->mCPUSpinBox->setValue(
      settings.value( QStringLiteral( "/engine/telemac/cpu-usage-count" ) ).toInt() );
  else
    ui->mCPUSpinBox->setValue( QThread::idealThreadCount() );

  connect( this, &QDialog::accepted, this, &ReosTelemacEngineConfigurationDialog::onAccepted );
  connect( ui->mTelemac2DPythonScriptButton, &QToolButton::clicked, this, [this]
  {
    QFileInfo info( ui->mTelemac2DPythonScriptLineEdit->text() );
    QString filePath = QFileDialog::getOpenFileName( this, tr( "TELEMAC 2D Python Script" ), info.path() );
    if ( !filePath.isEmpty() )
      ui->mTelemac2DPythonScriptLineEdit->setText( filePath );
  } );
  connect( ui->mTelemacConfigFileButton, &QToolButton::clicked, this, [this]
  {
    QFileInfo info( ui->mTelemacConfigFileLineEdit->text() );
    QString filePath = QFileDialog::getOpenFileName( this, tr( "TELEMAC Configuration File" ), info.path() );
    if ( !filePath.isEmpty() )
      ui->mTelemacConfigFileLineEdit->setText( filePath );
  } );
}

void ReosTelemacEngineConfigurationDialog::onAccepted()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ), ui->mTelemac2DPythonScriptLineEdit->text() );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-config-file" ), ui->mTelemacConfigFileLineEdit->text() );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-configuration" ), ui->mLineEditConfig->text() );
  settings.setValue( QStringLiteral( "/engine/telemac/cpu-usage-count" ), ui->mCPUSpinBox->value() );
}
