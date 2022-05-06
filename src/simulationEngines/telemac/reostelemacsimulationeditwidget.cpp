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

#include "reostelemac2dinitialcondition.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicscheme.h"
#include "reossettings.h"

ReosTelemacSimulationEditWidget::ReosTelemacSimulationEditWidget( ReosHydraulicStructure2D *structure, ReosTelemac2DSimulation *simulation, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosTelemacSimulationEditWidget )
  , mSimulation( simulation )
  , mStructure( structure )
{
  ui->setupUi( this );
  ui->mTimeStepWidget->setDuration( simulation->timeStep() );
  ui->mOutputPeriod2DWidget->setInteger( simulation->outputPeriodResult2D() );
  ui->mOutputPeriodHydWidget->setInteger( simulation->outputPeriodResultHydrograph() );

  ui->mInitialConditionTypeCombo->addItem( tr( "Constant water level, no velocity" ),
      static_cast<int>( ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity ) );
  ui->mInitialConditionTypeCombo->addItem( tr( "From other hydraulic scheme" ),
      static_cast<int>( ReosTelemac2DInitialCondition::Type::FromOtherSimulation ) );

  ui->mInitialConditionTypeCombo->setCurrentIndex( ui->mInitialConditionTypeCombo->findData(
        static_cast<int>( simulation->initialCondition()->initialConditionType() ) ) );
  connect( ui->mInitialConditionTypeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosTelemacSimulationEditWidget::onInitialConditionChanged );

  onInitialConditionChanged();

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

void ReosTelemacSimulationEditWidget::onInitialConditionChanged()
{
  ReosTelemac2DInitialCondition::Type type =
    static_cast< ReosTelemac2DInitialCondition::Type>( ui->mInitialConditionTypeCombo->currentData().toInt() );

  if ( mCurrentInitialConditionWidget )
  {
    ui->mInitialConditionWidget->removeWidget( mCurrentInitialConditionWidget );
    delete mCurrentInitialConditionWidget;
  }

  mSimulation->setInitialCondition( type );

  mCurrentInitialConditionWidget = ReosTelemac2DInitialConditionWidgetFactory::createWidget( mStructure, mSimulation->initialCondition(), this );
  if ( mCurrentInitialConditionWidget )
    ui->mInitialConditionWidget->addWidget( mCurrentInitialConditionWidget );
}

REOSEXTERN ReosHydraulicSimulationWidgetFactory *simulationWidgetFactory()
{
  return new ReosTelemacSimulationEditWidgetFactory;
}

QWidget *ReosTelemacSimulationEditWidgetFactory::simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, QWidget *parent ) const
{
  return new ReosTelemacSimulationEditWidget( structure, qobject_cast<ReosTelemac2DSimulation *>( simulation ), parent );
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

QWidget *ReosTelemac2DInitialConditionWidgetFactory::createWidget( ReosHydraulicStructure2D *structure, ReosTelemac2DInitialCondition *initialCondition, QWidget *parent )
{
  switch ( initialCondition->initialConditionType() )
  {
    case ReosTelemac2DInitialCondition::Type::FromOtherSimulation:
      return new  ReosTelemac2DInititalConditionFromOtherSimulationWidget(
               qobject_cast<ReosTelemac2DInitialConditionFromSimulation *>( initialCondition ), structure );
      break;
    case ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity:
    {
      ReosTelemac2DInitialConstantWaterLevel *ciwl = qobject_cast<ReosTelemac2DInitialConstantWaterLevel *>( initialCondition );
      Q_ASSERT( ciwl != nullptr );
      if ( ciwl )
        return new ReosParameterDoubleWidget( ciwl->initialWaterLevel(), parent );
    }
    break;
  }

  return nullptr;
}

ReosTelemac2DInititalConditionFromOtherSimulationWidget::ReosTelemac2DInititalConditionFromOtherSimulationWidget(
  ReosTelemac2DInitialConditionFromSimulation *initialCondition,
  ReosHydraulicStructure2D *structure )
  : mInitialCondition( initialCondition )
  , mStructure( structure )
{
  ReosHydraulicNetwork *network =  structure->hydraulicNetworkContext().network();

  QGridLayout *gridLayout = new QGridLayout( this );
  setLayout( gridLayout );

  gridLayout->addWidget( new QLabel( tr( "From hydraulic scheme" ), this ), 0, 0 );
  mSchemeCombo = new QComboBox( this );
  gridLayout->addWidget( mSchemeCombo, 0, 1 );
  mSchemeCombo->setModel( network->hydraulicSchemeCollection() );
  mSchemeCombo->setCurrentIndex( network->hydraulicSchemeCollection()->schemeIndex( mInitialCondition->otherSchemeId() ) );
  connect( mSchemeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ),
           this, &ReosTelemac2DInititalConditionFromOtherSimulationWidget::onSchemeChange );

  gridLayout->addWidget( new QLabel( tr( "Time step" ), this ), 1, 0 );
  mTimeStepCombo = new QComboBox( this );
  gridLayout->addWidget( mTimeStepCombo, 1, 1 );

  onSchemeChange();

  connect( mTimeStepCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]
  {
    mInitialCondition->setTimeStepIndex( mTimeStepCombo->currentIndex() );
  } );


}

void ReosTelemac2DInititalConditionFromOtherSimulationWidget::onSchemeChange()
{
  mTimeStepCombo->clear();

  ReosHydraulicNetwork *network =  mStructure->hydraulicNetworkContext().network();
  ReosHydraulicScheme *otherScheme = network->hydraulicSchemeCollection()->scheme( mSchemeCombo->currentIndex() );

  ReosHydraulicSimulation *sim = nullptr;
  if ( otherScheme )
  {
    mInitialCondition->setOtherSchemeId( otherScheme->id() );
    sim = mStructure->simulation( otherScheme );
    const QList<QDateTime> timeSteps = sim->theoricalTimeSteps( otherScheme );
    for ( const QDateTime &timeStep : timeSteps )
      mTimeStepCombo->addItem( QLocale().toString( timeStep, QLocale::ShortFormat ) );
  }

  if ( mInitialCondition->timeStepIndex() < mTimeStepCombo->count() )
    mTimeStepCombo->setCurrentIndex( mInitialCondition->timeStepIndex() );
  else
  {
    mTimeStepCombo->setCurrentIndex( -1 );
  }
}
