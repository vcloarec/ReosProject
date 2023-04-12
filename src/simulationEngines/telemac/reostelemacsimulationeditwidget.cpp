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
#include <QMessageBox>

#include "reostelemac2dinitialcondition.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicscheme.h"
#include "reossettings.h"
#include "reosstyleregistery.h"
#include "reosmaptool.h"
#include "reosguicontext.h"
#include "reosgisengine.h"

ReosTelemacSimulationEditWidget::ReosTelemacSimulationEditWidget(
  ReosHydraulicStructure2D *structure,
  ReosTelemac2DSimulation *simulation,
  const ReosGuiContext &guiContext ) :
  QWidget( guiContext.parent() ),
  ui( new Ui::ReosTelemacSimulationEditWidget )
  , mSimulation( simulation )
  , mStructure( structure )
  , mGuiContext( guiContext )
{
  ui->setupUi( this );
  ui->mTimeStepWidget->setDuration( simulation->timeStep() );
  ui->mOutputPeriod2DWidget->setInteger( simulation->outputPeriodResult2D() );
  ui->mOutputPeriodHydWidget->setInteger( simulation->outputPeriodResultHydrograph() );

  ui->mInitialConditionTypeCombo->addItem( tr( "Constant water level, no velocity" ),
      static_cast<int>( ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity ) );
  ui->mInitialConditionTypeCombo->addItem( tr( "From other hydraulic scheme" ),
      static_cast<int>( ReosTelemac2DInitialCondition::Type::FromOtherSimulation ) );
  ui->mInitialConditionTypeCombo->addItem( tr( "Interpolation line" ),
      static_cast<int>( ReosTelemac2DInitialCondition::Type::Interpolation ) );


  ui->mInitialConditionTypeCombo->setCurrentIndex( ui->mInitialConditionTypeCombo->findData(
        static_cast<int>( simulation->initialCondition()->initialConditionType() ) ) );
  connect( ui->mInitialConditionTypeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosTelemacSimulationEditWidget::onInitialConditionChanged );

  onInitialConditionChanged();

  ui->mEquationCombo->addItem( tr( "Finite Element" ), int( ReosTelemac2DSimulation::Equation::FiniteElement ) );
  ui->mEquationCombo->addItem( tr( "Finite Volume" ), int( ReosTelemac2DSimulation::Equation::FiniteVolume ) );

  ui->mEquationCombo->setCurrentIndex( ui->mEquationCombo->findData( static_cast<int>( simulation->equation() ) ) );

  ui->mVolumeFiniteGroupBox->setVisible( simulation->equation() == ReosTelemac2DSimulation::Equation::FiniteVolume );
  ui->mVFSchemeComboBox->addItem( tr( "Roe" ), int( ReosTelemac2DSimulation::VolumeFiniteScheme::Roe ) );
  ui->mVFSchemeComboBox->addItem( tr( "Kinetic" ), int( ReosTelemac2DSimulation::VolumeFiniteScheme::Kinetic ) );
  ui->mVFSchemeComboBox->addItem( tr( "Zokagoa" ), int( ReosTelemac2DSimulation::VolumeFiniteScheme::Zokagoa ) );
  ui->mVFSchemeComboBox->addItem( tr( "Tchamen" ), int( ReosTelemac2DSimulation::VolumeFiniteScheme::Tchamen ) );
  ui->mVFSchemeComboBox->addItem( tr( "HLLC" ), int( ReosTelemac2DSimulation::VolumeFiniteScheme::HLLC ) );
  ui->mVFSchemeComboBox->addItem( tr( "WAF" ), int( ReosTelemac2DSimulation::VolumeFiniteScheme::WAF ) );
  ui->mVFSchemeComboBox->setCurrentIndex( ui->mVFSchemeComboBox->findData( static_cast<int>( simulation->volumeFiniteScheme() ) ) );
  ui->mVfCourantNumberParameter->setDouble( simulation->courantNumber() );

  connect( ui->mEquationCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), simulation, [this, simulation]
  {
    ReosTelemac2DSimulation::Equation eq =
    static_cast<ReosTelemac2DSimulation::Equation>( ui->mEquationCombo->currentData().toInt() );
    simulation->setEquation( eq );
    ui->mVolumeFiniteGroupBox->setVisible( eq == ReosTelemac2DSimulation::Equation::FiniteVolume );
  } );

  connect( ui->mVFSchemeComboBox, QOverload<int>::of( &QComboBox::currentIndexChanged ), simulation, [this, simulation]
  {
    simulation->setVolumeFiniteEquation(
      static_cast<ReosTelemac2DSimulation::VolumeFiniteScheme>( ui->mVFSchemeComboBox->currentData().toInt() ) );
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

  mCurrentInitialConditionWidget = ReosTelemac2DInitialConditionWidgetFactory::createWidget( mStructure, mSimulation->initialCondition(), ReosGuiContext( mGuiContext, this ) );
  if ( mCurrentInitialConditionWidget )
    ui->mInitialConditionWidget->addWidget( mCurrentInitialConditionWidget );
}

REOSEXTERN ReosHydraulicSimulationWidgetFactory *simulationWidgetFactory()
{
  return new ReosTelemacSimulationEditWidgetFactory;
}

QWidget *ReosTelemacSimulationEditWidgetFactory::simulationSettingsWidget( ReosHydraulicStructure2D *structure, ReosHydraulicSimulation *simulation, const ReosGuiContext &guiContext ) const
{
  return new ReosTelemacSimulationEditWidget( structure, qobject_cast<ReosTelemac2DSimulation *>( simulation ), guiContext );
}

QDialog *ReosTelemacSimulationEditWidgetFactory::engineConfigurationDialog( QWidget *parent ) const
{
  return new ReosTelemacEngineConfigurationDialog( parent );
}

QWidget *ReosTelemacSimulationEditWidgetFactory::simulationEngineDescription( QWidget *parent ) const
{
  QWidget *w = new QWidget( parent );
  w->setLayout( new QVBoxLayout );

  w->layout()->addWidget( new QLabel( QStringLiteral( "TELEMAC 2D" ), w ) );
  QLabel *labelWeb = new QLabel( w );
  labelWeb->setTextFormat( Qt::RichText );
  labelWeb->setText( QStringLiteral( "<a href=\"https://www.opentelemac.org\"> www.opentelemac.org </a>" ) );
  labelWeb->setOpenExternalLinks( true );
  w->layout()->addWidget( labelWeb );
  return w;
}

ReosTelemacEngineConfigurationDialog::ReosTelemacEngineConfigurationDialog( QWidget *parent ):
  QDialog( parent ),
  ui( new Ui::ReosTelemacEngineConfigurationDialog )
{
  ui->setupUi( this );

  accordToSettings();

  connect( this, &QDialog::accepted, this, &ReosTelemacEngineConfigurationDialog::onAccepted );
  connect( ui->mTelemac2DPythonScriptButton, &QToolButton::clicked, this, [this]
  {
    const QFileInfo info( ui->mTelemac2DPythonScriptLineEdit->text() );
    QString filePath = QFileDialog::getOpenFileName( this, tr( "TELEMAC 2D Python Script" ), info.path() );
    if ( !filePath.isEmpty() )
      ui->mTelemac2DPythonScriptLineEdit->setText( filePath );
  } );
  connect( ui->mTelemacConfigFileButton, &QToolButton::clicked, this, [this]
  {
    const QFileInfo info( ui->mTelemacConfigFileLineEdit->text() );
    QString filePath = QFileDialog::getOpenFileName( this, tr( "TELEMAC Configuration File" ), info.path() );
    if ( !filePath.isEmpty() )
      ui->mTelemacConfigFileLineEdit->setText( filePath );
  } );

  connect( ui->mPythonPathButton, &QToolButton::clicked, this, [this]
  {
    const QFileInfo info( ui->mPythonPathLineEdit->text() );
    QString filePath = QFileDialog::getExistingDirectory( this, tr( "Python path" ), info.path() );
    if ( !filePath.isEmpty() )
      ui->mPythonPathLineEdit->setText( filePath );
  } );

  connect( ui->mDefaultButton, &QPushButton::clicked, this, &ReosTelemacEngineConfigurationDialog::onResetToDefault );
}

void ReosTelemacEngineConfigurationDialog::onAccepted()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ), ui->mTelemac2DPythonScriptLineEdit->text() );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-config-file" ), ui->mTelemacConfigFileLineEdit->text() );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-configuration" ), ui->mLineEditConfig->text() );
  settings.setValue( QStringLiteral( "/engine/telemac/cpu-usage-count" ), ui->mCPUSpinBox->value() );
  settings.setValue( QStringLiteral( "/engine/telemac/cpu-usage-count" ), ui->mCPUSpinBox->value() );
  settings.setValue( QStringLiteral( "/engine/telemac/additional_pathes" ), ui->mDependenciesPathTextEdit->toPlainText() );
  settings.setValue( QStringLiteral( "/python_path" ), ui->mPythonPathLineEdit->text() );
}

void ReosTelemacEngineConfigurationDialog::onResetToDefault()
{
  if ( QMessageBox::warning( this,
                             tr( "Reset default TELEMAC settings" ),
                             tr( "This will erase definitly the current settings. Continue?" ), QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) ==
       QMessageBox::Yes )
  {
    ReosTelemac2DSimulationEngineFactory::initializeSettingsStatic();
    accordToSettings();
  }
}

void ReosTelemacEngineConfigurationDialog::accordToSettings()
{
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

  ui->mPythonPathLineEdit->setText( settings.value( QStringLiteral( "python_path" ) ).toString() );

  ui->mDependenciesPathTextEdit->setText( settings.value( QStringLiteral( "/engine/telemac/additional_pathes" ) ).toString() );
}

QWidget *ReosTelemac2DInitialConditionWidgetFactory::createWidget(
  ReosHydraulicStructure2D *structure,
  ReosTelemac2DInitialCondition *initialCondition,
  const ReosGuiContext &guiContext )
{
  switch ( initialCondition->initialConditionType() )
  {
    case ReosTelemac2DInitialCondition::Type::FromOtherSimulation:
      return new  ReosTelemac2DInititalConditionFromOtherSimulationWidget(
               qobject_cast<ReosTelemac2DInitialConditionFromSimulation *>( initialCondition ), structure, guiContext.parent() );
      break;
    case ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity:
    {
      ReosTelemac2DInitialConstantWaterLevel *ciwl = qobject_cast<ReosTelemac2DInitialConstantWaterLevel *>( initialCondition );
      Q_ASSERT( ciwl != nullptr );
      if ( ciwl )
        return new ReosParameterDoubleWidget( ciwl->initialWaterLevel(), guiContext.parent() );
    }
    break;
    case ReosTelemac2DInitialCondition::Type::Interpolation:
    {
      ReosTelemac2DInitialConditionFromInterpolation *ciinter = qobject_cast<ReosTelemac2DInitialConditionFromInterpolation *>( initialCondition );
      Q_ASSERT( ciinter != nullptr );
      return new  ReosTelemac2DInititalConditionInterpolationWidget(
               qobject_cast<ReosTelemac2DInitialConditionFromInterpolation *>( initialCondition ), guiContext );
    }
    break;
  }

  return nullptr;
}

ReosTelemac2DInititalConditionFromOtherSimulationWidget::ReosTelemac2DInititalConditionFromOtherSimulationWidget(
  ReosTelemac2DInitialConditionFromSimulation *initialCondition,
  ReosHydraulicStructure2D *structure, QWidget *parent )
  : QWidget( parent )
  , mInitialCondition( initialCondition )
  , mStructure( structure )
{
  ReosHydraulicNetwork *network =  structure->network();

  QGridLayout *gridLayout = new QGridLayout( this );
  setLayout( gridLayout );
  layout()->setContentsMargins( 0, 0, 0, 0 );

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

  ReosHydraulicNetwork *network =  mStructure->network();
  ReosHydraulicScheme *otherScheme = network->hydraulicSchemeCollection()->scheme( mSchemeCombo->currentIndex() );

  if ( otherScheme && mStructure->results( otherScheme ) )
  {
    mInitialCondition->setOtherSchemeId( otherScheme->id() );
    ReosHydraulicSimulationResults *results = mStructure->results( otherScheme );
    const QList<QDateTime> timeSteps = results->timeSteps();
    for ( const QDateTime &timeStep : timeSteps )
      mTimeStepCombo->addItem( QLocale().toString( timeStep, QLocale::ShortFormat ) );
  }

  if ( mInitialCondition->timeStepIndex() < mTimeStepCombo->count() )
    mTimeStepCombo->setCurrentIndex( mInitialCondition->timeStepIndex() );
  else
    mTimeStepCombo->setCurrentIndex( -1 );
}

ReosTelemac2DInititalConditionInterpolationWidget::ReosTelemac2DInititalConditionInterpolationWidget( ReosTelemac2DInitialConditionFromInterpolation *initialCondition,
    const ReosGuiContext &guiContext )
  : QWidget( guiContext.parent() )
  , mMap( guiContext.map() )
  , mInitialCondition( initialCondition )
  , mActionDrawLine( new QAction( QIcon( QStringLiteral( ":/images/drawProfile.svg" ) ), tr( "Draw intepolation line" ), this ) )
  , mMapLine( mMap )
{
  QVBoxLayout *lay = new QVBoxLayout( this );
  setLayout( lay );
  lay->setContentsMargins( 0, 0, 0, 0 );

  ReosParameterDoubleWidget *wf = new ReosParameterDoubleWidget( initialCondition->firstValue(), this );
  wf->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  lay->addWidget( wf );
  ReosParameterDoubleWidget *ws = new ReosParameterDoubleWidget( initialCondition->secondValue(), this );
  ws->enableSpacer( ReosParameterWidget::SpacerInMiddle );
  lay->addWidget( ws );

  QToolBar *toolBar = new QToolBar( this );
  toolBar->setToolButtonStyle( Qt::ToolButtonTextBesideIcon );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  toolBar->addAction( mActionDrawLine );
  mActionDrawLine->setCheckable( true );
  mValueLabel = new QLabel( this );
  toolBar->addSeparator();
  toolBar->addWidget( mValueLabel );
  lay->addWidget( toolBar );

  mDrawLineMapTool = new ReosMapToolDrawPolyline( guiContext.map() );
  mDrawLineMapTool->setAction( mActionDrawLine );
  mDrawLineMapTool->setColor( ReosStyleRegistery::instance()->redReos() );
  mDrawLineMapTool->setSecondaryStrokeColor( Qt::white );
  mDrawLineMapTool->setStrokeWidth( 3 );
  mDrawLineMapTool->setLineStyle( Qt::DashLine );
  mDrawLineMapTool->activateMovingSignal( true );
  connect( mDrawLineMapTool, &ReosMapToolDrawPolyline::drawn, this, &ReosTelemac2DInititalConditionInterpolationWidget::onLineDrawn );
  connect( mMap, &ReosMap::cursorMoved, this, &ReosTelemac2DInititalConditionInterpolationWidget::onDrawLineMapToolMove );

  mMapLine.setColor( ReosStyleRegistery::instance()->redReos( 200 ) );
  mMapLine.setExternalColor( Qt::white );
  mMapLine.setWidth( 2 );
  mMapLine.setExternalWidth( 4 );

  mLine = mMap->engine()->transformToProjectCoordinates( mInitialCondition->crs(), mInitialCondition->line() );
  mMapLine.resetPolyline( mLine );
  mLineLength = ReosGeometryUtils::length( mLine );
}

void ReosTelemac2DInititalConditionInterpolationWidget::showEvent( QShowEvent *e )
{
  mMapLine.setVisible( true );
  QWidget::showEvent( e );
}

void ReosTelemac2DInititalConditionInterpolationWidget::hideEvent( QHideEvent *e )
{
  mMapLine.setVisible( false );
  mDrawLineMapTool->quitMap();
  QWidget::hideEvent( e );
}

void ReosTelemac2DInititalConditionInterpolationWidget::onLineDrawn( const QPolygonF &line )
{
  if ( line.count() > 1 )
  {
    mInitialCondition->setLine( line, mMap->mapCrs() );
    mLine = line;
    mLineLength = ReosGeometryUtils::length( mLine );
    mMapLine.resetPolyline( line );
  }
  else
  {
    mInitialCondition->setLine( QPolygonF(), QString() );
    mLine = QPolygonF();
    mLineLength = 0;
    mMapLine.resetPolyline();
  }
}

void ReosTelemac2DInititalConditionInterpolationWidget::onDrawLineMapToolMove( const QPointF &pt )
{
  if ( !mLine.empty() )
  {
    double dist = ReosGeometryUtils::projectedPointDistanceFromBegining( pt, mLine );
    double ratio =  dist / mLineLength;
    double firstValue = mInitialCondition->firstValue()->value();
    double secondValue = mInitialCondition->secondValue()->value();
    mValueLabel->setText( tr( "Value under cursor: %1" ).arg( QLocale().toString( firstValue + ( secondValue - firstValue )*ratio, 'f', 2 ) ) );
  }
  else
  {
    mValueLabel->setText( tr( "No interpolation line" ) );
  }
}
