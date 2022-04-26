/***************************************************************************
  reoshydraulicstructrure2dproperties.cpp - ReosHydraulicStructrure2DProperties

 ---------------------
 begin                : 10.1.2022
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
#include "reoshydraulicstructure2dproperties.h"
#include "ui_reoshydraulicstructure2dproperties.h"

#include <QMenu>
#include <QCheckBox>
#include <QSlider>
#include <QMessageBox>
#include <QFileDialog>

#include "reosedithydraulicstructure2dwidget.h"
#include "reos3dview.h"
#include "reoshydraulicsimulationconsole.h"
#include "reosstyleregistery.h"
#include "reosmeshscalarrenderingwidget.h"
#include "reoscolorbutton.h"
#include "reosprocesscontroler.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosplotitemlist.h"
#include "reoshydraulic2dsimulationwidget.h"


ReosHydraulicStructure2DProperties::ReosHydraulicStructure2DProperties( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosHydraulicElementWidget( context.parent() )
  , ui( new Ui::ReosHydraulicStructure2DProperties )
  , mStructure2D( structure2D )
  , mMap( context.map() )
  , mActionEditStructure( new QAction( QPixmap( QStringLiteral( ":/images/settings.svg" ) ), tr( "Edit Model" ), this ) )
  , mActionRunSimulation( new QAction( QPixmap( QStringLiteral( ":/images/runModel.svg" ) ), tr( "Run Simulation" ), this ) )
  , mActionExportSimulationFile( new QAction( QPixmap( QStringLiteral( ":/images/exportSimulation.svg" ) ), tr( "Export Simulation" ), this ) )
  , mActionEngineConfiguration( ( new QAction( QPixmap( QStringLiteral( ":/images/engineSettings.svg" ) ), tr( "Engine Settings" ), this ) ) )
  , mAction3DView( new QAction( QPixmap( QStringLiteral( ":/images/view3D.svg" ) ), tr( "3D View" ), this ) )
  , mScalarDatasetMenu( new QMenu( this ) )
  , mGuiContext( context, this )
{
  ui->setupUi( this );

  mGuiContext.addAction( mAction3DView );

  connect( mActionEditStructure, &QAction::triggered, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosEditHydraulicStructure2DWidget( mStructure2D, mGuiContext ) );
  } );

  connect( mActionRunSimulation, &QAction::triggered, this, &ReosHydraulicStructure2DProperties::onLaunchCalculation );
  connect( mActionExportSimulationFile, &QAction::triggered, this, &ReosHydraulicStructure2DProperties::onExportSimulation );
  connect( mActionEngineConfiguration, &QAction::triggered, this, [this]
  {
    if ( mStructure2D->currentSimulation() )
    {
      QString key = mStructure2D->currentSimulation()->key();
      QDialog *configDialog = ReosHydraulicSimulationWidgetRegistery::instance()->createConfigurationDialog( key, this );
      configDialog->exec();
      configDialog->deleteLater();
    }
    else
    {
      QMessageBox::warning( this, tr( "Simulation Engine Settings" ), tr( "No simulation selected in the modele settings" ) );
    }
  } );

  mView3D = new Reos3dView( mStructure2D->mesh(), ReosGuiContext( context, this ) );
  mView3D->setAction( mAction3DView );
  mAction3DView->setCheckable( true );
  mView3D->setMapSettings( mStructure2D->map3dSettings() );
  mView3D->setTerrainSettings( mStructure2D->terrain3DSettings() );
  connect( mView3D, &Reos3dView::mapSettingsChanged, this, [this]
  {
    mStructure2D->setMap3dSettings( mView3D->map3DSettings() );
  } );

  mView3D->addMesh( structure2D->mesh() );

  QToolBar *toolBar = new QToolBar( this );
  toolBar->addAction( mActionEditStructure );

  QToolButton *simulationToolButton = new QToolButton( toolBar );
  simulationToolButton->setPopupMode( QToolButton::MenuButtonPopup );
  simulationToolButton->setDefaultAction( mActionRunSimulation );
  QMenu *simulationMenu = new QMenu( toolBar );
  simulationMenu->addAction( mActionExportSimulationFile );
  simulationMenu->addAction( mActionEngineConfiguration );
  simulationToolButton->setMenu( simulationMenu );
  toolBar->addWidget( simulationToolButton );

  toolBar->addSeparator();

  QToolButton *datasetSettingsButton = new QToolButton( toolBar );
  datasetSettingsButton->setIcon( QPixmap( QStringLiteral( ":/images/scalarContour.svg" ) ) );
  datasetSettingsButton->setPopupMode( QToolButton::MenuButtonPopup );
  datasetSettingsButton->setMenu( mScalarDatasetMenu );
  toolBar->addWidget( datasetSettingsButton );

  connect( datasetSettingsButton, &QToolButton::clicked, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosMeshScalarRenderingWidget( mStructure2D->mesh(), mStructure2D->currentActivatedMeshDataset(), mGuiContext ) );
  } );

  toolBar->addAction( mAction3DView );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  toolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBoxLayout->addWidget( toolBar );

  mMap->addExtraRenderedObject( mStructure2D->mesh() );
  connect( mStructure2D->mesh(), &ReosMesh::repaintRequested, this, &ReosHydraulicStructure2DProperties::requestMapRefresh );

  updateDatasetMenu();
  connect( mStructure2D, &ReosHydraulicStructure2D::simulationResultChanged, this, &ReosHydraulicStructure2DProperties::updateDatasetMenu );

  QString settingsString = QStringLiteral( "hydraulic-network-structure-2D" );

  ui->mPlotWidget->setSettingsContext( settingsString );
  ui->mPlotWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAxeYright( false );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );

  if ( mStructure2D->currentSimulation() )
    ui->mSimulationEngineName->setText( mStructure2D->currentSimulation()->engineName() );

  mInputHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Input hydrographs" ), ui->mPlotWidget );
  mOutputHydrographPlotButton = new ReosVariableTimeStepPlotListButton( tr( "Output hydrographs" ), ui->mPlotWidget );
  ReosSettings settings;
  if ( settings.contains( settingsString ) )
  {
    mInputHydrographPlotButton->setChecked( settings.value( settingsString + QStringLiteral( "/Input-hydrograph-button-checked" ) ).toBool() );
    mOutputHydrographPlotButton->setChecked( settings.value( settingsString + QStringLiteral( "/Output-hydrograph-button-checked" ) ).toBool() );
  }
  else
  {
    mInputHydrographPlotButton->setChecked( true );
    mOutputHydrographPlotButton->setChecked( true );
  }

  ui->mHydrographTables->setConstantTimeStepParameter( mStructure2D->constantTimeStepInTable(), mStructure2D->useConstantTimeStepInTable() );
  populateHydrograph();
  connect( mStructure2D, &ReosHydraulicStructure2D::boundaryChanged, this, &ReosHydraulicStructure2DProperties::populateHydrograph );
  connect( mStructure2D, &ReosHydraulicStructure2D::simulationFinished, this, &ReosHydraulicStructure2DProperties::onSimulationFinished );
  connect( mStructure2D, &ReosHydraulicStructure2D::currentSimulationChanged, this, [this]
  {
    if ( mStructure2D->currentSimulation() )
      ui->mSimulationEngineName->setText( mStructure2D->currentSimulation()->engineName() );
  } );

  connect( mMap, &ReosMap::cursorMoved, this, &ReosHydraulicStructure2DProperties::onMapCursorMove );
}

ReosHydraulicStructure2DProperties::~ReosHydraulicStructure2DProperties()
{
  if ( !mMap.isNull() )
    mMap->removeExtraRenderedObject( mStructure2D->mesh() );

  if ( !mView3D.isNull() )
    mView3D->close();

  delete ui;
}

void ReosHydraulicStructure2DProperties::setCurrentCalculationContext( const ReosCalculationContext &context )
{
  mActionEditStructure->setEnabled( !mStructure2D->hasSimulationRunning() );
  mStructure2D->updateCalculationContext( context );
  mCalculationContext = context;

  setCurrentSimulationProcess( mStructure2D->simulationProcess( context ), context );

  if ( mStructure2D->currentSimulation() )
    ui->mSimulationEngineName->setText( mStructure2D->currentSimulation()->engineName() );

  emit calculationContextChanged();
}

void ReosHydraulicStructure2DProperties::setCurrentSimulationProcess( ReosSimulationProcess *process, const ReosCalculationContext &context )
{
  if ( !mCurrentProcess.isNull() )
    disconnect( mCurrentProcess, &ReosProcess::sendInformation, this, &ReosHydraulicStructure2DProperties::updateProgress );

  mCurrentProcess = process;

  if ( mCurrentProcess.isNull() )
  {
    if ( mStructure2D->hasResults( context ) )
    {
      fillResultGroupBox( context );
      ui->mProgressBar->setMaximum( 1 );
      ui->mProgressBar->setValue( 1 );
      const QDateTime lastRun = mStructure2D->resultsDateTime( context );
      if ( lastRun.isValid() )
        ui->mLastRunLabel->setText( QLocale().toString( lastRun, QLocale::ShortFormat ) );
      else
        ui->mLastRunLabel->setText( "-" );
    }
    else
    {
      disableResultGroupBox();
      ui->mProgressBar->setMaximum( 1 );
      ui->mProgressBar->setValue( 0 );
      ui->mLastRunLabel->setText( tr( "No existing results" ) );
    }

    ui->mPlotWidget->enableAutoScale( true );
    ui->mPlotWidget->updatePlot();
  }
  else
  {
    disableResultGroupBox();
    ui->mLastRunLabel->setText( "Simulation run in progress" );
    ui->mProgressBar->setMaximum( mCurrentProcess->maxProgression() );
    ui->mProgressBar->setValue( mCurrentProcess->currentProgression() );
    connect( mCurrentProcess, &ReosProcess::sendInformation, this, &ReosHydraulicStructure2DProperties::updateProgress );

    ui->mPlotWidget->enableAutoScaleY( true );
    ui->mPlotWidget->enableAutoScaleX( false );
    ui->mPlotWidget->setAxeXExtent( mCalculationContext.simulationStartTime(), mCalculationContext.simulationEndTime() );
    ui->mPlotWidget->updatePlot();
  }

  populateHydrograph();
}

void ReosHydraulicStructure2DProperties::disableResultGroupBox()
{
  ui->mLabelResultStartTime->setText( QString( '-' ) );
  ui->mLabelResultEndTime->setText( QString( '-' ) );
  ui->mLabelResultTimeStepCount->setText( QString( '-' ) );
  ui->mLabelResultValueDisplayed->setText( QString( '-' ) );
  ui->mLabelResultValueUnderCursor->setText( QString( '-' ) );

  ui->mGroupBoxResultInfo->setEnabled( false );
}

void ReosHydraulicStructure2DProperties::fillResultGroupBox( const ReosCalculationContext &context )
{
  ui->mGroupBoxResultInfo->setEnabled( true );

  ui->mLabelResultStartTime->setText( QLocale().toString( context.simulationStartTime(), QLocale::ShortFormat ) );
  ui->mLabelResultEndTime->setText( QLocale().toString( context.simulationEndTime(), QLocale::ShortFormat ) );
  ui->mLabelResultTimeStepCount->setText( QString::number( mStructure2D->resultsTimeStepCount( context ) ) );
  ui->mLabelResultValueDisplayed->setText( mStructure2D->currentDatasetName() );
  ui->mLabelResultValueUnderCursor->setText( QString( '-' ) );
}


void ReosHydraulicStructure2DProperties::updateProgress()
{
  if ( !mCurrentProcess.isNull() )
  {
    ui->mProgressBar->setMaximum( mCurrentProcess->maxProgression() );
    ui->mProgressBar->setValue( mCurrentProcess->currentProgression() );
  }
}

void ReosHydraulicStructure2DProperties::requestMapRefresh()
{
  mMap->refreshCanvas();
}

void ReosHydraulicStructure2DProperties::onLaunchCalculation()
{
  if ( !mStructure2D->simulationProcess( mCalculationContext ) )
  {
    if ( mStructure2D->hasResults( mCalculationContext ) )
    {
      if ( QMessageBox::warning( this, tr( "Run Simulation" ), tr( "Results exist for this modele and this hydraulic scheme.\nDo you want to overwrite this results?" ),
                                 QMessageBox::Yes | QMessageBox::No ) == QMessageBox::No )
        return;

      if ( !mStructure2D->currentSimulation() )
      {
        QMessageBox::information( this, tr( "Run Simulation" ), "No simulation selected." );
        return;
      }
    }

    mActionEditStructure->setEnabled( false );
    std::unique_ptr<ReosProcess> preparationProcess( mStructure2D->getPreparationProcessSimulation( mCalculationContext ) );
    ReosProcessControler *controler = new ReosProcessControler( preparationProcess.get(), this );
    controler->exec();

    setCurrentSimulationProcess( mStructure2D->startSimulation( mCalculationContext ), mCalculationContext );
  }

  ReosHydraulicSimulationConsole *console = new ReosHydraulicSimulationConsole( mStructure2D->simulationProcess( mCalculationContext ), mGuiContext );
  connect( this, &ReosHydraulicStructure2DProperties::calculationContextChanged, console, &ReosHydraulicSimulationConsole::backToPreviousPage );
  emit stackedPageWidgetOpened( console );
}

void ReosHydraulicStructure2DProperties::onExportSimulation()
{
  const QString dirPath = QFileDialog::getExistingDirectory( this, "Export Simulation File", QString(), QFileDialog::ShowDirsOnly );

  const QDir dir( dirPath );

  std::unique_ptr<ReosProcess> preparationProcess( mStructure2D->getPreparationProcessSimulation( mCalculationContext, dir ) );
  ReosProcessControler *controler = new ReosProcessControler( preparationProcess.get(), this );
  controler->exec();

  controler->deleteLater();
}

void ReosHydraulicStructure2DProperties::updateDatasetMenu()
{
  mScalarDatasetMenu->clear();
  const QStringList datasetIds = mStructure2D->meshDatasetIds();

  if ( mScalarDatasetActions )
    mScalarDatasetActions->deleteLater();

  mScalarDatasetActions = new QActionGroup( mScalarDatasetMenu );
  for ( const QString &id : datasetIds )
  {
    QAction *action = new QAction( mStructure2D->meshDatasetName( id ), mScalarDatasetActions );
    action->setCheckable( true );
    action->setChecked( mStructure2D->currentActivatedMeshDataset() == id );
    mScalarDatasetMenu->addAction( action );
    connect( action, &QAction::triggered, this, [id, this]( bool checked )
    {
      if ( checked )
        mStructure2D->activateResultDatasetGroup( id );
      fillResultGroupBox( mCalculationContext );
    } );
  }

  mScalarDatasetMenu->addSeparator();
  QWidgetAction *wa = new QWidgetAction( mScalarDatasetMenu );

  std::unique_ptr<ReosMeshWireframeSettingsWidget> meshSettingsWidget( new ReosMeshWireframeSettingsWidget );
  ReosMeshWireframeSettingsWidget *ptr = meshSettingsWidget.get();
  connect( meshSettingsWidget.get(), &ReosMeshWireframeSettingsWidget::changed, this, [this, ptr]
  {
    mStructure2D->mesh()->setWireFrameSettings( ptr->settings() );
  } );
  wa->setDefaultWidget( meshSettingsWidget.release() );
  mScalarDatasetMenu->addAction( wa );
  mScalarDatasetActions->setExclusive( true );

}

void ReosHydraulicStructure2DProperties::populateHydrograph()
{
  ui->mHydrographTables->clearSeries();
  mOutputHydrographPlotButton->clear();
  mInputHydrographPlotButton->clear();

  QList<ReosTimeSerieVariableTimeStep *> inList;
  QList<ReosTimeSerieVariableTimeStep *> outList;
  const QList<ReosHydraulicStructureBoundaryCondition *> boundaries = mStructure2D->boundaryConditions();

  for ( ReosHydraulicStructureBoundaryCondition *boundary : boundaries )
  {
    ReosTimeSerieVariableTimeStep *hyd = boundary->outputHydrograph();
    switch ( boundary->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        inList.append( hyd );
        mInputHydrographPlotButton->addData( hyd );
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
        if ( mCurrentProcess.isNull() )
        {
          outList.append( hyd );
          mOutputHydrographPlotButton->addData( hyd );
        }
        break;
    }
  }

  if ( !mCurrentProcess.isNull() )
  {
    const QList<ReosHydrograph *> outHyds = mCurrentProcess->outputHydrographs().values();
    for ( ReosHydrograph *hyd : outHyds )
    {
      outList.append( hyd );
      mOutputHydrographPlotButton->addData( hyd );
    }
  }

  inList.append( outList );
  ui->mHydrographTables->setSeries( inList, QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) );
}

void ReosHydraulicStructure2DProperties::onSimulationFinished()
{
  mActionEditStructure->setEnabled( !mStructure2D->hasSimulationRunning() );
  setCurrentSimulationProcess( mCurrentProcess, mCalculationContext );
}

void ReosHydraulicStructure2DProperties::onMapCursorMove( const QPointF &pos )
{
  ReosSpatialPosition position( pos, mMap->mapCrs() );

  ReosHydraulicSimulationResults::DatasetType dt = mStructure2D->currentActivatedDatasetResultType();
  double value = std::numeric_limits<double>::quiet_NaN();
  QString unit;

  if ( dt == ReosHydraulicSimulationResults::DatasetType::None )
  {
    value = mStructure2D->mesh()->datasetScalarValueAt( mStructure2D->terrainMeshDatasetId(), pos );
    unit = tr( "m" );
  }
  else
  {
    value = mStructure2D->resultsValueAt( mMap->currentTime(), position, dt, mCalculationContext );
    unit = mStructure2D->resultsUnits( dt, mCalculationContext );
  }

  if ( std::isnan( value ) )
    ui->mLabelResultValueUnderCursor->setText( tr( "No value" ) );
  else
    ui->mLabelResultValueUnderCursor->setText( QLocale().toString( value, 'f', 3 ) + ' ' + unit );
}


ReosHydraulicElementWidget *ReosHydraulicStructure2DPropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context )
{
  ReosHydraulicStructure2D *structure2D = qobject_cast<ReosHydraulicStructure2D *>( element );
  if ( structure2D )
    return new ReosHydraulicStructure2DProperties( structure2D, context );
  else
    return nullptr;
}

ReosMeshWireframeSettingsWidget::ReosMeshWireframeSettingsWidget( QWidget *parent )
  : QWidget( parent )
  , mEnableWireframeCheckBox( new QCheckBox( tr( "Enable mesh wireFrame" ), this ) )
  , mColorButton( new ReosColorButton( this ) )
  , mWidthSlider( new QSlider( Qt::Horizontal, this ) )
{
  setLayout( new QVBoxLayout );

  QHBoxLayout *topLayout = new QHBoxLayout;
  topLayout->setContentsMargins( 0, 0, 0, 0 );
  topLayout->addWidget( mEnableWireframeCheckBox );
  topLayout->addWidget( mColorButton );
  layout()->addItem( topLayout );

  mWidthSlider->setMinimum( 1 );
  mWidthSlider->setMaximum( 20 );
  layout()->addWidget( mWidthSlider );

  connect( mEnableWireframeCheckBox, &QCheckBox::stateChanged, this, [this]
  {
    mColorButton->setEnabled( mEnableWireframeCheckBox->isChecked() );
    mWidthSlider->setEnabled( mEnableWireframeCheckBox->isChecked() );

    emit changed();
  } );

  connect( mColorButton, &ReosColorButton::colorChanged, this, &ReosMeshWireframeSettingsWidget::changed );
  connect( mWidthSlider, &QSlider::valueChanged, this, &ReosMeshWireframeSettingsWidget::changed );
}

void ReosMeshWireframeSettingsWidget::setSettings( const ReosMesh::WireFrameSettings &settings )
{
  mEnableWireframeCheckBox->setChecked( settings.enabled );
  mColorButton->setColor( settings.color );
  mWidthSlider->setValue( settings.width * 20 );
}

ReosMesh::WireFrameSettings ReosMeshWireframeSettingsWidget::settings() const
{
  ReosMesh::WireFrameSettings ret;

  ret.enabled = mEnableWireframeCheckBox->isChecked();
  ret.color = mColorButton->color();
  ret.width = mWidthSlider->value() / 20.0;

  return ret;
}
