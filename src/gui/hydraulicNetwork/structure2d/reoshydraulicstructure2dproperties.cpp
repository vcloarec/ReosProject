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
#include "reosmeshvectorrenderingwidget.h"
#include "reoscolorbutton.h"
#include "reosprocesscontroler.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosplotitemlist.h"
#include "reoshydraulic2dsimulationwidget.h"
#include "reoshydraulicschemewidget.h"
#include "reoshydraulicstructureresultexport.h"
#include "reoshydraulicstructureprofileswidget.h"


class DatasetSettingsWidgetAction : public QWidgetAction
{
  public:
    DatasetSettingsWidgetAction( QObject *parent, QMenu *datasetMenu )
      : QWidgetAction( parent )
      , mMenu( datasetMenu )
    {}

    void setPixmap( const QPixmap &pm )
    {
      mPixMap = pm;
    }
    void setToolTip( const QString &toolTip )
    {
      mToolTip = toolTip;
    }

  protected:
    QWidget *createWidget( QWidget *parent ) override
    {
      QToolButton *button = new QToolButton( parent );
      button->setIcon( mPixMap );
      button->setPopupMode( QToolButton::InstantPopup );
      button->setMenu( mMenu );
      button->setToolTip( mToolTip );
      return button;
    }

  private:
    QMenu *mMenu = nullptr;
    QPixmap mPixMap;
    QString mToolTip;
};

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
  , mActionProfiles( new QAction( tr( "Profiles" ), this ) )
  , mActionExportAsMesh( new QAction( QPixmap( ":/images/exportToQGIS.svg" ), tr( "Export as Mesh to QGIS Project" ), this ) )
  , mScalarDatasetMenu( new QMenu( this ) )
  , mVectorDatasetMenu( new QMenu( this ) )
  , mActionScalarSettings( new QAction( QPixmap( QStringLiteral( ":/images/scalarContour.svg" ) ), tr( "Color Ramp" ), this ) )
  , mActionVectorSettings( new QAction( QPixmap( QStringLiteral( ":/images/vectorSettings.svg" ) ), tr( "Vector Settings" ), this ) )
  , mGuiContext( context, this )
{
  ui->setupUi( this );

  mGuiContext.addAction( mAction3DView );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mAction3DView );

  connect( mActionEditStructure, &QAction::triggered, this, [this]
  {
    ReosEditHydraulicStructure2DWidget *editWidget = new ReosEditHydraulicStructure2DWidget( mStructure2D, mGuiContext );
    connect( editWidget, &ReosEditHydraulicStructure2DWidget::hidden, this, &ReosHydraulicStructure2DProperties::restoreResults );
    mScalarWidgetAction->setEnabled( false );
    mVectorWidgetAction->setEnabled( false );
    emit stackedPageWidgetOpened( editWidget );
    emit askForShow();
  } );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mActionEditStructure );

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
  mView3D->setMapSettings( mStructure2D->map3dSettings(), false );
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
  simulationToolButton->setEnabled( mStructure2D->currentSimulation() != nullptr );
  connect( mStructure2D, &ReosHydraulicStructure2D::currentSimulationChanged, this, [this, simulationToolButton]
  {
    simulationToolButton->setEnabled( mStructure2D->currentSimulation() != nullptr );
  } );

  toolBar->addSeparator();

  mScalarWidgetAction = new DatasetSettingsWidgetAction( this, mScalarDatasetMenu );
  mScalarWidgetAction->setToolTip( tr( "Scalar results settings" ) );
  mScalarWidgetAction->setPixmap( QPixmap( QStringLiteral( ":/images/scalarContour.svg" ) ) );
  toolBar->addAction( mScalarWidgetAction );
  connect( mActionScalarSettings, &QAction::triggered, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosMeshScalarRenderingWidget( mStructure2D->mesh(), mStructure2D->currentActivatedMeshDataset(), true, mGuiContext ) );
    emit askForShow();
  } );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mScalarWidgetAction );


  mVectorWidgetAction = new DatasetSettingsWidgetAction( this, mVectorDatasetMenu );
  mVectorWidgetAction->setToolTip( tr( "Vector results settings" ) );
  mVectorWidgetAction->setPixmap( QPixmap( QStringLiteral( ":/images/vectorSettings.svg" ) ) );
  toolBar->addAction( mVectorWidgetAction );
  mCurrentVectorDatasetId = mStructure2D->currentActivatedVectorMeshDataset();
  connect( mActionVectorSettings, &QAction::triggered, this, [this]
  {
    if ( mStructure2D->currentActivatedVectorMeshDataset().isEmpty() )
      return;
    emit stackedPageWidgetOpened( new ReosMeshVectorRenderingWidget( mStructure2D->mesh(), mStructure2D->currentActivatedVectorMeshDataset(), mGuiContext ) );
    emit askForShow();
  } );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mVectorWidgetAction );

  connect( mActionProfiles, &QAction::triggered, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosHydraulicStructureProfilesWidget( mStructure2D, mGuiContext ) );
    emit askForShow();
  } );
  toolBar->addAction( mActionProfiles );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mActionProfiles );

  toolBar->addAction( mAction3DView );

  toolBar->addSeparator();

  toolBar->addAction( mActionExportAsMesh );

  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  toolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBoxLayout->addWidget( toolBar );

  mMap->addExtraRenderedObject( mStructure2D->mesh() );
  connect( mStructure2D->mesh(), &ReosMesh::repaintRequested, this, &ReosHydraulicStructure2DProperties::requestMapRefresh );

  updateDatasetMenus();

  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mActionEditStructure );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mActionRunSimulation );
  mGuiContext.addActionToMainToolBar( QStringLiteral( "hydraulic-network" ), mActionExportAsMesh );

  connect( mStructure2D, &ReosHydraulicStructure2D::simulationResultChanged, this, &ReosHydraulicStructure2DProperties::updateDatasetMenus );

  QString settingsString = QStringLiteral( "hydraulic-network-structure-2D" );

  ui->mPlotWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAxeYRight( false );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mPlotWidget->enableTimeLine( true );
  ui->mPlotWidget->setSettingsContext( settingsString );

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


  connect( mActionExportAsMesh, &QAction::triggered, this, [this]
  {
    QDialog *dial = new ReosHydraulicStructureResultExport( mStructure2D, mCalculationContext.schemeId(), this );
    dial->exec();
  } );
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
  mCurrentDatasetId = mStructure2D->currentActivatedMeshDataset();
  mCurrentVectorDatasetId = mStructure2D->currentActivatedVectorMeshDataset();
  mCalculationContext = context;

  setCurrentSimulationProcess( mStructure2D->simulationProcess( context ), context );

  if ( mStructure2D->currentSimulation() )
    ui->mSimulationEngineName->setText( mStructure2D->currentSimulation()->engineName() );

  emit calculationContextChanged();
}

void ReosHydraulicStructure2DProperties::setTime( const QDateTime &time )
{
  ui->mPlotWidget->setTime( time );
}

void ReosHydraulicStructure2DProperties::setCurrentSimulationProcess( ReosSimulationProcess *process, const ReosCalculationContext &context )
{
  if ( !mCurrentProcess.isNull() )
    disconnect( mCurrentProcess, &ReosProcess::sendInformation, this, &ReosHydraulicStructure2DProperties::updateProgress );

  mCurrentProcess = process;

  if ( mCurrentProcess.isNull() )
  {
    if ( mStructure2D->hasResults( context.schemeId() ) )
    {
      fillResultGroupBox( context );
      ui->mProgressBar->setMaximum( 1 );
      ui->mProgressBar->setValue( 1 );
      const QDateTime lastRun = mStructure2D->resultsRunDateTime( context.schemeId() );
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
  ui->mLabelResultTimeStepCount->setText( QString::number( mStructure2D->resultsTimeStepCount( context.schemeId() ) ) );
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
    if ( mStructure2D->hasResults( mCalculationContext.schemeId() ) )
    {
      if ( QMessageBox::warning( this, tr( "Run Simulation" ), tr( "Results exist for this modele and this hydraulic scheme.\nDo you want to overwrite this results?" ),
                                 QMessageBox::Yes | QMessageBox::No ) == QMessageBox::No )
        return;
    }

    if ( !mStructure2D->currentSimulation() )
    {
      QMessageBox::information( this, tr( "Run Simulation" ), tr( "No simulation selected." ) );
      return;
    }

    QString error;

    mStructure2D->removeResults( mCalculationContext );
    mStructure2D->updateResults( mCalculationContext.schemeId() );

    std::unique_ptr<ReosProcess> preparationProcess( mStructure2D->getPreparationProcessSimulation( mCalculationContext, error ) );
    if ( !preparationProcess )
    {
      QMessageBox::warning( this, tr( "Run Simulation" ), tr( "Simulation did not start for following reason:\n\n%1" ).arg( error ) );
      return;
    }

    ReosProcessControler *controler = new ReosProcessControler( preparationProcess.get(), this );
    controler->exec();

    if ( !preparationProcess->isSuccessful() )
    {
      QMessageBox::warning( this, tr( "Run Simulation" ), tr( "Something get wrong during simulation preparation.\nPlease, check the model." ) );
      return;
    }

    ReosSimulationProcess *process = mStructure2D->startSimulation( mCalculationContext, error );
    if ( process )
      setCurrentSimulationProcess( process, mCalculationContext );
    else
    {
      QMessageBox::warning( this, tr( "Run Simulation" ), tr( "Simulation did not start for following reason:\n\n%1" ).arg( error ) );
      return;
    }

    mActionEditStructure->setEnabled( false );
  }

  ReosHydraulicSimulationConsole *console = new ReosHydraulicSimulationConsole( mStructure2D->simulationProcess( mCalculationContext ), mGuiContext );
  connect( this, &ReosHydraulicStructure2DProperties::calculationContextChanged, console, &ReosHydraulicSimulationConsole::backToPreviousPage );
  emit stackedPageWidgetOpened( console );
  emit askForShow();
}

void ReosHydraulicStructure2DProperties::onExportSimulation()
{
  const QString dirPath = QFileDialog::getExistingDirectory( this, "Export Simulation File", QString(), QFileDialog::ShowDirsOnly );

  const QDir dir( dirPath );

  QString error;
  std::unique_ptr<ReosProcess> preparationProcess( mStructure2D->getPreparationProcessSimulation( mCalculationContext, error, dir ) );
  if ( !preparationProcess )
  {
    QMessageBox::warning( this, tr( "Export Simulation" ), tr( "Simulation can't be exported for following reason:\n\n%1" ).arg( error ) );
    return;
  }
  ReosProcessControler *controler = new ReosProcessControler( preparationProcess.get(), this );
  controler->exec();

  controler->deleteLater();
}


void ReosHydraulicStructure2DProperties::updateScalarDatasetMenu()
{
  mScalarDatasetMenu->clear();
  const QStringList datasetIds = mStructure2D->meshDatasetIds();

  if ( mScalarDatasetActions )
    mScalarDatasetActions->deleteLater();

  mScalarDatasetActions = new QActionGroup( mScalarDatasetMenu );
  bool hasDatasetChecked = false;
  for ( const QString &id : datasetIds )
  {
    QAction *action = new QAction( mStructure2D->meshDatasetName( id ), mScalarDatasetActions );
    action->setCheckable( true );
    bool hasToBeChecked = mCurrentDatasetId == id;
    hasDatasetChecked |= hasToBeChecked;
    action->setChecked( hasToBeChecked );
    mScalarDatasetMenu->addAction( action );
    connect( action, &QAction::triggered, this, [id, this]( bool checked )
    {
      if ( checked )
      {
        mCurrentDatasetId = id;
        restoreResults();
      }
    } );
  }
  QAction *actionNone = new QAction( tr( "None" ), mScalarDatasetActions );
  actionNone->setCheckable( true );
  actionNone->setChecked( ! hasDatasetChecked );
  mScalarDatasetMenu->addAction( actionNone );
  connect( actionNone, &QAction::triggered, this, [ this]( bool checked )
  {
    if ( checked )
    {
      mCurrentDatasetId = QString();
      mStructure2D->activateResultDatasetGroup( QString() );
      mActionScalarSettings->setEnabled( false );
    }
  } );
  mScalarDatasetMenu->addSeparator();
  mScalarDatasetMenu->addAction( mActionScalarSettings );
  mScalarDatasetMenu->addSeparator();
  QWidgetAction *wa = new QWidgetAction( mScalarDatasetMenu );

  std::unique_ptr<ReosMeshWireframeSettingsWidget> meshSettingsWidget( new ReosMeshWireframeSettingsWidget );
  meshSettingsWidget->setSettings( mStructure2D->mesh()->wireFrameSettings() );
  ReosMeshWireframeSettingsWidget *ptr = meshSettingsWidget.get();
  connect( meshSettingsWidget.get(), &ReosMeshWireframeSettingsWidget::changed, this, [this, ptr]
  {
    mStructure2D->mesh()->setWireFrameSettings( ptr->settings(), true );
  } );
  wa->setDefaultWidget( meshSettingsWidget.release() );
  mScalarDatasetMenu->addAction( wa );
  mScalarDatasetActions->setExclusive( true );

  mActionScalarSettings->setEnabled( !mCurrentDatasetId.isEmpty() );
}

void ReosHydraulicStructure2DProperties::updateVectorDatasetMenu()
{
  mVectorDatasetMenu->clear();
  const QStringList datasetIds = mStructure2D->meshVectorDatasetIds();

  if ( mVectorDatasetActions )
    mVectorDatasetActions->deleteLater();

  mVectorDatasetActions = new QActionGroup( mVectorDatasetMenu );
  mVectorDatasetActions->setExclusive( true );
  for ( const QString &id : datasetIds )
  {
    QAction *action = new QAction( mStructure2D->meshDatasetName( id ), mVectorDatasetActions );
    action->setCheckable( true );
    action->setChecked( mCurrentVectorDatasetId == id );
    mVectorDatasetMenu->addAction( action );
    connect( action, &QAction::triggered, this, [id, this]( bool checked )
    {
      if ( checked )
      {
        mCurrentVectorDatasetId = id;
        restoreResults();
        mActionVectorSettings->setEnabled( true );
      }
    } );
  }

  QAction *actionNone = new QAction( tr( "None" ), mVectorDatasetActions );
  actionNone->setCheckable( true );
  actionNone->setChecked( mCurrentVectorDatasetId.isEmpty() );
  mVectorDatasetMenu->addAction( actionNone );
  connect( actionNone, &QAction::triggered, this, [ this]( bool checked )
  {
    if ( checked )
    {
      mCurrentVectorDatasetId = QString();
      mStructure2D->activateResultVectorDatasetGroup( QString() );
      mActionVectorSettings->setEnabled( false );
    }
  } );
  mVectorDatasetMenu->addSeparator();
  mVectorDatasetMenu->addAction( mActionVectorSettings );

  mActionVectorSettings->setEnabled( !mCurrentVectorDatasetId.isEmpty() );
}

void ReosHydraulicStructure2DProperties::restoreResults()
{
  mStructure2D->updateResults( mCalculationContext.schemeId() );
  mStructure2D->activateResultDatasetGroup( mCurrentDatasetId );
  mStructure2D->activateResultVectorDatasetGroup( mCurrentVectorDatasetId );
  fillResultGroupBox( mCalculationContext );
  updateDatasetMenus();
  mScalarWidgetAction->setEnabled( true );
  mVectorWidgetAction->setEnabled( true );
}


void ReosHydraulicStructure2DProperties::updateDatasetMenus()
{
  updateScalarDatasetMenu();
  updateVectorDatasetMenu();
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
    value = mStructure2D->resultsValueAt( mMap->currentTime(), position, dt, mCalculationContext.schemeId() );
    unit = mStructure2D->resultsUnits( dt, mCalculationContext.schemeId() );
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
