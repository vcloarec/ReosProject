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

#include "reosedithydraulicstructure2dwidget.h"
#include "reos3dview.h"
#include "reoshydraulicsimulationconsole.h"
#include "reosstyleregistery.h"
#include "reosmeshscalarrenderingwidget.h"
#include "reoscolorbutton.h"
#include "reosprocesscontroler.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosplotitemlist.h"


ReosHydraulicStructure2DProperties::ReosHydraulicStructure2DProperties( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosHydraulicElementWidget( context.parent() )
  , ui( new Ui::ReosHydraulicStructure2DProperties )
  , mStructure2D( structure2D )
  , mMap( context.map() )
  , mActionEditStructure( new QAction( QPixmap( QStringLiteral( ":/images/settings.svg" ) ), tr( "Edit Model" ), this ) )
  , mActionRunSimulation( new QAction( QPixmap( QStringLiteral( ":/images/runModel.svg" ) ), tr( "Run Simulation" ), this ) )
  , mAction3DView( new QAction( QPixmap( QStringLiteral( ":/images/view3D.svg" ) ), tr( "3D View" ), this ) )
  , mScalarDatasetMenu( new QMenu( this ) )
  , mGuiContext( context, this )
{
  ui->setupUi( this );

  mGuiContext.addAction( mAction3DView );

  mActionEditStructure->setEnabled( mStructure2D->currentSimulationProcess() == nullptr );
  connect( mActionEditStructure, &QAction::triggered, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosEditHydraulicStructure2DWidget( mStructure2D, mGuiContext ) );
  } );

  connect( mActionRunSimulation, &QAction::triggered, this, &ReosHydraulicStructure2DProperties::onLaunchCalculation );

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
  toolBar->addAction( mActionRunSimulation );
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

  connect( mStructure2D, &ReosHydraulicStructure2D::simulationResultChanged, this, &ReosHydraulicStructure2DProperties::updateDatasetMenu );

  QString settingsString = QStringLiteral( "hydraulic-network-structure-2D" );

  ui->mPlotWidget->setSettingsContext( settingsString );
  ui->mPlotWidget->setTitleAxeX( tr( "Time" ) );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAxeYright( false );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );

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
}

ReosHydraulicStructure2DProperties::~ReosHydraulicStructure2DProperties()
{
  if ( !mMap.isNull() )
    mMap->removeExtraRenderedObject( mStructure2D->mesh() );

  if ( !mView3D.isNull() )
  {
    mView3D->close();
  }
  delete ui;
}

void ReosHydraulicStructure2DProperties::setCurrentCalculationContext( const ReosCalculationContext &context )
{
  mStructure2D->updateCalculationContext( context );
  mCalculationContext = context;
}

void ReosHydraulicStructure2DProperties::requestMapRefresh()
{
  mMap->refreshCanvas();
}

void ReosHydraulicStructure2DProperties::onLaunchCalculation()
{
  if ( !mStructure2D->currentSimulationProcess() )
  {
    mActionEditStructure->setEnabled( false );

    std::unique_ptr<ReosProcess> preparationProcess( mStructure2D->getPreparationProcessSimulation( mCalculationContext ) );
    ReosProcessControler *controler = new ReosProcessControler( preparationProcess.get(), this );
    controler->exec();

    ReosSimulationProcess *process = mStructure2D->startSimulation( mCalculationContext );

    if ( process )
      connect( process, &ReosProcess::finished, mActionEditStructure, [this] {mActionEditStructure->setEnabled( true );} );
  }

  emit stackedPageWidgetOpened( new ReosHydraulicSimulationConsole( mStructure2D->currentSimulationProcess(), mGuiContext ) );
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
      {
        mStructure2D->activateResultDatasetGroup( id );
      }
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
        outList.append( hyd );
        mOutputHydrographPlotButton->addData( hyd );
        break;
    }
  }

  inList.append( outList );
  ui->mHydrographTables->setSeries( inList, QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) );
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

  QHBoxLayout *topLayout = new QHBoxLayout( this );
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
