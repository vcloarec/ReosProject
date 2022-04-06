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

#include "reosedithydraulicstructure2dwidget.h"
#include "reos3dview.h"
#include "reoshydraulicsimulationconsole.h"
#include "reosstyleregistery.h"
#include "reosmeshscalarrenderingwidget.h"

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

  connect( mActionEditStructure, &QAction::triggered, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosEditHydraulicStructure2DWidget( mStructure2D, mGuiContext ) );
  } );

  connect( mActionRunSimulation, &QAction::triggered, this, &ReosHydraulicStructure2DProperties::onLaunchCalculation );

  mView3D = new Reos3dView( mStructure2D->mesh(), context.map()->mapCanvas() );
  mView3D->setAction( mAction3DView );
  mAction3DView->setCheckable( true );
  mView3D->setMapSettings( mStructure2D->map3dSettings() );
  mView3D->setTerrainSettings( mStructure2D->terrain3DSettings() );
  connect( mView3D, &Reos3dView::mapSettingsChanged, this, [this]
  {
    mStructure2D->setMap3dSettings( mView3D->map3DSettings() );
  } );

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
}

ReosHydraulicStructure2DProperties::~ReosHydraulicStructure2DProperties()
{
  if ( !mMap.isNull() )
    mMap->removeExtraRenderedObject( mStructure2D->mesh() );

  if ( !mView3D.isNull() )
  {
    mView3D->close();
    mView3D->deleteLater();
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
  mActionEditStructure->setEnabled( false );

  if ( !mStructure2D->currentProcess() )
  {
    ReosSimulationProcess *process = mStructure2D->startSimulation();
    connect( process, &ReosProcess::finished, mActionEditStructure, [this] {mActionEditStructure->setEnabled( true );} );
  }

  emit stackedPageWidgetOpened( new ReosHydraulicSimulationConsole( mStructure2D->currentProcess(), mGuiContext ) );
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
  mScalarDatasetActions->setExclusive( true );

}

ReosHydraulicElementWidget *ReosHydraulicStructure2DPropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context )
{
  ReosHydraulicStructure2D *structure2D = qobject_cast<ReosHydraulicStructure2D *>( element );
  if ( structure2D )
    return new ReosHydraulicStructure2DProperties( structure2D, context );
  else
    return nullptr;
}
