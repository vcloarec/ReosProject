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

#include "reosedithydraulicstructure2dwidget.h"
#include "reos3dview.h"
#include "reoshydraulicsimulationconsole.h"

ReosHydraulicStructure2DProperties::ReosHydraulicStructure2DProperties( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosHydraulicElementWidget( context.parent() )
  , ui( new Ui::ReosHydraulicStructure2DProperties )
  , mStructure2D( structure2D )
  , mMap( context.map() )
  , mAction3DView( new QAction( QPixmap( QStringLiteral( ":/images/view3D.svg" ) ), tr( "3D View" ), this ) )
  , mGuiContext( context, this )
{
  ui->setupUi( this );

  mGuiContext.addAction( mAction3DView );

  connect( ui->mEditStructureToolButton, &QToolButton::clicked, this, [this]
  {
    emit stackedPageWidgetOpened( new ReosEditHydraulicStructure2DWidget( mStructure2D, mGuiContext ) );
  } );

  connect( ui->mRunSimulationToolButton, &QToolButton::clicked, this, &ReosHydraulicStructure2DProperties::onLaunchCalculation );

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
  layout()->addWidget( toolBar );
  toolBar->addAction( mAction3DView );

  mMap->addExtraRenderedObject( mStructure2D->mesh() );
  connect( mStructure2D->mesh(), &ReosMesh::repaintRequested, this, &ReosHydraulicStructure2DProperties::requestMapRefresh );
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
  ui->mEditStructureToolButton->setEnabled( false );

  if ( !mStructure2D->currentProcess() )
  {
    ReosSimulationProcess *process = mStructure2D->startSimulation( mCalculationContext );
    connect( process, &ReosProcess::finished, ui->mEditStructureToolButton, [this] {ui->mEditStructureToolButton->setEnabled( true );} );
  }

  emit stackedPageWidgetOpened( new ReosHydraulicSimulationConsole( mStructure2D->currentProcess(), mGuiContext ) );
}

ReosHydraulicElementWidget *ReosHydraulicStructure2DPropertiesWidgetFactory::createWidget( ReosHydraulicNetworkElement *element, const ReosGuiContext &context )
{
  ReosHydraulicStructure2D *structure2D = qobject_cast<ReosHydraulicStructure2D *>( element );
  if ( structure2D )
    return new ReosHydraulicStructure2DProperties( structure2D, context );
  else
    return nullptr;
}
