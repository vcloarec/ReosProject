/***************************************************************************
  reosedithydraulicstructure2dwidget.cpp - ReosEditHydraulicStructure2DWidget

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
#include "reosedithydraulicstructure2dwidget.h"
#include "ui_reosedithydraulicstructure2dwidget.h"

#include <QToolBar>
#include <QPushButton>

#include "reosmeshgeneratorgui.h"
#include "reosmaptooleditgeometrystructure.h"
#include "reoshydraulicstructure2d.h"
#include "reosstyleregistery.h"
#include "reosgmshresolutioncontrollerwidget.h"
#include "reoseditpolylinestructurewidget.h"
#include "reossettings.h"


ReosEditHydraulicStructure2DWidget::ReosEditHydraulicStructure2DWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosStackedPageWidget( context.parent() )
  , ui( new Ui::ReosEditStructure2DWidget )
  , mMap( context.map() )
  , mStructure2D( structure2D )
  , mMapStructureItem( context.map(), structure2D->geometryStructure() )
{
  ui->setupUi( this );

  //mesh generation setup
  QList<QAction *> meshGenerationToolBarActions;
  QAction *actionGenerateMesh = new QAction( QPixmap( QStringLiteral( ":/images/generateMesh.svg" ) ), tr( "Generate Mesh" ), this );
  connect( actionGenerateMesh, &QAction::triggered, structure2D, &ReosHydraulicStructure2D::generateMesh );
  meshGenerationToolBarActions.append( actionGenerateMesh );
  meshGenerationToolBarActions.append( new ReosParameterWidgetAction( structure2D->meshGenerator()->autoUpdateParameter(), this ) );

  ReosEditPolylineStructureWidget *structureWidget = new ReosEditPolylineStructureWidget( structure2D->geometryStructure(), ReosGuiContext( context, this ) );
  structureWidget->addToolBarActions( meshGenerationToolBarActions );
  ui->pageMeshStructure->layout()->addWidget( structureWidget );

  ReosGmshResolutionControllerWidget *resolutionWidget = new ReosGmshResolutionControllerWidget( structure2D, ReosGuiContext( context, this ) );
  resolutionWidget->addToolBarActions( meshGenerationToolBarActions );
  ui->pageMeshResolution->layout()->addWidget( resolutionWidget );

  mInitialMapStructureItem = context.mapItems( ReosHydraulicStructure2D::staticType() );
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( false );

  connect( structure2D->geometryStructure(), &ReosDataObject::dataChanged, this, [this]
  {
    mMapStructureItem.updatePosition();
    mInitialMapStructureItem->updatePosition();
  } );

  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );
  connect( ui->mOptionListWidget, &QListWidget::currentRowChanged, this, &ReosEditHydraulicStructure2DWidget::onMeshOptionListChanged );
  connect( structure2D, &ReosDataObject::dataChanged, this, [this] {mMap->refreshCanvas();} );

  ReosSettings settings;
  ui->mOptionListWidget->setCurrentRow( settings.value( QStringLiteral( "/hydraulic-structure/edit-widget/current-row" ) ).toInt() );
}

ReosEditHydraulicStructure2DWidget::~ReosEditHydraulicStructure2DWidget()
{
  delete ui;
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( true );
}

void ReosEditHydraulicStructure2DWidget::onMeshOptionListChanged( int row )
{
  ui->mStackedWidget->setCurrentIndex( row );

  switch ( row )
  {
    case 0:
      mMapStructureItem.setDomainBaseWidth( 5 );
      break;
    case 1:
      mMapStructureItem.setDomainBaseWidth( 2 );
      break;
    default:
      break;
  }

  ReosSettings settings;
  settings.setValue( QStringLiteral( "/hydraulic-structure/edit-widget/current-row" ), row );
}

