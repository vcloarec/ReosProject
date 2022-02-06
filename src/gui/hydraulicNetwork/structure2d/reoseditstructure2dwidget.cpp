/***************************************************************************
  reoseditstructure2dwidget.cpp - ReosEditStructure2DWidget

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
#include "reoseditstructure2dwidget.h"
#include "ui_reoseditstructure2dwidget.h"
#include "ui_reoseditstructuregeometry2dwidget.h"

#include <QToolBar>
#include <QPushButton>
#include <QDebug>

#include "reosmaptooleditgeometrystructure.h"
#include "reoshydraulicstructure2d.h"
#include "reosstyleregistery.h"
#include "reosgmshresolutioncontrollerwidget.h"

ReosEditStructureGeometry2DWidget::ReosEditStructureGeometry2DWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : QWidget( context.parent() )
  , ui( new Ui::ReosEditStructureGeometry2DWidget )
  , mActionEditLine( new QAction( QPixmap( QStringLiteral( ":/images/editStructureLines.svg" ) ), tr( "Edit Structure Line" ), this ) )
  , mMapToolEditLine( new ReosMapToolEditGeometryStructure( structure2D->geometryStructure(), this, context.map() ) )
{
  ui->setupUi( this );

  QToolBar *toolBar = new QToolBar( this );
  ui->mToolBarWidget->layout()->addWidget( toolBar );

  toolBar->addAction( mActionEditLine );
  mActionEditLine->setCheckable( true );
  mMapToolEditLine->setAction( mActionEditLine );

  toolBar->addActions( mMapToolEditLine->mainActions()->actions() );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  ui->mAutoUpdateMesh->setBooleanParameter( structure2D->autoMeshUpdate() );

  connect( ui->mAutoUpdateMesh, &ReosParameterWidget::valueChanged, this, [this]
  {
    ui->mGenerateMeshButton->setEnabled( !ui->mAutoUpdateMesh->booleanParameter()->value() );
  } );

  connect( ui->mGenerateMeshButton, &QToolButton::clicked, structure2D, [structure2D, context]
  {
    structure2D->generateMesh();
    context.map()->refreshCanvas();
  } );

  ui->mGenerateMeshButton->setEnabled( !ui->mAutoUpdateMesh->booleanParameter()->value() );
}

ReosEditStructureGeometry2DWidget::~ReosEditStructureGeometry2DWidget()
{
  delete ui;
}

void ReosEditStructureGeometry2DWidget::hideEvent( QHideEvent *e )
{
  mMapToolEditLine->quitMap();
  QWidget::hideEvent( e );
}

void ReosEditStructureGeometry2DWidget::showEvent( QShowEvent *e )
{
  mMapToolEditLine->activate();
  mMapToolEditLine->setCurrentToolInMap();
  QWidget::showEvent( e );
}

ReosEditStructure2DWidget::ReosEditStructure2DWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosStackedPageWidget( context.parent() )
  , ui( new Ui::ReosEditStructure2DWidget )
  , mMap( context.map() )
  , mStructure2D( structure2D )
  , mMapStructureItem( context.map(), structure2D->geometryStructure() )
{
  ui->setupUi( this );
  ui->pageMeshStructure->layout()->addWidget( new ReosEditStructureGeometry2DWidget( structure2D, ReosGuiContext( context, this ) ) );
  ui->pageMeshResolution->layout()->addWidget( new ReosGmshResolutionControllerWidget( structure2D->meshResolutionController(), this ) );

  mInitialMapStructureItem = context.mapItems( ReosHydraulicStructure2D::staticType() );
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( false );

  connect( structure2D->geometryStructure(), &ReosDataObject::dataChanged, this, [this, context]
  {
    mMapStructureItem.updatePosition();
    mInitialMapStructureItem->updatePosition();
  } );

  connect( structure2D->geometryStructure(), &ReosDataObject::dataChanged, this, &ReosEditStructure2DWidget::autoGenerateMesh );
  connect( structure2D->meshResolutionController(), &ReosDataObject::dataChanged, this, &ReosEditStructure2DWidget::autoGenerateMesh );

  connect( ui->mBackButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );
  connect( ui->mOptionListWidget, &QListWidget::currentRowChanged, this, &ReosEditStructure2DWidget::onMeshOptionListChanged );
}

ReosEditStructure2DWidget::~ReosEditStructure2DWidget()
{
  delete ui;
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( true );
}

void ReosEditStructure2DWidget::onMeshOptionListChanged( int row )
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
}

void ReosEditStructure2DWidget::autoGenerateMesh()
{
  if ( mStructure2D->autoMeshUpdate()->value() )
    mStructure2D->generateMesh();
  mMap->refreshCanvas();
}
