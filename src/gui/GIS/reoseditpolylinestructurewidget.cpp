/***************************************************************************
  reoseditpolylinestructurewidget.cpp - ReosEditPolylineStructureWidget

 ---------------------
 begin                : 13.2.2022
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
#include "reoseditpolylinestructurewidget.h"
#include "ui_reoseditpolylinestructurewidget.h"

#include <QAction>
#include <QToolBar>
#include <QMessageBox>

#include "reosformwidget.h"
#include "reosstyleregistery.h"
#include "reosmaptooleditgeometrystructure.h"
#include "reospolylinesstructure.h"
#include "reosmaptooleditgeometrystructure.h"

ReosEditPolylineStructureWidget::ReosEditPolylineStructureWidget( ReosPolylinesStructure *structure, const ReosGuiContext &context )
  : QWidget( context.parent() )
  , ui( new Ui::ReosEditPolylineStructureWidget )
  , mActionEditLine( new QAction( QIcon( QStringLiteral( ":/images/editStructureLines.svg" ) ), tr( "Edit Structure Line" ), this ) )
  , mStructure( structure )
  , mMapToolEditLine( new ReosMapToolEditPolylineStructure( structure, this, context.map() ) )
  , mActionRemoveBoundary( new QAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Selected boundary" ), this ) )
  , mActionZoomOnBoundary( new QAction( QIcon( QStringLiteral( ":/images/zoom.svg" ) ), tr( "Zoom on Selected Boundary" ), this ) )
  , mActionRenameBoundary( new QAction( QIcon( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Selected Boundary" ), this ) )
  , mGuiContext( context )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  mToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBarWidget->layout()->addWidget( mToolBar );

  mToolBar->addAction( mActionEditLine );
  mActionEditLine->setCheckable( true );
  mMapToolEditLine->setAction( mActionEditLine );

  mToolBar->addActions( mMapToolEditLine->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );

  QToolBar *toolBarBoundary = new QToolBar( this );
  ui->mBoundaryToolBarLayout->addWidget( toolBarBoundary );
  toolBarBoundary->layout()->setContentsMargins( 0, 0, 0, 0 );
  toolBarBoundary->addAction( mActionRemoveBoundary );
  toolBarBoundary->addAction( mActionZoomOnBoundary );
  toolBarBoundary->addAction( mActionRenameBoundary );
  toolBarBoundary->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );

  mBoundaryModel = new ReosPolylineStructureClassModelList( structure, this );
  ui->mBoundaryListView->setModel( mBoundaryModel );

  mActionRemoveBoundary->setEnabled( ui->mBoundaryListView->currentIndex().isValid() );
  mActionZoomOnBoundary->setEnabled( ui->mBoundaryListView->currentIndex().isValid() );
  mActionRenameBoundary->setEnabled( ui->mBoundaryListView->currentIndex().isValid() );
  connect( ui->mBoundaryListView->selectionModel(), &QItemSelectionModel::currentChanged, this, [this]
  {
    mActionRemoveBoundary->setEnabled( ui->mBoundaryListView->currentIndex().isValid() );
    mActionZoomOnBoundary->setEnabled( ui->mBoundaryListView->currentIndex().isValid() );
    mActionRenameBoundary->setEnabled( ui->mBoundaryListView->currentIndex().isValid() );
    mStructure->setSelectedClass( mBoundaryModel->classId( ui->mBoundaryListView->currentIndex().row() ) );
    emit boundaryConditionSelectionChanged();
  } );

  connect( mActionRenameBoundary, &QAction::triggered, this, &ReosEditPolylineStructureWidget::onRenameBoundary );
  connect( mActionRemoveBoundary, &QAction::triggered, this, &ReosEditPolylineStructureWidget::onRemoveBoundary );
  connect( mActionZoomOnBoundary, &QAction::triggered, this, &ReosEditPolylineStructureWidget::onZoomOnBoundaryCondition );
}

ReosEditPolylineStructureWidget::~ReosEditPolylineStructureWidget()
{
  delete ui;
}

void ReosEditPolylineStructureWidget::addToolBarActions( const QList<QAction *> actions )
{
  mToolBar->addActions( actions );
}

void ReosEditPolylineStructureWidget::setSettingsWidget( QWidget *widget )
{
  ui->mSettingsLayout->addWidget( widget );
}

void ReosEditPolylineStructureWidget::setInformationWidget( QWidget *widget )
{
  ui->mInformationLayout->addWidget( widget );
}

void ReosEditPolylineStructureWidget::hideEvent( QHideEvent *e )
{
  mMapToolEditLine->quitMap();
  QWidget::hideEvent( e );
}

void ReosEditPolylineStructureWidget::showEvent( QShowEvent *e )
{
  mMapToolEditLine->activate();
  mMapToolEditLine->setCurrentToolInMap();
  QWidget::showEvent( e );
}

void ReosEditPolylineStructureWidget::onRenameBoundary()
{
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Rename Boundary condition" ) );
  ReosParameterString newName( tr( "New boundary condition name" ), false );
  QString classId = mBoundaryModel->classId( ui->mBoundaryListView->currentIndex().row() );
  newName.setValue( mStructure->value( classId ).toString() );
  dial->addParameter( &newName );
  if ( dial->exec() )
  {
    if ( newName.value() != mStructure->value( classId ).toString() )
      mStructure->changeClassValue( classId, newName.value() );
  }

  dial->deleteLater();
}

void ReosEditPolylineStructureWidget::onRemoveBoundary()
{
  QModelIndex modelIndex = ui->mBoundaryListView->currentIndex();
  if ( modelIndex.isValid() )
  {
    QString classId = mBoundaryModel->classId( ui->mBoundaryListView->currentIndex().row() );
    mStructure->removeBoundaryCondition( classId );
  }
}

void ReosEditPolylineStructureWidget::onZoomOnBoundaryCondition()
{
  QString classId = mBoundaryModel->classId( ui->mBoundaryListView->currentIndex().row() );
  QRectF extent = mStructure->classExtent( classId, mGuiContext.map()->mapCrs() );
  extent.adjust( -extent.width() / 10, -extent.height() / 10, extent.width() / 10, extent.height() / 10 );
  mGuiContext.map()->setExtent( ReosMapExtent( extent ) );
}
