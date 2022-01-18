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

#include <QToolBar>
#include <QPushButton>
#include <QDebug>

#include "reosmaptooleditgeometrystructure.h"
#include "reoshydraulicstructure2d.h"

ReosEditStructure2DWidget::ReosEditStructure2DWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosStackedPageWidget( context.parent() )
  , ui( new Ui::ReosEditStructure2DWidget )
  , mActionEditLine( new QAction( tr( "Edit Structure Line" ), this ) )
  , mMapToolEditLine( new ReosMapToolEditGeometryStructure( structure2D->geometryStructure(), this, context.map() ) )
  , mMapStructureItem( context.map(), structure2D->geometryStructure() )
{
  ui->setupUi( this );

  QPushButton *backButton = new QPushButton( tr( "Back" ), this );
  layout()->addWidget( backButton );
  connect( backButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  QToolBar *toolBar = new QToolBar( this );
  layout()->addWidget( toolBar );

  toolBar->addAction( mActionEditLine );
  mActionEditLine->setCheckable( true );
  mMapToolEditLine->setAction( mActionEditLine );

  connect( structure2D->geometryStructure(), &ReosDataObject::dataChanged, this, [this, structure2D, context]
  {
    structure2D->generateMesh();
    mMapStructureItem.updatePosition();
    mInitialMapStructureItem->updatePosition();

    context.map()->refreshCanvas();
  } );

  mInitialMapStructureItem = context.mapItems( ReosHydraulicStructure2D::staticType() );
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( false );

  mUndoStack = structure2D->geometryStructure()->undoStack();
  toolBar->addAction( mUndoStack->createUndoAction( this ) );
  toolBar->addAction( mUndoStack->createRedoAction( this ) );
}

ReosEditStructure2DWidget::~ReosEditStructure2DWidget()
{
  delete ui;
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( true );
}
