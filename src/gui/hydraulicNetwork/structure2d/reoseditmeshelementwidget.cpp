/***************************************************************************
  reoseditmeshelementwidget.cpp - ReosEditMeshElementWidget

 ---------------------
 begin                : 9.3.2022
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
#include "reoseditmeshelementwidget.h"
#include "ui_reoseditmeshelementwidget.h"

#include "reosmesh.h"
#include "reosmaptooleditmeshframe.h"
#include "reosstyleregistery.h"

ReosEditMeshElementWidget::ReosEditMeshElementWidget( ReosMesh *mesh, const ReosGuiContext &context )
  : QWidget( context.parent() )
  , ui( new Ui::ReosEditMeshElementWidget )
  , mMesh( mesh )
  , mGuiContext( context, this )
  , mActionEditMeshFrame( new QAction( QPixmap( QStringLiteral( ":/images/editMeshFrameTool.svg" ) ), tr( "Edit Mesh Elements" ), this ) )
  , mMapToolEditMeshFrame( new ReosMapToolEditMeshFrame( mesh, this, mGuiContext.map() ) )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  mToolBar->setContentsMargins( 0, 0, 0, 0 );
  mToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBarLayout->addWidget( mToolBar );

  mToolBar->addAction( mActionEditMeshFrame );
  mActionEditMeshFrame->setCheckable( true );
  mMapToolEditMeshFrame->setAction( mActionEditMeshFrame );

  mToolBar->addActions( mMapToolEditMeshFrame->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
}

ReosEditMeshElementWidget::~ReosEditMeshElementWidget()
{
  delete ui;
}

void ReosEditMeshElementWidget::hideEvent( QHideEvent *e )
{
  mMapToolEditMeshFrame->deactivate();
  QWidget::hideEvent( e );
}

void ReosEditMeshElementWidget::showEvent( QShowEvent *e )
{
  mMapToolEditMeshFrame->activate();
  mMapToolEditMeshFrame->setCurrentToolInMap();
  QWidget::showEvent( e );
}
