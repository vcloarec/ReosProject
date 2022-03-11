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

#include "reosstyleregistery.h"
#include "reosmaptooleditgeometrystructure.h"
#include "reospolylinesstructure.h"

ReosEditPolylineStructureWidget::ReosEditPolylineStructureWidget( ReosPolylinesStructure *structure, const ReosGuiContext &context )
  : QWidget( context.parent() )
  , ui( new Ui::ReosEditPolylineStructureWidget )
  , mActionEditLine( new QAction( QPixmap( QStringLiteral( ":/images/editStructureLines.svg" ) ), tr( "Edit Structure Line" ), this ) )
  , mMapToolEditLine( new ReosMapToolEditPolylineStructure( structure, this, context.map() ) )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  mToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBarWidget->layout()->addWidget( mToolBar );

  mToolBar->addAction( mActionEditLine );
  mActionEditLine->setCheckable( true );
  mMapToolEditLine->setAction( mActionEditLine );

  mToolBar->addActions( mMapToolEditLine->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
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
