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


ReosEditStructure2DWidget::ReosEditStructure2DWidget( QWidget *parent )
  : ReosStackedPageWidget( parent )
  , ui( new Ui::ReosEditStructure2DWidget )
  , mActionEditLine( new QAction( tr( "Edit Structure Line" ), this ) )
{
  ui->setupUi( this );

  QPushButton *backButton = new QPushButton( tr( "Back" ), this );
  layout()->addWidget( backButton );
  connect( backButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );

  QToolBar *toolBar = new QToolBar( this );
  layout()->addWidget( toolBar );

  toolBar->addAction( mActionEditLine );
  mActionEditLine->setCheckable( true );
}

ReosEditStructure2DWidget::~ReosEditStructure2DWidget()
{
  delete ui;
}
