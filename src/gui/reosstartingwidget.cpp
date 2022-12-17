/***************************************************************************
                      reosstartingwidget.cpp
                     --------------------------------------
Date                 : 23-05-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosstartingwidget.h"
#include "ui_reosstartingwidget.h"

ReosStartingWidget::ReosStartingWidget( ReosMainWindow *parent ) :
  QDialog( parent ),
  ui( new Ui::ReosStartingWidget ),
  mMainWindow( parent )
{
  ui->setupUi( this );
  connect( ui->mPushButtonNewProject, SIGNAL( clicked() ), this, SLOT( onNewProject() ) );
  connect( ui->mPushButtonOpenProject, SIGNAL( clicked() ), this, SLOT( onOpenProject() ) );

  if ( parent )
    setWindowTitle( parent->windowTitle() );
}

ReosStartingWidget::~ReosStartingWidget()
{
  delete ui;
}

void ReosStartingWidget::setBan( const QPixmap &image )
{
  ui->imageBan->setPixmap( image );
}

void ReosStartingWidget::onNewProject()
{
  accept();
}

void ReosStartingWidget::onOpenProject()
{
  mMainWindow->openFile();
  accept();
}
