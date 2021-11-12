/***************************************************************************
  reosdelftfewssettingswidget.cpp - ReosDelftFewsSettingsWidget

 ---------------------
 begin                : 12.11.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosdelftfewssettingswidget.h"
#include "ui_reosdelftfewssettingswidget.h"

#include "reosdelftfewsxmlprovider.h"
#include "reosapplication.h"

ReosDelftFewsSettingsWidget::ReosDelftFewsSettingsWidget( ReosDataProvider *provider, QWidget *parent ) :
  ReosDataProviderSettingsWidget( parent ),
  ui( new Ui::ReosDelftFewsSettingsWidget )
{
  ui->setupUi( this );
  if ( provider )
  {
    ui->mTextBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );
    ui->mTextBrowser->setText( provider->htmlDescription() );
  }
}

ReosDelftFewsSettingsWidget::~ReosDelftFewsSettingsWidget()
{
  delete ui;
}
