/***************************************************************************
  reoshubeausettingswidget.cpp - ReosHubEauSettingsWidget

 ---------------------
 begin                : 9.11.2021
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
#include "reoshubeausettingswidget.h"
#include "ui_reoshubeausettingswidget.h"

#include "reosapplication.h"
#include "reoshubeauhydrographprovider.h"

ReosHubEauSettingsWidget::ReosHubEauSettingsWidget( ReosDataProvider *provider, QWidget *parent )
  :  ReosDataProviderSettingsWidget( parent )
  ,  ui( new Ui::ReosHubEauSettingsWidget )
  , mProvider( qobject_cast<ReosHubEauHydrographProvider *>( provider ) )
{
  ui->setupUi( this );
  populateDescription();
  connect( mProvider, &ReosDataProvider::dataChanged, this, [this]
  {
    enableLoadButton();
    populateDescription();
  } );

  enableLoadButton();
  connect( ui->mReloadButton, &QPushButton::clicked, this, &ReosHubEauSettingsWidget::onReload );
}

ReosHubEauSettingsWidget::~ReosHubEauSettingsWidget()
{
  delete ui;
}

void ReosHubEauSettingsWidget::onReload()
{
  if ( mProvider )
    mProvider->load();
  enableLoadButton();
}

void ReosHubEauSettingsWidget::populateDescription()
{
  ui->mTextBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );
  ui->mTextBrowser->clear();
  if ( mProvider )
    ui->mTextBrowser->setText( mProvider->htmlDescription() );
}

void ReosHubEauSettingsWidget::enableLoadButton()
{

  if ( mProvider && mProvider->status() != ReosHubEauHydrographProvider::Status::Loaded )
  {
    ui->mReloadButton->setEnabled( false );
    ui->mReloadButton->setText( tr( "Loading" ) );
  }
  else
  {
    ui->mReloadButton->setEnabled( true );
    ui->mReloadButton->setText( tr( "Reload" ) );
  }

}
