/***************************************************************************
  reossavegriddedrainfallasdialog.cpp - ReosSaveGriddedRainfallAsDialog

 ---------------------
 begin                : 16.1.2023
 copyright            : (C) 2023 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reossavegriddedrainfallasdialog.h"
#include "ui_reossavegriddedrainfallasdialog.h"

#include <QKeyEvent>
#include <QFileDialog>

#include "reosgriddedrainitem.h"
#include "reosguicontext.h"
#include "reosdataprovidergui.h"
#include "reossettings.h"


ReosSaveGriddedRainfallAsDialog::ReosSaveGriddedRainfallAsDialog( ReosGriddedRainfall *rainfall, const ReosGuiContext &context ) :
  QDialog( context.parent() ),
  ui( new Ui::ReosSaveGriddedRainfallAsDialog )
{
  ui->setupUi( this );
  ui->mOptionWidget->syncRainfall( rainfall );

  QStringList providerKeys = ReosDataProviderRegistery::instance()->ableToWrite( ReosGriddedRainfall::staticType() );
  std::sort( providerKeys.begin(), providerKeys.end() );

  for ( const QString &provKey : std::as_const( providerKeys ) )
  {
    std::unique_ptr<ReosDataProvider> provider( ReosDataProviderRegistery::instance()->createProvider( provKey ) );
    QString name = ReosDataProviderGuiRegistery::instance()->providerDisplayText( provKey );
    ui->mFormatCombo->addItem( name, provKey );
  }

  connect( ui->mFormatCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ),
           this, &ReosSaveGriddedRainfallAsDialog::onFormatChanged );

  onFormatChanged();
  if ( mUriWidget )
  {
    mUriWidget->setUri( rainfall->dataProvider()->dataSource() );
    mUriWidget->setDataType( ReosGriddedRainfall::staticType() );
  }
}

ReosSaveGriddedRainfallAsDialog::~ReosSaveGriddedRainfallAsDialog()
{
  delete ui;
}

void ReosSaveGriddedRainfallAsDialog::saveAs( ReosGriddedRainfall *rainfall ) const
{
  std::unique_ptr<ReosDataProvider> provider = currentFormatProvider();
  ReosGriddedRainfallProvider *grProv = qobject_cast<ReosGriddedRainfallProvider * >( provider.get() );

  if ( mUriWidget )
    grProv->write( rainfall, mUriWidget->uri(), ui->mOptionWidget->rasterExtent(), ui->mOptionWidget->timeWindow() );
}

void ReosSaveGriddedRainfallAsDialog::keyPressEvent( QKeyEvent *evt )
{
  if ( evt->key() == Qt::Key_Enter || evt->key() == Qt::Key_Return )
    return;
  QDialog::keyPressEvent( evt );
}

void ReosSaveGriddedRainfallAsDialog::onFormatChanged()
{
  if ( mCurrentProviderKey == ui->mFormatCombo->currentData().toString() )
    return;

  mCurrentProviderKey = ui->mFormatCombo->currentData().toString();
  std::unique_ptr<ReosDataProvider> provider = currentFormatProvider();

  ReosGriddedRainfallProvider *grProv = qobject_cast<ReosGriddedRainfallProvider * >( provider.get() );
  if ( grProv )
    ui->mOptionWidget->setSupportedGridOrigin( grProv->supportedOrigin() );

  QString currentUri;
  if ( mUriWidget )
  {
    currentUri = mUriWidget->uri();
    ui->mProviderUriLayout->removeWidget( mUriWidget );
    mUriWidget->deleteLater();
    mUriWidget = nullptr;
  }

  mUriWidget = ReosDataProviderGuiRegistery::instance()->createUriWidget( mCurrentProviderKey, this );
  if ( mUriWidget )
  {
    ui->mProviderUriLayout->addWidget( mUriWidget );
    mUriWidget->setUri( currentUri );
    mUriWidget->setDataType( ReosGriddedRainfall::staticType() );
  }
}


std::unique_ptr<ReosDataProvider> ReosSaveGriddedRainfallAsDialog::currentFormatProvider() const
{
  std::unique_ptr<ReosDataProvider> provider(
    ReosDataProviderRegistery::instance()->createProvider( mCurrentProviderKey, ReosGriddedRainfall::staticType() ) );
  return provider;
}
