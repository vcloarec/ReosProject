/***************************************************************************
  reosdssgriddedrainfallselector.cpp - ReosDssGriddedRainfallSelector

 ---------------------
 begin                : 13.1.2023
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
#include "reosdssgriddedrainfallselector.h"
#include "ui_reosdssgriddedrainfallselector.h"

#include "reosdssprovider.h"
#include "reosdssprovideruriwidget.h"

REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosDssGuiFactory();
}

ReosDssGriddedRainfallSelector::ReosDssGriddedRainfallSelector( QWidget *parent ) :
  ReosGriddedRainDataProviderSelectorWidget( parent ),
  ui( new Ui::ReosDssGriddedRainfallSelector )
{
  ui->setupUi( this );
  connect( ui->mPathesCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosDssGriddedRainfallSelector::onComboChanged );
}

ReosDssGriddedRainfallSelector::~ReosDssGriddedRainfallSelector()
{
  delete ui;
}

ReosGriddedRainfall *ReosDssGriddedRainfallSelector::createData( QObject *parent ) const
{
  return new ReosGriddedRainfall( mProvider->dataSource(), ReosDssProviderGriddedRainfall::staticKey(), parent );
}

QVariantMap ReosDssGriddedRainfallSelector::selectedMetadata() const
{
  QVariantMap ret;
  ret.insert( QStringLiteral( "provider-key" ), ReosDssProviderBase::staticKey() );
  ret.insert( QStringLiteral( "data-type" ), ReosGriddedRainfall::staticType() );

  if ( mProvider && mProvider->count() > 0 )
  {
    QPair<QDateTime, QDateTime> timeExtent = {mProvider->startTime( 0 ), mProvider->endTime( mProvider->count() - 1 )};
    ret.insert( QStringLiteral( "start" ), timeExtent.first );
    ret.insert( QStringLiteral( "end" ), timeExtent.second );
  }

  return ret;
}

ReosGriddedRainfallProvider::FileDetails ReosDssGriddedRainfallSelector::setSource( const QString &source, ReosModule::Message &message )
{
  ui->mPathesCombo->clear();

  mProvider = std::make_unique<ReosDssProviderGriddedRainfall>();
  const ReosGriddedRainfallProvider::FileDetails detail = mProvider->details( source, message );

  for ( const QString &pathes : detail.availableVariables )
    ui->mPathesCombo->addItem( pathes );
  mSource = source;
  onComboChanged();
  return detail;
}

void ReosDssGriddedRainfallSelector::onComboChanged()
{
  mProvider = std::make_unique<ReosDssProviderGriddedRainfall>();
  mProvider->setDataSource( ReosDssProviderBase::createUri( mSource, ReosDssPath( ui->mPathesCombo->currentText() ) ) );

  emit dataSelectionChanged( true );
}

ReosDataProviderGuiFactory::GuiCapabilities ReosDssGuiFactory::capabilities() const
{
  return GuiCapability::DataSelector;
}

QString ReosDssGuiFactory::key() const
{
  return ReosDssProviderBase::staticKey();
}

ReosDssGriddedRainfallSelector *ReosDssGuiFactory::createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent ) const
{
  if ( dataType == ReosDssProviderGriddedRainfall::dataType() )
    return new ReosDssGriddedRainfallSelector( parent );

  return nullptr;
}

ReosDataProviderSettingsWidget *ReosDssGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return nullptr;
}

ReosDataProviderUriWidget *ReosDssGuiFactory::createUriWidget( QWidget *parent ) const
{
  return new ReosDssProviderUriWidget( parent );
}

QString ReosDssGuiFactory::dataType() const
{
  return ReosDssProviderGriddedRainfall::dataType();
}

QString ReosDssGuiFactory::displayText() const
{
  return QObject::tr( "DSS format" );
}
