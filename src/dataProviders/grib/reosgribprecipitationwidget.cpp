/***************************************************************************
  reosgribprecipitationwidget.cpp - ReosGribPrecipitationWidget

 ---------------------
 begin                : 16.11.2022
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
#include "reosgribprecipitationwidget.h"
#include "qdebug.h"
#include "ui_reosgribprecipitationwidget.h"

#include <QFileDialog>
#include "QDockWidget"

#include "reosmap.h"
#include "reosgriddedrainitem.h"
#include "reossettings.h"


REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosGribGuiFactory();
}

ReosGribPrecipitationWidget::ReosGribPrecipitationWidget( QWidget *parent )
  : ReosGriddedRainDataProviderSelectorWidget( parent )
  , ui( new Ui::ReosGribPrecipitationWidget )
  ,  mProvider( new ReosGribGriddedDataProvider )
{
  ui->setupUi( this );

  connect( ui->mVariablesCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGribPrecipitationWidget::updateRainfall );

  ui->mValueTypeCombo->addItem( tr( "Intensity" ), static_cast<int>( ReosGriddedRainfallProvider::ValueType::Instantaneous ) );
  ui->mValueTypeCombo->addItem( tr( "Height during time step" ), static_cast<int>( ReosGriddedRainfallProvider::ValueType::CumulativeOnTimeStep ) );
  ui->mValueTypeCombo->addItem( tr( "Cumulative height from start" ), static_cast<int>( ReosGriddedRainfallProvider::ValueType::Cumulative ) );
  connect( ui->mValueTypeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosGribPrecipitationWidget::updateRainfall );

  onPathChanged();
}

ReosGribPrecipitationWidget::~ReosGribPrecipitationWidget()
{
  delete ui;
}

QVariantMap ReosGribPrecipitationWidget::selectedMetadata() const
{
  QVariantMap ret;
  ret.insert( QStringLiteral( "provider-key" ), GRIB_KEY );
  ret.insert( QStringLiteral( "data-type" ), ReosGriddedRainfall::staticType() );

  if ( mCurrentRainfall )
  {
    QPair<QDateTime, QDateTime> timeExtent = mCurrentRainfall->timeExtent();
    ret.insert( QStringLiteral( "start" ), timeExtent.first );
    ret.insert( QStringLiteral( "end" ), timeExtent.second );
  }

  return ret;
}

ReosGriddedRainfall *ReosGribPrecipitationWidget::createData( QObject *parent ) const
{
  return new ReosGriddedRainfall( mCurrentDataUri, mProvider->key(), parent );
}

ReosDataObject *ReosGribPrecipitationWidget::selectedData() const
{
  return mCurrentRainfall.get();
}

ReosGriddedRainfallProvider::FileDetails  ReosGribPrecipitationWidget::setSource( const QString &source, ReosModule::Message &message )
{
  mSource = source;
  onPathChanged();
  return mDetails;
}

void ReosGribPrecipitationWidget::onPathChanged()
{
  if ( ui->mVariablesCombo->currentIndex() != -1 )
    mCurrentVariable = ui->mVariablesCombo->currentText();

  ui->mVariablesCombo->blockSignals( true );
  ui->mVariablesCombo->clear();
  mCurrentSourceIsValid = false;

  ReosModule::Message message;
  mDetails = mProvider->details( mSource, message );

  if ( message.type == ReosModule::Simple )
  {
    ui->mVariablesCombo->addItems( mDetails.availableVariables );
    int currentIndex = ui->mVariablesCombo->findText( mCurrentVariable );
    if ( currentIndex >= 0 )
      ui->mVariablesCombo->setCurrentIndex( currentIndex );
    mCurrentSourceIsValid = true;
  }

  ui->mVariablesCombo->blockSignals( false );
  updateRainfall();
  emit dataSelectionChanged( mCurrentSourceIsValid );
}

void ReosGribPrecipitationWidget::updateRainfall()
{
  QString variable = ui->mVariablesCombo->currentText();
  ReosGriddedRainfallProvider::ValueType valueType = static_cast<ReosGriddedRainfallProvider::ValueType>( ui->mValueTypeCombo->currentData().toInt() );

  QString candidateUri = ReosGribGriddedDataProvider::uri( mSource, variable, valueType );

  if ( mCurrentDataUri == candidateUri )
    return;

  mCurrentDataUri = candidateUri;
  mCurrentRainfall.reset();

  if ( mCurrentSourceIsValid )
    mCurrentRainfall.reset( new ReosGriddedRainfall( mCurrentDataUri, mProvider->key() ) );


  emit dataSelectionChanged( mCurrentSourceIsValid );
}



ReosDataProviderGuiFactory::GuiCapabilities ReosGribGuiFactory::capabilities() const
{
  return GuiCapability::DataSelector;
}

QString ReosGribGuiFactory::key() const
{
  return GRIB_KEY;
}

ReosGribPrecipitationWidget *ReosGribGuiFactory::createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent ) const
{
  if ( dataType == ReosGribGriddedDataProvider::dataType() )
    return new ReosGribPrecipitationWidget( parent );

  return nullptr;
}

ReosDataProviderSettingsWidget *ReosGribGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return nullptr;
}

QString ReosGribGuiFactory::dataType() const
{
  return ReosGribGriddedDataProvider::dataType();
}

QString ReosGribGuiFactory::displayText() const
{
  return QObject::tr( "GRIB2 format" );
}

