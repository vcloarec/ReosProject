/***************************************************************************
  reosmeteofrancearomeapiwidget.cpp - ReosMeteoFranceAromeApiWidget

 ---------------------
 begin                : 3.4.2023
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
#include "reosmeteofrancearomeapiwidget.h"
#include "ui_reosmeteofrancearomeapiwidget.h"

#include <QFileDialog>
#include <QTextStream>
#include <QDebug>

#include "reosmeteofrancearomeprovider.h"
#include "reossettings.h"
#include "reosoverridecursor.h"
#include "reosmap.h"
#include "reosgisengine.h"
#include "reosmaptool.h"
#include "reosstyleregistery.h"
#include "QMessageBox"

REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosMeteoFranceAromeApiGuiFactory();
}

ReosMeteoFranceAromeApiWidget::ReosMeteoFranceAromeApiWidget( ReosMap *map, QWidget *parent )
  : ReosGriddedRainDataProviderSelectorWidget( parent )
  ,  ui( new Ui::ReosMeteoFranceAromeApiWidget )
  , mMap( map )
  , mMapToolDrawExtent( new ReosMapToolDrawExtent( this, mMap ) )
  , mActionDrawExtent( new QAction( QIcon( QStringLiteral( ":/images/extentOnMap.svg" ) ), tr( "Draw extent" ), this ) )
{
  ui->setupUi( this );

  mActionDrawExtent->setCheckable( true );
  mMapToolDrawExtent->setAction( mActionDrawExtent );
  connect( mMapToolDrawExtent, &ReosMapToolDrawExtent::extentDrawn, this, &ReosMeteoFranceAromeApiWidget::onExtentDrawn );
  ui->mExtentButton->setDefaultAction( mActionDrawExtent );

  mMapToolDrawExtent->setColor( ReosStyleRegistery::instance()->redReos() );
  mMapToolDrawExtent->setFillColor( ReosStyleRegistery::instance()->redReos( 50 ) );

  ReosSettings settings;
  QString settingsFile = QStringLiteral( "Rainfall/meteofrance-api-key-file" );
  if ( settings.contains( settingsFile ) )
  {
    ui->mKeyFileLineEdit->setText( settings.value( settingsFile ).toString() );
    testKey();
  }
  else
  {
    ui->mConnectButton->setEnabled( false );
  }

  connect( ui->mApiKeyFileButton, &QToolButton::clicked, this, &ReosMeteoFranceAromeApiWidget::onApiKeyFileButton );
  connect( ui->mConnectButton, &QToolButton::clicked, this, &ReosMeteoFranceAromeApiWidget::onConnectButton );

  ui->mRunCombo->setEnabled( false );
  ui->mExtentButton->setEnabled( false );

  const QList<ReosMeteoFranceApiArome::Model> models = ReosMeteoFranceApiArome::availableModels();

  for ( const ReosMeteoFranceApiArome::Model &model : models )
  {
    if ( mModels.contains( model.zone ) )
      mModels[model.zone].append( model.resol );
    else
      mModels.insert( model.zone, QStringList( {model.resol} ) );
  }

  const QStringList zones = mModels.keys();
  for ( const QString &zone : zones )
    ui->mZoneCombo->addItem( zone, zone );

  connect( ui->mZoneCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosMeteoFranceAromeApiWidget::onZoneChange );
  connect( ui->mResolCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosMeteoFranceAromeApiWidget::resetModel );
  onZoneChange();

  connect( ui->mRunCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this] {emit dataSelectionChanged( true );} );
}

ReosMeteoFranceAromeApiWidget::~ReosMeteoFranceAromeApiWidget()
{
  delete ui;
}

ReosGriddedRainfall *ReosMeteoFranceAromeApiWidget::createData( QObject *parent ) const
{
  ReosOverrideCursor overrideCursor;
  QString uri = ReosMeteoFranceAromeApiProvider::uri(
                  ui->mKeyFileLineEdit->text(),
                  ui->mZoneCombo->currentData().toString(),
                  ui->mResolCombo->currentData().toString(),
                  mExtent,
                  ui->mRunCombo->currentIndex() );


  ui->mProgressBar->setMinimum( 0 );
  ui->mProgressBar->setMaximum( 0 );

  mCurrentRainfall = new ReosGriddedRainfall( uri, ReosMeteoFranceAromeApiProvider::staticKey(), parent );
  connect( mCurrentRainfall, &ReosDataObject::dataChanged, this, &ReosMeteoFranceAromeApiWidget::onCurrentRainfallChanged );
  connect( mCurrentRainfall, &ReosGriddedRainfall::loadingFinished, this, &ReosMeteoFranceAromeApiWidget::onLoadingFinished );

  return mCurrentRainfall;
}

QVariantMap ReosMeteoFranceAromeApiWidget::selectedMetadata() const
{
  QVariantMap ret;
  ret.insert( QStringLiteral( "provider-key" ), AROME_KEY );
  ret.insert( QStringLiteral( "data-type" ), ReosGriddedRainfall::staticType() );

  if ( mCurrentRainfall )
  {
    QPair<QDateTime, QDateTime> timeExtent = mCurrentRainfall->timeExtent();
    ret.insert( QStringLiteral( "start" ), timeExtent.first );
    ret.insert( QStringLiteral( "end" ), timeExtent.second );
  }

  return ret;
}

QString ReosMeteoFranceAromeApiWidget::dataName() const
{
  return tr( "AROME forecasting %1-%2 %3" ).arg( ui->mZoneCombo->currentData().toString(), ui->mResolCombo->currentData().toString(), ui->mRunCombo->currentText() );
}

void ReosMeteoFranceAromeApiWidget::onApiKeyFileButton()
{
  ReosSettings settings;
  QString settingsFile = QStringLiteral( "Rainfall/meteofrance-api-key-file" );
  QString settingsPath = QStringLiteral( "Rainfall/meteofrance-api-key-file-path" );

  QString dir;
  if ( settings.contains( settingsPath ) )
    dir = settings.value( settingsPath ).toString();

  QString keyFile = QFileDialog::getOpenFileName( this, tr( "Select a file containing the API Key" ), dir );

  if ( keyFile.isEmpty() )
    return;

  QFileInfo fileInfo( keyFile );
  settings.setValue( settingsPath, fileInfo.dir().path() );

  if ( testKey() )
    settings.setValue( settingsFile, fileInfo.filePath() );
}

void ReosMeteoFranceAromeApiWidget::onConnectButton()
{
  mCurrentRainfall = nullptr;
  mRunFrameCount = 0;
  const QString zone = ui->mZoneCombo->currentData().toString();
  const QString resol = ui->mResolCombo->currentData().toString();
  ui->mRunCombo->setEnabled( false );
  ui->mExtentButton->setEnabled( false );

  ReosMeteoFranceApiArome::Model model {zone, resol};

  ReosOverrideCursor overrideCursor;
  mApi.reset( new ReosMeteoFranceApiArome( ui->mKeyFileLineEdit->text() ) );
  QString error;

  ui->mProgressBar->setMinimum( 0 );
  ui->mProgressBar->setMaximum( 0 );
  mDataIsValid = mApi->connectToServiceBlocking( model, error );

  if ( mDataIsValid )
  {
    QList<QDateTime> availableRun = mApi->availableRuns();
    if ( availableRun.count() > 1 )
    {
      ReosMeteoFranceApiArome::RunInfo runInfo = mApi->runInfoBlocking( availableRun.first() );
      ui->mRunCombo->setEnabled( true );
      ui->mExtentButton->setEnabled( true );
      setExtent( runInfo.extent, true );
      mRunFrameCount = runInfo.frameCount;
      ui->mProgressBar->setMaximum( runInfo.frameCount );
    }
  }
  else
  {
    ui->mProgressBar->setMaximum( 100 );
  }

  ui->mProgressBar->setMinimum( 0 );
}

void ReosMeteoFranceAromeApiWidget::onZoneChange()
{
  ui->mResolCombo->clear();
  const QString zone = ui->mZoneCombo->currentData().toString();
  const QStringList resols = mModels.value( zone );
  resetModel();

  for ( const QString &resol : resols )
    ui->mResolCombo->addItem( resol, resol );
}

void ReosMeteoFranceAromeApiWidget::resetModel()
{
  ui->mRunCombo->setEnabled( false );
  ui->mExtentButton->setEnabled( false );
  mDataIsValid = false;
}

void ReosMeteoFranceAromeApiWidget::onExtentDrawn( const QRectF &extent )
{
  ReosMapExtent me( extent );
  me.setCrs( mMap->mapCrs() );
  setExtent( me, false );
}

void ReosMeteoFranceAromeApiWidget::onCurrentRainfallChanged()
{
  if ( mCurrentRainfall )
  {
    if ( mCurrentRainfall->gridCount() != 0 )
    {
      int frameCount = mCurrentRainfall->gridCount();
      int frameLoaded = 0;
      for ( int i = 0; i < frameCount; ++i )
      {
        if ( mCurrentRainfall->intensityValues( i ).count() > 0 )
          ++frameLoaded;
      }

      ui->mProgressBar->setValue( frameLoaded );
      ui->mProgressBar->setMaximum( frameCount );
    }
  }
  else
  {
    ui->mProgressBar->setValue( 0 );
    ui->mProgressBar->setMaximum( 0 );
  }
}

void ReosMeteoFranceAromeApiWidget::onLoadingFinished()
{
  if ( mCurrentRainfall )
  {
    if ( mCurrentRainfall->gridCount() != 0 )
    {
      int frameCount = mCurrentRainfall->gridCount();
      int frameLoaded = 0;
      for ( int i = 0; i < frameCount; ++i )
      {
        if ( mCurrentRainfall->intensityValues( i ).count() > 0 )
          ++frameLoaded;
      }

      ui->mProgressBar->setValue( frameLoaded );
      ui->mProgressBar->setMaximum( frameCount );

      if ( frameLoaded < frameCount )
      {
        QMessageBox::warning( this, tr( "AROME forecasting" ), tr( "It seems some frames are not valid. Maybe all the frames are not ready on the server side." ) );
      }
    }
  }
}

void ReosMeteoFranceAromeApiWidget::setExtent( const ReosMapExtent &extent, bool zoom )
{
  mCurrentRainfall = nullptr;
  mExtent = extent;
  if ( zoom )
    mMap->setExtent( extent );

  QStringList valExtent = ReosMeteoFranceApiArome::extentToLonLatList( extent, 2 );
  ui->mExtentButton->setText( tr( "Longitude(%1,%2), Latitude(%3,%4)" ).
                              arg( valExtent.at( 0 ), valExtent.at( 1 ), valExtent.at( 2 ), valExtent.at( 3 ) ) );

  emit dataSelectionChanged( true );
}

bool ReosMeteoFranceAromeApiWidget::testKey()
{
  QString keyFile = ui->mKeyFileLineEdit->text();

  QFile file( keyFile );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QTextStream stream( &file );
  QString key = stream.readAll();
  ui->mConnectButton->setEnabled( !key.isEmpty() );

  return !key.isEmpty();
}

ReosDataProviderGuiFactory::GuiCapabilities ReosMeteoFranceAromeApiGuiFactory::capabilities() const
{
  return GuiCapability::DataSelector;
}

QString ReosMeteoFranceAromeApiGuiFactory::key() const
{
  return AROME_KEY;
}

ReosMeteoFranceAromeApiWidget *ReosMeteoFranceAromeApiGuiFactory::createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent ) const
{
  return new ReosMeteoFranceAromeApiWidget( map, parent );
}

ReosDataProviderSettingsWidget *ReosMeteoFranceAromeApiGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return nullptr;
}

QString ReosMeteoFranceAromeApiGuiFactory::dataType() const
{
  return ReosGriddedRainfall::staticType();
}

QString ReosMeteoFranceAromeApiGuiFactory::displayText() const
{
  return QObject::tr( "Météo France Arome fine-scale atmospheric forecasting model" );
}
