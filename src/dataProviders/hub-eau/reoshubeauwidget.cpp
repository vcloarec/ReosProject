/***************************************************************************
  reoshubeauwidget.cpp - ReosHubEauWidget

 ---------------------
 begin                : 1.11.2021
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
#include "reoshubeauwidget.h"
#include "ui_reoshubeauwidget.h"

#include "reoshubeauserver.h"
#include "reosmap.h"
#include "reosgisengine.h"
#include "reosmapitem.h"
#include "reosmaptool.h"
#include "reoshydrograph.h"
#include "reosplottimeconstantinterval.h"
#include "reosapplication.h"
#include "reoshubeauhydrographprovider.h"
#include "reosstyleregistery.h"


ReosHubEauWidget::ReosHubEauWidget( ReosMap *map, QWidget *parent )
  :  ReosDataProviderSelectorWidget( parent )
  ,  ui( new Ui::ReosHubEauWidget )
  , mMap( map )
  , mSelectStation( new ReosMapToolSelectMapItem( map, QStringLiteral( "hub-eau-station" ) ) )

{
  ui->setupUi( this );

  ui->mToolWidget->setLayout( new QHBoxLayout );
  ui->mToolWidget->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->mToolWidget );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->mToolWidget->layout()->addWidget( toolBar );
  QAction *selectStationAction = toolBar->addAction( QIcon( QStringLiteral( ":/hub-eau-images/selectOnMap.svg" ) ), tr( "Select Station" ) );
  selectStationAction->setCheckable( true );
  mSelectStation->setAction( selectStationAction );
  mSelectStation->setSearchUnderPoint( true );
  mSelectStation->setCursor( Qt::ArrowCursor );

  mHydrographPlot = new ReosPlotTimeSerieVariableStep( tr( "Hydrograph" ) );
  ui->mPlotWidget->setSettingsContext( QStringLiteral( "hub-eau-widget" ) );
  ui->mPlotWidget->addPlotItem( mHydrographPlot );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAutoMinimumSize( true );
  ui->mPlotWidget->setLegendEnabled( false );
  ui->mPlotWidget->setAxesTextSize( 10 );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
  ui->mNotificationButton->setVisible( false );

  mServer = new ReosHubEauServer( this );

  connect( mMap, &ReosMap::extentChanged, this, &ReosHubEauWidget::onMapExtentChanged );
  connect( mServer, &ReosHubEauServer::stationsUpdated, this, &ReosHubEauWidget::onStationUpdated );
  connect( mServer, &ReosHubEauServer::errorOccured, this, &ReosHubEauWidget::onErrorOccured );
  connect( mSelectStation, &ReosMapToolSelectMapItem::found, this, &ReosHubEauWidget::onSelectStation );
}

ReosHubEauWidget::~ReosHubEauWidget()
{
  delete ui;
}

ReosHydrograph *ReosHubEauWidget::createData( QObject *parent ) const
{
  if ( mCurrentStationId.isEmpty() )
    return nullptr;

  std::unique_ptr<ReosHydrograph> hyd( mServer->createHydrograph( mCurrentStationId, mCurrentHubEauStationMeta, parent ) );
  hyd->setName( hyd->name() + QStringLiteral( " - " ) + tr( "real time" ) );
  return hyd.release();
}

ReosHydrograph *ReosHubEauWidget::selectedData() const
{
  return mCurrentHydrograph;
}

QVariantMap ReosHubEauWidget::selectedMetadata() const
{
  /* provider-key: provider key
  * station: if the data is associated with a station, the name of the station
  * station-descritpion: a short text desciption of the station
  * x-coord: x coordinate (or longitude)
  * y-coord: y coordinate (or longitude)
  * crs: wkt coordinate system
  * descritpion: a short text desciption of the data*/
  QVariantMap ret;

  ret.insert( QStringLiteral( "station" ), mCurrentHubEauStationMeta.value( QStringLiteral( "libelle_station" ) ) );
  ret.insert( QStringLiteral( "x-coord" ), mCurrentHubEauStationMeta.value( QStringLiteral( "longitude_station" ) ) );
  ret.insert( QStringLiteral( "y-coord" ), mCurrentHubEauStationMeta.value( QStringLiteral( "latitude_station" ) ) );
  ret.insert( QStringLiteral( "crs" ), ReosGisEngine::crsFromEPSG( 4326 ) );

  return ret;
}

void ReosHubEauWidget::onMapExtentChanged()
{
  const ReosMapExtent extent = mMap->extent();
  const ReosMapExtent hubEauExtent = mMap->engine()->transformFromProjectExtent( extent, ReosGisEngine::wktEPSGCrs( 4326 ) );
  mServer->setExtent( hubEauExtent );
  ui->mStationCountLabel->setVisible( true );
  ui->mStationCountLabel->setText( tr( "Loading stations on extent" ) );
}

void ReosHubEauWidget::onStationUpdated()
{
  ui->mNotificationButton->setVisible( false );
  mStationsMarker.clear();
  mCurrentMarker = nullptr;

  mStations = mServer->stations();

  for ( int i = 0; i < mStations.count(); ++i )
  {
    const ReosHubEauStation &station = mStations.at( i );
    const QPointF pt = mMap->engine()->transformToProjectCoordinates( ReosGisEngine::wktEPSGCrs( 4326 ), QPointF( station.longitude, station.latitude ) );
    mStationsMarker.emplace_back( std::make_unique<ReosHubEauStationMarker>( mMap, pt ) );
    mStationsMarker.back()->stationIndex = i;
    formatMarker( mStationsMarker.back().get(), station.meta, station.id == mCurrentStationId );
    if ( station.id == mCurrentStationId )
      mCurrentMarker = mStationsMarker.back().get();
  }
  ui->mStationCountLabel->setVisible( true );
  ui->mStationCountLabel->setText( tr( "%n station displayed", nullptr, mStations.count() ) );
}

void ReosHubEauWidget::onErrorOccured()
{
  const ReosModule::Message serverMessage = mServer->lastMessage();
  ui->mStationCountLabel->setVisible( false );
  ui->mNotificationButton->setVisible( true );
  ui->mNotificationButton->setMessage( serverMessage );
  ReosModule::Message sendedMessage = serverMessage;
  sendedMessage.prefixMessage( tr( "Hubeau server: " ) );
  mMap->message( sendedMessage );
}

void ReosHubEauWidget::onClosed()
{
  mStationsMarker.clear();
}

void ReosHubEauWidget::onOpened()
{
  onMapExtentChanged();
  mSelectStation->setCurrentToolInMap();
}

void ReosHubEauWidget::onSelectStation( ReosMapItem *item, const QPointF & )
{
  if ( mCurrentHydrograph )
  {
    disconnect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosHubEauWidget::onHydrographUpdated );
    mCurrentHydrograph->deleteLater();
  }

  if ( mCurrentMarker )
    formatMarker( mCurrentMarker, mCurrentHubEauStationMeta, false );

  mCurrentHydrograph = nullptr;
  mCurrentMarker = static_cast<ReosHubEauStationMarker *>( item );
  if ( mCurrentMarker )
  {
    const ReosHubEauStation &station =  mStations.at( mCurrentMarker->stationIndex );
    mCurrentStationId = station.id;
    mCurrentHubEauStationMeta = station.meta;
    mCurrentMetadata.clear();

    formatMarker( mCurrentMarker, mCurrentHubEauStationMeta, true );

    mCurrentHydrograph = mServer->createHydrograph( mCurrentStationId, mCurrentHubEauStationMeta, this );
    ui->mCurrentStateLabel->setText( "Loading hydrograph" );
    connect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosHubEauWidget::onHydrographUpdated );
    emit dataIsLoading();
    emit dataSelectionChanged( true );
  }
  else
  {
    mCurrentStationId.clear();
    mCurrentHubEauStationMeta.clear();
    ui->mCurrentStateLabel->setText( "No station selected" );
    emit dataSelectionChanged( false );
  }

  populateMeta( mCurrentHubEauStationMeta );
  mHydrographPlot->setTimeSerie( mCurrentHydrograph );
}

void ReosHubEauWidget::onHydrographUpdated()
{
  if ( !mCurrentHydrograph )
    return;

  ReosHubEauHydrographProvider *provider = qobject_cast<ReosHubEauHydrographProvider *>( mCurrentHydrograph->dataProvider() );

  if ( !provider )
    return;

  switch ( provider->status() )
  {
    case ReosHubEauHydrographProvider::Status::NoData:
      ui->mCurrentStateLabel->setText( "No Data for selected station" );
      break;
    case ReosHubEauHydrographProvider::Status::Loading:
      ui->mCurrentStateLabel->setText( "Loading hydrograph" );
      break;
    case ReosHubEauHydrographProvider::Status::Loaded:
      ui->mCurrentStateLabel->setText( "Hydrograph loaded" );
      emit dataIsReady();
      break;
  }
}

QString ReosHubEauWidget::currentStationId() const
{
  return mCurrentStationId;
}

void ReosHubEauWidget::setCurrentStationId( const QString &currentStationId )
{
  mCurrentStationId = currentStationId;
}

void ReosHubEauWidget::populateMeta( const QVariantMap &meta )
{
  ui->mTextBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );
  ui->mTextBrowser->clear();

  ui->mTextBrowser->setText( ReosHubEauHydrographProvider::htmlDescriptionFromMeta( meta ) );
}

void ReosHubEauWidget::formatMarker( ReosHubEauStationMarker *marker, const QVariantMap &meta, bool currentMarker )
{
  QColor externalColor;

  if ( currentMarker )
  {
    externalColor = Qt::red;
    marker->setWidth( 14 );
    marker->setExternalWidth( 20 );
  }
  else
  {
    externalColor = Qt::black;
    marker->setWidth( 10 );
    marker->setExternalWidth( 14 );
  }

  marker->setExternalColor( externalColor );

  if ( !meta.value( QStringLiteral( "en_service" ) ).toBool() )
  {
    marker->setColor( QColor( 200, 120, 120 ) );
  }
  else
  {
    marker->setColor( QColor( 12, 114, 185 ) );
  }
}

ReosHubEauStationMarker::ReosHubEauStationMarker( ReosMap *map, const QPointF &point ): ReosMapMarkerFilledCircle( map, point )
{
  setDescription( QStringLiteral( "hub-eau-station" ) );
}

ReosDataProviderGuiFactory::GuiCapabilities ReosHubEauHydrometryGuiFactory::capabilities() const
{
  return ReosDataProviderGuiFactory::GuiCapability::DataSelector;
}

QString ReosHubEauHydrometryGuiFactory::key() const
{
  return ReosHubEauHydrographProvider::staticKey();
}

ReosHubEauWidget *ReosHubEauHydrometryGuiFactory::createProviderSelectorWidget( ReosMap *map, const QString &, QWidget *parent ) const
{
  return new ReosHubEauWidget( map, parent );
}

ReosHubEauSettingsWidget *ReosHubEauHydrometryGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return new ReosHubEauSettingsWidget( provider, parent );
}

QString ReosHubEauHydrometryGuiFactory::dataType() const
{
  return ReosHydrograph::staticType();
}

QIcon ReosHubEauHydrometryGuiFactory::icon() const
{
  return QIcon( QStringLiteral( ":/hub-eau-images/icon-hubeau-blue.svg" ) );
}

QString ReosHubEauHydrometryGuiFactory::displayText() const {return QStringLiteral( "Hub'Eau" );}

REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosHubEauHydrometryGuiFactory();
}

