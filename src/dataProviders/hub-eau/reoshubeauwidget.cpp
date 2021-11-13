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


ReosHubEauWidget::ReosHubEauWidget( ReosMap *map, QWidget *parent )
  :  ReosDataProviderSelectorWidget( parent )
  ,  ui( new Ui::ReosHubEauWidget )
  , mMap( map )
  , mSelectStation( new ReosMapToolSelectMapItem( map, QStringLiteral( "hub-eau-station" ) ) )

{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  ui->mToolWidget->setLayout( new QHBoxLayout );
  ui->mToolWidget->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->mToolWidget );
  ui->mToolWidget->layout()->addWidget( toolBar );
  QAction *selectStationAction = toolBar->addAction( QPixmap( ":/images/neutral.svg" ), tr( "Select station" ) );
  selectStationAction->setCheckable( true );
  mSelectStation->setAction( selectStationAction );
  mSelectStation->setSearchUnderPoint( true );
  mSelectStation->setCursor( Qt::ArrowCursor );

  mHydrographPlot = new ReosPlotTimeSerieVariableStep( tr( "Hydrograph" ) );
  ui->mPlotWidget->addPlotItem( mHydrographPlot );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAutoMinimumSize( true );
  ui->mPlotWidget->setLegendVisible( false );
  ui->mPlotWidget->setAxeTextSize( 7 );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
  mServer = new ReosHubEauServer( this );

  connect( mMap, &ReosMap::extentChanged, this, &ReosHubEauWidget::onMapExtentChanged );
  connect( mServer, &ReosHubEauServer::stationsUpdated, this, &ReosHubEauWidget::onStationUpdated );
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

  std::unique_ptr<ReosHydrograph> hyd( mServer->createHydrograph( mCurrentStationId, mCurrentStationMeta, parent ) );
  hyd->setName( hyd->name() + QStringLiteral( " - " ) + tr( "real time" ) );
  return hyd.release();
}

ReosHydrograph *ReosHubEauWidget::selectedData() const
{
  return mCurrentHydrograph;
}

void ReosHubEauWidget::onMapExtentChanged()
{
  ReosMapExtent extent = mMap->extent();
  ReosMapExtent hubEauExtent = mMap->engine()->transformFromProjectExtent( extent, ReosGisEngine::wktEPSGCrs( 4326 ) );
  mServer->setExtent( hubEauExtent );
  ui->mStationCountLabel->setText( tr( "Loading station on extent", nullptr, mStations.count() ) );
}

void ReosHubEauWidget::onStationUpdated()
{
  mStationsMarker.clear();
  mCurrentMarker = nullptr;

  mStations = mServer->stations();

  for ( int i = 0; i < mStations.count(); ++i )
  {
    const ReosHubEauStation &station = mStations.at( i );
    const QPointF pt = mMap->engine()->transformToProjectCoordinates( ReosGisEngine::wktEPSGCrs( 4326 ), QPointF( station.longitude, station.latitude ) );
    mStationsMarker.emplace_back( std::make_unique<ReosHubEauStationMarker>( mMap, pt ) );
    mStationsMarker.back()->setColor( QColor( 12, 114, 185 ) );
    mStationsMarker.back()->setWidth( 10 );
    mStationsMarker.back()->setExternalColor( Qt::black );
    mStationsMarker.back()->setExternalWidth( 14 );
    mStationsMarker.back()->setDescription( QStringLiteral( "hub-eau-station" ) );
    mStationsMarker.back()->stationIndex = i;
  }
  ui->mStationCountLabel->setText( tr( "%n station displayed", nullptr, mStations.count() ) );
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
    mCurrentMarker->setExternalColor( Qt::black );

  mCurrentHydrograph = nullptr;
  mCurrentMarker = static_cast<ReosHubEauStationMarker *>( item );
  if ( mCurrentMarker )
  {
    mCurrentStationId = mStations.at( mCurrentMarker->stationIndex ).id;
    mCurrentStationMeta = mStations.at( mCurrentMarker->stationIndex ).meta;
    mCurrentMarker->setExternalColor( Qt::red );
    mCurrentHydrograph = mServer->createHydrograph( mCurrentStationId, mCurrentStationMeta, this );
    ui->mCurrentStateLabel->setText( "Loading hydrograph" );
    connect( mCurrentHydrograph, &ReosDataObject::dataChanged, this, &ReosHubEauWidget::onHydrographUpdated );
    emit dataIsLoading();
    emit dataSelectionChanged( true );
  }
  else
  {
    mCurrentStationId.clear();
    mCurrentStationMeta.clear();
    mHydrographPlot->setTimeSerie( nullptr );
    ui->mCurrentStateLabel->setText( "No station selected" );
    emit dataSelectionChanged( false );
  }

  populateMeta( mCurrentStationMeta );
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

  QString htmlText = QStringLiteral( "<html>\n<body>\n" );
  htmlText += QLatin1String( "<table class=\"list-view\">\n" );
  ui->mTextBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );
  // Begin Provider section

  if ( meta.isEmpty() )
  {
    htmlText += QStringLiteral( "<h2>" ) + tr( "No station selected" ) + QStringLiteral( "</h2>\n<hr>\n" );
  }
  else
  {
    htmlText += QStringLiteral( "<h2>" ) + meta.value( QStringLiteral( "libelle_station" ) ).toString() + QStringLiteral( "</h2>\n<hr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>On duty</b>" ) + QStringLiteral( "</td><td>" )
                + ( meta.value( QStringLiteral( "en_service" ) ).toBool() ? tr( "yes" ) : tr( "no" ) )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Station type</b>" ) + QStringLiteral( "</td><td>" )
                + meta.value( QStringLiteral( "type_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Opening date</b>" ) + QStringLiteral( "</td><td>" )
                + QLocale().toString( QDateTime::fromString( meta.value( QStringLiteral( "date_ouverture_station" ) ).toString(), Qt::ISODate ) )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Closing date</b>" ) + QStringLiteral( "</td><td>" )
                + QLocale().toString( QDateTime::fromString( meta.value( QStringLiteral( "date_fermeture_station" ) ).toString(), Qt::ISODate ) )
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Local influence</b>" ) + QStringLiteral( "</td><td>" )
                + meta.value( QStringLiteral( "influence_locale_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Local influence comments</b>" ) + QStringLiteral( "</td><td>" )
                + meta.value( QStringLiteral( "commentaire_influence_locale_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );

    htmlText += QStringLiteral( "<tr><td class=\"highlight\">" )
                + tr( "<b>Comments</b>" ) + QStringLiteral( "</td><td>" )
                + meta.value( QStringLiteral( "commentaire_station" ) ).toString()
                + QStringLiteral( "</td></tr>\n" );
  }

  htmlText += QLatin1String( "\n</body>\n</html>\n" );
  ui->mTextBrowser->setText( htmlText );
}

ReosHubEauStationMarker::ReosHubEauStationMarker( ReosMap *map, const QPointF &point ): ReosMapMarkerFilledCircle( map, point ) {}

ReosDataProviderGuiFactory::GuiCapabilities ReosHubEauHydrometryGuiFactory::capabilities() const
{
  return ReosDataProviderGuiFactory::GuiCapability::DataSelector;
}

QString ReosHubEauHydrometryGuiFactory::key() const
{
  return ReosHubEauHydrographProvider::staticKey();
}

ReosHubEauWidget *ReosHubEauHydrometryGuiFactory::createProviderSelectorWidget( ReosMap *map, QWidget *parent ) const
{
  return new ReosHubEauWidget( map, parent );
}

ReosHubEauSettingsWidget *ReosHubEauHydrometryGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return new ReosHubEauSettingsWidget( provider, parent );
}

QString ReosHubEauHydrometryGuiFactory::dataType() const
{
  return QStringLiteral( "hydrograph" );
}

QPixmap ReosHubEauHydrometryGuiFactory::icon() const
{
  return QPixmap( QStringLiteral( ":/hub-eau-images/icon-hubeau-blue.svg" ) );
}

REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosHubEauHydrometryGuiFactory();
}

