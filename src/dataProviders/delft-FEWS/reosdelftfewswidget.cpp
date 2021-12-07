/***************************************************************************
  reosdelftfewswidget.cpp - ReosDelftFewsWidget

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
#include "reosdelftfewswidget.h"
#include "ui_reosdelftfewswidget.h"

#include <QFileDialog>
#include <QDomDocument>

#include "reoscore.h"
#include "reossettings.h"
#include "reosgisengine.h"
#include "reosdelftfewsxmlprovider.h"
#include "reosplottimeconstantinterval.h"
#include "reosmaptool.h"
#include "reosapplication.h"
#include "reosdelftfewssettingswidget.h"

ReosDelftFewsWidget::ReosDelftFewsWidget( ReosMap *map, const QString &dataType, QWidget *parent )
  : ReosDataProviderSelectorWidget( parent )
  , ui( new Ui::ReosDelftFewsWidget )
  , mMap( map )
  , mDataType( dataType )
  , mStationsModel( new ReosDelftFewsStationsModel( dataType, this ) )
{
  ui->setupUi( this );

  mHydrographPlot = new ReosPlotTimeSerieVariableStep( tr( "Hydrograph" ) );
  mRainfallPlot = new ReosPlotTimeHistogram( tr( "Rainfall" ) );
  mHydrographPlot->setAsMasterItem( true );
  mRainfallPlot->setAsMasterItem( true );
  ui->mPlotWidget->setSettingsContext( QStringLiteral( "delft-fews-widget" ) );
  ui->mPlotWidget->addPlotItem( mHydrographPlot );
  ui->mPlotWidget->addPlotItem( mRainfallPlot );
  ui->mPlotWidget->setAxeXType( ReosPlotWidget::temporal );
  ui->mPlotWidget->enableAutoMinimumSize( true );
  ui->mPlotWidget->setLegendEnabled( false );
  ui->mPlotWidget->setAxesTextSize( 10 );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );

  QToolBar *toolBar = new QToolBar( this );
  ui->mToolBarWidget->setLayout( new QHBoxLayout );
  ui->mToolBarWidget->layout()->addWidget( toolBar );
  mActionSelectOnMap = toolBar->addAction( QPixmap( ":/delft-fews-images/selectOnMap.svg" ), tr( "Select Station" ) );
  mActionSelectOnMap->setCheckable( true );
  mMapToolSelectOnMap = new ReosMapToolSelectMapItem( mMap, QStringLiteral( "delft-fews-station" ) );
  mMapToolSelectOnMap->setAction( mActionSelectOnMap );
  mMapToolSelectOnMap->setCursor( Qt::ArrowCursor );
  connect( mMapToolSelectOnMap, &ReosMapToolSelectMapItem::found, this, &ReosDelftFewsWidget::onStationSelectOnMap );

  ui->mListView->setModel( mStationsModel );
  connect( ui->mFileButton, &QToolButton::clicked, this, &ReosDelftFewsWidget::onOpenFile );
  connect( ui->mLineEditFileName, &QLineEdit::textChanged, this, &ReosDelftFewsWidget::onFileNameChanged );
  connect( ui->mListView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosDelftFewsWidget::onStationChanged );
}

ReosDelftFewsWidget::~ReosDelftFewsWidget()
{
  delete ui;
}

ReosDataObject *ReosDelftFewsWidget::createData( QObject *parent ) const
{
  int stationIndex = ui->mListView->currentIndex().row();
  if ( stationIndex < 0 )
    return nullptr;

  const ReosDelftFewsStation &station = mStationsModel->station( stationIndex );

  std::unique_ptr<ReosTimeSerie> dataObject;
  ReosDelftFewsXMLProviderInterface *provider = nullptr;

  if ( station.dataType() == ReosDelftFewsXMLHydrographProvider::dataType() )
  {
    dataObject.reset( createHydrograph( parent ) );
    if ( dataObject )
      provider = static_cast<ReosDelftFewsXMLProviderInterface *>(
                   qobject_cast<ReosDelftFewsXMLHydrographProvider *>( dataObject->dataProvider() ) );
  }

  if ( station.dataType() == ReosDelftFewsXMLRainfallProvider::dataType() )
  {
    dataObject.reset( createRainfall( parent ) );
    if ( dataObject )
      provider = static_cast<ReosDelftFewsXMLProviderInterface *>(
                   qobject_cast<ReosDelftFewsXMLRainfallProvider *>( dataObject->dataProvider() ) );
  }

  if ( provider )
  {
    provider->setMetadata( station.meta );
    dataObject->setName( station.meta.value( QStringLiteral( "name" ) ).toString() );
    return dataObject.release();
  }

  return nullptr;
}

ReosDataObject *ReosDelftFewsWidget::selectedData() const
{
  if ( mDataType == ReosDelftFewsXMLHydrographProvider::dataType() )
    return mCurrentHydrograph;

  if ( mDataType == ReosDelftFewsXMLRainfallProvider::dataType() )
    return mCurrentRainfall;

  return nullptr;
}

QVariantMap ReosDelftFewsWidget::selectedMetadata() const
{
  QVariantMap ret;
  int stationIndex = ui->mListView->currentIndex().row();
  if ( stationIndex < 0 )
    return ret;

  const ReosDelftFewsStation  &station = mStationsModel->station( stationIndex );

  ret.insert( QStringLiteral( "provider-key" ), station.meta.value( ReosDelftFewsXMLProviderInterface::staticKey() ) );
  ret.insert( QStringLiteral( "station" ), station.meta.value( QStringLiteral( "name" ) ) );
  ret.insert( QStringLiteral( "station-descritpion" ), station.meta.value( QStringLiteral( "locationIdd" ) ) );
  ret.insert( QStringLiteral( "x-coord" ), station.meta.value( QStringLiteral( "longitude" ) ) );
  ret.insert( QStringLiteral( "y-coord" ), station.meta.value( QStringLiteral( "latitude" ) ) );
  ret.insert( QStringLiteral( "crs" ), ReosGisEngine::wktEPSGCrs( 4326 ) );
  ret.insert( QStringLiteral( "start" ), station.meta.value( QStringLiteral( "start-time" ) ) );
  ret.insert( QStringLiteral( "end" ), station.meta.value( QStringLiteral( "end-time" ) ) );

  return ret;
}

void ReosDelftFewsWidget::onOpenFile()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/Delft-FEWS" ) ).toString();
  QString fileName = QFileDialog::getOpenFileName( this, tr( "Open XML Delft-FEWS file" ), path, "XML file (*.xml)" );
  if ( fileName.isEmpty() )
    return;

  QFileInfo fileInfo( fileName );
  settings.setValue( QStringLiteral( "Path/Delft-FEWS" ), fileInfo.path() );

  ui->mLineEditFileName->setText( fileName );
}

void ReosDelftFewsWidget::onFileNameChanged()
{
  if ( !parseFile( ui->mLineEditFileName->text() ) )
    mStationsModel->setStationsList( QList<ReosDelftFewsStation>() );
}

void ReosDelftFewsWidget::onStationChanged()
{
  int stationIndex = ui->mListView->currentIndex().row();

  if ( mCurrentHydrograph )
    delete mCurrentHydrograph;
  mCurrentHydrograph = nullptr;

  if ( mCurrentRainfall )
    delete mCurrentRainfall;
  mCurrentRainfall = nullptr;

  for ( size_t i = 0; i < mStationsMarker.size(); i++ )
  {
    const auto &marker = mStationsMarker.at( i );
    if ( marker )
    {
      if ( stationIndex != -1 && i == static_cast<size_t>( ( stationIndex ) ) )
        marker->setExternalColor( Qt::red );
      else
        marker->setExternalColor( Qt::black );
    }
  }

  if ( stationIndex != -1 )
  {
    const ReosDelftFewsStation &station = mStationsModel->station( stationIndex );

    if ( station.dataType() == ReosDelftFewsXMLHydrographProvider::dataType() )
    {
      mCurrentHydrograph = createHydrograph( this );
      if ( mCurrentHydrograph )
      {
        mHydrographPlot->setTimeSerie( mCurrentHydrograph, false );
        ui->mPlotWidget->setTitleAxeYLeft( tr( "Flow rate (%1)" ).arg( QString( "m%1/s" ).arg( QChar( 0x00B3 ) ) ) );
      }
    }

    if ( station.dataType() == ReosDelftFewsXMLRainfallProvider::dataType() )
    {
      mCurrentRainfall = createRainfall( this );
      if ( mCurrentRainfall )
        mRainfallPlot->setTimeSerie( mCurrentRainfall, false );
    }
    if ( station.dataType() == mDataType )
    {
      emit dataSelectionChanged( true );
      emit dataIsLoading();
      emit dataIsReady();
    }
    else
      emit dataSelectionChanged( false );
  }
  else
  {
    emit dataSelectionChanged( false );
  }

  populateTextBrowser();
  ui->mPlotWidget->updatePlot();
}

void ReosDelftFewsWidget::onStationSelectOnMap( ReosMapItem *item, const QPointF & )
{
  ReosDelftFewsStationMarker *stationMarker = static_cast<ReosDelftFewsStationMarker *>( item );
  if ( stationMarker )
  {
    ui->mListView->setCurrentIndex( mStationsModel->index( stationMarker->stationIndex, 0, QModelIndex() ) );
  }
}

void ReosDelftFewsWidget::populateTextBrowser()
{
  ui->mTextBrowser->document()->setDefaultStyleSheet( ReosApplication::styleSheet() );

  int stationIndex = ui->mListView->currentIndex().row();
  QVariantMap meta;
  if ( stationIndex != -1 )
  {
    const ReosDelftFewsStation &station = mStationsModel->station( stationIndex );
    meta = station.meta;
  }

  ui->mTextBrowser->setText( ReosDelftFewsXMLProviderInterface::htmlDescriptionFromMetada( meta ) );
}

bool ReosDelftFewsWidget::parseFile( const QString &fileName )
{
  QDomDocument xmlDoc( "delft-document" );
  QFile file( fileName );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;
  if ( !xmlDoc.setContent( &file ) )
  {
    file.close();
    return false;
  }
  file.close();

  QDomElement rooElement = xmlDoc.firstChildElement( QStringLiteral( "TimeSeries" ) );

  QList<ReosDelftFewsStation> stations;

  mStationsMarker.clear();

  QDomElement serieElement = rooElement.firstChildElement( QStringLiteral( "series" ) );
  if ( serieElement.isNull() )
    return false;

  do
  {
    QDomElement headerElement = serieElement.firstChildElement( QStringLiteral( "header" ) );
    if ( headerElement.isNull() )
      continue;

    ReosDelftFewsStation station;

    if ( !headerElement.firstChildElement( QStringLiteral( "parameterId" ) ).isNull() )
    {
      const QString paramater = ReosDelftFewsXMLProviderInterface::valueStringFromElement( headerElement.firstChildElement( QStringLiteral( "parameterId" ) ) );
      if ( paramater.isEmpty() )
        continue;
      const QString prefix = paramater.split( QString( '.' ) ).at( 0 );
      if ( prefix == QString( 'P' ) )
        station.meta[QStringLiteral( "data-type" )] = ReosDelftFewsXMLRainfallProvider::dataType();
      else if ( prefix == QString( 'Q' ) )
        station.meta[QStringLiteral( "data-type" )] = ReosDelftFewsXMLHydrographProvider::dataType();
      else
        continue;
    }
    else
      continue;

    bool isSpatial = true;
    double longitude = 0;
    double latitude = 0;

    if ( !headerElement.firstChildElement( QStringLiteral( "stationName" ) ).isNull() )
      station.meta[QStringLiteral( "name" )] = ReosDelftFewsXMLProviderInterface::valueStringFromElement( headerElement.firstChildElement( QStringLiteral( "stationName" ) ) );

    if ( !headerElement.firstChildElement( QStringLiteral( "lat" ) ).isNull() )
    {
      QDomElement element = headerElement.firstChildElement( QStringLiteral( "lat" ) );
      QString latStr = element.firstChild().nodeValue();
      bool ok = false;
      latitude = latStr.toDouble( &ok );
      isSpatial &= ok;
      if ( isSpatial )
        station.meta[QStringLiteral( "latitude" )] = latitude;
    }

    if ( !headerElement.firstChildElement( QStringLiteral( "lon" ) ).isNull() )
    {
      QDomElement element = headerElement.firstChildElement( QStringLiteral( "lon" ) );
      QString lonStr = element.firstChild().nodeValue();
      bool ok = false;
      longitude = lonStr.toDouble( &ok );
      isSpatial &= ok;
      if ( isSpatial )
        station.meta[QStringLiteral( "longitude" )] = longitude;
    }

    if ( !headerElement.firstChildElement( QStringLiteral( "locationId" ) ).isNull() )
      station.meta[QStringLiteral( "location-id" )] = ReosDelftFewsXMLProviderInterface::valueStringFromElement( headerElement.firstChildElement( QStringLiteral( "locationId" ) ) );

    if ( !headerElement.firstChildElement( QStringLiteral( "startDate" ) ).isNull() )
      station.meta[QStringLiteral( "start-time" )] = ReosDelftFewsXMLProviderInterface::timefromElement( headerElement.firstChildElement( QStringLiteral( "startDate" ) ) );

    if ( !headerElement.firstChildElement( QStringLiteral( "endDate" ) ).isNull() )
      station.meta[QStringLiteral( "end-time" )] = ReosDelftFewsXMLProviderInterface::timefromElement( headerElement.firstChildElement( QStringLiteral( "endDate" ) ) );

    if ( isSpatial )
    {
      const QPointF geoPt = QPointF( longitude, latitude );
      const QString crs = ReosGisEngine::wktEPSGCrs( 4326 );
      const QPointF pt = mMap->engine()->transformToProjectCoordinates( crs, geoPt );
      mStationsMarker.emplace_back( std::make_unique<ReosDelftFewsStationMarker>( mMap, pt ) );
      mStationsMarker.back()->stationIndex = stations.count();
    }
    else
      mStationsMarker.push_back( nullptr );

    stations.append( station );

    serieElement = serieElement.nextSiblingElement( QStringLiteral( "series" ) );
  }
  while ( !serieElement .isNull() );

  mStationsModel->setStationsList( stations );

  ui->mListView->setCurrentIndex( mStationsModel->index( 0, 0, QModelIndex() ) );

  return true;
}

ReosHydrograph *ReosDelftFewsWidget::createHydrograph( QObject *parent ) const
{
  std::unique_ptr<ReosHydrograph> hyd =
    std::make_unique<ReosHydrograph>( parent, ReosDelftFewsXMLHydrographProvider::staticKey()
                                      + QStringLiteral( "::" )
                                      + ReosDelftFewsXMLHydrographProvider::dataType(), currentUri() );
  hyd->setColor( QColor( 92, 142, 177 ) );
  return hyd.release();
}

ReosSerieRainfall *ReosDelftFewsWidget::createRainfall( QObject *parent ) const
{
  std::unique_ptr<ReosSerieRainfall> rainfall =
    std::make_unique<ReosSerieRainfall>( parent, ReosDelftFewsXMLHydrographProvider::staticKey()
                                         + QStringLiteral( "::" )
                                         + ReosDelftFewsXMLRainfallProvider::dataType(), currentUri() );

  rainfall->setValueMode( ReosTimeSerieConstantInterval::Intensity );
  return rainfall.release();
}

QString ReosDelftFewsWidget::currentUri() const
{
  int stationIndex = ui->mListView->currentIndex().row();
  if ( stationIndex < 0 )
    return nullptr;

  const ReosDelftFewsStation &station = mStationsModel->station( stationIndex );

  QString uri = QStringLiteral( "\"" ) + ui->mLineEditFileName->text() + QStringLiteral( "\"::" )
                + station.meta.value( QStringLiteral( "location-id" ) ).toString() + QStringLiteral( "::" )
                + station.meta.value( QStringLiteral( "start-time" ) ).toDateTime().toString( Qt::ISODate ) + QStringLiteral( "::" )
                + station.meta.value( QStringLiteral( "end-time" ) ).toDateTime().toString( Qt::ISODate );

  return uri;
}

ReosDataProviderGuiFactory::GuiCapabilities ReosDelftFewsGuiFactory::capabilities() const
{
  GuiCapabilities cap;
  cap |= ReosDataProviderGuiFactory::GuiCapability::DataSelector;
  cap |= ReosDataProviderGuiFactory::GuiCapability::ProviderSettings;
  cap |= ReosDataProviderGuiFactory::GuiCapability::StationIdentification;
  return cap;
}

QString ReosDelftFewsGuiFactory::key() const
{
  return ReosDelftFewsXMLProviderInterface::staticKey();
}

ReosDelftFewsWidget *ReosDelftFewsGuiFactory::createProviderSelectorWidget( ReosMap *map, const QString &dataType, QWidget *parent ) const
{
  return new ReosDelftFewsWidget( map, dataType, parent );
}

ReosDelftFewsSettingsWidget *ReosDelftFewsGuiFactory::createProviderSettingsWidget( ReosDataProvider *provider, QWidget *parent ) const
{
  return new ReosDelftFewsSettingsWidget( provider, parent );
}

QString ReosDelftFewsGuiFactory::dataType() const
{
  return ReosDelftFewsXMLHydrographProvider::dataType() + QStringLiteral( "::" ) + ReosDelftFewsXMLRainfallProvider::dataType();
}

QPixmap ReosDelftFewsGuiFactory::icon() const
{
  return QPixmap();
}

QString ReosDelftFewsGuiFactory::displayText() const
{
  return QStringLiteral( "Delft-FEWS" );
}

ReosDelftFewsStationsModel::ReosDelftFewsStationsModel( const QString &dataType, QObject *parent )
  : QAbstractListModel( parent )
  , mDataType( dataType )
{}

QModelIndex ReosDelftFewsStationsModel::index( int row, int column, const QModelIndex & ) const
{
  return createIndex( row, column );
}

QModelIndex ReosDelftFewsStationsModel::parent( const QModelIndex & ) const
{
  return QModelIndex();
}

int ReosDelftFewsStationsModel::rowCount( const QModelIndex & ) const
{
  return mStations.count();
}

int ReosDelftFewsStationsModel::columnCount( const QModelIndex & ) const
{
  return 1;
}

QVariant ReosDelftFewsStationsModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() < mStations.count() )
  {
    switch ( role )
    {
      case Qt::DisplayRole:
        return mStations.at( index.row() ).meta.value( QStringLiteral( "name" ) );
        break;
      case Qt::ForegroundRole:
        if ( mDataType.isEmpty() || mStations.at( index.row() ).dataType() == mDataType )
          return QColor( Qt::black );
        else
          return QColor( Qt::gray );
        break;
    }
  }
  return QVariant();
}

void ReosDelftFewsStationsModel::setStationsList( const QList<ReosDelftFewsStation> &stations )
{
  beginResetModel();
  mStations = stations;
  endResetModel();
}

ReosDelftFewsStation ReosDelftFewsStationsModel::station( int i ) const
{
  return mStations.at( i );
}

ReosDelftFewsStationMarker::ReosDelftFewsStationMarker( ReosMap *map, const QPointF &point ): ReosMapMarkerFilledCircle( map, point )
{
  setColor( QColor( 92, 142, 177 ) );
  setWidth( 10 );
  setExternalColor( Qt::black );
  setExternalWidth( 14 );
  setDescription( QStringLiteral( "delft-fews-station" ) );
}

QString ReosDelftFewsStation::dataType() const
{
  return meta.value( QStringLiteral( "data-type" ) ).toString();
}

REOSEXTERN ReosDataProviderGuiFactory *providerGuiFactory()
{
  return new ReosDelftFewsGuiFactory();
}
