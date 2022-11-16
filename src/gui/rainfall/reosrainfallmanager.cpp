/***************************************************************************
  reosrainfallmanager.cpp - ReosRainfallManager

 ---------------------
 begin                : 24.1.2021
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
#include "reosrainfallmanager.h"
#include "ui_reosrainfallmanager.h"

#include <QAction>
#include <QFileDialog>
#include <QMenu>
#include <QMessageBox>
#include <QPushButton>
#include <QCloseEvent>
#include <QToolBar>
#include <QTreeView>
#include <QToolButton>
#include <QLabel>
#include <QDialogButtonBox>
#include <QSplitter>

#include "reossettings.h"
#include "reosrainfallmodel.h"
#include "reosrainfallitem.h"
#include "reosparameterwidget.h"
#include "reosplotwidget.h"
#include "reosformwidget.h"
#include "reosimportfromtextfile.h"
#include "reostextfiledata.h"
#include "reosintensitydurationselectedcurvewidget.h"
#include "reosrainfallregistery.h"
#include "reosidfcurves.h"
#include "reosmaptool.h"
#include "reosplotidfcurve.h"
#include "reosplottimeconstantinterval.h"
#include "reosrainfalldataform.h"
#include "reosgisengine.h"
#include "reosdataprovidergui.h"
#include "reosguicontext.h"
#include "reosstyleregistery.h"


class ReosStationMapMarker: public ReosMapMarkerSvg
{
  public:
    ReosStationMapMarker( ReosMap *map, ReosStationItem *item )
      : ReosMapMarkerSvg( QStringLiteral( ":/images/station.svg" ), map, item->position() )
      , item( item )
    {
      setDescription( staticDescritpion() );
    }

    ReosStationItem *item = nullptr;

    static QString staticDescritpion() {return QStringLiteral( "rainfall-station" );}
};

ReosRainfallManager::ReosRainfallManager( ReosMap *map, ReosRainfallModel *rainfallmodel, QWidget *parent ) :
  ReosActionWidget( parent )
  , ui( new Ui::ReosRainfallManager )
  , mMap( map )
  , mModel( rainfallmodel )
  , mActionOpenRainfallDataFile( new QAction( QIcon( QStringLiteral( ":/images/openRainfall.svg" ) ), tr( "Open Rainfal Data File" ), this ) )
  , mActionSaveRainfallDataFile( new QAction( QIcon( QStringLiteral( ":/images/saveRainfall.svg" ) ), tr( "Save Rainfal Data File" ), this ) )
  , mActionSaveAsRainfallDataFile( new QAction( QIcon( QStringLiteral( ":/images/saveAsRainfall.svg" ) ), tr( "Save Rainfal Data File as ..." ), this ) )
  , mActionAddRootZone( new QAction( QIcon( QStringLiteral( ":/images/addZone.svg" ) ), tr( "Add New Zone to the Root" ), this ) )
  , mActionAddZoneToZone( new QAction( QIcon( QStringLiteral( ":/images/addZone.svg" ) ), tr( "Add New Sub Zone" ), this ) )
  , mActionAddStation( new QAction( QIcon( QStringLiteral( ":/images/addStation.svg" ) ), tr( "Add Station" ), this ) )
  , mActionAddStationFromMap( new QAction( QIcon( QStringLiteral( ":/images/addStation.svg" ) ), tr( "Add Station from Map" ), this ) )
  , mActionAddGaugedRainfall( new QAction( QIcon( QStringLiteral( ":/images/addGaugedRainfall.svg" ) ), tr( "Gauged Rainfall in a Table" ), this ) )
  , mActionAddChicagoRainfall( new QAction( QIcon( QStringLiteral( ":/images/chicagoRainfall.svg" ) ), tr( "Add Chicago Rainfall" ), this ) )
  , mActionAddAlternatingBlockRainfall( new QAction( QIcon( QStringLiteral( ":/images/alternatingBlockRainfall.svg" ) ), tr( "Add Alternating Block Rainfall" ), this ) )
  , mActionAddDoubleTriangleRainfall( new QAction( QIcon( QStringLiteral( ":/images/doubleTriangleRainfall.svg" ) ), tr( "Add Double Triangle Rainfall" ), this ) )
  , mActionAddIDFCurves( new QAction( QIcon( QStringLiteral( ":/images/addIntensityDurationCurves.svg" ) ), tr( "Add Intensity Duration Frequency Curves" ), this ) )
  , mActionAddIDCurve( new QAction( QIcon( QStringLiteral( ":/images/addIntensityDurationCurve.svg" ) ), tr( "Add Intensity Duration Curve" ), this ) )
  , mActionReorderIdVurve( new QAction( tr( "Reorder Intensity Duration Curves" ), this ) )
  , mActionRemoveItem( new QAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove item" ), this ) )
  , mActionImportFromTextFile( new QAction( QIcon( QStringLiteral( ":/images/importRainfall.svg" ) ), tr( "Import Rainfall from Text File" ), this ) )
  , mActionSelectStationFromMap( new QAction( QIcon( QStringLiteral( ":/images/selectStationOnMap.svg" ) ), tr( "Select Station from Map" ), this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  ui->mSplitter->setStretchFactor( 0, 3 );
  ui->mSplitter->setStretchFactor( 1, 1 );

  ui->mTreeView->setModel( mModel );
  ui->mTreeView->setContextMenuPolicy( Qt::CustomContextMenu );
  ui->mTreeView->setDragEnabled( true );
  ui->mTreeView->setAcceptDrops( true );
  ui->mTreeView->setDragDropMode( QAbstractItemView::InternalMove );

  QToolBar *toolBar = new QToolBar( this );
  ui->mToolBarWidget->layout()->addWidget( toolBar );
  toolBar->addAction( mActionOpenRainfallDataFile );
  toolBar->addAction( mActionSaveRainfallDataFile );
  toolBar->addAction( mActionSaveAsRainfallDataFile );
  toolBar->addAction( mActionAddRootZone );
  toolBar->addAction( mActionImportFromTextFile );
  toolBar->addAction( mActionSelectStationFromMap );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );

  mMapToolAddStationOnMap = new ReosMapToolDrawPoint( this, map );
  mMapToolAddStationOnMap->setCursor( QCursor( QStringLiteral( ":/images/station.svg" ), 12, 12 ) ) ;
  mMapToolAddStationOnMap->setAction( mActionAddStationFromMap );
  mActionAddStationFromMap->setCheckable( true );
  connect( mMapToolAddStationOnMap, &ReosMapTool::activated, this, [this] {ui->mTreeView->setEnabled( false );} );
  connect( mMapToolAddStationOnMap, &ReosMapTool::deactivated, this, [this] {ui->mTreeView->setEnabled( true );} );

  mMapToolSelectStation = new ReosMapToolSelectMapItem( this, map, ReosStationMapMarker::staticDescritpion() );
  mMapToolSelectStation->setAction( mActionSelectStationFromMap );
  mActionSelectStationFromMap->setCheckable( true );
  mMapToolSelectStation->setCursor( Qt::ArrowCursor );
  connect( mMapToolSelectStation, &ReosMapToolSelectMapItem::found, this, [this]( ReosMapItem * mapItem, const QPointF & )
  {
    if ( !mapItem || mapItem->description() != ReosStationMapMarker::staticDescritpion() )
      return;

    ReosStationMapMarker *marker = static_cast<ReosStationMapMarker *>( mapItem );

    selectItem( marker->item );
  } );

  connect( mMap, &ReosMap::crsChanged, this, &ReosRainfallManager::updateMarkers );

  connect( mActionOpenRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::onOpenRainfallFile );
  connect( mActionSaveRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::saveRainfallFile );
  connect( mActionSaveAsRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::onSaveAsRainfallFile );
  connect( mActionImportFromTextFile, &QAction::triggered, this, &ReosRainfallManager::onImportFromTextFile );

  mActionsAddSyntheticRainfall << mActionAddChicagoRainfall
                               << mActionAddDoubleTriangleRainfall
                               << mActionAddAlternatingBlockRainfall;

  mActionsAddStations << mActionAddStation
                      << mActionAddStationFromMap;

  mActionsAddGaugedRainfall << mActionAddGaugedRainfall;

  populateProviderActions( toolBar );

  connect( mActionAddRootZone, &QAction::triggered, this, &ReosRainfallManager::onAddRootZone );
  connect( mActionAddZoneToZone, &QAction::triggered, this, &ReosRainfallManager::onAddZoneToZone );
  connect( mActionAddStation, &QAction::triggered, this, &ReosRainfallManager::onAddStation );
  connect( mMapToolAddStationOnMap, &ReosMapToolDrawPoint::drawn, this, &ReosRainfallManager::onAddStationOnMap );
  connect( mActionAddGaugedRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddGaugedRainfall );
  connect( mActionAddChicagoRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddChicagoRainfall );
  connect( mActionAddAlternatingBlockRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddAlternatingBlockRainfall );
  connect( mActionAddDoubleTriangleRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddDoubleTriangleRainfall );
  connect( mActionAddIDFCurves, &QAction::triggered, this, &ReosRainfallManager::onAddIDFCurves );
  connect( mActionAddIDCurve, &QAction::triggered, this, &ReosRainfallManager::onAddIDCurve );
  connect( mActionReorderIdVurve, &QAction::triggered, this, &ReosRainfallManager::onReorderIDCurve );

  connect( mActionRemoveItem, &QAction::triggered, this, &ReosRainfallManager::onRemoveItem );

  connect( ui->mTreeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosRainfallManager::onCurrentTreeIndexChanged );
  connect( ui->mTreeView, &QWidget::customContextMenuRequested, this, &ReosRainfallManager::onTreeViewContextMenu );

  connect( this, &ReosActionWidget::opened, this, [this] {setMarkersVisible( true );} );
  connect( this, &ReosActionWidget::closed, this, [this]
  {
    if ( ui->stackedWidget->currentIndex() == 1 )
      backToMainIndex();
    setMarkersVisible( false );
  } );

  connect( ui->mProviderBackButton, &QPushButton::clicked, this, &ReosRainfallManager::backToMainIndex );
  connect( ui->mProviderAddButton, &QPushButton::clicked, this, [this]
  {
    addDataFromProvider( false );
  } );
  connect( ui->mProviderAddCopyButton, &QPushButton::clicked, this, [this]
  {
    addDataFromProvider( true );
  } );

  ReosIdfFormulaRegistery::instance()->registerFormula( new ReosIdfFormulaMontana );
  ReosIdfFormulaRegistery::instance()->registerFormula( new ReosIdfFormulaSherman );

  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallIntensityDurationFrequencyFactory );
  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallIntensityDurationFactory );
  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallSerieFactory );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetTimeSerieConstantIntervalFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetChicagoRainfalFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetAlternatingBlockRainfalFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetDoubleTriangleRainfalFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetIntensityDurationCurveFactory );
}

ReosRainfallManager::~ReosRainfallManager()
{
  delete ui;
}

void ReosRainfallManager::loadDataFile()
{
  QString fileName;
  ReosSettings settings;
  fileName = settings.value( QStringLiteral( "Rainfall/dataFile" ) ).toString();

  if ( fileName.isEmpty() )
    return;

  if ( mModel->loadFromFile( fileName ) )
  {
    ui->labelFileName->setText( fileName );
    buildMarkers();
  }
  else
    QMessageBox::warning( this, tr( "Open Rainfall Data" ), tr( "Unable to open the current rainfall data file: %1" ).arg( fileName ) );

  ui->mTreeView->expandAll();
  ui->mTreeView->resizeColumnToContents( 0 );
  selectItem( mModel->indexToItem( mModel->index( 0, 0, QModelIndex() ) ) );
}

void ReosRainfallManager::onOpenRainfallFile()
{
  if ( mModel->rootZoneCount() > 0 )
  {
    int ret = QMessageBox::warning( this, tr( "Open Rainfall Data File" ),
                                    tr( "This action will remove the actual rainfall data, do you want to save before?" ),
                                    QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel );

    if ( ret == QMessageBox::Cancel )
      return;

    if ( ret == QMessageBox::Yes )
    {
      saveRainfallFile();
    }
  }

  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "Rainfall/fileDirectory" ) ).toString();
  QString fileName = QFileDialog::getOpenFileName( this, tr( "Open Rainfall Data" ), dir, QStringLiteral( " *.rrf" ) );

  if ( fileName.isEmpty() )
    return;

  if ( mModel->loadFromFile( fileName ) )
  {
    ui->labelFileName->setText( fileName );
    settings.setValue( QStringLiteral( "Rainfall/dataFile" ), fileName );
    QFileInfo fileInfo( fileName );
    settings.setValue( QStringLiteral( "Rainfall/fileDirectory" ), fileInfo.path() );
    buildMarkers();
  }
  else
  {
    QMessageBox::critical( this, tr( "Open Rainfall Data" ), tr( "Unable to open the file: %1" ).arg( fileName ) );
  }
}


bool ReosRainfallManager::saveOnFile( const QString &fileName )
{
  return mModel->saveToFile( fileName );
}

QList<QAction *> ReosRainfallManager::dataItemActions( ReosRainfallDataItem *dataItem )
{
  QList<QAction *> actions;
  if ( !dataItem )
    return actions;

  if ( dataItem->dataType() == QStringLiteral( "idf-curves" ) || dataItem->dataType() == QStringLiteral( "id-curve" ) )
  {
    actions.append( mActionAddIDCurve );
    actions.append( mActionReorderIdVurve );
  }

  return actions;
}

bool ReosRainfallManager::addSimpleItemDialog( const QString &title, QString nameLabel, QString &name, QString &descript )
{
  ReosParameterString string( nameLabel );
  if ( !name.isEmpty() )
    string.setValue( name );
  std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
  dial->addParameter( &string );
  ReosParameterString description( tr( "Description" ) );
  if ( !descript.isEmpty() )
    description.setValue( descript );
  dial->addParameter( &description );
  dial->setWindowTitle( title );

  if ( dial->exec() )
  {
    name = string.value();
    descript = description.value();
    return true;
  }

  return false;
}

void ReosRainfallManager::populateProviderActions( QToolBar *toolBar )
{
  if ( !toolBar->actions().isEmpty() )
    toolBar->addSeparator();

  const QString dataType = QStringLiteral( "rainfall" );

  ReosDataProviderGuiRegistery *registery = ReosDataProviderGuiRegistery::instance();

  const QStringList providers =
    registery->providers( dataType, ReosDataProviderGuiFactory::GuiCapability::DataSelector );

  for ( const QString &providerKey : providers )
  {
    if ( registery->hasCapability( providerKey, ReosDataProviderGuiFactory::GuiCapability::StationIdentification ) )
    {
      QAction *actionAddStation = new QAction( registery->providerIcon( providerKey ), tr( "From %1" ).arg( registery->providerDisplayText( providerKey ) ) );
      mActionsAddStations.append( actionAddStation );

      connect( actionAddStation, &QAction::triggered, this, [this, providerKey]
      {
        showProviderSelector( providerKey );
      } );
    }

    QAction *actionAddRainfall = new QAction( registery->providerIcon( providerKey ), tr( "From %1" ).arg( registery->providerDisplayText( providerKey ) ) );
    mActionsAddGaugedRainfall.append( actionAddRainfall );
    connect( actionAddRainfall, &QAction::triggered, this, [this, providerKey]
    {
      showProviderSelector( providerKey );
    } );

  }
}

void ReosRainfallManager::showProviderSelector( const QString &providerKey )
{
  const QString dataType = ReosSeriesRainfall::staticType();
  ReosDataProviderGuiRegistery *registery = ReosDataProviderGuiRegistery::instance();

  if ( mCurrentProviderSelector )
  {
    ui->mProviderLayout->removeWidget( mCurrentProviderSelector );
    delete mCurrentProviderSelector;
  }

  mCurrentProviderSelector = registery->createProviderSelectorWidget( providerKey, dataType, mMap, this );
  if ( !mCurrentProviderSelector )
    return;

  ui->mProviderLayout->addWidget( mCurrentProviderSelector );
  ui->stackedWidget->setCurrentIndex( 1 );

  ui->mProviderAddButton->setEnabled( false );
  ui->mProviderAddCopyButton->setEnabled( false );

  connect( mCurrentProviderSelector, &ReosDataProviderSelectorWidget::dataSelectionChanged, this, [this]( bool dataSelected )
  {
    ui->mProviderAddButton->setEnabled( dataSelected );
    ui->mProviderAddCopyButton->setEnabled( dataSelected );
  } );

  connect( mCurrentProviderSelector, &ReosDataProviderSelectorWidget::dataIsLoading, this, [this]
  {
    ui->mProviderAddButton->setEnabled( true );
    ui->mProviderAddCopyButton->setEnabled( false );
  } );

  connect( mCurrentProviderSelector, &ReosDataProviderSelectorWidget::dataIsReady, this, [this]
  {
    ui->mProviderAddCopyButton->setEnabled( true );
  } );
}


void ReosRainfallManager::backToMainIndex()
{
  ui->stackedWidget->setCurrentIndex( 0 );
  ui->providerPage->layout()->removeWidget( mCurrentProviderSelector );
  mCurrentProviderSelector->deleteLater();
  mCurrentProviderSelector = nullptr;
}

void ReosRainfallManager::addDataFromProvider( bool copy )
{
  if ( !mCurrentProviderSelector )
    return;

  QVariantMap meta = mCurrentProviderSelector->selectedMetadata();

  QString providerName = ReosDataProviderGuiRegistery::instance()->providerDisplayText( meta.value( QStringLiteral( "provider-key" ) ).toString() );

  QModelIndex index = ui->mTreeView->currentIndex();
  ReosRainfallItem *item = nullptr;

  if ( index.isValid() )
    item = mModel->indexToItem( index );

  if ( !item )
  {
    backToMainIndex();
    return;
  }

  switch ( item->type() )
  {
      break;
    case ReosRainfallItem::Zone:
      addRainfallFromProvider( qobject_cast<ReosZoneItem *>( item ), meta, copy );
      break;
    case ReosRainfallItem::Station:
      addRainfallFromProvider( qobject_cast<ReosStationItem *>( item ), meta, copy );
      break;
    case ReosRainfallItem::Data:
    case ReosRainfallItem::Root:
      break;
  }

  backToMainIndex();
}

void ReosRainfallManager::addRainfallFromProvider( ReosZoneItem *destination, const QVariantMap &meta, bool copy )
{
  QString stationName = meta.value( QStringLiteral( "station" ) ).toString();
  QString stationDescription = meta.value( QStringLiteral( "station-descritpion" ) ).toString();
  QString providerName = ReosDataProviderGuiRegistery::instance()->providerDisplayText( meta.value( QStringLiteral( "provider-key" ) ).toString() );

  for ( int i = 0; i < destination->childrenCount(); ++i )
  {
    ReosStationItem *otherStation = qobject_cast<ReosStationItem *>( destination->itemAt( i ) );
    if ( !otherStation )
      continue;
    if ( otherStation->name() == stationName )
    {
      switch ( QMessageBox::warning( this,
                                     tr( "Add a station from %1" ).arg( providerName ),
                                     tr( "The zone \"%1\" has already a station with name %2. Do you want to add the rainfall in this station?\n"
                                         "If not, another station will be created" ).arg( destination->name(), stationName ),
                                     QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Cancel ) )
      {
        case QMessageBox::Yes:
          addRainfallFromProvider( otherStation, meta, copy );
          return;
          break;
        case QMessageBox::No:
        {
          stationName.append( '_' + tr( "copy" ) );
          if ( !addSimpleItemDialog( tr( "Create Other Station" ), tr( "Station name" ), stationName, stationDescription ) )
            return;
          QVariantMap otherMeta = meta;
          otherMeta[QStringLiteral( "station" )] = stationName;
          addRainfallFromProvider( destination, otherMeta, copy );
          return;
        }
        break;
        case QMessageBox::Cancel:
          return;
          break;
        default:
          break;

      }
    }
  }

  ReosSpatialPosition position;
  if ( meta.contains( QStringLiteral( "x-coord" ) ) &&
       meta.contains( QStringLiteral( "y-coord" ) ) &&
       meta.contains( QStringLiteral( "crs" ) ) )
  {
    bool okX = false;
    bool okY = false;
    double x = meta.value( QStringLiteral( "x-coord" ) ).toDouble( &okX );
    double y = meta.value( QStringLiteral( "y-coord" ) ).toDouble( &okY );
    if ( okX && okY )
      position = ReosSpatialPosition( QPointF( x, y ), meta.value( QStringLiteral( "crs" ) ).toString() );
  }

  ReosStationItem *newStationItem = mModel->addStation( stationName, stationDescription, mModel->itemToIndex( destination ), position );
  addMapItem( newStationItem );

  addRainfallFromProvider( newStationItem, meta, copy );

  ui->mTreeView->expand( mModel->itemToIndex( newStationItem ) );

}

void ReosRainfallManager::addRainfallFromProvider( ReosStationItem *stationItem, const QVariantMap &meta, bool copy )
{
  if ( !mCurrentProviderSelector )
    return;

  QString dateFormat = QLocale().dateFormat( QLocale::ShortFormat );
  QString rainfallName = tr( "From %1 to %2" ).arg( meta.value( "start" ).toDateTime().toString( dateFormat ),
                         meta.value( "end" ).toDateTime().toString( dateFormat ) );

  addRainfallFromProvider( stationItem, rainfallName, copy );

}

void ReosRainfallManager::addRainfallFromProvider( ReosStationItem *stationItem, const QString &rainfallName, bool copy )
{
  if ( !mCurrentProviderSelector )
    return;

  QString descritpion;

  if ( stationItem->hasChildItemName( rainfallName ) )
  {
    switch ( QMessageBox::warning( this,
                                   tr( "Add a Rainfall" ),
                                   tr( "The station \"%1\" has already a rainfall with name \"%2\". Do you want to add the rainfall with another name?" ).arg( stationItem->name(), rainfallName ),
                                   QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Cancel ) )
    {
      case QMessageBox::Yes:
      {
        QString otherName = rainfallName;
        otherName.append( '_' + tr( "copy" ) );

        if ( !addSimpleItemDialog( tr( "Add a Rainfall" ), tr( "Rainfall name" ), otherName, descritpion ) )
          return;
        addRainfallFromProvider( stationItem, otherName, copy );
        return;
      }
      break;
      case QMessageBox::Cancel:
        return;
        break;
      default:
        break;

    }
  }

  std::unique_ptr<ReosSeriesRainfall> newRainfall;

  if ( copy )
  {
    newRainfall = std::make_unique<ReosSeriesRainfall>();
    newRainfall->copyFrom( qobject_cast<ReosSeriesRainfall *>( mCurrentProviderSelector->selectedData() ) );
  }
  else
  {
    newRainfall.reset( qobject_cast<ReosSeriesRainfall *>( mCurrentProviderSelector->createData() ) ) ;
  }

  selectItem( mModel->addGaugedRainfall( rainfallName,
                                         descritpion,
                                         mModel->itemToIndex( stationItem ),
                                         newRainfall.release() ) );
}

void ReosRainfallManager::onSaveAsRainfallFile()
{
  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "Rainfall/fileDirectory" ) ).toString();
  QString fileName = QFileDialog::getSaveFileName( this, tr( "Save Rainfall Data as..." ), dir, QStringLiteral( " *.rrf" ) );

  if ( fileName.isEmpty() )
    return;

  QFileInfo fileInfo( fileName );
  if ( fileInfo.suffix().isEmpty() )
    fileName.append( QStringLiteral( ".rrf" ) );

  if ( !saveOnFile( fileName ) )
    QMessageBox::warning( this, tr( "Save Rainfall Data as..." ), tr( "Unable to write the file" ) );
  else
  {
    ui->labelFileName->setText( fileName );
    settings.setValue( QStringLiteral( "Rainfall/dataFile" ), fileName );
    settings.setValue( QStringLiteral( "Rainfall/fileDirectory" ), fileInfo.path() );
  }
}

void ReosRainfallManager::onAddRootZone()
{
  QString name;
  QString description;

  if ( addSimpleItemDialog( tr( "Add Zone on Root" ), tr( "Zone name" ), name, description ) )
    selectItem( mModel->addZone( name, description ) );
}


void ReosRainfallManager::saveRainfallFile()
{
  QFileInfo fileInfo( ui->labelFileName->text() );
  if ( !fileInfo.exists() )
  {
    onSaveAsRainfallFile();
    return;
  }

  if ( !saveOnFile( ui->labelFileName->text() ) )
    QMessageBox::warning( this, tr( "Save Rainfall Data" ), tr( "Unable to write the file" ) );
}

void ReosRainfallManager::onAddZoneToZone()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Zone" ), tr( "Zone name" ), name, description ) )
      selectItem( mModel->addZone( name, description, index ) );
  }
}

void ReosRainfallManager::onAddStation()
{
  addStation();
}

void ReosRainfallManager::addStation( const QPointF &point, bool isSpattial )
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Station" ), tr( "Station name" ), name, description ) )
    {
      ReosSpatialPosition position;

      if ( isSpattial )
        position = ReosSpatialPosition( point, mMap->engine()->crs() );

      ReosStationItem *stationItem = mModel->addStation( name, description, index, position );

      addMapItem( stationItem );

      selectItem( stationItem );
    }
  }
}

ReosSpatialStationWidgetToolbar::ReosSpatialStationWidgetToolbar( ReosMap *map,  ReosMapItem *marker, QWidget *parent )
  : QWidget( parent )
  , mCurrentMarker( marker )
{
  setLayout( new QVBoxLayout );
  setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( this );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  mActionSetPosition = toolBar->addAction( QIcon( QStringLiteral( ":/images/station.svg" ) ), tr( "Set Position on Map" ) );
  mActionMovePosition = toolBar->addAction( QIcon( QStringLiteral( ":/images/moveStation.svg" ) ), tr( "Move Position on Map" ) );
  mActiontRemovePosition = toolBar->addAction( QIcon( QStringLiteral( ":/images/removeStation.svg" ) ), tr( "Remove Position on Map" ) );
  mActionMapOnStation = toolBar->addAction( QIcon( QStringLiteral( ":/images/mapOnStation.svg" ) ), tr( "Move Map on Station Position" ) );
  layout()->addWidget( toolBar );

  mSetPositionTool = new ReosMapToolDrawPoint( this, map );
  mSetPositionTool->setCursor( QCursor( QStringLiteral( ":/images/station.svg" ), 12, 12 ) );
  mSetPositionTool->setAction( mActionSetPosition );
  mActionSetPosition->setCheckable( true );

  connect( mSetPositionTool, &ReosMapToolDrawPoint::drawn, this, [map, this]( const QPointF & point )
  {
    ReosSpatialPosition position( point, map->engine()->crs() );
    emit setMarker( position );
    mSetPositionTool->quitMap();
    updateTools();
  } );

  mMovePositionTool = new ReosMapToolMoveMapItem( this, map );
  mMovePositionTool->setAction( mActionMovePosition );
  mActionMovePosition->setCheckable( true );
  mMovePositionTool->setCurrentMapItem( marker );

  connect( mMovePositionTool, &ReosMapToolMoveMapItem::itemMoved, this, [map, this]( ReosMapItem * item )
  {
    ReosSpatialPosition position( static_cast<ReosMapMarker *>( item )->mapPoint(), map->engine()->crs() );
    emit movePosition( position );
  } );

  connect( mActiontRemovePosition, &QAction::triggered, this, [this]
  {
    if ( QMessageBox::warning( this, tr( "Remove spatial position of a rainfall station" ),
                               tr( "Do you want to remove the spatial position of the station?" ), QMessageBox::Yes | QMessageBox::No, QMessageBox::No )
         == QMessageBox::Yes )
    {
      mCurrentMarker = nullptr;
      emit removeMarker();
      updateTools();
    }
  } );

  connect( mActionMapOnStation, &QAction::triggered, this, [this]
  {
    emit mapOnMarker();
  } );

  updateTools();
}

ReosSpatialStationWidgetToolbar::~ReosSpatialStationWidgetToolbar()
{}

void ReosSpatialStationWidgetToolbar::setCurrentMarker( ReosMapItem *currentMarker )
{
  mCurrentMarker = currentMarker;
  mMovePositionTool->setCurrentMapItem( currentMarker );
  updateTools();
}

void ReosSpatialStationWidgetToolbar::updateTools()
{
  mActionMapOnStation->setEnabled( mCurrentMarker );
  mActionMovePosition->setEnabled( mCurrentMarker );
  mActiontRemovePosition->setEnabled( mCurrentMarker );
  mActionSetPosition->setEnabled( !mCurrentMarker );
}


ReosFormWidget *ReosRainfallManager::createForm( ReosRainfallItem *item )
{
  ReosFormWidget *form = new ReosFormWidget( this, Qt::Vertical, false );
  form->addParameters( item->parameters() );

  ReosStationItem *stationItem = qobject_cast<ReosStationItem *>( item );
  if ( stationItem )
  {
    setupFormForStation( form, stationItem );
  }

  ReosGuiContext context( this );
  if ( !item->data() ||  !form->addData( item->data(), context ) )
  {
    form->addItem( new QSpacerItem( 20, 40, QSizePolicy::Ignored, QSizePolicy::Expanding ) );
  }

  form->setStretch( form->count() - 1, 1 );

  return form;
}

void ReosRainfallManager::setupFormForStation( ReosFormWidget *form, ReosStationItem *stationItem )
{
  auto it = mStationsMarker.find( stationItem );
  ReosMapItem *mapItem = nullptr;
  if ( it != mStationsMarker.end() )
    mapItem = it->second.get();

  ReosSpatialStationWidgetToolbar *stationWidget = new ReosSpatialStationWidgetToolbar( mMap, mapItem, form );
  form->addLine();
  form->addWidget( stationWidget );
  form->addLine();

  connect( stationWidget, &ReosSpatialStationWidgetToolbar::removeMarker, this, [this, stationItem]
  {
    stationItem->setPosition( ReosSpatialPosition() );
    removeMarker( stationItem );
    updateCurrentMapItemMarker( stationItem );
  } );

  connect( stationWidget, &ReosSpatialStationWidgetToolbar::mapOnMarker, this, [this, stationItem]
  {
    mMap->setCenter( stationItem->position() );
  } );

  connect( stationWidget, &ReosSpatialStationWidgetToolbar::movePosition, this, [this, stationItem]( const ReosSpatialPosition & position )
  {
    stationItem->setPosition( position );
    updateCurrentMapItemMarker( stationItem );
  } );

  connect( stationWidget, &ReosSpatialStationWidgetToolbar::setMarker, this, [this, stationItem, stationWidget]( const ReosSpatialPosition & position )
  {
    stationItem->setPosition( position );
    stationWidget->setCurrentMarker( addMapItem( stationItem ) );
    updateCurrentMapItemMarker( stationItem );
  } );

}

void ReosRainfallManager::updateCurrentMapItemMarker( ReosRainfallItem *item )
{
  mCurrentStationMarker.reset();

  ReosStationItem *stationItem = nullptr;

  while ( !stationItem && item->parentItem() )
  {
    stationItem = qobject_cast<ReosStationItem *>( item );
    if ( !stationItem )
      item = item->parentItem();
  }

  if ( stationItem && stationItem->position().isValid() )
  {
    mCurrentStationMarker.reset( new  ReosMapMarkerEmptyCircle( mMap, stationItem->position() ) );
    mCurrentStationMarker->setWidth( 24 );
    mCurrentStationMarker->setColor( Qt::red );
    mCurrentStationMarker->setExternalColor( Qt::white );
    mCurrentStationMarker->setExternalWidth( 32 );
  }
}

void ReosRainfallManager::onAddStationOnMap( const QPointF &point )
{
  if ( mMapToolAddStationOnMap->isActive() )
    mMapToolAddStationOnMap->quitMap();

  addStation( point, true );
}

void ReosRainfallManager::onAddGaugedRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Gauged Rainfall" ), tr( "Rainfall name" ), name, description ) )
      selectItem( mModel->addGaugedRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddChicagoRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Chicago Rainfall" ), tr( "Rainfall name" ), name, description ) )
      selectItem( mModel->addChicagoRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddAlternatingBlockRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Alternating Block Rainfall" ), tr( "Rainfall name" ), name, description ) )
      selectItem( mModel->addAlternatingBlockRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddDoubleTriangleRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Double Triangle Rainfall" ), tr( "Rainfall name" ), name, description ) )
      selectItem( mModel->addDoubleTriangleRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddIDFCurves()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name;
    QString description;

    if ( addSimpleItemDialog( tr( "Add Intensity Duration Frequency Curves" ), tr( "IDF group name" ), name, description ) )
      selectItem( mModel->addIDFCurves( name, description, index ) );
  }
}

void ReosRainfallManager::onAddIDCurve()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
    ReosParameterDuration returnPeriod( tr( "Return period" ) );
    returnPeriod.setValue( ReosDuration( 10, ReosDuration::year ) );
    dial->addParameter( &returnPeriod );
    ReosParameterString descritpion( tr( "Descriprition" ) );
    dial->addParameter( &descritpion );
    dial->setWindowTitle( tr( "Add new Intensity Duration Curve" ) );

    if ( dial->exec() )
      selectItem( mModel->addIDCurve( returnPeriod.value(), descritpion.value(), index ) );
  }
}

void ReosRainfallManager::onReorderIDCurve()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    ReosRainfallItem *item = mModel->indexToItem( index );
    if ( !item )
      return;

    ReosRainfallIdfCurvesItem *idfCurvesItem = qobject_cast<ReosRainfallIdfCurvesItem *>( item );
    if ( !idfCurvesItem )
    {
      //try with parent
      idfCurvesItem = qobject_cast<ReosRainfallIdfCurvesItem *>( item->parentItem() );
      if ( !idfCurvesItem )
        return;
    }

    int childrenCount = idfCurvesItem->childrenCount();
    for ( int i = 0; i < childrenCount; ++i )
    {
      //search for the smallest return period
      int smallest = i;
      for ( int j = i + 1; j < childrenCount; ++j )
      {
        if ( idfCurvesItem->curve( j )->returnPeriod()->value() < idfCurvesItem->curve( smallest )->returnPeriod()->value() )
          smallest = j;
      }

      if ( smallest != i )
        mModel->swapItems( idfCurvesItem, smallest, i );
    }
  }
}

void ReosRainfallManager::onRemoveItem()
{
  QModelIndex index = ui->mTreeView->currentIndex();
  ReosRainfallItem *item = mModel->indexToItem( index );

  if ( !item )
    return;

  if ( QMessageBox::question( this, tr( "Remove item" ), tr( "Remove: %1" ).arg( item->name() ) ) == QMessageBox::Yes )
  {
    removeMarker( item );
    mModel->removeItem( item );
  }
}


void ReosRainfallManager::onCurrentTreeIndexChanged()
{
  QModelIndex currentIndex = ui->mTreeView->currentIndex();

  ReosRainfallItem *item = mModel->indexToItem( currentIndex );

  updateCurrentMapItemMarker( item );

  if ( item )
  {
    item->setupData();
    // First the form to access parameter
    ReosFormWidget *newForm = createForm( item );

    if ( mCurrentForm )
    {
      ui->mEditorWidget->layout()->replaceWidget( mCurrentForm, newForm );
      delete mCurrentForm;
      mCurrentForm = newForm;
    }
    else
    {
      mCurrentForm = newForm;
      ui->mEditorWidget->layout()->addWidget( mCurrentForm );
    }

    // Then the plot to visualize the data
    if ( item->data() && ReosPlotItemFactories::isInstantiate() )
    {
      ReosPlotWidget *newPlot = new ReosPlotWidget( this );
      ReosPlotItemFactories::instance()->buildPlotItemsAndSetup( newPlot, item->data() );
      newPlot->setSettingsContext( item->data()->type() );
      if ( mCurrentPlot )
      {
        ui->mPlotWidget->layout()->replaceWidget( mCurrentPlot, newPlot );
        delete mCurrentPlot;
        mCurrentPlot = newPlot;
      }
      else
      {
        mCurrentPlot = newPlot;
        ui->mPlotWidget->layout()->addWidget( mCurrentPlot );
      }
    }
    else
    {
      if ( mCurrentPlot )
      {
        ui->mPlotWidget->layout()->removeWidget( mCurrentPlot );
        delete mCurrentPlot;
        mCurrentPlot = nullptr;
      }
    }
  }
  else
  {
    if ( mCurrentForm )
    {
      ui->mEditorWidget->layout()->removeWidget( mCurrentForm );
      delete mCurrentForm;
      mCurrentForm = nullptr;
    }
    if ( mCurrentPlot )
    {
      ui->mPlotWidget->layout()->removeWidget( mCurrentPlot );
      delete mCurrentPlot;
      mCurrentPlot = nullptr;
    }
  }
}

void ReosRainfallManager::onTreeViewContextMenu( const QPoint &pos )
{
  QMenu menu;

  QModelIndex index = ui->mTreeView->indexAt( pos );

  if ( index.isValid() )
  {
    ReosRainfallItem *item = mModel->indexToItem( index );
    if ( item )
    {
      switch ( item->type() )
      {
        case ReosRainfallItem::Zone:
        {
          menu.addAction( mActionAddZoneToZone );
          QMenu *addStationMenu = menu.addMenu( QIcon( QStringLiteral( ":/images/addStation.svg" ) ), tr( "Add station…" ) );
          for ( QAction *act : std::as_const( mActionsAddStations ) )
            addStationMenu->addAction( act );
        }
        break;
        case ReosRainfallItem::Data:
          menu.addActions( dataItemActions( qobject_cast<ReosRainfallDataItem *>( item ) ) );
          break;
        case ReosRainfallItem::Station:
        {
          if ( mActionsAddGaugedRainfall.count() <= 1 )
          {
            mActionAddGaugedRainfall->setText( tr( "Add Gauged Rainfall in a Table" ) );
            menu.addAction( mActionAddGaugedRainfall );
          }
          else
          {
            mActionAddGaugedRainfall->setText( tr( "Gauged Rainfall in a Table" ) );
            QMenu *gaugedMenu = menu.addMenu( QIcon( QStringLiteral( ":/images/addGaugedRainfall.svg" ) ), tr( "Add Gauged Rainfall…" ) );
            for ( QAction *act : std::as_const( mActionsAddGaugedRainfall ) )
              gaugedMenu->addAction( act );
          }

          QMenu *syntheticMenu = menu.addMenu( QIcon( QStringLiteral( ":/images/addSyntheticRainfall.svg" ) ), tr( "Add Synthetic Rainfall…" ) );
          syntheticMenu->addAction( mActionAddChicagoRainfall );
          syntheticMenu->addAction( mActionAddAlternatingBlockRainfall );
          syntheticMenu->addAction( mActionAddDoubleTriangleRainfall );
          menu.addAction( mActionAddIDFCurves );
        }
        break;
      }
      menu.addAction( mActionRemoveItem );
    }
  }
  else
    menu.addAction( mActionAddRootZone );

  menu.exec( ui->mTreeView->viewport()->mapToGlobal( pos ) );
}

void ReosRainfallManager::onImportFromTextFile()
{
  ReosTextFileData textFile;

  ReosImportRainfallDialog *dialog = new ReosImportRainfallDialog( mModel, this );

  dialog->exec();

  dialog->deleteLater();
}

ReosMapItem *ReosRainfallManager::addMapItem( ReosRainfallItem *item )
{
  ReosStationItem *stationItem = qobject_cast < ReosStationItem *> ( item );
  if ( stationItem )
  {
    if ( !stationItem->position().isValid() )
      return nullptr;

    std::pair< std::map<ReosStationItem *, std::unique_ptr<ReosStationMapMarker>>::iterator, bool> res =
          mStationsMarker.emplace( stationItem, std::make_unique<ReosStationMapMarker>( mMap, stationItem ) );
    if ( res.second )
      return res.first->second.get();
  }

  return nullptr;
}

void ReosRainfallManager::removeMarker( ReosRainfallItem *item )
{
  ReosStationItem *stationItem = qobject_cast<ReosStationItem *>( item );
  if ( item )
  {
    auto it = mStationsMarker.find( stationItem );
    if ( it != mStationsMarker.end() )
    {
      mStationsMarker.erase( it );
    }
  }
}

void ReosRainfallManager::buildMarkers()
{
  QList<ReosRainfallItem *> itemToTreat;
  for ( int rz = 0; rz < mModel->rootZoneCount(); ++rz )
  {
    ReosZoneItem *rootZone = mModel->rootZone( rz );
    if ( rootZone )
      itemToTreat.append( rootZone );
  }

  while ( !itemToTreat.isEmpty() )
  {
    ReosRainfallItem *currentItem = itemToTreat.takeFirst();
    for ( int i = 0; i < currentItem->childrenCount(); ++i )
      itemToTreat.append( currentItem->itemAt( i ) );

    addMapItem( currentItem );
  }

  setMarkersVisible( isVisible() );
}

void ReosRainfallManager::clearMarkers()
{
  mStationsMarker.clear();
}

void ReosRainfallManager::updateMarkers()
{
  clearMarkers();
  buildMarkers();
}

void ReosRainfallManager::setMarkersVisible( bool b )
{
  for ( auto &it : mStationsMarker )
    it.second.get()->setVisible( b );

  if ( mCurrentStationMarker )
    mCurrentStationMarker->setVisible( b );
}

void ReosRainfallManager::selectItem( ReosRainfallItem *item )
{
  if ( !item )
    return;
  QModelIndex index = mModel->itemToIndex( item );
  ui->mTreeView->resizeColumnToContents( 0 );
  ui->mTreeView->setCurrentIndex( index );
}


ReosImportRainfallDialog::ReosImportRainfallDialog( ReosRainfallModel *model, QWidget *parent ):
  QDialog( parent )
  , mModel( model )
  , mTextFile( new ReosTextFileData( this ) )
  , mImportedRainfall( new ReosSeriesRainfall )
  , mName( new ReosParameterString( tr( "name" ), false, this ) )
  , mDescription( new ReosParameterString( tr( "Description" ), false, this ) )
{

  setWindowTitle( tr( "Import Rainfall" ) );
  ReosTextFileData mtextFile;

  mImportedRainfall->setValueUnit( tr( "mm" ) );
  mImportedRainfall->setValueModeName( ReosTimeSerieConstantInterval::Value, tr( "Height per time step" ) );
  mImportedRainfall->setValueModeName( ReosTimeSerieConstantInterval::Cumulative, tr( "Total height" ) );
  mImportedRainfall->setValueModeName( ReosTimeSerieConstantInterval::Intensity, tr( "Rainfall intensity" ) );
  mImportedRainfall->setValueModeColor( ReosTimeSerieConstantInterval::Value, QColor( 0, 0, 200, 200 ) );
  mImportedRainfall->setValueModeColor( ReosTimeSerieConstantInterval::Intensity, QColor( 50, 100, 255, 200 ) );
  mImportedRainfall->setValueModeColor( ReosTimeSerieConstantInterval::Cumulative, QColor( 255, 50, 0 ) );
  mImportedRainfall->setAddCumulative( true );

  setLayout( new QHBoxLayout );
  layout()->setContentsMargins( 0, 0, 0, 0 );
  QSplitter *mainWidget = new QSplitter( this );
  layout()->addWidget( mainWidget );

  ReosImportFromTextFile *importWidget = new ReosImportFromTextFile( mTextFile, this );
  mainWidget->addWidget( importWidget );

  QWidget *selectionWidget = new QWidget( this );
  QVBoxLayout *selectionLayout = new QVBoxLayout;
  selectionWidget->setLayout( selectionLayout );
  selectionLayout->addItem( new QSpacerItem( 10, 10, QSizePolicy::Minimum, QSizePolicy::Expanding ) );
  selectionLayout->addWidget( new QLabel( tr( "Field to import" ) ) );
  mComboSelectedField = importWidget->createAvailableFieldComboBox( this );
  selectionLayout->addWidget( mComboSelectedField );
  mImportButton = new QToolButton( selectionWidget );
  mImportButton->setIcon( QIcon( QStringLiteral( ":/images/moveRight.svg" ) ) );
  QHBoxLayout *buttonLayout = new QHBoxLayout;
  buttonLayout->addWidget( mImportButton );
  selectionLayout->addItem( buttonLayout );
  selectionLayout->addItem( new QSpacerItem( 10, 10, QSizePolicy::Minimum, QSizePolicy::Expanding ) );
  mainWidget->addWidget( selectionWidget );

  QWidget *importedRainfallWidget = new QWidget( this );
  QVBoxLayout *rainfallLayout = new QVBoxLayout;
  importedRainfallWidget->setLayout( rainfallLayout );
  rainfallLayout->setContentsMargins( 9, 9, 9, 9 );
  rainfallLayout->addWidget( new ReosParameterStringWidget( mName, this ) );
  rainfallLayout->addWidget( new ReosParameterStringWidget( mDescription, this ) );
  ReosGuiContext guiContext( importedRainfallWidget );
  rainfallLayout->addWidget( ReosFormWidget::createDataWidget( mImportedRainfall, guiContext ) );
  mainWidget->addWidget( importedRainfallWidget );

  QWidget *stationWidget = new QWidget( this );
  QVBoxLayout *stationLayout = new QVBoxLayout;
  stationWidget->setLayout( stationLayout );
  stationLayout->addItem( new QSpacerItem( 10, 10, QSizePolicy::Minimum, QSizePolicy::Expanding ) );
  mSelectStationButton = new QToolButton( stationWidget );
  mSelectStationButton->setIcon( QIcon( QStringLiteral( ":/images/station.svg" ) ) );
  stationLayout->addWidget( new QLabel( tr( "Select a Station to Import" ), this ) );
  QHBoxLayout *finalizeLayout = new QHBoxLayout;
  mSelectStationButton->setEnabled( false );
  finalizeLayout->addWidget( mSelectStationButton );
  stationLayout->addItem( finalizeLayout );

  stationLayout->addItem( new QSpacerItem( 10, 10, QSizePolicy::Minimum, QSizePolicy::Expanding ) );
  mainWidget->addWidget( stationWidget );

  connect( mImportButton, &QToolButton::clicked, this, &ReosImportRainfallDialog::onImportButton );
  connect( mSelectStationButton, &QToolButton::clicked, this, &ReosImportRainfallDialog::onSelectStationButton );
}

void ReosImportRainfallDialog::onImportButton()
{
  int index  = mComboSelectedField->currentIndex();
  QVector<QString> stringValues = mTextFile->columnValues( index );

  if ( stringValues.isEmpty() )
    return;

  mImportedRainfall->clear();

  for ( const QString &str : std::as_const( stringValues ) )
  {
    bool ok;
    double value = str.toDouble( &ok );
    if ( !ok )
      value = 0;
    mImportedRainfall->appendValue( value );
  }

  mSelectStationButton->setEnabled( true );

}

void ReosImportRainfallDialog::onSelectStationButton()
{
  ReosRainfallItemSelectionDialog *dialog = new ReosRainfallItemSelectionDialog( this );
  dialog->setSelectionType( ReosRainfallItem::Station );
  dialog->setText( tr( "Select a station where to import the rainfall" ) );
  if ( dialog->exec() )
  {
    ReosRainfallItem *item = dialog->selectedItem();
    ReosStationItem *stationItem = qobject_cast<ReosStationItem *>( item );
    dialog->deleteLater();
    if ( stationItem )
    {
      mModel->addGaugedRainfall( mName->value(), mDescription->value(), mModel->itemToIndex( item ), mImportedRainfall );
      close();
    }
  }
}


QString ReosPlotItemRainfallIntensityDurationFrequencyFactory::datatype() const {return ReosIntensityDurationFrequencyCurves::staticType();}

void ReosPlotItemRainfallIntensityDurationFrequencyFactory::buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  if ( data && data->type().contains( ReosIntensityDurationFrequencyCurves::staticType() ) )
  {
    ReosIntensityDurationFrequencyCurves *_data = static_cast<ReosIntensityDurationFrequencyCurves *>( data );
    for ( int i = 0; i < _data->curvesCount(); ++i )
    {
      QColor color = QColor::fromHsvF( 1 - double( i ) / _data->curvesCount(), 0.5, 0.85 );
      if ( _data->curve( i ) )
      {
        ReosPlotIdfCurve *plotCurve = new ReosPlotIdfCurve( _data->curve( i ), _data->name( i ) );
        plotCurve->setColors( color );
        plotWidget->addPlotItem( plotCurve );
      }
    }

    QRectF extent = _data->fullExtent();
    double xMin = extent.left() - extent.width() * 0.1;
    double xMax = extent.right() + extent.width() * 0.1;
    double yMin = extent.top() - extent.height() * 0.1;
    double yMax = extent.bottom() + extent.height() * 0.1;

    plotWidget->setAxeXExtent( xMin, xMax );
    plotWidget->setAxeYLeftExtent( yMin, yMax );
    plotWidget->setLegendAlignement( Qt::AlignRight );
    plotWidget->setLegendEnabled( true );
    plotWidget->setTitleAxeX( QObject::tr( "Rainfall duration (mn)" ) );
    plotWidget->setTitleAxeYLeft( QObject::tr( "Rainfall intensity (mm/h)" ) );
    plotWidget->enableScaleTypeChoice( true );
  }
}

QString ReosPlotItemRainfallIntensityDurationFactory::datatype() const {return ReosIntensityDurationCurve::staticType();}

void ReosPlotItemRainfallIntensityDurationFactory::buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  ReosIntensityDurationCurve *_data = static_cast<ReosIntensityDurationCurve *>( data );
  ReosPlotIdfCurve *curve = new ReosPlotIdfCurve( _data );
  curve->setColors( Qt::red );
  plotWidget->addPlotItem( curve );
  curve->fullExtent();

  plotWidget->setLegendEnabled( false );
  plotWidget->setTitleAxeX( QObject::tr( "Rainfall duration (mn)" ) );
  plotWidget->setTitleAxeYLeft( QObject::tr( "Rainfall intensity (mm/h)" ) );

  plotWidget->enableScaleTypeChoice( true );
}

QString ReosPlotItemRainfallSerieFactory::datatype() const {return ReosSeriesRainfall::staticType();}

void ReosPlotItemRainfallSerieFactory::buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  ReosTimeSerieConstantInterval *_data = static_cast<ReosTimeSerieConstantInterval *>( data );

  if ( _data->valueMode() != ReosTimeSerieConstantInterval::Cumulative )
  {
    std::unique_ptr<ReosPlotTimeHistogram> histogram = std::make_unique<ReosPlotTimeHistogram>( _data->name() + QObject::tr( ", instant value" ), true );
    histogram->setTimeSerie( _data );
    plotWidget->addPlotItem( histogram.release() );
  }

  if ( _data->valueMode() == ReosTimeSerieConstantInterval::Cumulative || _data->addCumultive() )
  {
    plotWidget->enableAxeYRight( true );
    std::unique_ptr<ReosPlotTimeCumulativeCurve> cumulCurve = std::make_unique<ReosPlotTimeCumulativeCurve>( _data->name() + QObject::tr( ", cumulative value" ) );
    cumulCurve->setTimeSerie( _data );
    cumulCurve->setOnRightAxe();
    plotWidget->addPlotItem( cumulCurve.release() );
    plotWidget->setTitleAxeYRight( QObject::tr( "Cumulative rainfall (mm)" ) );
  }

  plotWidget->setTitleAxeX( QObject::tr( "Time" ) );
  plotWidget->setAxeXType( ReosPlotWidget::temporal );
  plotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
  plotWidget->updatePlot();
}
