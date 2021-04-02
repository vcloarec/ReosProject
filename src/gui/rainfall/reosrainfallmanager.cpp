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
#include "reosplotidfcurve.h"
#include "reosplottimeconstantinterval.h"
#include "reosrainfalldataform.h"

ReosRainfallManager::ReosRainfallManager( ReosRainfallModel *rainfallmodel, QWidget *parent ) :
  ReosActionWidget( parent )
  , ui( new Ui::ReosRainfallManager )
  , mModel( rainfallmodel )
  , mActionOpenRainfallDataFile( new QAction( QPixmap( QStringLiteral( ":/images/openRainfall.svg" ) ), tr( "Open Rainfal Data File" ), this ) )
  , mActionSaveRainfallDataFile( new QAction( QPixmap( QStringLiteral( ":/images/saveRainfall.svg" ) ), tr( "Save Rainfal Data File" ), this ) )
  , mActionSaveAsRainfallDataFile( new QAction( QPixmap( QStringLiteral( ":/images/saveAsRainfall.svg" ) ), tr( "Save Rainfal Data File as ..." ), this ) )
  , mActionAddRootZone( new QAction( QPixmap( QStringLiteral( ":/images/addZone.svg" ) ), tr( "Add New Zone to the Root" ), this ) )
  , mActionAddZoneToZone( new QAction( QPixmap( QStringLiteral( ":/images/addZone.svg" ) ), tr( "Add New Sub Zone" ), this ) )
  , mActionAddStation( new QAction( QPixmap( QStringLiteral( ":/images/addStation.svg" ) ), tr( "Add Station" ), this ) )
  , mActionAddGaugedRainfall( new QAction( QPixmap( QStringLiteral( ":/images/addGaugedRainfall.svg" ) ), tr( "Add Gauged Rainfall" ), this ) )
  , mActionAddChicagoRainfall( new QAction( QPixmap( QStringLiteral( ":/images/addChicagoRainfall.svg" ) ), tr( "Add Chicago Rainfall" ), this ) )
  , mActionAddDoubleTriangleRainfall( new QAction( QPixmap( QStringLiteral( ":/images/addDoubleTriangleRainfall.svg" ) ), tr( "Add Double Triangle Rainfall" ), this ) )
  , mActionAddIDFCurves( new QAction( QPixmap( QStringLiteral( ":/images/addIntensityDurationCurves.svg" ) ), tr( "Add Intensity Duration Frequency Curves" ), this ) )
  , mActionAddIDCurve( new QAction( QPixmap( QStringLiteral( ":/images/addIntensityDurationCurve.svg" ) ), tr( "Add Intensity Duration Curve" ), this ) )
  , mActionReorderIdVurve( new QAction( tr( "Reorder Intensity Duration Curves" ), this ) )
  , mActionRemoveItem( new QAction( tr( "Remove item" ), this ) )
  , mActionImportFromTextFile( new QAction( QPixmap( QStringLiteral( ":/images/importRainfall.svg" ) ), tr( "Import Rainfall from Text File" ), this ) )
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

  connect( mActionOpenRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::onOpenRainfallFile );
  connect( mActionSaveRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::saveRainfallFile );
  connect( mActionSaveAsRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::onSaveAsRainfallFile );
  connect( mActionImportFromTextFile, &QAction::triggered, this, &ReosRainfallManager::onImportFromTextFile );

  connect( mActionAddRootZone, &QAction::triggered, this, &ReosRainfallManager::onAddRootZone );
  connect( mActionAddZoneToZone, &QAction::triggered, this, &ReosRainfallManager::onAddZoneToZone );
  connect( mActionAddStation, &QAction::triggered, this, &ReosRainfallManager::onAddStation );
  connect( mActionAddGaugedRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddGaugedRainfall );
  connect( mActionAddChicagoRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddChicagoRainfall );
  connect( mActionAddDoubleTriangleRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddDoubleTriangleRainfall );
  connect( mActionAddIDFCurves, &QAction::triggered, this, &ReosRainfallManager::onAddIDFCurves );
  connect( mActionAddIDCurve, &QAction::triggered, this, &ReosRainfallManager::onAddIDCurve );
  connect( mActionReorderIdVurve, &QAction::triggered, this, &ReosRainfallManager::onReorderIDCurve );

  connect( mActionRemoveItem, &QAction::triggered, this, &ReosRainfallManager::onRemoveItem );


  connect( ui->mTreeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosRainfallManager::onCurrentTreeIndexChanged );
  connect( ui->mTreeView, &QWidget::customContextMenuRequested, this, &ReosRainfallManager::onTreeViewContextMenu );

  ReosIdfFormulaRegistery::instance()->registerFormula( new ReosIdfFormulaMontana );
  ReosIdfFormulaRegistery::instance()->registerFormula( new ReosIdfFormulaSherman );

  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallIntensityDurationFrequencyFactory );
  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallIntensityDurationFactory );
  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallSerieFactory );
  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallChicagoFactory );
  ReosPlotItemFactories::instance()->addFactory( new ReosPlotItemRainfallDoubleTriangleFactory );

  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetRainFallSerieFactory );
  ReosFormWidgetFactories::instance()->addDataWidgetFactory( new ReosFormWidgetChicagoRainfalFactory );
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

  if ( mModel->loadFromFile( fileName, QStringLiteral( "Rainfall data" ) ) )
    ui->labelFileName->setText( fileName );
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

  if ( mModel->loadFromFile( fileName, QStringLiteral( "rainfall data" ) ) )
  {
    ui->labelFileName->setText( fileName );
    settings.setValue( QStringLiteral( "Rainfall/dataFile" ), fileName );
    QFileInfo fileInfo( fileName );
    settings.setValue( QStringLiteral( "Rainfall/fileDirectory" ), fileInfo.path() );
  }
  else
  {
    QMessageBox::critical( this, tr( "Open Rainfall Data" ), tr( "Unable to open the file: %1" ).arg( fileName ) );
  }
}


bool ReosRainfallManager::saveOnFile( const QString &fileName )
{
  return mModel->saveToFile( fileName, QStringLiteral( "rainfall data" ) );
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

bool ReosRainfallManager::addSimpleItemDialog( const QString &title, QString &name, QString &descript )
{
  ReosParameterString string( name );
  std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
  dial->addParameter( &string );
  ReosParameterString descritpion( tr( "Description" ) );
  dial->addParameter( &descritpion );
  dial->setWindowTitle( title );

  if ( dial->exec() )
  {
    name = string.value();
    descript = descritpion.value();
    return true;
  }

  return false;
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
  QString name = tr( "Zone name" );
  QString description;

  if ( addSimpleItemDialog( tr( "Add Zone on Root" ), name, description ) )
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
    QString name = tr( "Zone name" );
    QString description;

    if ( addSimpleItemDialog( tr( "Add Zone" ), name, description ) )
      selectItem( mModel->addZone( name, description, index ) );

  }
}

void ReosRainfallManager::onAddStation()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name = tr( "Station name" );
    QString description;

    if ( addSimpleItemDialog( tr( "Add Station" ), name, description ) )
      selectItem( mModel->addStation( name, description, index ) );
  }
}

void ReosRainfallManager::onAddGaugedRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name = tr( "Rainfall name" );
    QString description;

    if ( addSimpleItemDialog( tr( "Add Gauged Rainfall" ), name, description ) )
      selectItem( mModel->addGaugedRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddChicagoRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name = tr( "Rainfall name" );
    QString description;

    if ( addSimpleItemDialog( tr( "Add Chicago Rainfall" ), name, description ) )
      selectItem( mModel->addChicagoRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddDoubleTriangleRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name = tr( "Rainfall name" );
    QString description;

    if ( addSimpleItemDialog( tr( "Add Double Triangle Rainfall" ), name, description ) )
      selectItem( mModel->addDoubleTriangleRainfall( name, description, index ) );
  }
}

void ReosRainfallManager::onAddIDFCurves()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    QString name = tr( "IDF group name" );
    QString description;

    if ( addSimpleItemDialog( tr( "Add Intensity Duration Frequency Curves" ), name, description ) )
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

    ReosIntensityDurationCurve *curve( int i );
  }
}

void ReosRainfallManager::onRemoveItem()
{
  QModelIndex index = ui->mTreeView->currentIndex();
  ReosRainfallItem *item = mModel->indexToItem( index );

  if ( !item )
    return;

  if ( QMessageBox::question( this, tr( "Remove item" ), tr( "Remove: %1" ).arg( item->name() ) ) == QMessageBox::Yes )
    mModel->removeItem( item );
}


void ReosRainfallManager::onCurrentTreeIndexChanged()
{
  QModelIndex currentIndex = ui->mTreeView->currentIndex();

  ReosRainfallItem *item = mModel->indexToItem( currentIndex );

  if ( item )
  {
    item->setupData();
    // First the form to acces parameter
    ReosFormWidget *newForm = new ReosFormWidget( this );
    newForm->addParameters( item->parameters() );
    if ( !item->data() ||  !newForm->addData( item->data() ) )
    {
      newForm->addItem( new QSpacerItem( 20, 40, QSizePolicy::Ignored, QSizePolicy::Expanding ) );
      newForm->setStretch( 2, 1 );
    }

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
      ReosPlotItemFactories::instance()->buildPlotItems( newPlot, item->data() );
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
          menu.addAction( mActionAddZoneToZone );
          menu.addAction( mActionAddStation );
          break;
        case ReosRainfallItem::Data:
          menu.addActions( dataItemActions( qobject_cast<ReosRainfallDataItem *>( item ) ) );
        case ReosRainfallItem::Station:
          menu.addAction( mActionAddGaugedRainfall );
          menu.addAction( mActionAddChicagoRainfall );
          menu.addAction( mActionAddDoubleTriangleRainfall );
          menu.addAction( mActionAddIDFCurves );
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

void ReosRainfallManager::selectItem( ReosRainfallItem *item )
{
  if ( !item )
    return;
  QModelIndex index = mModel->itemToIndex( item );
  ui->mTreeView->setCurrentIndex( index );
}


ReosImportRainfallDialog::ReosImportRainfallDialog( ReosRainfallModel *model, QWidget *parent ):
  QDialog( parent )
  , mModel( model )
  , mTextFile( new ReosTextFileData( this ) )
  , mImportedRainfall( new ReosSerieRainfall )
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
  mImportButton->setIcon( QPixmap( QStringLiteral( ":/images/moveRight.svg" ) ) );
  QHBoxLayout *buttonLayout = new QHBoxLayout( this );
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
  rainfallLayout->addWidget( ReosFormWidget::createDataWidget( mImportedRainfall, importedRainfallWidget ) );
  mainWidget->addWidget( importedRainfallWidget );

  QWidget *stationWidget = new QWidget( this );
  QVBoxLayout *stationLayout = new QVBoxLayout;
  stationWidget->setLayout( stationLayout );
  stationLayout->addItem( new QSpacerItem( 10, 10, QSizePolicy::Minimum, QSizePolicy::Expanding ) );
  mSelectStationButton = new QToolButton( stationWidget );
  mSelectStationButton->setIcon( QPixmap( QStringLiteral( ":/images/station.svg" ) ) );
  stationLayout->addWidget( new QLabel( tr( "Select a Station to Import" ), this ) );
  QHBoxLayout *finalizeLayout = new QHBoxLayout( this );
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


void ReosPlotItemRainfallIntensityDurationFrequencyFactory::buildPlotItems( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  if ( data && data->type() == QStringLiteral( "rainfall-intensity-duration-frequency-curves" ) )
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
    double xmax = extent.right() + extent.width() * 0.1;
    double yMin = extent.top() - extent.height() * 0.1;
    double yMax = extent.bottom() + extent.height() * 0.1;

    plotWidget->setAxeXExtent( xMin, xmax );
    plotWidget->setAxeYLeftExtent( yMin, yMax );
    plotWidget->setLegendAlignement( Qt::AlignRight );
    plotWidget->setLegendVisible( true );
    plotWidget->setTitleAxeX( QObject::tr( "Rainfall duration (mn)" ) );
    plotWidget->setTitleAxeYLeft( QObject::tr( "Rainfall duration (mm/h)" ) );
    plotWidget->enableScaleTypeChoice( true );
  }
}

void ReosPlotItemRainfallIntensityDurationFactory::buildPlotItems( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  ReosIntensityDurationCurve *_data = static_cast<ReosIntensityDurationCurve *>( data );
  ReosPlotIdfCurve *curve = new ReosPlotIdfCurve( _data );
  curve->setColors( Qt::red );
  plotWidget->addPlotItem( curve );
  curve->fullExtent();
  plotWidget->setLegendVisible( false );
  plotWidget->setTitleAxeX( QObject::tr( "Rainfall duration (mn)" ) );
  plotWidget->setTitleAxeYLeft( QObject::tr( "Rainfall intensity (mm/h)" ) );

  plotWidget->enableScaleTypeChoice( true );
}

void ReosPlotItemRainfallSerieFactory::buildPlotItems( ReosPlotWidget *plotWidget, ReosDataObject *data )
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
    plotWidget->enableAxeYright( true );
    std::unique_ptr<ReosPlotTimeCumulativeCurve> cumulCurve = std::make_unique<ReosPlotTimeCumulativeCurve>( _data->name() + QObject::tr( ", cumulative value" ) );
    cumulCurve->setTimeSerie( _data );
    cumulCurve->setOnRightAxe();
    plotWidget->addPlotItem( cumulCurve.release() );
    plotWidget->setTitleAxeYRight( QObject::tr( "cumulative rainfall (mm)" ) );
  }

  plotWidget->setTitleAxeX( QObject::tr( "Time" ) );
  plotWidget->setAxeXType( ReosPlotWidget::temporal );
  plotWidget->setMagnifierType( ReosPlotWidget::positiveMagnifier );
}

