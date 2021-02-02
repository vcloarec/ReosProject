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
#include <QCloseEvent>
#include <QToolBar>
#include <QTreeView>

#include "reossettings.h"
#include "reosrainfallmodel.h"
#include "reosrainfallitem.h"
#include "reosparameterwidget.h"
#include "reosplotwidget.h"
#include "reosformwidget.h"

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
  , mActionRemoveItem( new QAction( tr( "Remove item" ), this ) )
  , mActionAddGaugedRainfall( new QAction( QPixmap( QStringLiteral( ":/images/addGaugedRainfall.svg" ) ), tr( "Add Gauged Rainfall" ), this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

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

  connect( mActionOpenRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::onOpenRainfallFile );
  connect( mActionSaveRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::onSaveRainfallFile );
  connect( mActionSaveAsRainfallDataFile, &QAction::triggered, this, &ReosRainfallManager::OnSaveAsRainfallFile );

  connect( mActionAddRootZone, &QAction::triggered, this, &ReosRainfallManager::onAddRootZone );
  connect( mActionAddZoneToZone, &QAction::triggered, this, &ReosRainfallManager::onAddZoneToZone );
  connect( mActionAddStation, &QAction::triggered, this, &ReosRainfallManager::onAddStation );
  connect( mActionAddGaugedRainfall, &QAction::triggered, this, &ReosRainfallManager::onAddGaugedRainfall );

  connect( mActionRemoveItem, &QAction::triggered, this, &ReosRainfallManager::onRemoveItem );

  connect( ui->mTreeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosRainfallManager::onCurrentTreeIndexChanged );
  connect( ui->mTreeView, &QWidget::customContextMenuRequested, this, &ReosRainfallManager::onTreeViewContextMenu );

  restore();
}

ReosRainfallManager::~ReosRainfallManager()
{
  delete ui;
}

void ReosRainfallManager::loadDataFile()
{
  QString fileName;
  ReosSettings settings;
  fileName = settings.value( QStringLiteral( "/rainfall/dataFile" ) ).toString();

  if ( fileName.isEmpty() )
    return;

  if ( mModel->loadFromFile( fileName, QStringLiteral( "rainfall data" ) ) )
  {
    mCurrentFileName = fileName;
  }
  else
  {
    QMessageBox::warning( this, tr( "Open Rainfall Data" ), tr( "Unable to open the current rainfall data file: %1" ).arg( fileName ) );
  }

  ui->mTreeView->expandAll();
  ui->mTreeView->resizeColumnToContents( 0 );
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
      onSaveRainfallFile();
    }
  }

  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "/rainfall/fileDirectory" ) ).toString();
  QString fileName = QFileDialog::getOpenFileName( this, tr( "Open Rainfall Data" ), dir, QStringLiteral( " *.rrf" ) );

  if ( fileName.isEmpty() )
    return;

  if ( mModel->loadFromFile( fileName, QStringLiteral( "rainfall data" ) ) )
  {
    mCurrentFileName = fileName;
    QMessageBox::information( this, tr( "Open Rainfall Data" ), tr( "Rainfall data file open: %1" ).arg( mCurrentFileName ) );
    settings.setValue( QStringLiteral( "/rainfall/dataFile" ), fileName );
    QFileInfo fileInfo( fileName );
    settings.setValue( QStringLiteral( "/rainfall/fileDirectory" ), fileInfo.path() );
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

void ReosRainfallManager::onSaveRainfallFile()
{
  QFileInfo fileInfo( mCurrentFileName );
  if ( !fileInfo.exists() )
    OnSaveAsRainfallFile();

  if ( !saveOnFile( mCurrentFileName ) )
    QMessageBox::warning( this, tr( "Save Rainfall Data" ), tr( "Unable to write the file" ) );
  else
    QMessageBox::information( this, tr( "Save Rainfall Data" ), tr( "Rainfall data save on file: %1" ).arg( mCurrentFileName ) );
}

void ReosRainfallManager::OnSaveAsRainfallFile()
{
  ReosSettings settings;
  QString dir = settings.value( QStringLiteral( "/rainfall/fileDirectory" ) ).toString();
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
    mCurrentFileName = fileName;
    settings.setValue( QStringLiteral( "/rainfall/fileDirectory" ), fileInfo.path() );
  }
}

void ReosRainfallManager::onAddRootZone()
{
  ReosParameterString string( "Zone name" );
  std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
  dial->addParameter( &string );
  ReosParameterString descritpion( tr( "Descriprition" ) );
  dial->addParameter( &descritpion );

  if ( dial->exec() )
    selectItem( mModel->addZone( string.value(), descritpion.value() ) );
}

void ReosRainfallManager::onAddZoneToZone()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    ReosParameterString name( tr( "Zone name" ) );
    std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
    dial->addParameter( &name );
    ReosParameterString descritpion( tr( "Descriprition" ) );
    dial->addParameter( &descritpion );

    if ( dial->exec() )
    {
      selectItem( mModel->addZone( name.value(), descritpion.value(), index ) );
    }

  }
}

void ReosRainfallManager::onAddStation()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
    ReosParameterString name( tr( "Station name" ) );
    dial->addParameter( &name );
    ReosParameterString descritpion( tr( "Descriprition" ) );
    dial->addParameter( &descritpion );


    if ( dial->exec() )
    {
      selectItem( mModel->addStation( name.value(), descritpion.value(), index ) );
    }
  }
}

void ReosRainfallManager::onAddGaugedRainfall()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    std::unique_ptr<ReosFormDialog> dial = std::make_unique<ReosFormDialog>( this );
    ReosParameterString name( tr( "Gauged Rainfall name" ) );
    dial->addParameter( &name );
    ReosParameterString descritpion( tr( "Descriprition" ) );
    dial->addParameter( &descritpion );

    if ( dial->exec() )
    {
      selectItem( mModel->addGaugedRainfall( name.value(), descritpion.value(), index ) );
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
    mModel->removeItem( item );
}


void ReosRainfallManager::onCurrentTreeIndexChanged()
{
  QModelIndex currentIndex = ui->mTreeView->currentIndex();

  ReosRainfallItem *item = mModel->indexToItem( currentIndex );

  if ( item )
  {
    ReosFormWidget *newForm = new ReosFormWidget( this );
    newForm->addParameters( item->parameters() );
    newForm->addData( item->data() );

    if ( mCurrentForm )
    {
      ui->mEditorWidget->layout()->replaceWidget( mCurrentForm, newForm );
      mCurrentForm->deleteLater();
      mCurrentForm = newForm;
    }
    else
    {
      mCurrentForm = newForm;
      ui->mEditorWidget->layout()->addWidget( mCurrentForm );
    }

    if ( item->data() )
    {
      ReosPlotWidget *newPlot = new ReosPlotWidget( this );
      newPlot->addDataObject( item->data() );
      if ( mCurrentPlot )
      {
        ui->mPlotWidget->layout()->replaceWidget( mCurrentPlot, newPlot );
        mCurrentPlot->deleteLater();
        mCurrentPlot = newPlot;
      }
      else
      {
        mCurrentPlot = newPlot;
        ui->mPlotWidget->layout()->addWidget( mCurrentPlot );
      }

      mCurrentPlot->setAxeXType( ReosPlotWidget::temporal );
    }
    else
    {
      if ( mCurrentPlot )
      {
        ui->mPlotWidget->layout()->removeWidget( mCurrentPlot );
        mCurrentPlot->deleteLater();
        mCurrentPlot = nullptr;
      }
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
        case ReosRainfallItem::Station:
          menu.addAction( mActionAddGaugedRainfall );
          break;
        case ReosRainfallItem::Data:
          break;
      }

      menu.addAction( mActionRemoveItem );
    }
  }
  else
    menu.addAction( mActionAddRootZone );

  menu.exec( ui->mTreeView->mapToGlobal( pos ) );
}

void ReosRainfallManager::selectItem( ReosRainfallItem *item )
{
  if ( !item )
    return;
  QModelIndex index = mModel->itemToIndex( item );
  ui->mTreeView->setCurrentIndex( index );
}

