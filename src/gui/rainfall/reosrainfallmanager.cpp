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

#include<QAction>
#include <QMenu>
#include <QCloseEvent>
#include <QTreeView>

#include "reossettings.h"
#include "reosrainfallmodel.h"
#include "reosrainfallitem.h"
#include "reosparameterwidget.h"
#include "reosformwidget.h"

ReosRainfallManager::ReosRainfallManager( ReosRainfallModel *rainfallmodel, QWidget *parent ) :
  ReosActionWidget( parent )
  , ui( new Ui::ReosRainfallManager )
  , mModel( rainfallmodel )
  , mActionOpenRainfallDataFile( new QAction( tr( "Open rainfal Data File" ), this ) )
  , mActionAddRootZone( new QAction( tr( "Add New Zone to the Root" ), this ) )
  , mActionAddZoneToZone( new QAction( tr( "Add New Sub Zone" ), this ) )
  , mActionAddStation( new QAction( tr( "Add Station" ), this ) )
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
  toolBar->addAction( mActionAddRootZone );

  connect( mActionAddRootZone, &QAction::triggered, this, &ReosRainfallManager::onAddRootZone );
  connect( mActionAddZoneToZone, &QAction::triggered, this, &ReosRainfallManager::onAddZoneToZone );
  connect( mActionAddStation, &QAction::triggered, this, &ReosRainfallManager::onAddStation );
  connect( ui->mTreeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosRainfallManager::onCurrentTreeIndexChanged );
  connect( ui->mTreeView, &QWidget::customContextMenuRequested, this, &ReosRainfallManager::onTreeViewContextMenu );

  restore();
}

ReosRainfallManager::~ReosRainfallManager()
{
  delete ui;
}

void ReosRainfallManager::onAddRootZone()
{
  ReosParameterString string( "Zone name" );

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->addParameter( &string );
  ReosParameterString descritpion( tr( "Descriprition" ) );
  dial->addParameter( &descritpion );

  if ( dial->exec() )
    mModel->addZone( string.value(), descritpion.value() );


}

void ReosRainfallManager::onAddZoneToZone()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    ReosParameterString name( tr( "Zone name" ) );
    ReosFormDialog *dial = new ReosFormDialog( this );
    dial->addParameter( &name );
    ReosParameterString descritpion( tr( "Descriprition" ) );
    dial->addParameter( &descritpion );

    if ( dial->exec() )
      selectItem( mModel->addZone( name.value(), descritpion.value(), index ) );

    dial->deleteLater();
  }
}

void ReosRainfallManager::onAddStation()
{
  QModelIndex index = ui->mTreeView->currentIndex();

  if ( index.isValid() )
  {
    ReosFormDialog *dial = new ReosFormDialog( this );
    ReosParameterString name( tr( "Station name" ) );
    dial->addParameter( &name );
    ReosParameterString descritpion( tr( "Descriprition" ) );
    dial->addParameter( &descritpion );


    if ( dial->exec() )
    {
      selectItem( mModel->addStation( name.value(), descritpion.value(), index ) );
    }

    dial->deleteLater();
  }
}


void ReosRainfallManager::onCurrentTreeIndexChanged()
{
  QModelIndex currentIndex = ui->mTreeView->currentIndex();

  ReosRainfallItem *item = mModel->indexToItem( currentIndex );

  if ( item )
  {
    ReosFormWidget *newForm = new ReosFormWidget( this );
    newForm->addParameters( item->parameters() );

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
          break;
        case ReosRainfallItem::Data:
          break;
      }
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
  ui->mTreeView->selectionModel()->select( index, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows );
  ui->mTreeView->setCurrentIndex( index );
}
