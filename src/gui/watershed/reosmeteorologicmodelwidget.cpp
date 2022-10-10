/***************************************************************************
  reosmeteorologicmodelwidget.cpp - ReosMeteorologicModelWidget

 ---------------------
 begin                : 16.2.2021
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
#include "reosmeteorologicmodelwidget.h"
#include "ui_reosmeteorologicmodelwidget.h"

#include <QToolBar>
#include <QMenu>
#include <QMessageBox>

#include "reosmeteorologicmodel.h"
#include "reosrainfallregistery.h"
#include "reosformwidget.h"
#include "reosrainfallregistery.h"
#include "reosrainfallmodel.h"
#include "reoswatershedtree.h"
#include "reosstyleregistery.h"


ReosMeteorologicModelWidget::ReosMeteorologicModelWidget( ReosWatershedItemModel *watershedModel,
    ReosMeteorologicModelsCollection *meteoModelsCollection,
    QWidget *parent ) :
  ReosActionWidget( parent )
  , mMeteorologicItemModel( new ReosMeteorologicItemModel( watershedModel ) )
  , mModelsCollections( meteoModelsCollection )
  , ui( new Ui::ReosMeteorologicModelWidget )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  if ( ReosRainfallRegistery::isInstantiate() )
    ui->treeViewRainfall->setModel( ReosRainfallRegistery::instance()->rainfallModel() );
  ui->treeViewRainfall->expandAll();
  ui->treeViewRainfall->setDragEnabled( true );
  ui->treeViewRainfall->header()->resizeSections( QHeaderView::ResizeToContents );

  ui->treeViewMeteorologicModel->setAcceptDrops( true );
  ui->treeViewMeteorologicModel->expandAll();
  ui->treeViewMeteorologicModel->header()->setSectionResizeMode( QHeaderView::ResizeToContents );
  ui->treeViewMeteorologicModel->setModel( mMeteorologicItemModel );
  ui->treeViewMeteorologicModel->setContextMenuPolicy( Qt::CustomContextMenu );
  connect( ui->treeViewMeteorologicModel, &QWidget::customContextMenuRequested, this, &ReosMeteorologicModelWidget::onMeteoTreeViewContextMenu );
  connect( watershedModel, &QAbstractItemModel::modelReset, ui->treeViewMeteorologicModel, &QTreeView::expandAll );
  connect( mMeteorologicItemModel, &QAbstractItemModel::modelReset, ui->treeViewMeteorologicModel, &QTreeView::expandAll );

  ui->comboBoxCurrentModel->setModel( meteoModelsCollection );

  ui->widgetToolBar->setLayout( new QHBoxLayout );
  ui->widgetToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->widgetToolBar );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  ui->widgetToolBar->layout()->addWidget( toolBar );

  mActionAddMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/add.svg" ) ), tr( "Add Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onAddMeteoModel );
  mActionDuplicateMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/duplicateMeteoModel.svg" ) ), tr( "Duplicate Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onDuplicateMeteoModel );
  mActionRemoveMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onRemoveMeteoModel );
  mActionRenameMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onRenameMeteoModel );

  connect( ui->comboBoxCurrentModel, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosMeteorologicModelWidget::onCurrentModelChanged );
  onCurrentModelChanged();
}

void ReosMeteorologicModelWidget::setCurrentMeteorologicalModel( int index )
{
  ui->comboBoxCurrentModel->setCurrentIndex( index );
}

ReosMeteorologicModelWidget::~ReosMeteorologicModelWidget()
{
  delete ui;
}

void ReosMeteorologicModelWidget::onAddMeteoModel()
{
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Add Meteo Model" ) );
  dial->addText( tr( "Add a new meteorologic model, choose a name:" ) );
  ReosParameterString nameParam( tr( "Meteorologic Model name" ) );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    mModelsCollections->addMeteorologicModel( nameParam.value() );
    ui->comboBoxCurrentModel->setCurrentIndex( mModelsCollections->modelCount() - 1 );
  }

  dial->deleteLater();
}

void ReosMeteorologicModelWidget::onDuplicateMeteoModel()
{
  ReosMeteorologicModel *modelToDuplicate = currentModel();
  if ( !modelToDuplicate )
    return;

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Duplicate Meteo Model" ) );
  dial->addText( tr( "Duplicate meteorologic model '%1',\nchoose a name:" ).arg( modelToDuplicate->name()->value() ) );
  ReosParameterString nameParam( tr( "Meteorologic Model name" ) );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    mModelsCollections->addMeteorologicModel( modelToDuplicate->duplicate( nameParam.value() ) );
    ui->comboBoxCurrentModel->setCurrentIndex( mModelsCollections->modelCount() - 1 );
  }

  dial->deleteLater();
}

void ReosMeteorologicModelWidget::onRemoveMeteoModel()
{
  ReosMeteorologicModel *modelToRemove = currentModel();
  if ( !modelToRemove )
    return;

  if ( QMessageBox::warning( this, tr( "Remove Meteo Model" ),
                             tr( "Do you want to remove the model '%1'?" ).arg( modelToRemove->name()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
  {
    mModelsCollections->removeMeteorologicModel( ui->comboBoxCurrentModel->currentIndex() );
  }
}

void ReosMeteorologicModelWidget::onRenameMeteoModel()
{
  ReosMeteorologicModel *modelToRename = currentModel();
  if ( !modelToRename )
    return;

  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "Rename Meteo Model" ) );
  dial->addText( tr( "Rename meteorologic model '%1',\nchoose a new name:" ).arg( modelToRename->name()->value() ) );
  ReosParameterString nameParam( tr( "Meteorologic Model name" ) );
  nameParam.setValue( modelToRename->name()->value() );
  dial->addParameter( &nameParam );

  if ( dial->exec() )
  {
    modelToRename->name()->setValue( nameParam.value() );
    ui->comboBoxCurrentModel->setCurrentText( nameParam.value() );
  }

  dial->deleteLater();
}

void ReosMeteorologicModelWidget::onCurrentModelChanged()
{
  ReosMeteorologicModel *current = currentModel();
  mMeteorologicItemModel->setCurrentMeteorologicalModel( currentModel() );
  mActionDuplicateMeteoModel->setEnabled( current != nullptr );
  mActionRemoveMeteoModel->setEnabled( current != nullptr );
  mActionRenameMeteoModel->setEnabled( current != nullptr );

  emit currentModelChanged( ui->comboBoxCurrentModel->currentIndex() );
}

void ReosMeteorologicModelWidget::onMeteoTreeViewContextMenu( const QPoint &pos )
{
  QMenu menu;
  menu.addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Disassociate rainfall" ), &menu, [this, pos]
  {
    mMeteorologicItemModel->removeAssociation( ui->treeViewMeteorologicModel->indexAt( pos ) );
  } );

  menu.exec( ui->treeViewMeteorologicModel->mapToGlobal( pos ) );
}

ReosMeteorologicModel *ReosMeteorologicModelWidget::currentModel() const
{
  int currentIndex = ui->comboBoxCurrentModel->currentIndex();
  return mModelsCollections->meteorologicModel( currentIndex );
}

