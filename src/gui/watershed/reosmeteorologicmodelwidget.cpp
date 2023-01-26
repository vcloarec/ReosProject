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
#include "reosrenderedobject.h"
#include "reosmap.h"


ReosMeteorologicModelWidget::ReosMeteorologicModelWidget(
  ReosWatershedItemModel *watershedModel,
  ReosHydraulicNetwork *hydraulicNetwork,
  ReosMeteorologicModelsCollection *meteoModelsCollection,
  const ReosGuiContext &guiContext ) :
  ReosActionWidget( guiContext.parent() )
  , mMeteorologicItemModel( new ReosMeteorologicItemModel( watershedModel, this ) )
  , mMeteorologicStructureModel( new ReosMeteorologicStructureItemModel( hydraulicNetwork, this ) )
  , mHydraulicNetwork( hydraulicNetwork )
  , mModelsCollections( meteoModelsCollection )
  , mMap( guiContext.map() )
  , ui( new Ui::ReosMeteorologicModelWidget )
  , mActionDisplayGriddedOnMap( new QAction( QIcon( QStringLiteral( ":/images/griddedRainfallDisplay.svg" ) ), tr( "Display Gridded Precipitation on Map" ), this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  mActionDisplayGriddedOnMap->setCheckable( true );

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

  ui->treeViewHydraulicNetwork->setAcceptDrops( true );
  ui->treeViewHydraulicNetwork->expandAll();
  ui->treeViewHydraulicNetwork->header()->setSectionResizeMode( QHeaderView::ResizeToContents );
  ui->treeViewHydraulicNetwork->setModel( mMeteorologicStructureModel );
  ui->treeViewHydraulicNetwork->setContextMenuPolicy( Qt::CustomContextMenu );
  connect( ui->treeViewHydraulicNetwork, &QWidget::customContextMenuRequested, this, &ReosMeteorologicModelWidget::onMeteoStructureTreeViewContextMenu );

  ui->comboBoxCurrentModel->setModel( meteoModelsCollection );

  ui->widgetToolBar->setLayout( new QHBoxLayout );
  ui->widgetToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  QToolBar *toolBar = new QToolBar( ui->widgetToolBar );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  ui->widgetToolBar->layout()->addWidget( toolBar );

  mActionAddMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/add.svg" ) ), tr( "Add Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onAddMeteoModel );
  mActionDuplicateMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/duplicateMeteoModel.svg" ) ), tr( "Duplicate Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onDuplicateMeteoModel );
  mActionRemoveMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onRemoveMeteoModel );
  mActionRenameMeteoModel = toolBar->addAction( QIcon( QStringLiteral( ":/images/rename.svg" ) ), tr( "Rename Meteorologic Model" ), this, &ReosMeteorologicModelWidget::onRenameMeteoModel );

  connect( ui->comboBoxCurrentModel, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosMeteorologicModelWidget::onCurrentModelChanged );
  onCurrentModelChanged();
  connect( mActionDisplayGriddedOnMap, &QAction::toggled, this, &ReosMeteorologicModelWidget::displayRenderedObject );

  handleRenderedObject();
}

void ReosMeteorologicModelWidget::setCurrentMeteorologicalModel( int index )
{
  ui->comboBoxCurrentModel->setCurrentIndex( index );
}

ReosMeteorologicModelWidget::~ReosMeteorologicModelWidget()
{
  delete ui;
}

ReosTimeWindow ReosMeteorologicModelWidget::timeWindow() const
{
  if ( mCurrentModel )
    return mCurrentModel->timeWindow();

  return ReosTimeWindow();
}

ReosDuration ReosMeteorologicModelWidget::mapTimeStep() const
{
  if ( mCurrentModel )
    return mCurrentModel->mapTimeStep();

  return ReosDuration();
}

QAction *ReosMeteorologicModelWidget::displayGriddedPrecipitationOnMapAction() const
{
  return mActionDisplayGriddedOnMap;
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
  if ( mCurrentModel )
  {
    disconnect( mCurrentModel, &ReosMeteorologicModel::timeWindowChanged, this, &ReosMeteorologicModelWidget::timeWindowChanged );
    disconnect( mCurrentModel, &ReosMeteorologicModel::mapTimeStepChanged, this, &ReosMeteorologicModelWidget::mapTimeStepChanged );
    disconnect( mCurrentModel, &ReosMeteorologicModel::dataChanged, this, &ReosMeteorologicModelWidget::handleRenderedObject );
  }

  ReosMeteorologicModel *current = currentModel();

  mMeteorologicItemModel->setCurrentMeteorologicalModel( current );
  mMeteorologicStructureModel->setCurrentMeteoModel( current );
  mActionDuplicateMeteoModel->setEnabled( current != nullptr );
  mActionRemoveMeteoModel->setEnabled( current != nullptr );
  mActionRenameMeteoModel->setEnabled( current != nullptr );

  mCurrentModel = current;
  emit currentModelChanged( ui->comboBoxCurrentModel->currentIndex() );

  emit timeWindowChanged();
  emit mapTimeStepChanged();

  if ( mCurrentModel )
  {
    connect( mCurrentModel, &ReosMeteorologicModel::timeWindowChanged, this, &ReosMeteorologicModelWidget::timeWindowChanged );
    connect( mCurrentModel, &ReosMeteorologicModel::mapTimeStepChanged, this, &ReosMeteorologicModelWidget::mapTimeStepChanged );
    connect( mCurrentModel, &ReosMeteorologicModel::dataChanged, this, &ReosMeteorologicModelWidget::handleRenderedObject );
  }

  handleRenderedObject();
}

void ReosMeteorologicModelWidget::onMeteoTreeViewContextMenu( const QPoint &pos )
{
  QModelIndex index = ui->treeViewMeteorologicModel->indexAt( pos );
  if ( !index.isValid() )
    return;
  QMenu menu;
  menu.addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Disassociate rainfall" ), &menu, [this, index]
  {
    mMeteorologicItemModel->removeAssociation( index );
  } );

  menu.exec( ui->treeViewMeteorologicModel->mapToGlobal( pos ) );
}

void ReosMeteorologicModelWidget::onMeteoStructureTreeViewContextMenu( const QPoint &pos )
{
  QModelIndex index = ui->treeViewHydraulicNetwork->indexAt( pos );
  if ( !index.isValid() )
    return;
  QMenu menu;
  menu.addAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Disassociate rainfall" ), &menu, [this, index]
  {
    mMeteorologicStructureModel->removeAssociation( index );
  } );

  menu.exec( ui->treeViewHydraulicNetwork->mapToGlobal( pos ) );
}

ReosMeteorologicModel *ReosMeteorologicModelWidget::currentModel() const
{
  int currentIndex = ui->comboBoxCurrentModel->currentIndex();
  return mModelsCollections->meteorologicModel( currentIndex );
}

void ReosMeteorologicModelWidget::handleRenderedObject()
{
  if ( mCurrentModel )
  {
    const QHash<QString, ReosDataObject *> newActiveObjects = mCurrentModel->allRainfall();
    QList<ReosRenderedObject *> renderedObjectToRemove;
    QList<ReosRenderedObject *> renderedObjectToAdd;

    for ( ReosDataObject *objCurrent : std::as_const( mActiveRenderedObject ) )
    {
      auto itNew = newActiveObjects.find( objCurrent->id() );
      if ( itNew == newActiveObjects.end() )
      {
        if ( ReosRenderedObject *rendObj = qobject_cast<ReosRenderedObject *>( objCurrent ) )
          renderedObjectToRemove.append( rendObj );
      }
    }

    for ( ReosRenderedObject *rendObj : std::as_const( renderedObjectToRemove ) )
    {
      mActiveRenderedObject.remove( rendObj->id() );
      mMap->removeExtraRenderedObject( rendObj );
    }

    for ( ReosDataObject *objNew : newActiveObjects )
    {
      auto itCurrent = mActiveRenderedObject.find( objNew->id() );
      if ( itCurrent == mActiveRenderedObject.end() )
      {
        if ( ReosRenderedObject *rendObj = qobject_cast<ReosRenderedObject *>( objNew ) )
          renderedObjectToAdd.append( rendObj );
      }
    }

    for ( ReosRenderedObject *rendObj : std::as_const( renderedObjectToAdd ) )
    {
      mActiveRenderedObject.insert( rendObj->id(), rendObj );
    }
  }
  else
  {
    mActiveRenderedObject.clear();
  }

  displayRenderedObject( mActionDisplayGriddedOnMap->isChecked() );
}

void ReosMeteorologicModelWidget::displayRenderedObject( bool display )
{
  if ( display )
  {
    for ( ReosRenderedObject *objCurrent : std::as_const( mActiveRenderedObject ) )
      mMap->addExtraRenderedObject( objCurrent );
  }
  else
  {
    for ( ReosRenderedObject *objCurrent : std::as_const( mActiveRenderedObject ) )
      mMap->removeExtraRenderedObject( objCurrent );
  }
}


