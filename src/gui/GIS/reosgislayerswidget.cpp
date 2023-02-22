/***************************************************************************
                      reosgislayerwidget.cpp
                     --------------------------------------
Date                 : 17-09-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosgislayerswidget.h"

#include <QVBoxLayout>
#include <QFileDialog>
#include <QMessageBox>
#include <qgslayertreeview.h>
#include <qgslayertree.h>
#include <qgslayertreeutils.h>
#include <qgslayertreeregistrybridge.h>
#include <qgslayertreeutils.h>
#include <qgslayertreeviewdefaultactions.h>
#include <qgslayertreeviewindicator.h>
#include <qgsmapcanvas.h>
#include <qgsmeshlayerproperties.h>
#include <qgsmessagebar.h>
#include <qgsprojectionselectiondialog.h>
#include <qgsprojectionselectiontreewidget.h>
#include <qgsrasterlayerproperties.h>
#include <qgsvectorlayerproperties.h>
#include <qgslayertreemodel.h>


#include "reosgisengine.h"
#include "reossettings.h"
#include "reosmap.h"
#include "reosstyleregistery.h"

#include "reoslayertreecontextmenuprovider_p.h"


ReosGisLayersWidget::ReosGisLayersWidget( ReosGisEngine *engine, ReosMap *map, QWidget *parent ):
  QWidget( parent ),
  mGisEngine( engine ),
  mMap( map ),
  mTreeView( new QgsLayerTreeView( this ) ),
  mToolBar( new QToolBar( this ) ),
  mActionLoadQGISProject( new QAction( QIcon( QStringLiteral( ":/images/openQGISProject.svg" ) ), tr( "Load QGIS Project" ), this ) ),
  mActionLoadVectorLayer( new QAction( QIcon( QStringLiteral( ":/images/mActionAddVectorLayer.svg" ) ), tr( "Add Vector Layer" ), this ) ),
  mActionLoadRasterLayer( new QAction( QIcon( QStringLiteral( ":/images/mActionAddRasterLayer.svg" ) ), tr( "Add Raster Layer" ), this ) ),
  mActionLoadMeshLayer( new QAction( QIcon( QStringLiteral( ":/images/mActionAddMeshLayer.svg" ) ), tr( "Add Mesh Layer" ), this ) ),
  mActionSetProjectCrs( new QAction( QIcon( QStringLiteral( ":/images/CRS.svg" ) ), tr( "Project coordinate reference system" ), this ) )
{
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  connect( mGisEngine, &ReosGisEngine::updated, this, &ReosGisLayersWidget::onGISEngineUpdated );

  mTreeView->setModel( engine->layerTreeModel() );

  setLayout( new QVBoxLayout() );

  layout()->addWidget( mToolBar );
  layout()->addWidget( mTreeView );

  mToolBar->addAction( mActionLoadQGISProject );
  mToolBar->addAction( mActionLoadVectorLayer );
  mToolBar->addAction( mActionLoadRasterLayer );
  mToolBar->addAction( mActionLoadMeshLayer );
  mToolBar->addAction( mActionSetProjectCrs );

  auto defaulAction = new QgsLayerTreeViewDefaultActions( mTreeView );

  mToolBar->addAction( defaulAction->actionAddGroup( this ) );
  mToolBar->addAction( defaulAction->actionRemoveGroupOrLayer( this ) );

  mDemIndicator = new QgsLayerTreeViewIndicator( mTreeView );
  mDemIndicator->setIcon( QIcon( QStringLiteral( ":/images/dem.svg" ) ) );
  mDemIndicator->setToolTip( tr( "Digital Elevation Model" ) );

  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mMap->mapCanvas() );
  if ( mapCanvas )
  {
    mToolBar->addAction( defaulAction->actionZoomToLayers( mapCanvas, this ) );
  }

  connect( mActionLoadQGISProject, &QAction::triggered, this, &ReosGisLayersWidget::onLoadQGISProject );
  connect( mActionLoadVectorLayer, &QAction::triggered, this, &ReosGisLayersWidget::onLoadVectorLayer );
  connect( mActionLoadRasterLayer, &QAction::triggered, this, &ReosGisLayersWidget::onLoadRasterLayer );
  connect( mActionLoadMeshLayer, &QAction::triggered, this, &ReosGisLayersWidget::onLoadMeshLayer );
  connect( mActionSetProjectCrs, &QAction::triggered, this, &ReosGisLayersWidget::onSetCrs );

  connect( mTreeView, &QAbstractItemView::doubleClicked, this, &ReosGisLayersWidget::onTreeLayerDoubleClick );
  connect( mTreeView->selectionModel(), &QItemSelectionModel::currentChanged, this, &ReosGisLayersWidget::updateLayerInsertionPoint );

  QgsLayerTreeNode *rootNode = mTreeView->index2node( QModelIndex() );
  connect( rootNode, &QgsLayerTreeNode::addedChildren, this, &ReosGisLayersWidget::updateIndicator );

  mTreeView->setMenuProvider( new ReosGisLayerTreeContextMenuProvider( this, mTreeView, mMap ) );
}

bool ReosGisLayersWidget::isLayerDigitalElevationModel( const QString &layerId )
{
  return mGisEngine->isDigitalElevationModel( layerId );
}

void ReosGisLayersWidget::registerCurrentLayerAsDigitalElevationModel()
{
  QgsMapLayer *mapLayer = mTreeView->currentLayer();
  if ( mapLayer )
  {
    if ( !mGisEngine->registerLayerAsDigitalElevationModel( mapLayer->id() ) )
      QMessageBox::warning( this, tr( "Register Layer as DEM" ), tr( "This layer is not recognized as a possible DEM" ) );
  }
}

void ReosGisLayersWidget::unRegisterCurrentLayerAsDigitalElevationModel()
{
  QgsMapLayer *mapLayer = mTreeView->currentLayer();
  if ( mapLayer )
    mGisEngine->unRegisterLayerAsDigitalElevationModel( mapLayer->id() );

  mTreeView->removeIndicator( mTreeView->currentNode(), mDemIndicator );
}

void ReosGisLayersWidget::layerProperties()
{
  onTreeLayerDoubleClick();
}

void ReosGisLayersWidget::onLoadQGISProject()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/GisProject" ) ).toString();
  const QString projectFileName = QFileDialog::getOpenFileName( this, tr( "Load QGIS Project" ), path );
  if ( !projectFileName.isEmpty() )
  {
    mGisEngine->loadQGISProject( projectFileName );
    settings.setValue( QStringLiteral( "Path/GisProject" ), projectFileName ) ;
  }
}

void ReosGisLayersWidget::onLoadVectorLayer()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/GisLayer" ) ).toString();

  const QString vectorFileName = QFileDialog::getOpenFileName( this, tr( "Load Vector Layer" ), path, mGisEngine->vectorLayerFilters() );
  const QFileInfo fileInfo( vectorFileName );
  if ( fileInfo.exists() )
    if ( mGisEngine->addVectorLayer( vectorFileName, fileInfo.fileName() ).isEmpty() )
      QMessageBox::warning( this, tr( "Loading Vector Layer" ), tr( "Invalid vector layer, file not loaded." ) );

  settings.setValue( QStringLiteral( "Path/GisLayer" ), fileInfo.path() );
}

void ReosGisLayersWidget::onLoadRasterLayer()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/GisLayer" ) ).toString();

  const QString rasterFileName = QFileDialog::getOpenFileName( this, tr( "Load Raster Layer" ), path, mGisEngine->rasterLayerFilters() );
  const QFileInfo fileInfo( rasterFileName );
  if ( fileInfo.exists() )
  {
    bool isDEM = false;
    if ( mGisEngine->canBeRasterDem( rasterFileName ) )
    {
      isDEM = QMessageBox::question( this, tr( "Loading Raster Layer" ),
                                     tr( "This raster layer could be a DEM, do you want to register as a DEM?" ) ) == QMessageBox::Yes;
    }
    if ( mGisEngine->addRasterLayer( rasterFileName, fileInfo.fileName(), &isDEM ).isEmpty() )
      QMessageBox::warning( this, tr( "Loading Raster Layer" ), tr( "Invalid raster layer, file not loaded." ) );
  }

  settings.setValue( QStringLiteral( "Path/GisLayer" ), fileInfo.path() );
}

void ReosGisLayersWidget::onLoadMeshLayer()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/GisLayer" ) ).toString();
  QString filter = mGisEngine->meshLayerFilters();

  if ( filter.isEmpty() )
    return;

  const QString meshFileName = QFileDialog::getOpenFileName( this, tr( "Load Mesh Layer" ), path, filter );

  const QFileInfo fileInfo( meshFileName );
  if ( fileInfo.exists() )
    if ( mGisEngine->addMeshLayer( meshFileName, fileInfo.fileName() ).isEmpty() )
      QMessageBox::warning( this, tr( "Loading Raster Layer" ), tr( "Invalid raster layer, file not loaded." ) );

}

void ReosGisLayersWidget::onTreeLayerDoubleClick()
{
  if ( ! mTreeView && !mMap )
    return;

  QgsMapLayer *mapLayer = mTreeView->currentLayer();

  if ( !mapLayer )
    return;

  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( mMap->mapCanvas() );

  std::unique_ptr<QDialog> dial = nullptr;
  QgsMessageBar messageBar;
  switch ( mapLayer->type() )
  {
    case Qgis::LayerType::Vector:
      dial.reset( new QgsVectorLayerProperties( mapCanvas, &messageBar, qobject_cast<QgsVectorLayer *>( mapLayer ), this ) );
      break;
    case Qgis::LayerType::Raster:
      dial.reset( new QgsRasterLayerProperties( mapLayer, mapCanvas, this ) );
      break;
    case Qgis::LayerType::Mesh:
      dial.reset( new QgsMeshLayerProperties( mapLayer, mapCanvas, this ) );
      break;
    default:
      break;
  }

  if ( dial )
    dial->exec();

}

void ReosGisLayersWidget::onSetCrs()
{
  QgsProjectionSelectionDialog dial;
  QString crs = mGisEngine->crs();
  dial.setCrs( QgsCoordinateReferenceSystem::fromWkt( crs ) );
  if ( dial.exec() )
    mGisEngine->setCrs( dial.crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED ) );

}

void ReosGisLayersWidget::updateLayerInsertionPoint() const
{
  // from QGIS sources :  QgisApp::layerTreeInsertionPoint() in src/app/qgisapp.cpp

  QgsLayerTreeRegistryBridge::InsertionPoint insertPoint( nullptr, -1 );
  QgsLayerTreeGroup *insertGroup = mTreeView->layerTreeModel()->rootGroup();
  QgsLayerTreeNode *currentNode = mTreeView->currentNode();

  if ( currentNode )
  {
    // if the insertion point is actually a group, insert new layers into the group
    if ( QgsLayerTree::isGroup( currentNode ) )
    {
      // if the group is embedded go to the first non-embedded group, at worst the top level item
      QgsLayerTreeGroup *insertGroup = QgsLayerTreeUtils::firstGroupWithoutCustomProperty( QgsLayerTree::toGroup( currentNode ), QStringLiteral( "embedded" ) );
      insertPoint = QgsLayerTreeRegistryBridge::InsertionPoint( insertGroup, 0 );
    }
    else
    {
      int ind = mTreeView->currentIndex().row();
      // otherwise just set the insertion point in front of the current node
      QgsLayerTreeNode *parentNode = currentNode->parent();
      if ( QgsLayerTree::isGroup( parentNode ) )
      {
        // if the group is embedded go to the first non-embedded group, at worst the top level item
        QgsLayerTreeGroup *parentGroup = QgsLayerTree::toGroup( parentNode );
        insertGroup = QgsLayerTreeUtils::firstGroupWithoutCustomProperty( parentGroup, QStringLiteral( "embedded" ) );
        if ( parentGroup != insertGroup )
          ind = 0;
      }

      insertPoint = QgsLayerTreeRegistryBridge::InsertionPoint( insertGroup, ind );
    }
  }

  QgsProject::instance()->layerTreeRegistryBridge()->setLayerInsertionPoint( insertPoint );
}

void ReosGisLayersWidget::onGISEngineUpdated()
{
  updateIndicator();
}


static void updateTreeIndicator( QgsLayerTreeView *treeView, QgsLayerTreeNode *node, QStringList layerIdList, QgsLayerTreeViewIndicator *indicator )
{
  if ( node->nodeType() == QgsLayerTreeNode::NodeLayer )
  {
    if ( layerIdList.contains( static_cast<QgsLayerTreeLayer *>( node )->layerId() ) )
    {
      treeView->removeIndicator( node, indicator );
      treeView->addIndicator( node, indicator );
    }
  }

  QList<QgsLayerTreeNode *> children = node->children();
  for ( QgsLayerTreeNode *child : children )
    updateTreeIndicator( treeView, child, layerIdList, indicator );
}

void ReosGisLayersWidget::updateIndicator()
{
  QStringList demList = mGisEngine->digitalElevationModelIds();

  QgsLayerTreeNode *rootNode = mTreeView->index2node( QModelIndex() );

  updateTreeIndicator( mTreeView, rootNode, demList, mDemIndicator );
}
