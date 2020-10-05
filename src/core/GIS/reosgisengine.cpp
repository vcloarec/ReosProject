/***************************************************************************
                      reosgisengine.cpp
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

#include "reosgisengine.h"
#include "reosdigitalelevationmodel.h"
#include "reosdigitalelevationmodel_p.h"

#include <qgslayertreemodel.h>
#include <qgslayertree.h>
#include <qgslayertreeutils.h>
#include <qgslayertreeregistrybridge.h>
#include <qgsproject.h>
#include <qgsvectorlayer.h>
#include <qgsrasterlayer.h>
#include <qgsmeshlayer.h>
#include <qgsprovidermetadata.h>
#include <qgsproviderregistry.h>
#include <qgsapplication.h>

#define  mLayerTreeModel _layerTreeModel(mAbstractLayerTreeModel)
static QgsLayerTreeModel *_layerTreeModel( QAbstractItemModel *sourceModel )
{
  return qobject_cast<QgsLayerTreeModel *>( sourceModel );
}

ReosGisEngine::ReosGisEngine( QObject *parent ): ReosModule( parent )
{
  QgsApplication::init();
  QgsProviderRegistry::instance( QGIS_PLUGINS );
  mAbstractLayerTreeModel = new QgsLayerTreeModel( QgsProject::instance()->layerTreeRoot(), this );

  mLayerTreeModel->setFlag( QgsLayerTreeModel::AllowNodeReorder );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::AllowNodeRename );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::AllowNodeChangeVisibility );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::ShowLegendAsTree );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::UseEmbeddedWidgets );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::UseTextFormatting );
  mLayerTreeModel->setAutoCollapseLegendNodes( 10 );
}

QString ReosGisEngine::addVectorLayer( const QString &uri, const QString &name )
{
  std::unique_ptr<QgsVectorLayer> vectorLayer( new QgsVectorLayer( uri, name ) );

  if ( vectorLayer->isValid() )
  {
    QgsMapLayer *mapLayer = QgsProject::instance()->addMapLayer( vectorLayer.release() );
    message( tr( "Vector layer loaded: %1" ).arg( uri ) );
    return mapLayer->id();
  }
  else
  {
    warning( tr( "Vector layer not loaded: %1" ).arg( uri ) );
    return QString();
  }
}

QString ReosGisEngine::addRasterLayer( const QString &uri, const QString &name )
{
  std::unique_ptr<QgsRasterLayer> rasterlayer( new QgsRasterLayer( uri, name ) );

  if ( rasterlayer->isValid() )
  {
    QgsMapLayer *mapLayer = QgsProject::instance()->addMapLayer( rasterlayer.release() );
    message( tr( "Raster layer loaded: %1" ).arg( uri ) );
    return mapLayer->id();
  }
  else
  {
    warning( tr( "Raster layer not loaded: %1" ).arg( uri ) );
    return QString();
  }
}

QString ReosGisEngine::addMeshLayer( const QString &uri, const QString &name )
{
  std::unique_ptr<QgsMeshLayer> meshLayer( new QgsMeshLayer( uri, name, "mdal" ) );

  if ( meshLayer->isValid() )
  {
    QgsMapLayer *mapLayer = QgsProject::instance()->addMapLayer( meshLayer.release() );
    message( tr( "Mesh layer loaded: %1" ).arg( uri ) );
    return mapLayer->id();;
  }
  else
  {
    warning( tr( "Mesh layer not loaded: %1" ).arg( uri ) );
    return QString();
  }

}

void ReosGisEngine::addGroupLayer()
{

}

QAbstractItemModel *ReosGisEngine::layerTreeModel() {return mLayerTreeModel;}

QString ReosGisEngine::vectorLayerFilters() const
{
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( "ogr" );
  return meta->filters( QgsProviderMetadata::FilterType::FilterVector );
}

QString ReosGisEngine::rasterLayerFilters() const
{
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( "gdal" );
  return meta->filters( QgsProviderMetadata::FilterType::FilterRaster );
}

QString ReosGisEngine::meshLayerFilters() const
{
  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( "mdal" );

  if ( meta )
    return meta->filters( QgsProviderMetadata::FilterType::FilterMesh );
  else
  {
    error( tr( "Could not find MDAL" ) );
    return QString();
  }
}

QString ReosGisEngine::crs() const
{
  return QgsProject::instance()->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}

void ReosGisEngine::setCrs( const QString &wktCrs )
{
  QgsCoordinateReferenceSystem crs = QgsCoordinateReferenceSystem::fromWkt( wktCrs );
  QgsProject::instance()->setCrs( crs );
  emit crsChanged( wktCrs );
}

void ReosGisEngine::loadQGISProject( const QString &fileName )
{
  QgsProject::instance()->read( fileName );
}

void ReosGisEngine::saveQGISProject( const QString &fileName ) const
{
  QgsProject::instance()->write( fileName );
}

bool ReosGisEngine::registerLayerAsDigitalElevationModel( const QString &layerId )
{
  QgsMapLayer *layer = QgsProject::instance()->mapLayer( layerId );
  if ( !layer )
    return false;

  if ( layer->type() == QgsMapLayerType::RasterLayer )
  {
    mAsDEMRegisteredLayer.append( layerId );
    return true;
    emit digitalElevationRegistered( layerId );
  }
  return false;
}

void ReosGisEngine::unRegisterLayerAsDigitalElevationModel( const QString &layerId )
{
  if ( mAsDEMRegisteredLayer.removeOne( layerId ) )
    emit digitalElevationRegistered( layerId );
}

bool ReosGisEngine::isDigitalElevationModel( const QString &layerId ) const
{
  return mAsDEMRegisteredLayer.contains( layerId );
}

static void allLayersOrder( QgsLayerTreeNode *node, QList<QgsMapLayer *> &allLayers )
{
  if ( QgsLayerTree::isLayer( node ) )
  {
    QgsLayerTreeLayer *nodeLayer = QgsLayerTree::toLayer( node );
    if ( nodeLayer->layer() && nodeLayer->layer()->isSpatial() )
    {
      allLayers << nodeLayer->layer();
    }
  }

  const QList<QgsLayerTreeNode *> children = node->children();
  for ( QgsLayerTreeNode *child : children )
    allLayersOrder( child, allLayers );
}

ReosDigitalElevationModel *ReosGisEngine::getTopDigitalElevationModel() const
{
  QgsLayerTreeNode *rootNode = mLayerTreeModel->rootGroup();
  QList<QgsMapLayer *> layersOrder;
  allLayersOrder( rootNode, layersOrder );

  for ( QgsMapLayer *layer : layersOrder )
  {
    if ( mAsDEMRegisteredLayer.contains( layer->id() ) )
    {
      QgsRasterLayer *rl = qobject_cast<QgsRasterLayer *>( layer );
      if ( rl )
      {
        QgsCoordinateTransformContext transformContext = QgsProject::instance()->transformContext();
        return ReosDigitalElevationModelFactory::createDEM( rl, transformContext );
      }
      else
        return nullptr;
    }
  }
  return nullptr;
}

QMap<QString, QString> ReosGisEngine::digitalElevationModelRasterList() const
{
  QgsLayerTreeNode *rootNode = mLayerTreeModel->rootGroup();
  QList<QgsMapLayer *> layersOrder;
  allLayersOrder( rootNode, layersOrder );

  QMap<QString, QString> demList;
  for ( QgsMapLayer *layer : layersOrder )
  {
    if ( mAsDEMRegisteredLayer.contains( layer->id() ) )
    {
      QgsRasterLayer *rl = qobject_cast<QgsRasterLayer *>( layer );
      if ( rl )
      {
        demList[rl->id()] = rl->name();
      }
    }
  }

  return demList;
}

ReosGisEngine::LayerType ReosGisEngine::layerType( const QString layerId ) const
{
  QgsMapLayer *layer = QgsProject::instance()->mapLayer( layerId );

  if ( !layer )
    return NoLayer;

  switch ( layer->type() )
  {
    case QgsMapLayerType::VectorLayer:
      return VectorLayer;
      break;
    case QgsMapLayerType::RasterLayer:
      return RasterLayer;
      break;
    case QgsMapLayerType::MeshLayer:
      return MeshLayer;
      break;
    default:
      return NotSupported;
      break;
  }

  return NotSupported;


}
