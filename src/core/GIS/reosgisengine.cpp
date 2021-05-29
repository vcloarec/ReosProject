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

#include <QStandardPaths>
#include <qmath.h>

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
#include <qgslinestring.h>
#include <qgsgeometry.h>
#include <qgspolygon.h>
#include <qgsprojutils.h>
#include <qgssinglebandpseudocolorrenderer.h>

#define  mLayerTreeModel _layerTreeModel(mAbstractLayerTreeModel)
static QgsLayerTreeModel *_layerTreeModel( QAbstractItemModel *sourceModel )
{
  return qobject_cast<QgsLayerTreeModel *>( sourceModel );
}

ReosGisEngine::ReosGisEngine( QObject *parent ): ReosModule( parent )
{

  initGisEngine();

  mLayerTreeModel->setFlag( QgsLayerTreeModel::AllowNodeReorder );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::AllowNodeRename );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::AllowNodeChangeVisibility );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::ShowLegendAsTree );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::UseEmbeddedWidgets );
  mLayerTreeModel->setFlag( QgsLayerTreeModel::UseTextFormatting );
  mLayerTreeModel->setAutoCollapseLegendNodes( 10 );

  connect( QgsProject::instance(), &QgsProject::layerRemoved, this, &ReosGisEngine::layerRemoved );
  connect( QgsProject::instance(), &QgsProject::crsChanged, this, [this]
  {
    QString wktCrs = QgsProject::instance()->crs().toWkt();
    emit crsChanged( wktCrs );
  } );
}

ReosGisEngine::~ReosGisEngine()
{
  QgsApplication::exitQgis();
}

void ReosGisEngine::initGisEngine()
{
  QString profileFolder = QStandardPaths::standardLocations( QStandardPaths::AppDataLocation ).value( 0 );
  // here we do not want profile folder as QGIS has, but only one unique folder for QGIS stuff, so we gives only the App data location
  // Give a profile folder also avoid QGIS to override the settings path
  QgsApplication::init( profileFolder );

  // Here we use the provider files that are in the folder "qgisProvider", if this folder do not exist (local build),
  // then we use the folder defined by the varaible QGIS_PLUGINS
  QString qgisProviderPath = QCoreApplication::applicationDirPath();
  QDir providerDir( qgisProviderPath );
  if ( providerDir.cd( QStringLiteral( "qgisProvider" ) ) )
    qgisProviderPath = providerDir.absolutePath();
  else
    qgisProviderPath = QGIS_PLUGINS;
  QgsProviderRegistry::instance( qgisProviderPath );

  //check for proj data
  const QStringList projPaths = QgsProjUtils::searchPaths();
  bool projDataPresent = false;
  for ( const QString &projPath : projPaths )
  {
    QDir dir( projPath );
    QString projDbPath = dir.filePath( QStringLiteral( "proj.db" ) );
    QFileInfo fileInfo( projDbPath );
    projDataPresent |= fileInfo.exists();
  }

  if ( !projDataPresent )
  {
    QDir projDir( QApplication::applicationDirPath() );
    if ( projDir.cdUp() && projDir.cd( QStringLiteral( "share" ) ) && projDir.cd( QStringLiteral( "proj" ) ) )
    {
      QStringList filesList = projDir.entryList( QDir::NoDotAndDotDot | QDir::Files );
      QString destinationPath = QStandardPaths::writableLocation( QStandardPaths::AppDataLocation );
      QDir destinationDir( destinationPath );
      if ( !destinationDir.cd( QStringLiteral( "proj" ) ) )
      {
        destinationDir.mkdir( QStringLiteral( "proj" ) );
        destinationDir.cd( QStringLiteral( "proj" ) );
      }
      for ( const QString &fileName : filesList )
      {
        QFile file( projDir.absoluteFilePath( fileName ) );
        file.copy( destinationDir.absoluteFilePath( fileName ) );
      }
    }
  }

  mAbstractLayerTreeModel = new QgsLayerTreeModel( QgsProject::instance()->layerTreeRoot(), this );
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

QString ReosGisEngine::addRasterLayer( const QString &uri, const QString &name, bool *isDEM )
{
  std::unique_ptr<QgsRasterLayer> rasterlayer( new QgsRasterLayer( uri, name ) );

  if ( rasterlayer->isValid() )
  {
    if ( isDEM && *isDEM )
      defaultstyleRasterLayer( rasterlayer.get() );
    QgsMapLayer *mapLayer = QgsProject::instance()->addMapLayer( rasterlayer.release() );
    message( tr( "Raster layer loaded: %1" ).arg( uri ) );
    if ( isDEM && *isDEM )
      registerLayerAsDigitalElevationModel( mapLayer->id() );
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

void ReosGisEngine::setCrs( const QString &crsString )
{
  QgsCoordinateReferenceSystem crs( crsString );
  QgsProject::instance()->setCrs( crs );
  emit crsChanged( crs.toWkt() );
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
    QgsRasterLayer *rasterLayer = qobject_cast<QgsRasterLayer *>( layer );
    if ( canBeRasterDem( rasterLayer ) )
    {
      mAsDEMRegisteredLayer.append( layerId );
      QgsRectangle layerExtent = layer->extent();
      if ( QgsProject::instance()->crs().isGeographic() &&
           !layer->crs().isValid() &&
           ( std::fabs( layerExtent.xMinimum() ) > 360 ||
             std::fabs( layerExtent.xMaximum() ) > 360 ||
             std::fabs( layerExtent.yMinimum() ) > 360 ||
             std::fabs( layerExtent.yMaximum() ) > 360 ) )
        warning( tr( "This layer doesn't have a valid or known coordinates system, and calculation on DEM will "
                     "be done considering its coordinate system is the same as the project.\n"
                     "The project's coordinates system is a geographic reference system (latitude/longitude) and "
                     "the extent of this layer suggests that it is actually a projected coordinates system.\n"
                     "If so, the result of area or distance calculation will be incorrect.\n"
                     "To fix this, set an appropriate map coordinate system in the layer properties "
                     "or/and for the project coordinate project." ), true );

      emit updated();
      return true;
    }
  }
  return false;
}

void ReosGisEngine::unRegisterLayerAsDigitalElevationModel( const QString &layerId )
{
  if ( mAsDEMRegisteredLayer.removeOne( layerId ) )
    emit updated();
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

ReosDigitalElevationModel *ReosGisEngine::getDigitalElevationModel( const QString &layerId ) const
{
  QgsMapLayer *layer = QgsProject::instance()->mapLayer( layerId );
  QgsRasterLayer *rl = qobject_cast<QgsRasterLayer *>( layer );
  if ( rl )
  {
    QgsCoordinateTransformContext transformContext = QgsProject::instance()->transformContext();
    return ReosDigitalElevationModelFactory::createDEM( rl, transformContext );
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

QStringList ReosGisEngine::digitalElevationModelIds() const
{
  return mAsDEMRegisteredLayer;
}

ReosArea ReosGisEngine::polygonArea( const QPolygonF &polygon, const QString &crs ) const
{

  QgsCoordinateReferenceSystem qgsCrs;
  if ( crs.isEmpty() )
    qgsCrs = QgsProject::instance()->crs();
  else
    qgsCrs = QgsCoordinateReferenceSystem::fromWkt( crs );

  QgsDistanceArea areaCalculation;
  areaCalculation.setSourceCrs( qgsCrs, QgsProject::instance()->transformContext() );

  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( polygon ) );
  std::unique_ptr<QgsPolygon> qgsPolygon = std::make_unique<QgsPolygon>( linestring.release() );
  double area = areaCalculation.measureArea( QgsGeometry( qgsPolygon.release() ) );
  QgsUnitTypes::AreaUnit unit = areaCalculation.areaUnits();

  double transFormFactorToSquareMeter = QgsUnitTypes::fromUnitToUnitFactor( unit, QgsUnitTypes::AreaSquareMeters );

  return ReosArea( area * transFormFactorToSquareMeter );
}

double ReosGisEngine::convertLengthFromMeterToMapunit( double length )
{
  double unitFactor = QgsUnitTypes::fromUnitToUnitFactor( QgsUnitTypes::DistanceMeters, QgsProject::instance()->crs().mapUnits() );
  return length * unitFactor;
}


ReosEncodedElement ReosGisEngine::encode( const QString &path, const QString baseFileName )
{
  QFileInfo fileInfo;
  QDir dir( path );
  QString fileName = baseFileName;
  fileName.append( ".gpj" );
  fileInfo.setFile( dir, fileName );

  QgsProject::instance()->write( fileInfo.filePath() );
  ReosEncodedElement encodedElement( QStringLiteral( "GIS-engine" ) );
  encodedElement.addData( QStringLiteral( "registered-dem-layer-ids" ), mAsDEMRegisteredLayer );
  return encodedElement;
}

bool ReosGisEngine::decode( const ReosEncodedElement &encodedElement, const QString &path, const QString baseFileName )
{
  if ( encodedElement.description() != QStringLiteral( "GIS-engine" ) )
    return false;

  QFileInfo fileInfo;
  QDir dir( path );
  QString fileName = baseFileName;
  fileName.append( ".gpj" );
  fileInfo.setFile( dir, fileName );

  QgsProject::instance()->clear();
  QgsProject::instance()->read( fileInfo.filePath() );

  mAsDEMRegisteredLayer.clear();
  if ( !encodedElement.getData( QStringLiteral( "registered-dem-layer-ids" ), mAsDEMRegisteredLayer ) )
  {
    emit updated();
    return false;
  }

  emit updated();
  return true;
}

void ReosGisEngine::clearProject()
{
  if ( QgsProject::instance() )
    QgsProject::instance()->clear();

  setCrs( QStringLiteral( "EPSG:4326" ) );

  emit updated();
}

bool ReosGisEngine::canBeRasterDem( const QString &uri ) const
{
  std::unique_ptr<QgsRasterLayer> rasterLayer = std::make_unique<QgsRasterLayer>( uri );

  return canBeRasterDem( rasterLayer.get() );
}

QString ReosGisEngine::gisEngineName()
{
  return QStringLiteral( "QGIS" );
}

QString ReosGisEngine::gisEngineVersion()
{
  return Qgis::version();
}

QString ReosGisEngine::gisEngineLink()
{
  return QStringLiteral( "www.qgis.org" );
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

QString ReosGisEngine::layerName( const QString layerId ) const
{
  QgsMapLayer *layer = QgsProject::instance()->mapLayer( layerId );

  if ( !layer )
    return QString();

  return layer->name();
}

void ReosGisEngine::layerRemoved( const QString &layerId )
{
  unRegisterLayerAsDigitalElevationModel( layerId );
}

void ReosGisEngine::defaultstyleRasterLayer( QgsRasterLayer *layer )
{
  if ( !canBeRasterDem( layer ) )
    return;
  QString defaultStylePath = QCoreApplication::applicationDirPath() + QStringLiteral( "/../resources/" );
  bool ok;
  layer->loadNamedStyle( defaultStylePath + QStringLiteral( "dem.qml" ), ok );
}


bool ReosGisEngine::canBeRasterDem( QgsRasterLayer *layer ) const
{
  QgsRasterDataProvider *provider = layer->dataProvider();
  if ( !provider )
    return false;
  int bandCount = provider->bandCount();
  if ( bandCount == 1 )
  {
    Qgis::DataType dataType = provider->dataType( 1 );
    switch ( dataType )
    {
      case Qgis::UnknownDataType:
      case Qgis::Byte:
      case Qgis::UInt16:
      case Qgis::Int16:
      case Qgis::UInt32:
      case Qgis::Int32:
      case Qgis::CInt16:
      case Qgis::CInt32:
      case Qgis::ARGB32:
      case Qgis::ARGB32_Premultiplied:
        return false;
        break;
      case Qgis::CFloat32:
      case Qgis::CFloat64:
      case Qgis::Float32:
      case Qgis::Float64:
        return true;
        //could be a DEM
        break;
    }
  }

  return false;
}
