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
#include "reosmeshdataprovider_p.h"

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
#include <qgis.h>
#include <qgsdistancearea.h>
#include <qgsnetworkaccessmanager.h>
#include <qgsauthmethodregistry.h>
#include <qgsauthmanager.h>
#include <qgsprojecttimesettings.h>
#include <qgsprojectviewsettings.h>
#include <qgslayerdefinition.h>
#include <qgslayertree.h>


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

  connect( QgsProject::instance(), &QgsProject::layerRemoved, this, &ReosGisEngine::onLayerRemoved );
  connect( QgsProject::instance(), &QgsProject::crsChanged, this, [this]
  {
    QString wktCrs = QgsProject::instance()->crs().toWkt();
    emit crsChanged( wktCrs );
  } );

  connect( QgsProject::instance(), &QgsProject::dirtySet, this, &ReosModule::dirtied );
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

  //! init the QGIS net work manager to access remote GIS data
  QgsApplication::authManager()->init( qgisProviderPath, QgsApplication::qgisAuthDatabaseFilePath() );
  QgsAuthMethodRegistry::instance( qgisProviderPath );
  QgsNetworkAccessManager::instance();

  mAbstractLayerTreeModel = new QgsLayerTreeModel( QgsProject::instance()->layerTreeRoot(), this );

  //! Add reos data provider to Qgis instances
  QgsProviderRegistry::instance()->registerProvider( new ReosMeshProviderMetaData() );

  qRegisterMetaTypeStreamOperators<QgsFeature>( "QgsFeature" ); //necessary to allow the serialisation
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
  QString oldCrs = crs();
  QgsProject::instance()->read( fileName );
  if ( crs() != oldCrs )
    emit crsChanged( crs() );
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
  loadQGISProject( fileInfo.filePath() );

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

ReosMapExtent ReosGisEngine::transformToProjectExtent( const ReosMapExtent &extent ) const
{
  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( extent.crs() ),
                                    QgsProject::instance()->crs(),
                                    QgsProject::instance()->transformContext() );

  QgsRectangle qgsExtent( extent.toRectF() );
  ReosMapExtent ret;

  try
  {
    QgsRectangle transformExtent = transform.transform( qgsExtent );
    ret = ReosMapExtent( transformExtent.toRectF() );
    ret.setCrs( QgsProject::instance()->crs().toWkt() );
  }
  catch ( ... )
  {
    ret = ReosMapExtent( qgsExtent.toRectF() );
  }

  return ret;
}

ReosMapExtent ReosGisEngine::transformFromProjectExtent( const ReosMapExtent &extent, const QString &wktCrs ) const
{
  QgsCoordinateTransform transform( QgsProject::instance()->crs(),
                                    QgsCoordinateReferenceSystem::fromWkt( wktCrs ),
                                    QgsProject::instance()->transformContext() );

  QgsRectangle qgsExtent( extent.toRectF() );
  ReosMapExtent ret;

  try
  {
    QgsRectangle transformExtent = transform.transform( qgsExtent );
    ret = ReosMapExtent( transformExtent.toRectF() );
    ret.setCrs( wktCrs );
  }
  catch ( ... )
  {
    ret = ReosMapExtent( qgsExtent.toRectF() );
  }

  return ret;
}

QPointF ReosGisEngine::transformToProjectCoordinates( const QString &sourceCRS, const QPointF &sourcePoint ) const
{
  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( sourceCRS ),
                                    QgsProject::instance()->crs(),
                                    QgsProject::instance()->transformContext() );

  try
  {
    QgsPointXY transformPoint = transform.transform( QgsPointXY( sourcePoint ) );
    return transformPoint.toQPointF();
  }
  catch ( ... )
  {
    return sourcePoint;
  }
}

QPointF ReosGisEngine::transformToProjectCoordinates( const ReosSpatialPosition &position ) const
{
  return transformToProjectCoordinates( position.crs(), position.position() );
}

QPolygonF ReosGisEngine::transformToProjectCoordinates( const QString &sourceCRS, const QPolygonF &sourcePolygon ) const
{
  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( sourceCRS ),
                                    QgsProject::instance()->crs(),
                                    QgsProject::instance()->transformContext() );

  QgsGeometry geom = QgsGeometry::fromQPolygonF( sourcePolygon );
  QgsGeometry transformGeom = geom;

  try
  {
    transformGeom.transform( transform );
  }
  catch ( ... )
  {
    transformGeom = geom;
  }

  QPolygonF ret = transformGeom.asQPolygonF();
  if ( transformGeom.type() == QgsWkbTypes::PolygonGeometry && ret.count() > 1 )
    ret.removeLast();

  return ret;
}

QPointF ReosGisEngine::transformToCoordinates( const ReosSpatialPosition &position, const QString &destinationCrs )
{
  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( position.crs() ),
                                    QgsCoordinateReferenceSystem::fromWkt( destinationCrs ),
                                    QgsProject::instance()->transformContext() );

  try
  {
    QgsPointXY transformPoint = transform.transform( QgsPointXY( position.position() ) );
    return transformPoint.toQPointF();
  }
  catch ( ... )
  {
    return position.position();
  }
}

QPolygonF ReosGisEngine::transformToCoordinates( const QString &sourceCRS, const QPolygonF &sourcePolygon, const QString &destinationCrs )
{
  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( sourceCRS ),
                                    QgsCoordinateReferenceSystem::fromWkt( destinationCrs ),
                                    QgsProject::instance()->transformContext() );

  const QgsGeometry geom = QgsGeometry::fromQPolygonF( sourcePolygon );
  QgsGeometry transfromGeom = geom;
  if ( transform.isValid() )
  {
    try
    {
      transfromGeom.transform( transform );
      return transfromGeom.asQPolygonF();
    }
    catch ( ... )
    {
      return geom.asQPolygonF();
    }
  }

  return geom.asQPolygonF();
}

void ReosGisEngine::setTemporalRange( const QDateTime &startTime, const QDateTime &endTime )
{
  QgsProjectTimeSettings *timeSettings = QgsProject::instance()->timeSettings();
  if ( timeSettings )
  {
    timeSettings->setTemporalRange( QgsTemporalRange( startTime, endTime ) );
    emit temporalRangeChanged( startTime, endTime );
  }
}

QPair<QDateTime, QDateTime> ReosGisEngine::temporalRange() const
{
  QgsProjectTimeSettings *timeSettings = QgsProject::instance()->timeSettings();
  if ( timeSettings )
    return QPair<QDateTime, QDateTime>( {timeSettings->temporalRange().begin(), timeSettings->temporalRange().end()} );

  return QPair<QDateTime, QDateTime>();
}

bool ReosGisEngine::createProjectFile( const QString &projectFileName, bool keepLayer )
{
  std::unique_ptr<QgsProject> project = std::make_unique<QgsProject>();
  project->setCrs( QgsProject::instance()->crs() );
  project->setTransformContext( QgsProject::instance()->transformContext() );

  if ( keepLayer )
  {
    QDomDocument doc( QStringLiteral( "layers" ) );
    QString error;
    if ( QgsLayerDefinition::exportLayerDefinition( doc, QgsProject::instance()->layerTreeRoot()->children(), error, QgsReadWriteContext() ) )
    {
      QgsReadWriteContext readWriteContext;
      QgsLayerDefinition::loadLayerDefinition( doc, project.get(), project->layerTreeRoot(), error, readWriteContext );
    }
  }

  return project->write( projectFileName );
}

bool ReosGisEngine::addMeshLayerToExistingProject(
  const QString &projectFileName,
  const QString &layerName,
  const QString &uri,
  const ReosEncodedElement &meshFrameSymbology,
  const QMap<QString, ReosEncodedElement> &scalarSymbologies,
  const QMap<QString, ReosEncodedElement> &vectorSymbologies )
{
  std::unique_ptr<QgsProject> project = std::make_unique<QgsProject>();
  project->read( projectFileName );
  std::unique_ptr<QgsMeshLayer> meshLayer = std::make_unique<QgsMeshLayer>( uri, layerName, QStringLiteral( "mdal" ) );
  project->viewSettings()->setDefaultViewExtent( QgsReferencedRectangle( meshLayer->extent(), meshLayer->crs() ) );

  QgsMeshRendererSettings settings = meshLayer->rendererSettings();
  {
    QDomDocument doc( QStringLiteral( "frame-symbology" ) );
    QString docString;
    meshFrameSymbology.getData( QStringLiteral( "frame-symbology" ), docString );
    if ( doc.setContent( docString ) )
    {
      const QDomElement &domElem = doc.firstChildElement( QStringLiteral( "mesh-settings" ) );
      QgsMeshRendererMeshSettings meshSettings;
      meshSettings.readXml( domElem );
      settings.setNativeMeshSettings( meshSettings );
    }
  }

  const QList<int> &indexes = meshLayer->datasetGroupsIndexes();
  for ( int i : indexes )
  {
    QgsMeshDatasetGroupMetadata meta = meshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( i, 0 ) );

    if ( scalarSymbologies.contains( meta.name() ) )
    {
      const ReosEncodedElement &symbology = scalarSymbologies.value( meta.name() );
      QString docString;
      symbology.getData( QStringLiteral( "symbology" ), docString );
      QDomDocument doc( QStringLiteral( "dataset-symbology" ) );

      if ( doc.setContent( docString ) )
      {
        const QDomElement &domElem = doc.firstChildElement( QStringLiteral( "scalar-settings" ) );
        QgsMeshRendererScalarSettings scalarSettings;
        scalarSettings.readXml( domElem );
        settings.setScalarSettings( i, scalarSettings );
      }
    }

    if ( vectorSymbologies.contains( meta.name() ) )
    {
      const ReosEncodedElement &symbology = vectorSymbologies.value( meta.name() );
      QString docString;
      symbology.getData( QStringLiteral( "symbology" ), docString );
      QDomDocument doc( QStringLiteral( "dataset-vector-symbology" ) );

      if ( doc.setContent( docString ) )
      {
        const QDomElement &domElem = doc.firstChildElement( QStringLiteral( "vector-settings" ) );
        QgsMeshRendererVectorSettings vectorSettings;
        vectorSettings.readXml( domElem );
        settings.setVectorSettings( i, vectorSettings );
      }
    }
  }

  meshLayer->setRendererSettings( settings );

  project->addMapLayer( meshLayer.release() );
  return project->write( projectFileName );
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

QString ReosGisEngine::wktEPSGCrs( int code )
{
  return QgsCoordinateReferenceSystem::fromEpsgId( code ).toWkt();
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

bool ReosGisEngine::hasValidLayer( const QString layerId ) const
{
  QgsMapLayer *layer = QgsProject::instance()->mapLayer( layerId );

  return ( layer && layer->isValid() );
}

void ReosGisEngine::onLayerRemoved( const QString &layerId )
{
  unRegisterLayerAsDigitalElevationModel( layerId );
  emit layerRemoved( layerId );
}

void ReosGisEngine::defaultstyleRasterLayer( QgsRasterLayer *layer )
{
  if ( !canBeRasterDem( layer ) )
    return;
  QString defaultStylePath =  QgsApplication::pkgDataPath() + QStringLiteral( "/resources/" );
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
      case Qgis::DataType::UnknownDataType:
      case Qgis::DataType::Byte:
      case Qgis::DataType::ARGB32:
      case Qgis::DataType::ARGB32_Premultiplied:
        return false;
        break;
      case Qgis::DataType::UInt16:
      case Qgis::DataType::Int16:
      case Qgis::DataType::UInt32:
      case Qgis::DataType::Int32:
      case Qgis::DataType::CInt16:
      case Qgis::DataType::CInt32:
      case Qgis::DataType::CFloat32:
      case Qgis::DataType::CFloat64:
      case Qgis::DataType::Float32:
      case Qgis::DataType::Float64:
        return true;
        //could be a DEM
        break;
    }
  }

  return false;
}
