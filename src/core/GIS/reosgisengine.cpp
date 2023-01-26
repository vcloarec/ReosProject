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
#include "reosgriddedrainfallrenderer_p.h"

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
#include <qgsmeshlayertemporalproperties.h>
#include <qgsspatialindex.h>


#define  mLayerTreeModel _layerTreeModel(mAbstractLayerTreeModel)
static QgsLayerTreeModel *_layerTreeModel( QAbstractItemModel *sourceModel )
{
  return qobject_cast<QgsLayerTreeModel *>( sourceModel );
}

class ReosCoordinateSystemTransformer_impl
{
  public:
    QgsCoordinateTransformContext mTransformContext;
};

ReosCoordinateSystemTransformer::ReosCoordinateSystemTransformer( const ReosCoordinateSystemTransformer &other )
{
  if ( !other.d )
    return;
  d.reset( new ReosCoordinateSystemTransformer_impl );
  d->mTransformContext = other.d->mTransformContext;
}

ReosCoordinateSystemTransformer &ReosCoordinateSystemTransformer::operator=( const ReosCoordinateSystemTransformer &rhs )
{
  d.reset();
  if ( rhs.d )
  {
    d.reset( new ReosCoordinateSystemTransformer_impl );
    d->mTransformContext = rhs.d->mTransformContext;
  }
  return *this;
}

ReosCoordinateSystemTransformer::~ReosCoordinateSystemTransformer() = default;

QPolygonF ReosCoordinateSystemTransformer::transformToCoordinates( const QString &sourceCRS, const QPolygonF &sourcePolygon, const QString &destinationCrs ) const
{
  if ( !d )
    return sourcePolygon;

  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( sourceCRS ),
                                    QgsCoordinateReferenceSystem::fromWkt( destinationCrs ),
                                    d->mTransformContext );

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

ReosCoordinateSystemTransformer::ReosCoordinateSystemTransformer()
{}

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

  //! init the QGIS network manager to access remote GIS data
  QgsApplication::authManager()->init( qgisProviderPath, QgsApplication::qgisAuthDatabaseFilePath() );
  QgsAuthMethodRegistry::instance( qgisProviderPath );
  QgsNetworkAccessManager::instance();

  mAbstractLayerTreeModel = new QgsLayerTreeModel( QgsProject::instance()->layerTreeRoot(), this );

  //! Add reos data provider to Qgis instances
  QgsProviderRegistry::instance()->registerProvider( new ReosMeshProviderMetaData() );
  QgsProviderRegistry::instance()->registerProvider( new ReosGriddedRainfallProviderMetaData() );

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

QString ReosGisEngine::crsFromEPSG( int epsgCode )
{
  return QgsCoordinateReferenceSystem::fromEpsgId( epsgCode ).toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}

QString ReosGisEngine::crsFromProj( const QString &projtring )
{
  return QgsCoordinateReferenceSystem::fromProj( projtring ).toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );;
}

QString ReosGisEngine::crsWkt1( const QString &crs )
{
  QgsCoordinateReferenceSystem crs_( crs );
  return crs_.toWkt();
}

QString ReosGisEngine::crsEsriWkt( const QString &crs )
{
  return QgsCoordinateReferenceSystem( crs ).toWkt( QgsCoordinateReferenceSystem::WKT1_ESRI );
}

void ReosGisEngine::setCrs( const QString &crsString )
{
  QgsCoordinateReferenceSystem crs( crsString );
  QgsProject::instance()->setCrs( crs );
  emit crsChanged( crs.toWkt() );
}

bool ReosGisEngine::crsIsValid( const QString &crsString )
{
  QgsCoordinateReferenceSystem qgsCrs( crsString );
  return qgsCrs.isValid();
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


ReosDigitalElevationModel *ReosGisEngine::getRasterDigitalElevationModel( const QString &uri )
{
  QgsCoordinateTransformContext transformContext = QgsProject::instance()->transformContext();
  return ReosDigitalElevationModelFactory::createDEMFromUri( uri, transformContext );
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
  QString effCrs = crs;
  if ( effCrs.isEmpty() )
    effCrs = this->crs();

  return polygonAreaWithCrs( polygon, effCrs );
}

ReosArea ReosGisEngine::polygonAreaWithCrs( const QPolygonF &polygon, const QString &crs )
{
  QgsCoordinateReferenceSystem qgsCrs;
  if ( crs.isEmpty() )
    return ReosArea();
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


ReosEncodedElement ReosGisEngine::encode( const QString &path, const QString &baseFileName )
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

bool ReosGisEngine::decode( const ReosEncodedElement &encodedElement, const QString &path, const QString &baseFileName )
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

ReosCoordinateSystemTransformer ReosGisEngine::getCoordinateTransformer() const
{
  ReosCoordinateSystemTransformer ret;
  ret.d.reset( new ReosCoordinateSystemTransformer_impl );
  ret.d->mTransformContext = QgsProject::instance()->transformContext();
  return ret;
}

ReosMapExtent ReosGisEngine::transformExtent( const ReosMapExtent &extent, const QString &crs )
{
  if ( extent.crs() == crs )
    return extent;

  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( extent.crs() ),
                                    QgsCoordinateReferenceSystem::fromWkt( crs ),
                                    QgsProject::instance()->transformContext() );

  QgsRectangle qgsExtent( extent.toRectF() );
  ReosMapExtent ret;

  try
  {
    QgsRectangle transformExtent = transform.transformBoundingBox( qgsExtent );
    ret = ReosMapExtent( transformExtent.toRectF() );
    ret.setCrs( crs );
  }
  catch ( ... )
  {
    ret = ReosMapExtent( qgsExtent.toRectF() );
  }

  return ret;
}

ReosMapExtent ReosGisEngine::transformToProjectExtent( const ReosMapExtent &extent ) const
{
  return transformExtent( extent, QgsProject::instance()->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED ) );
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

double ReosGisEngine::distance( const QPointF &point1, const QPointF &point2, const QString &pointCrs )
{
  QgsCoordinateReferenceSystem qgisCrs = QgsCoordinateReferenceSystem::fromWkt( pointCrs );
  QgsDistanceArea distanceArea;
  distanceArea.setSourceCrs( qgisCrs, QgsProject::instance()->transformContext() );

  QgsUnitTypes::DistanceUnit unit = distanceArea.lengthUnits();
  double factor = QgsUnitTypes::fromUnitToUnitFactor( unit, QgsUnitTypes::DistanceMeters );

  return factor * distanceArea.measureLine( point1, point2 );
}

double ReosGisEngine::locateOnPolyline( const QPointF &point, const QPolygonF &polyline, const QString &crs )
{
  QgsCoordinateReferenceSystem qgisCrs = QgsCoordinateReferenceSystem::fromWkt( crs );
  QgsDistanceArea distanceArea;
  distanceArea.setSourceCrs( qgisCrs, QgsProject::instance()->transformContext() );

  QgsUnitTypes::DistanceUnit unit = distanceArea.lengthUnits();
  double factor = QgsUnitTypes::fromUnitToUnitFactor( unit, QgsUnitTypes::DistanceMeters );

  QgsGeometry geomPoly = QgsGeometry::fromQPolygonF( polyline );

  QgsPointXY minDist;
  int nextVertex;
  geomPoly.closestSegmentWithContext( point, minDist, nextVertex );

  double distFromBegin = 0;
  for ( int i = 0; i < nextVertex - 1; ++i )
    distFromBegin += distanceArea.measureLine( polyline.at( i ), polyline.at( i + 1 ) );

  if ( nextVertex > 0 )
    distFromBegin += distanceArea.measureLine( polyline.at( nextVertex - 1 ), minDist );

  return distFromBegin * factor;
}

QPointF ReosGisEngine::setPointOnPolyline( double distance, const QPolygonF &polyline, const QString &crs, int &segmentVertex )
{
  if ( polyline.isEmpty() )
    return QPointF();

  if ( polyline.count() == 1 )
    return polyline.first();

  QgsCoordinateReferenceSystem qgisCrs = QgsCoordinateReferenceSystem::fromWkt( crs );
  QgsDistanceArea distanceArea;
  distanceArea.setSourceCrs( qgisCrs, QgsProject::instance()->transformContext() );

  QgsUnitTypes::DistanceUnit unit = distanceArea.lengthUnits();
  double factor = QgsUnitTypes::fromUnitToUnitFactor( unit, QgsUnitTypes::DistanceMeters );

  double distFromBegin = 0;
  int i = 0;
  do
  {
    const QPointF &p1 = polyline.at( i );
    const QPointF &p2 = polyline.at( i + 1 );
    distFromBegin += distanceArea.measureLine( p1, p2 ) * factor;
  }
  while ( distance >= distFromBegin && ++i < polyline.count() - 1 );

  if ( distance > distFromBegin )
  {
    segmentVertex = -1;
    return QPointF();
  }

  if ( i >= polyline.count() - 1 )
  {
    segmentVertex = -1;
    return QPointF();
  }

  segmentVertex = i;
  const QPointF &p1 = polyline.at( i );
  const QPointF &p2 = polyline.at( i + 1 );
  double segDist = distanceArea.measureLine( p1, p2 ) * factor;
  double distFromP2 = distFromBegin - distance;
  double ratio = distFromP2 / segDist;
  return p2 - ratio * ( p2 - p1 );
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

double ReosGisEngine::factorUnitToMeter( const QString &crs )
{
  QgsUnitTypes::DistanceUnit unit = QgsCoordinateReferenceSystem::fromWkt( crs ).mapUnits();

  return QgsUnitTypes::fromUnitToUnitFactor( unit, QgsUnitTypes::DistanceMeters );
}

ReosRasterMemory<QList<QPair<double, QPoint>>>  ReosGisEngine::transformRasterExtent(
  const ReosRasterExtent &extent,
  const ReosMapExtent &destination,
  double resolX,
  double resolY,
  ReosRasterExtent &resultingExtent,
  bool &success )
{
  ReosRasterMemory<QList<QPair<double, QPoint>>> ret;

  QgsCoordinateTransform transform( QgsCoordinateReferenceSystem::fromWkt( extent.crs() ),
                                    QgsCoordinateReferenceSystem::fromWkt( destination.crs() ),
                                    QgsProject::instance()->transformContext() );

  if ( transform.isValid() )
  {
    int xCount = extent.xCellCount();
    int yCount = extent.yCellCount();

    if ( xCount == 0 || yCount == 0 )
    {
      success = false;
      return ret;
    }

    QVector<QVector<QgsPointXY>> sourceExtentVerticesInDestination( xCount + 1, QVector < QgsPointXY>( yCount + 1 ) );
    QgsRectangle destExtent;

    double sourceDx = std::fabs( extent.xCellSize() );
    double sourceDy = std::fabs( extent.yCellSize() );

    destExtent.setMinimal();
    for ( int iy = 0; iy < yCount + 1; ++iy )
    {
      for ( int ix = 0; ix < xCount + 1 ; ++ix )
      {
        const QgsPointXY sourcePoint = QgsPointXY( extent.xMapMin() + ix * sourceDx, extent.yMapMax() - iy * sourceDy );
        QgsPointXY destPoint;
        try
        {
          destPoint = transform.transform( sourcePoint );
        }
        catch ( QgsCsException & )
        {
          success = false;
          return ret;
        }
        sourceExtentVerticesInDestination[ix][iy] = destPoint;
        destExtent.include( destPoint );
      }
    }

    int xDestCount = static_cast<int>( std::round( destination.width() / std::fabs( resolX ) ) ); // + 1;
    int yDestCount =  static_cast<int>( std::round( destination.height() / std::fabs( resolY ) ) ); // + 1;

    ret.reserveMemory( yDestCount, xDestCount );

    double effWidth = xDestCount * std::fabs( resolX );
    double effHeight = yDestCount * std::fabs( resolY );

    double xOri = resolX > 0 ? destination.xMapMin() - ( effWidth - destination.width() ) / 2 : destination.xMapMax() + ( effWidth - destination.width() ) / 2;
    double yOri =  resolY > 0 ? destination.yMapMin() - ( effHeight - destination.height() ) / 2 : destination.yMapMax() + ( effHeight - destination.height() ) / 2;
    //double xMax = xMin + effWidth;
    //double yMax = yMin + effHeight;

    resultingExtent = ReosRasterExtent( xOri, yOri, xDestCount, yDestCount, resolX, resolY );
    resultingExtent.setCrs( destination.crs() );
    QgsSpatialIndex spatialIndex;
    QgsFeatureId id = 0;
    for ( int iy = 0; iy < yCount; ++iy )
    {
      for ( int ix = 0; ix < xCount ; ++ix )
      {
        QgsRectangle cellBB;
        cellBB.setMinimal();
        cellBB.include( sourceExtentVerticesInDestination.at( ix ).at( iy ) );
        cellBB.include( sourceExtentVerticesInDestination.at( ix + 1 ).at( iy ) );
        cellBB.include( sourceExtentVerticesInDestination.at( ix + 1 ).at( iy + 1 ) );
        cellBB.include( sourceExtentVerticesInDestination.at( ix ).at( iy + 1 ) );
        spatialIndex.addFeature( id, cellBB );
        id++;
      }
    }

    for ( int idy = 0; idy < yDestCount; ++idy )
    {
      for ( int idx = 0; idx < xDestCount; ++idx )
      {
        QgsRectangle cell( idx * resolX + xOri, ( idy + 1 ) * resolY + yOri,
                           ( idx + 1 ) * resolX + xOri, idy * resolY + yOri, true );
        QList<QgsFeatureId> sourceInter = spatialIndex.intersects( cell );

        QList<QPair<double, QPoint>> destIntersect;
        double areaSum = 0;

        for ( QgsFeatureId id : sourceInter )
        {
          int destRow = id /  xCount;
          int destCol = id - destRow * ( xCount ) ;
          QgsPolygonXY sourceCell;
          QgsPolylineXY outer;
          outer << sourceExtentVerticesInDestination.at( destCol ).at( destRow );
          outer << sourceExtentVerticesInDestination.at( destCol ).at( destRow + 1 );
          outer << sourceExtentVerticesInDestination.at( destCol + 1 ).at( destRow + 1 );
          outer << sourceExtentVerticesInDestination.at( destCol + 1 ).at( destRow );
          sourceCell << outer;
          QgsGeometry sourceCellGeom = QgsGeometry::fromPolygonXY( sourceCell );

          QgsGeometry intersect = sourceCellGeom.clipped( cell );
          if ( !intersect.isNull() )
          {
            double area = intersect.area();
            Q_ASSERT( destRow < yCount && destCol < xCount );
            destIntersect.append( {area, QPoint( destCol, destRow )} );
            areaSum += area;
          }
        }

        if ( areaSum > 0 )
        {
          for ( int i = 0; i < destIntersect.count(); ++i )
          {
            destIntersect[i] = {destIntersect.at( i ).first / areaSum, destIntersect.at( i ).second};
          }

          ret.setValue( idy, idx, destIntersect );
        }
      }
    }
  }

  return ret;
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

ReosTimeWindow ReosGisEngine::mapTimeWindow() const
{
  return ReosTimeWindow( temporalRange() );
}

void ReosGisEngine::setMapTimeWindow( const ReosTimeWindow &timeWindow )
{
  setTemporalRange( timeWindow.start(), timeWindow.end() );
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
  const QMap<QString, ReosEncodedElement> &vectorSymbologies,
  const ReosDuration &timeStep )
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

  QgsDateTimeRange timeExtent = static_cast<QgsMeshLayerTemporalProperties *>( meshLayer->temporalProperties() )->timeExtent();
  timeExtent = QgsDateTimeRange( timeExtent.begin(), timeExtent.end().addMSecs( 1000 ) );
  project->timeSettings()->setTemporalRange( timeExtent );

  project->addMapLayer( meshLayer.release() );

  switch ( timeStep.unit() )
  {
    case ReosDuration::millisecond:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalMilliseconds );
      break;
    case ReosDuration::second:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalSeconds );
      break;
    case ReosDuration::minute:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalMinutes );
      break;
    case ReosDuration::hour:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalHours );
      break;
    case ReosDuration::day:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalDays );
      break;
    case ReosDuration::week:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalWeeks );
      break;
    case ReosDuration::month:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalMonths );
      break;
    case ReosDuration::year:
      project->timeSettings()->setTimeStepUnit( QgsUnitTypes::TemporalYears );
      break;
  };
  project->timeSettings()->setTimeStep( timeStep.valueUnit() );

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

ReosGisEngine::LayerType ReosGisEngine::layerType( const QString &layerId ) const
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

QString ReosGisEngine::layerName( const QString &layerId ) const
{
  QgsMapLayer *layer = QgsProject::instance()->mapLayer( layerId );

  if ( !layer )
    return QString();

  return layer->name();
}

bool ReosGisEngine::hasValidLayer( const QString &layerId ) const
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
