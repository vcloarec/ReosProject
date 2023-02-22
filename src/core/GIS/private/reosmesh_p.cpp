/***************************************************************************
  reosmesh_p.cpp - ReosMesh_p

 ---------------------
 begin                : 13.1.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmesh_p.h"

#include <qgsproject.h>
#include <qgsmesheditor.h>
#include <qgsmeshlayerrenderer.h>
#include <qgsmapcanvas.h>
#include <qgsrendercontext.h>
#include <qgsproviderregistry.h>
#include <qgsmeshlayertemporalproperties.h>
#include <qgsmeshlayer3drenderer.h>
#include <qgsgeometryengine.h>
#include <qgsgeometryutils.h>
#include <qgsunittypes.h>

#include "reosmeshdataprovider_p.h"
#include "reosparameter.h"
#include "reosencodedelement.h"
#include "reosmapextent.h"
#include "reostopographycollection.h"
#include "reostopographycollection_p.h"
#include "reosgisengine.h"
#include "reosrenderedobject.h"
#include "reosrenderersettings_p.h"
#include "reosmeshrenderer_p.h"

static QgsMeshRendererScalarSettings getScalarSettingsFromEncoded( const ReosEncodedElement &elem )
{
  if ( elem.description() == QStringLiteral( "dataset-symbology" ) )
  {
    QString docString;
    elem.getData( QStringLiteral( "symbology" ), docString );

    QDomDocument doc( QStringLiteral( "dataset-symbology" ) );

    if ( doc.setContent( docString ) )
    {
      QDomElement domElem = doc.firstChildElement( QStringLiteral( "scalar-settings" ) );
      QgsReadWriteContext context;
      QgsMeshRendererScalarSettings scalarSettings;
      scalarSettings.readXml( domElem );

      return scalarSettings;
    }
  }

  return QgsMeshRendererScalarSettings();
}

static ReosEncodedElement encodedFromScalarSettings( const QgsMeshRendererScalarSettings &scalarSettings )
{
  QDomDocument doc( QStringLiteral( "dataset-symbology" ) );
  doc.appendChild( scalarSettings.writeXml( doc ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "dataset-symbology" ) );
  QString docString = doc.toString();
  encodedElem.addData( QStringLiteral( "symbology" ), docString );

  return encodedElem;
}


static QgsMeshRendererVectorSettings getVectorSettingsFromEncoded( const ReosEncodedElement &elem )
{
  QString docString;
  elem.getData( QStringLiteral( "symbology" ), docString );
  QDomDocument docFrom( QStringLiteral( "dataset-vector-symbology" ) );

  QgsMeshRendererVectorSettings vectorSettings;
  if ( docFrom.setContent( docString ) )
  {
    QDomElement domElem = docFrom.firstChildElement( QStringLiteral( "vector-settings" ) );
    QgsReadWriteContext context;
    vectorSettings.readXml( domElem );
    return vectorSettings;
  }

  return QgsMeshRendererVectorSettings();
}

static ReosEncodedElement encodedFromVectorSettings( const QgsMeshRendererVectorSettings &vectorSettings )
{
  QDomDocument docTo( QStringLiteral( "dataset-vector-symbology" ) );
  docTo.appendChild( vectorSettings.writeXml( docTo ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "dataset-vector-symbology" ) );
  QString docStringTo = docTo.toString();
  encodedElem.addData( QStringLiteral( "symbology" ), docStringTo );
  return encodedElem;
}

ReosMeshFrame_p::ReosMeshFrame_p( const QString &crs, QObject *parent ): ReosMesh( parent )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );
  QgsCoordinateReferenceSystem qgisCrs;
  qgisCrs.createFromWkt( crs );
  mMeshLayer->setCrs( qgisCrs );
  mMeshDataProvider = qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
  mMeshDataProvider->overrideCrs( qgisCrs );
  mMeshLayer->updateTriangularMesh();
  init();
}

ReosMeshFrame_p::ReosMeshFrame_p( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );
  mMeshDataProvider = qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
  resetMeshFrameFromeFile( dataPath, destinationCrs, message );
  init();
}

ReosMeshFrame_p::~ReosMeshFrame_p() = default;


void ReosMeshFrame_p::resetMeshFrameFromeFile( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message )
{
  mMeshDataProvider->loadMeshFrame( dataPath, message );

  QgsCoordinateReferenceSystem qgisDestinationCrs;
  qgisDestinationCrs.createFromWkt( destinationCrs );

  if ( mMeshDataProvider->crs().isValid() )
    mMeshLayer->setCrs( mMeshDataProvider->crs() );
  else
  {
    mMeshLayer->setCrs( qgisDestinationCrs );
    mMeshDataProvider->overrideCrs( qgisDestinationCrs );
  }

  mMeshLayer->reload();

  QgsCoordinateTransform transform( mMeshDataProvider->crs(), qgisDestinationCrs, QgsProject::instance() );
  mMeshLayer->updateTriangularMesh( transform );

  activateDataset( mCurrentScalarDatasetId );
}

void ReosMeshFrame_p::save( const QString &dataPath )
{
  QDir dir( dataPath );

  bool isEditable = mMeshLayer->isEditable();

  if ( isEditable )
    stopFrameEditing( true, true );

  mMeshDataProvider->setFilePath( dir.filePath( QStringLiteral( "meshFrame.nc" ) ) );
  mMeshDataProvider->saveMeshFrameToFile( *mMeshLayer->nativeMesh() );
}

void ReosMeshFrame_p::init()
{
  mScalarShaderSettings.reset( new ReosMeshScalarColorShaderSettings_p( this ) );
  mTerrainShaderSettings.reset( new ReosMeshTerrainColorShaderSettings_p( this ) );
  mVectorShaderSettings.reset( new ReosMeshVectorColorShaderSettings_p( this ) );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererMeshSettings meshSettings = settings.nativeMeshSettings();
  meshSettings.setLineWidth( 0.1 );
  meshSettings.setColor( Qt::gray );
  settings.setNativeMeshSettings( meshSettings );
  mMeshLayer->setRendererSettings( settings );
  qobject_cast<QgsMeshLayerTemporalProperties *>( mMeshLayer->temporalProperties() )->setAlwaysLoadReferenceTimeFromSource( true );

  connect( mMeshLayer.get(), &QgsMapLayer::repaintRequested, this, &ReosMesh::repaintRequested );
  connect( mMeshLayer.get(), &QgsMeshLayer::layerModified, this, &ReosDataObject::dataChanged );
  connect( mTerrainShaderSettings.get(), &ReosMeshTerrainColorShaderSettings_p::meshTerrainSettingsChanged, this, &ReosMesh::terrainSymbologyChanged );
}

void ReosMeshFrame_p::stopFrameEditing( bool commit, bool continueEditing )
{
  int activeScalarDatasetIndex = mMeshLayer->rendererSettings().activeScalarDatasetGroup();
  QString activeGroupId;

  for ( auto it = mDatasetGroupsIndex.begin(); it != mDatasetGroupsIndex.end(); ++it )
    if ( it.value() == activeScalarDatasetIndex )
      activeGroupId = it.key();

  if ( mMeshLayer->isEditable() )
  {

    QgsCoordinateTransform transform( mMeshLayer->crs(), QgsProject::instance()->crs(), QgsProject::instance() );
    if ( commit )
      mMeshLayer->commitFrameEditing( transform, continueEditing );
    else
      mMeshLayer->rollBackFrameEditing( transform, continueEditing );

    if ( !continueEditing )
      restoreVertexElevationDataset();
  }

  activateDataset( activeGroupId );
}

ReosColorShaderSettings *ReosMeshFrame_p::scalarColorShaderSettings() const
{
  if ( mCurrentScalarDatasetId == mVerticesElevationDatasetId )
    return mTerrainShaderSettings.get();
  else
    return mScalarShaderSettings.get();
}

ReosColorShaderSettings *ReosMeshFrame_p::vectorColorShaderSettings() const
{
  return mVectorShaderSettings.get();
}

ReosColorShaderSettings *ReosMeshFrame_p::terrainColorShaderSettings() const
{
  return mTerrainShaderSettings.get();
}

ReosEncodedElement ReosMeshFrame_p::restoreScalarSymbologyOnMeshDatasetGroup( const QString &id )
{
  ReosEncodedElement symbology;
  if ( mDatasetScalarSymbologies.contains( id ) )
    symbology = ReosEncodedElement( mDatasetScalarSymbologies.value( id ) );
  else if ( !id.isEmpty() )
  {
    symbology = datasetGroupScalarSymbologyfromLayer( id );
    mDatasetScalarSymbologies.insert( id, symbology.bytes() );
  }

  QgsMeshRendererScalarSettings settings = getScalarSettingsFromEncoded( symbology );
  applySymbologyOnScalarDataSet( id, settings );

  return symbology;
}

void ReosMeshFrame_p::applySymbologyOnScalarDataSet( const QString &id, QgsMeshRendererScalarSettings scalarSettings )
{
  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  int groupIndex = mDatasetGroupsIndex.value( id, -1 );
  settings.setScalarSettings( groupIndex, scalarSettings );
  mMeshLayer->setRendererSettings( settings );
  mRendererCache.reset();

  if ( id == mVerticesElevationDatasetId )
    emit terrainSymbologyChanged();
}

ReosEncodedElement ReosMeshFrame_p::restoreVectorSymbologyOnMeshDatasetGroup( const QString &id )
{
  ReosEncodedElement symbology;
  if ( mDatasetVectorSymbologies.contains( id ) )
    symbology = ReosEncodedElement( mDatasetVectorSymbologies.value( id ) );
  else if ( !id.isEmpty() )
  {
    symbology = datasetGroupVectorSymbologyfromLayer( id );
    mDatasetScalarSymbologies.insert( id, symbology.bytes() );
  }

  QgsMeshRendererVectorSettings settings = getVectorSettingsFromEncoded( symbology );
  applySymbologyOnVectorDataSet( id, settings );

  return symbology;
}

void ReosMeshFrame_p::applySymbologyOnVectorDataSet( const QString &id, QgsMeshRendererVectorSettings vectorSettings )
{
  mRendererCache.reset();
  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  settings.setVectorSettings( mDatasetGroupsIndex.value( id, -1 ), vectorSettings );
  mMeshLayer->setRendererSettings( settings );
}

ReosEncodedElement ReosMeshFrame_p::datasetScalarGroupSymbology( const QString &id ) const
{
  if ( !mDatasetScalarSymbologies.contains( id ) )
  {
    return datasetGroupScalarSymbologyfromLayer( id ); //if not exist, the layer will returns the default symbology
  }

  return ReosEncodedElement( mDatasetScalarSymbologies.value( id ) );
}

ReosEncodedElement ReosMeshFrame_p::datasetVectorGroupSymbology( const QString &id ) const
{
  if ( !mDatasetVectorSymbologies.contains( id ) )
  {
    return datasetGroupVectorSymbologyfromLayer( id ); //if not exist, the layer will returns the default symbology
  }

  return ReosEncodedElement( mDatasetVectorSymbologies.value( id ) );
}

void ReosMeshFrame_p::activateDynamicTraces( bool activated )
{
  mTraceIsActive = activated;
  ReosEncodedElement symbology = datasetVectorGroupSymbology( mCurrentActiveVectorDatasetId );
  symbology.addData( QStringLiteral( "dynamic-traces" ), activated );
  mRendererCache.reset();
  setDatasetVectorGroupSymbology( symbology, mCurrentActiveVectorDatasetId );
}

bool ReosMeshFrame_p::isDynamicTracesActive() const
{
  if ( mCurrentActiveVectorDatasetId.isEmpty() )
    return false;

  ReosEncodedElement symbology = datasetVectorGroupSymbology( mCurrentActiveVectorDatasetId );
  bool activated = false;
  symbology.getData( QStringLiteral( "dynamic-traces" ), activated );
  return activated;
}

void ReosMeshFrame_p::setDatasetVectorGroupSymbology( const ReosEncodedElement &encodedElement, const QString &id )
{
  if ( encodedElement.description() != QStringLiteral( "dataset-vector-symbology" ) )
    return;

  mDatasetVectorSymbologies.insert( id, encodedElement.bytes() );
  restoreVectorSymbologyOnMeshDatasetGroup( id );

  if ( id == mCurrentActiveVectorDatasetId )
  {
    QgsMeshRendererSettings qgsSettings = mMeshLayer->rendererSettings();
    if ( mTraceIsActive )
      qgsSettings.setActiveVectorDatasetGroup( -1 );
    else
      qgsSettings.setActiveVectorDatasetGroup( mDatasetGroupsIndex.value( id ) );

    mMeshLayer->setRendererSettings( qgsSettings );
  }

  update3DRenderer();
}

void ReosMeshFrame_p::activateWireFrame( bool activate )
{
  mWireFrameSettings.enabled = activate;
  updateWireFrameSettings( true );
}

bool ReosMeshFrame_p::isWireFrameActive() const
{
  return mMeshLayer->rendererSettings().nativeMeshSettings().isEnabled();
}

ReosEncodedElement ReosMeshFrame_p::wireFrameSymbology() const
{
  const QgsMeshRendererMeshSettings &meshSettings = mMeshLayer->rendererSettings().nativeMeshSettings();

  QDomDocument doc( QStringLiteral( "frame-symbology" ) );
  doc.appendChild( meshSettings.writeXml( doc ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "frame-symbology" ) );
  QString docString = doc.toString();
  encodedElem.addData( QStringLiteral( "frame-symbology" ), docString );
  return encodedElem;
}

ReosRendererObjectMapTimeStamp *ReosMeshFrame_p::createMapTimeStamp( ReosRendererSettings *settings ) const
{
  QDateTime rendererTime = settings->mapTime();
  QgsDateTimeRange timeRange( rendererTime, rendererTime.addSecs( 1 ) );

  int scalarGroupIndex = mDatasetGroupsIndex.value( mCurrentScalarDatasetId );
  const QgsMeshDatasetIndex scalarIndex = mMeshLayer->datasetIndexAtTime( timeRange, scalarGroupIndex );
  int vectorGroupIndex = mDatasetGroupsIndex.value( mCurrentActiveVectorDatasetId );
  const QgsMeshDatasetIndex vectorIndex = mMeshLayer->datasetIndexAtTime( timeRange, vectorGroupIndex );

  return new ReosRendererMeshMapTimeStamp_p(
           scalarIndex, vectorIndex, mRendererCache ? mRendererCache->tracesAges() : 0 );
}

ReosObjectRenderer *ReosMeshFrame_p::createRenderer( ReosRendererSettings *settings )
{
  if ( !mRendererCache && mTraceIsActive )
  {
    mRendererCache.reset(
      new ReosMeshRendererCache_p( this, mDatasetGroupsIndex.value( mCurrentActiveVectorDatasetId ) ) );
  }

  return new ReosMeshRenderer_p( settings, mMeshLayer.get(), this );
}

void ReosMeshFrame_p::updateInternalCache( ReosObjectRenderer *renderer )
{
  if ( mRendererCache )
    mRendererCache->updateInternalCache( qobject_cast<ReosMeshRenderer_p *>( renderer ) );
}

ReosMeshQualityChecker *ReosMeshFrame_p::getQualityChecker( ReosMesh::QualityMeshChecks qualitiChecks, const QString &destinatonCrs ) const
{
  QgsDistanceArea distanceArea;
  distanceArea.setSourceCrs( mMeshLayer->crs(), QgsProject::instance()->transformContext() );
  QgsCoordinateReferenceSystem destCrs;
  destCrs.createFromWkt( destinatonCrs );
  QgsCoordinateTransform transform( mMeshLayer->crs(), destCrs, QgsProject::instance() );
  return new ReosMeshQualityChecker_p( *mMeshLayer->nativeMesh(), mQualityMeshParameters, distanceArea, qualitiChecks, transform );
}

bool ReosMeshFrame_p::isValid() const
{
  if ( mMeshLayer )
    return mMeshLayer->isValid();

  return false;
}

int ReosMeshFrame_p::vertexCount() const
{
  return mMeshDataProvider->vertexCount();
}

QPointF ReosMeshFrame_p::vertexPosition( int vertexIndex, const QString &destinationCrs ) const
{
  if ( destinationCrs.isEmpty() )
    return mMeshDataProvider->vertexPosition( vertexIndex );


  QgsCoordinateReferenceSystem crs;
  crs.createFromWkt( destinationCrs );
  QgsCoordinateTransform transform( mMeshDataProvider->crs(), crs, QgsProject::instance() );
  if ( transform.isValid() )
  {
    try
    {
      QgsPointXY pt( mMeshDataProvider->vertexPosition( vertexIndex ) );
      return transform.transform( pt ).toQPointF();
    }
    catch ( QgsCsException & )
    {
      return mMeshDataProvider->vertexPosition( vertexIndex );
    }
  }

  return mMeshDataProvider->vertexPosition( vertexIndex );
}

double ReosMeshFrame_p::vertexElevation( int vertexIndex ) const
{
  return mMeshDataProvider->vertexElevation( vertexIndex );
}

QVector<QVector<int> > ReosMeshFrame_p::faces() const
{
  return mMeshDataProvider->faces();
}

QVector<int> ReosMeshFrame_p::face( int faceIndex ) const
{
  return mMeshDataProvider->face( faceIndex );
}

int ReosMeshFrame_p::faceCount() const
{
  return mMeshDataProvider->faceCount();
}

static void lamTol( double &lam )
{
  const static double eps = 1e-6;
  if ( ( lam < 0.0 ) && ( lam > -eps ) )
  {
    lam = 0.0;
  }
}

static bool E3T_physicalToBarycentric( const QgsPointXY &pA, const QgsPointXY &pB, const QgsPointXY &pC, const QgsPointXY &pP,
                                       double &lam1, double &lam2, double &lam3 )
{
  // from QGIS: ./src/core/mesh/qgsmeshlayerutils.cpp
  // Compute vectors
  const double xa = pA.x();
  const double ya = pA.y();
  const double v0x = pC.x() - xa ;
  const double v0y = pC.y() - ya ;
  const double v1x = pB.x() - xa ;
  const double v1y = pB.y() - ya ;
  const double v2x = pP.x() - xa ;
  const double v2y = pP.y() - ya ;

  // Compute dot products
  const double dot00 = v0x * v0x + v0y * v0y;
  const double dot01 = v0x * v1x + v0y * v1y;
  const double dot02 = v0x * v2x + v0y * v2y;
  const double dot11 = v1x * v1x + v1y * v1y;
  const double dot12 = v1x * v2x + v1y * v2y;

  // Compute barycentric coordinates
  double invDenom =  dot00 * dot11 - dot01 * dot01;
  if ( invDenom == 0 )
    return false;
  invDenom = 1.0 / invDenom;
  lam1 = ( dot11 * dot02 - dot01 * dot12 ) * invDenom;
  lam2 = ( dot00 * dot12 - dot01 * dot02 ) * invDenom;
  lam3 = 1.0 - lam1 - lam2;

  // Apply some tolerance to lam so we can detect correctly border points
  lamTol( lam1 );
  lamTol( lam2 );
  lamTol( lam3 );

  // Return if POI is outside triangle
  if ( ( lam1 < 0 ) || ( lam2 < 0 ) || ( lam3 < 0 ) )
  {
    return false;
  }

  return true;
}

QList<ReosMeshPointValue> ReosMeshFrame_p::drapePolyline( const QPolygonF &polyline, double tolerance ) const
{
  QMap<double, ReosMeshPointValue> ret;
  QString localCrs = crs();

  const QgsTriangularMesh *triMesh = mMeshLayer->triangularMesh();
  const QgsMesh *nativeMesh = mMeshLayer->nativeMesh();
  if ( !triMesh )
    return ret.values();

  const QVector<QgsMeshVertex> vertices = triMesh->vertices();

  QgsGeometry polyGeom = QgsGeometry::fromQPolygonF( polyline );



  double lenghtFromStart = 0;

  for ( int i = 0; i < polyline.count() - 1; ++i )
  {
    const QgsPoint pt1( polyline.at( i ) );
    const QgsPoint pt2( polyline.at( i + 1 ) );

    QSet<int> intersectedVertex;

    QgsGeometry segmentGeom = QgsGeometry::fromPolylineXY( {QgsPointXY( pt1 ), QgsPointXY( pt2 )} );
    std::unique_ptr<QgsGeometryEngine> segmentEngine( QgsGeometry::createGeometryEngine( segmentGeom.constGet() ) );
    segmentEngine->prepareGeometry();
    const QList<int> faces = triMesh->faceIndexesForRectangle( segmentGeom.boundingBox() );
    const QList<int> nativeFaces = triMesh->nativeFaceIndexForRectangle( segmentGeom.boundingBox() );

    // Start to check distance from all vertices
    for ( int fi : faces )
    {
      const QgsMeshFace &face = triMesh->triangles().at( fi );
      int nativeface = triMesh->trianglesToNativeFaces().at( fi );
      for ( int vi : face )
      {
        if ( intersectedVertex.contains( vi ) )
          continue;

        const QgsMeshVertex vert = vertices.at( vi );
        double minX = 0;
        double minY = 0;
        if ( QgsGeometryUtils::sqrDistToLine( vert.x(), vert.y(), pt1.x(), pt1.y(), pt2.x(), pt2.y(), minX, minY, 0 ) < tolerance * tolerance )
        {
          double pointDistFromStart = lenghtFromStart + sqrt( pow( pt1.x() - minX, 2 ) + pow( pt1.y() - minY, 2 ) );
          intersectedVertex.insert( vi );
          ret.insert( pointDistFromStart, ReosMeshPointValue( new ReosMeshPointValueOnVertex( vi, nativeface, QPointF( minX, minY ) ) ) );
        }
      }
    }

    // Find intersection with edges
    QHash <QPair<int, int>, int> intersectEdges; //key is edge and value is the face index of the first face found
    for ( int fi : nativeFaces )
    {
      const QgsMeshFace &face = nativeMesh->face( fi );
      int faceSize = face.size();
      for ( int vfi = 0; vfi < faceSize; ++vfi )
      {
        int vi1 = face.at( vfi );
        int vi2 = face.at( ( vfi + 1 ) % faceSize );

        const QgsMeshVertex &vert1 = vertices.at( vi1 );
        const QgsMeshVertex &vert2 = vertices.at( vi2 );

        QgsPoint intersectPoint;
        bool isIntersect;
        if ( QgsGeometryUtils::segmentIntersection( vert1, vert2, pt1, pt2, intersectPoint, isIntersect, tolerance, false ) )
        {
          QPair<int, int> edge;
          if ( vi1 > vi2 )
            edge = {vi2, vi1};
          else
            edge = {vi1, vi2};

          auto it = intersectEdges.find( edge );
          if ( it != intersectEdges.end() )
          {
            double distTot = vert1.distance( vert2 );
            double dist = vertices.at( edge.first ).distance( intersectPoint );
            ret.insert( pt1.distance( intersectPoint ) + lenghtFromStart,
                        ReosMeshPointValue( new ReosMeshPointValueOnEdge( edge.first, edge.second, it.value(), fi, dist / distTot, intersectPoint.toQPointF() ) ) );
            intersectEdges.remove( edge );
          }
          else
          {
            intersectEdges.insert( edge, fi );
          }
        }
      }
    }
    //Threat remaining edged that does have only on face
    for ( auto it = intersectEdges.constBegin(); it != intersectEdges.constEnd(); ++it )
    {
      const QgsMeshVertex &vert1 = vertices.at( it.key().first );
      const QgsMeshVertex &vert2 = vertices.at( it.key().second );
      QgsPoint intersectPoint;
      bool isIntersect;
      QgsGeometryUtils::segmentIntersection( vert1, vert2, pt1, pt2, intersectPoint, isIntersect, tolerance, false );
      double distTot = vert1.distance( vert2 );
      double dist = vertices.at( it.key().first ).distance( intersectPoint );
      ret.insert( pt1.distance( intersectPoint ) + lenghtFromStart,
                  ReosMeshPointValue( new ReosMeshPointValueOnEdge( it.key().first,  it.key().second, it.value(), -1, dist / distTot, intersectPoint.toQPointF() ) ) );
    }

    // insert point value for the current vertex of the polyline
    auto faceFunct = [ & ]( const QgsPoint pt )
    {
      int includingFace = triMesh->faceIndexForPoint_v2( pt );
      if ( includingFace != -1 )
      {
        const QgsMeshFace &face = triMesh->triangles().at( includingFace );
        int nativeface = triMesh->trianglesToNativeFaces().at( includingFace );
        double lam1 = 0;
        double lam2 = 0;
        double lam3 = 0;
        E3T_physicalToBarycentric(
          vertices.at( face.at( 0 ) ), vertices.at( face.at( 1 ) ), vertices.at( face.at( 2 ) ), pt, lam1, lam2, lam3 );
        ret.insert( lenghtFromStart, ReosMeshPointValue( new ReosMeshPointValueOnFace( face.at( 0 ), face.at( 1 ), face.at( 2 ), nativeface,
                    lam1, lam2, lam3, pt.toQPointF() ) ) );
      }
    };

    faceFunct( pt1 );

    lenghtFromStart += sqrt( pow( pt1.x() - pt2.x(), 2 ) + pow( pt1.y() - pt2.y(), 2 ) );

    //If the last segment insert point value for the last vertex of the polyline
    if ( i == polyline.count() - 2 )
    {
      faceFunct( QgsPoint( polyline.last() ) );
    }
  }

  return ret.values();
}


void ReosMeshFrame_p::restoreVertexElevationDataset()
{
  std::unique_ptr<QgsMeshDatasetGroup> group( new QgsMeshVerticesElevationDatasetGroup( mVerticesElevationDatasetName, mMeshLayer->nativeMesh() ) );
  mZVerticesDatasetGroup = group.get();
  addDatasetGroup( group.release(), mVerticesElevationDatasetId );
}

void ReosMeshFrame_p::updateWireFrameSettings( bool updateRenderer )
{
  if ( !mMeshLayer )
    return;

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererMeshSettings wireframeSettings = settings.nativeMeshSettings();
  wireframeSettings.setEnabled( mWireFrameSettings.enabled );
  wireframeSettings.setColor( mWireFrameSettings.color );
  wireframeSettings.setLineWidth( mWireFrameSettings.width );
  settings.setNativeMeshSettings( wireframeSettings );
  mMeshLayer->setRendererSettings( settings );

  mRendererCache.reset();
  mMeshLayer->triggerRepaint();

  if ( updateRenderer )
    update3DRenderer();
}

QPointF ReosMeshFrame_p::tolayerCoordinates( const ReosSpatialPosition &position ) const
{
  QgsCoordinateReferenceSystem sourceCrs;
  sourceCrs.createFromWkt( position.crs() );

  const QgsCoordinateTransform transform( sourceCrs, mMeshDataProvider->crs(), QgsProject::instance() );
  QgsPointXY ret;
  if ( transform.isValid() )
  {
    try
    {
      ret = transform.transform( position.position() );
    }
    catch ( const QgsCsException & )
    {
      ret = position.position();
    }
  }
  else
  {
    ret = position.position();
  }

  return ret.toQPointF();
}

ReosEncodedElement ReosMeshFrame_p::datasetGroupScalarSymbologyfromLayer( const QString &datasetId ) const
{
  int dsgi = mDatasetGroupsIndex.value( datasetId, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( dsgi );
  QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( dsgi, -1 ) );
  if ( meta.dataType() == QgsMeshDatasetGroupMetadata::DataOnFaces )
    scalarSettings.setDataResamplingMethod( QgsMeshRendererScalarSettings::NeighbourAverage );

  return encodedFromScalarSettings( scalarSettings );
}

ReosEncodedElement ReosMeshFrame_p::datasetGroupVectorSymbologyfromLayer( const QString &datasetId ) const
{
  int dsgi = mDatasetGroupsIndex.value( datasetId, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererVectorSettings vectorSettings = settings.vectorSettings( dsgi );

  return encodedFromVectorSettings( vectorSettings );
}

void ReosMeshFrame_p::update3DRenderer()
{
  if ( !mMeshLayer )
    return;

  const QgsMeshRendererScalarSettings scalarSettings =
    mMeshLayer->rendererSettings().scalarSettings( mDatasetGroupsIndex.value( mCurrentScalarDatasetId, -1 ) );

  const QgsMeshRendererMeshSettings frameSettings = mMeshLayer->rendererSettings().nativeMeshSettings();

  std::unique_ptr<QgsMeshLayer3DRenderer> renderer;
  if ( mMeshLayer->renderer3D() )
    renderer.reset( static_cast<QgsMeshLayer3DRenderer *>( mMeshLayer->renderer3D()->clone() ) );

  int verticalIndex = mDatasetGroupsIndex.value( mVerticalDataset3DId, -1 );

  std::unique_ptr<QgsMesh3DSymbol> symbol;
  if ( !renderer )
    symbol.reset( new QgsMesh3DSymbol() );
  else
    symbol.reset( renderer->symbol()->clone() );

  symbol->setSmoothedTriangles( true );
  symbol->setWireframeEnabled( frameSettings.isEnabled() );
  symbol->setWireframeLineColor( frameSettings.color() );
  symbol->setWireframeLineWidth( frameSettings.lineWidth() * 2 );
  symbol->setLevelOfDetailIndex( 0 );

  symbol->setVerticalScale( mVerticaleSCale );
  symbol->setRenderingStyle( static_cast<QgsMesh3DSymbol::RenderingStyle>( QgsMesh3DSymbol::ColorRamp ) );
  symbol->setSingleMeshColor( Qt::blue );
  symbol->setVerticalDatasetGroupIndex( verticalIndex );
  symbol->setIsVerticalMagnitudeRelative( false );

  if ( symbol->renderingStyle() == QgsMesh3DSymbol::ColorRamp )
  {
    QgsColorRampShader ramp = scalarSettings.colorRampShader();
    symbol->setColorRampShader( ramp );
  }

//  sym->setArrowsEnabled( mGroupBoxArrowsSettings->isChecked() );
//  sym->setArrowsSpacing( mArrowsSpacingSpinBox->value() );
//  sym->setArrowsFixedSize( mArrowsFixedSizeCheckBox->isChecked() );

  if ( !renderer )
    renderer.reset( new QgsMeshLayer3DRenderer( symbol.release() ) );
  else
    renderer->setSymbol( symbol.release() );

  mMeshLayer->setRenderer3D( renderer.release() );
}


ReosMesh::WireFrameSettings ReosMeshFrame_p::wireFrameSettings() const
{
  return mWireFrameSettings;
}

void ReosMeshFrame_p::setWireFrameSettings( const WireFrameSettings &wireFrameSettings, bool update )
{
  mWireFrameSettings = wireFrameSettings;
  updateWireFrameSettings( update );
}


//from QGIS src/core/mesh/qgsmeshlayerutils.cpp
static double interpolate( const QgsPointXY &pA, const QgsPointXY &pB, const QgsPointXY &pC, const QgsPointXY &pP, double vA, double vB, double vC, bool &ok )
{
  // Compute vectors
  const double xa = pA.x();
  const double ya = pA.y();
  const double v0x = pC.x() - xa ;
  const double v0y = pC.y() - ya ;
  const double v1x = pB.x() - xa ;
  const double v1y = pB.y() - ya ;
  const double v2x = pP.x() - xa ;
  const double v2y = pP.y() - ya ;

  // Compute dot products
  const double dot00 = v0x * v0x + v0y * v0y;
  const double dot01 = v0x * v1x + v0y * v1y;
  const double dot02 = v0x * v2x + v0y * v2y;
  const double dot11 = v1x * v1x + v1y * v1y;
  const double dot12 = v1x * v2x + v1y * v2y;

  // Compute barycentric coordinates
  double invDenom =  dot00 * dot11 - dot01 * dot01;
  if ( invDenom == 0 )
  {
    ok = false;
    return std::numeric_limits<double>::quiet_NaN();
  }
  invDenom = 1.0 / invDenom;
  double lam1 = ( dot11 * dot02 - dot01 * dot12 ) * invDenom;
  double lam2 = ( dot00 * dot12 - dot01 * dot02 ) * invDenom;
  double lam3 = 1.0 - lam1 - lam2;

  // Apply some tolerance to lam so we can detect correctly border points
  lamTol( lam1 );
  lamTol( lam2 );
  lamTol( lam3 );

  // Return if POI is outside triangle
  if ( ( lam1 < 0 ) || ( lam2 < 0 ) || ( lam3 < 0 ) )
  {
    ok = false;
    return std::numeric_limits<double>::quiet_NaN();
  }

  ok = true;
  return lam1 * vC + lam2 * vB + lam3 * vA;
}


double ReosMeshFrame_p::interpolateDatasetValueOnPoint(
  const ReosMeshDatasetSource *datasetSource,
  const ReosSpatialPosition &position,
  int sourceGroupindex,
  int datasetIndex ) const
{
  if ( datasetIndex < 0 )
    return std::numeric_limits<double>::quiet_NaN();

  const QVector<double> datasetValues = datasetSource->datasetValues( sourceGroupindex, datasetIndex );
  const QVector<int> facesActive = datasetSource->activeFaces( datasetIndex );
  bool isScalar = datasetSource->groupIsScalar( sourceGroupindex );

  QgsPointXY positionInLayer = QgsMeshVertex( tolayerCoordinates( position ) );
  QgsTriangularMesh *triangularMesh = mMeshLayer->triangularMesh();
  const QgsMeshVertex triVert = triangularMesh->nativeToTriangularCoordinates( QgsMeshVertex( positionInLayer ) );
  int faceIndex = triangularMesh->nativeFaceIndexForPoint( triVert );

  if ( faceIndex < 0 || faceIndex >= triangularMesh->triangles().count() )
    return std::numeric_limits<double>::quiet_NaN();

  if ( facesActive.at( faceIndex ) == 0 )
    return std::numeric_limits<double>::quiet_NaN();

  double result = std::numeric_limits<double>::quiet_NaN();

  if ( datasetSource->groupLocation( sourceGroupindex ) == ReosMeshDatasetSource::Location::Face )
  {
    Q_ASSERT( datasetValues.count() == mMeshLayer->meshFaceCount() * ( isScalar ? 1 : 2 ) );
    if ( isScalar )
      result = datasetValues.at( faceIndex );
    else
    {
      double valX = datasetValues.at( faceIndex * 2 );
      double valY = datasetValues.at( faceIndex * 2 + 1 );
      result = std::sqrt( std::pow( valX, 2 ) + std::pow( valY, 2 ) );
    }
  }
  else if ( datasetSource->groupLocation( sourceGroupindex ) == ReosMeshDatasetSource::Location::Vertex )
  {
    bool ok = false;
    Q_ASSERT( datasetValues.count() == mMeshLayer->meshVertexCount() * ( isScalar ? 1 : 2 ) );

    const QgsMeshFace &face = triangularMesh->triangles().at( faceIndex );
    const QgsMesh nativeMesh = *mMeshLayer->nativeMesh();

    int i0 = face.at( 0 );
    int i1 = face.at( 1 );
    int i2 = face.at( 2 );

    if ( isScalar )
      result = interpolate( nativeMesh.vertices.at( i0 ), nativeMesh.vertices.at( i1 ), nativeMesh.vertices.at( i2 ),
                            positionInLayer,
                            datasetValues.at( i0 ), datasetValues.at( i1 ), datasetValues.at( i2 ), ok );
    else
    {
      double v0x = datasetValues.at( 2 * i0 );
      double v0y = datasetValues.at( 2 * i0 + 1 );
      double v1x = datasetValues.at( 2 * i1 );
      double v1y = datasetValues.at( 2 * i1 + 1 );
      double v2x = datasetValues.at( 2 * i2 );
      double v2y = datasetValues.at( 2 * i2 + 1 );

      double resultX = interpolate( nativeMesh.vertices.at( i0 ), nativeMesh.vertices.at( i1 ), nativeMesh.vertices.at( i2 ),
                                    positionInLayer,
                                    v0x, v1x, v2x, ok );

      if ( !ok )
        return std::numeric_limits<double>::quiet_NaN();

      double resultY = interpolate( nativeMesh.vertices.at( i0 ), nativeMesh.vertices.at( i1 ), nativeMesh.vertices.at( i2 ),
                                    positionInLayer,
                                    v0y, v1y, v2y, ok );

      if ( !ok )
        return std::numeric_limits<double>::quiet_NaN();

      result = std::sqrt( std::pow( resultX, 2 ) + std::pow( resultY, 2 ) );
    }

    if ( !ok )
      return std::numeric_limits<double>::quiet_NaN();;
  }

  return result;
}

QString ReosMeshFrame_p::exportAsMesh( const QString &fileName, ReosModule::Message &message ) const
{
  QgsProviderMetadata *mdalMeta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );

  if ( !mdalMeta )
  {
    message.type = ReosModule::Error;
    message.addText( tr( "MDAL is not found. Unable to export the mesh. Verify your installation." ) );
    return QString();
  }

  QString effectiveFileName = fileName;

  QFileInfo fileInfo( effectiveFileName );
  if ( fileInfo.suffix() != QStringLiteral( "nc" ) )
  {
    effectiveFileName.append( QStringLiteral( ".nc" ) );
  }

  if ( mdalMeta->createMeshData( *mMeshLayer->nativeMesh(), effectiveFileName, QStringLiteral( "Ugrid" ), mMeshLayer->crs() ) )
    return effectiveFileName;
  else
  {
    message.type = ReosModule::Error;
    message.addText( tr( "Unable to create a new mesh file." ) );
    return QString();
  }
}

static int datasetGroupIndexFromName( QgsMeshLayer *mesh, const QString &groupName )
{
  const QList<int> groupIndexes = mesh->enabledDatasetGroupsIndexes();

  for ( int gi : groupIndexes )
  {
    QgsMeshDatasetGroupMetadata meta = mesh->datasetGroupMetadata( QgsMeshDatasetIndex( gi, 0 ) );
    if ( meta.name() == groupName )
      return gi;
  }

  return -1;
}

ReosModule::Message ReosMeshFrame_p::exportSimulationResults( ReosHydraulicSimulationResults *result, const QString &fileName ) const
{
  ReosModule::Message message;
  if ( !result )
  {
    message.type = ReosModule::Warning;
    message.text = tr( "No results present." );
    return message;
  }

  std::unique_ptr<QgsMeshLayer> exportedMesh = std::make_unique<QgsMeshLayer>( fileName, QStringLiteral( "mesh" ), QStringLiteral( "mdal" ) );

  int groupCount = result->groupCount();

  for ( int gi = 0; gi < groupCount; ++gi )
  {
    std::unique_ptr<ReosResultDatasetGroup > dsg = std::make_unique<ReosResultDatasetGroup>( result, gi, false );
    dsg->initialize();
    const QString groupName = dsg->name();

    bool fail = false;
    if ( exportedMesh->addDatasets( dsg.release() ) )
    {
      fail = exportedMesh->saveDataset( fileName, datasetGroupIndexFromName( exportedMesh.get(), groupName ), QStringLiteral( "Ugrid" ) );
    }

    if ( fail )
    {
      message.type = ReosModule::Warning;
      message.addText( tr( "Unable to add result \"%1\"." ).arg( groupName ) );
    }
  }

  return message;
}

QList<ReosColorShaderSettings *> ReosMeshFrame_p::colorShaderSettings() const
{
  QList<ReosColorShaderSettings *> ret;
  ret << mScalarShaderSettings.get();
  ret << mTerrainShaderSettings.get();

  return ret;
}

ReosMeshRendererCache_p *ReosMeshFrame_p::rendererCache() const
{
  return mRendererCache.get();
}

void ReosMeshFrame_p::renderingNeedUpdate()
{
  if ( mMeshLayer )
    emit mMeshLayer->request3DUpdate();

  emit repaintRequested();
}

QString ReosMeshFrame_p::enableVertexElevationDataset( const QString &name )
{
  mVerticesElevationDatasetName = name;
  restoreVertexElevationDataset();

  int index = mDatasetGroupsIndex.value( mVerticesElevationDatasetId, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( index );
  if ( scalarSettings.classificationMinimum() >= scalarSettings.classificationMaximum() )
  {
    QgsColorRampShader colorRamp = scalarSettings.colorRampShader();
    colorRamp.setMinimumValue( 0 );
    colorRamp.setMaximumValue( 0 );
    scalarSettings.setClassificationMinimumMaximum( 0, 0 );
    scalarSettings.setColorRampShader( colorRamp );
    ReosEncodedElement symbology = encodedFromScalarSettings( scalarSettings );
    mDatasetScalarSymbologies.insert( mVerticesElevationDatasetId, symbology.bytes() );
    applySymbologyOnScalarDataSet( mVerticesElevationDatasetId, scalarSettings );
  }

  return mVerticesElevationDatasetId;
}

bool ReosMeshFrame_p::isFrameModified() const
{
  return mMeshLayer->isModified();
}


void ReosMeshFrame_p::addDatasetGroup( QgsMeshDatasetGroup *group, const QString &id )
{
  QString name = group->name();

  mMeshLayer->addDatasets( group );
  QList<int> groupIndexes = mMeshLayer->datasetGroupsIndexes();
  int index = -1;
  for ( int i : groupIndexes )
  {
    QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( i ) );
    if ( meta.name() == name )
    {
      index = i;
      break;
    }
  }

  mDatasetGroupsIndex[id] = index;

  if ( !mDatasetScalarSymbologies.contains( id ) )
    mDatasetScalarSymbologies.insert( id, datasetGroupScalarSymbologyfromLayer( id ).bytes() );
}

void ReosMeshFrame_p::firstUpdateOfTerrainScalarSetting()
{
  if ( !mZVerticesDatasetGroup || !mDatasetGroupsIndex.contains( mVerticesElevationDatasetId ) )
    return;

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( mDatasetGroupsIndex.value( mVerticesElevationDatasetId, -1 ) );
  QgsColorRampShader colorRamp = scalarSettings.colorRampShader();

  if ( colorRamp.colorRampItemList().count() < 2 )
  {
    double min = mZVerticesDatasetGroup->minimum();
    double max = mZVerticesDatasetGroup->maximum();

    if ( min <= max )
    {
      colorRamp.setMinimumValue( min );
      colorRamp.setMaximumValue( max );
      colorRamp.classifyColorRamp( 10, -1 );
      scalarSettings.setClassificationMinimumMaximum( min, max );
      scalarSettings.setColorRampShader( colorRamp );
    }
  }

  ReosEncodedElement encodedElem = encodedFromScalarSettings( scalarSettings );
  mDatasetScalarSymbologies.insert( mVerticesElevationDatasetId, encodedElem.bytes() );
  //applySymbologyOnScalarDataSet( mVerticesElevationDatasetId, scalarSettings );
}

bool ReosMeshFrame_p::activateDataset( const QString &id, bool update )
{
  mCurrentScalarDatasetId = id;

  ReosEncodedElement symbology = restoreScalarSymbologyOnMeshDatasetGroup( id );

  if ( !id.isEmpty() )
  {
    if ( id == mVerticesElevationDatasetId )
    {
      mTerrainShaderSettings->setCurrentSymbology( symbology );
      mTerrainShaderSettings->setActive( true );
      mScalarShaderSettings->setActive( false );
    }
    else
    {
      mScalarShaderSettings->setCurrentSymbology( symbology );
      mTerrainShaderSettings->setActive( false );
      mScalarShaderSettings->setActive( true );
    }
  }

  int index = mDatasetGroupsIndex.value( id, -1 );
  if ( id == verticesElevationDatasetId() )
  {
    mMeshLayer->temporalProperties()->setIsActive( false );
    mMeshLayer->setStaticScalarDatasetIndex( QgsMeshDatasetIndex( index, 0 ) );
  }
  else
  {
    mMeshLayer->setStaticScalarDatasetIndex( - 1 );
    QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
    settings.setActiveScalarDatasetGroup( index );
    mMeshLayer->setRendererSettings( settings );
    mMeshLayer->temporalProperties()->setIsActive( true );
  }

  if ( update )
    update3DRenderer();

  return true;
}

bool ReosMeshFrame_p::activateVectorDataset( const QString &id, bool update )
{
  mCurrentActiveVectorDatasetId = id;

  ReosEncodedElement symbology = restoreVectorSymbologyOnMeshDatasetGroup( id );

  if ( !id.isEmpty() )
    mVectorShaderSettings->setCurrentSymbology( symbology );

  int index;
  bool dynamicTraces = false;
  if ( symbology.getData( QStringLiteral( "dynamic-traces" ), dynamicTraces ) )
    index = -1;
  else
    index = mDatasetGroupsIndex.value( id, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  settings.setActiveVectorDatasetGroup( index );
  mMeshLayer->setRendererSettings( settings );

  if ( update )
    update3DRenderer();

  return true;
}

QStringList ReosMeshFrame_p::datasetIds() const
{
  QMap<int, QString> mapRet;
  QStringList ids = mDatasetGroupsIndex.keys();
  QList<int> indexes = mMeshLayer->datasetGroupsIndexes();

  for ( const QString &id : ids )
  {
    int ind = mDatasetGroupsIndex.value( id, -1 );
    if ( indexes.contains( ind ) )
      mapRet.insert( mDatasetGroupsIndex.value( id, -1 ), id );
  }
  return mapRet.values();
}

QStringList ReosMeshFrame_p::vectorDatasetIds() const
{
  QMap<int, QString> mapRet;
  QStringList ids = mDatasetGroupsIndex.keys();
  QList<int> indexes = mMeshLayer->datasetGroupsIndexes();

  for ( const QString &id : ids )
  {
    int datasetGroupIndex =  mDatasetGroupsIndex.value( id, -1 );
    if ( !indexes.contains( datasetGroupIndex ) )
      continue;
    const QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( datasetGroupIndex, 0 ) );
    if ( meta.isVector() )
      mapRet.insert( datasetGroupIndex, id );
  }

  return mapRet.values();
}

QString ReosMeshFrame_p::datasetName( const QString &id ) const
{
  QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( mDatasetGroupsIndex.value( id, -1 ) );
  return meta.name();
}

bool ReosMeshFrame_p::hasDatasetGroupIndex( const QString &id ) const
{
  return mDatasetGroupsIndex.contains( id );
}

void ReosMeshFrame_p::generateMesh( const ReosMeshFrameData &data )
{
  if ( mMeshLayer->isEditable() )
    stopFrameEditing( false );

  setBoundariesVertices( data.boundaryVertices );
  setHolesVertices( data.holesVertices );

  mMeshDataProvider->generateMesh( data );
  mMeshLayer->reload();
  if ( mZVerticesDatasetGroup )
    mZVerticesDatasetGroup->setStatisticObsolete();

  renderingNeedUpdate();

  emit dataChanged();
}

QString ReosMeshFrame_p::crs() const
{
  return mMeshDataProvider->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
}

QObject *ReosMeshFrame_p::data() const
{
  return mMeshLayer.get();
}

ReosMeshData ReosMeshFrame_p::meshDataFrame() const
{
  return ReosMeshData( new ReosMeshData_( *mMeshLayer->nativeMesh() ) );
}

ReosMapExtent ReosMeshFrame_p::extent() const
{
  if ( mMeshLayer )
  {
    ReosMapExtent ret( mMeshLayer->extent().toRectF() );
    ret.setCrs( mMeshLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED ) );

    return ret;
  }

  return ReosMapExtent();
}

int ReosMeshFrame_p::datasetGroupIndex( const QString &id ) const
{
  return mDatasetGroupsIndex.value( id, -1 );
}

class ApplyTopopraphyProcess : public ReosProcess
{
  public:
    ApplyTopopraphyProcess( ReosTopographyCollection_p *topographyCollection, ReosMeshDataProvider_p *meshProvider )
      : mTopographyCollection( topographyCollection )
      , mProvider( meshProvider )
    {
      mTopographyCollection->prepare_p( meshProvider->crs() );
    }

    ~ApplyTopopraphyProcess()
    {
      mTopographyCollection->clean_p();
    }

    void start()
    {
      mProvider->applyTopographyOnVertices( mTopographyCollection, this );
      mIsSuccessful = true;
    }

  private:
    ReosTopographyCollection_p *mTopographyCollection = nullptr;
    ReosMeshDataProvider_p *mProvider = nullptr;
};

ReosProcess *ReosMeshFrame_p::applyTopographyOnVertices( ReosTopographyCollection *topographyCollection )
{
  if ( mMeshLayer->isEditable() )
    stopFrameEditing( true );

  std::unique_ptr<ReosProcess> process(
    new ApplyTopopraphyProcess( qobject_cast<ReosTopographyCollection_p *>( topographyCollection ), mMeshDataProvider ) );

  connect( process.get(), &ReosProcess::finished, this, [this, topographyCollection]
  {
    mMeshLayer->reload();
    QgsCoordinateTransform tranform( mMeshLayer->crs(), QgsCoordinateReferenceSystem::fromWkt( topographyCollection->gisEngine()->crs() ), QgsProject::instance()->transformContext() );
    mMeshLayer->updateTriangularMesh( tranform );
    if ( mZVerticesDatasetGroup )
      mZVerticesDatasetGroup->setStatisticObsolete();

    firstUpdateOfTerrainScalarSetting();

    renderingNeedUpdate();

    emit dataChanged();
  } );

  return process.release();
}

void ReosMeshFrame_p::applyDemOnVertices( ReosDigitalElevationModel *dem, const QString &destinationCrs )
{
  if ( mMeshLayer->isEditable() )
    stopFrameEditing( true );

  QgsCoordinateReferenceSystem qgisDestinationCrs;
  qgisDestinationCrs.createFromWkt( destinationCrs );

  mMeshDataProvider->applyDemOnVertices( dem );
  mMeshLayer->reload();
  QgsCoordinateTransform tranform( mMeshLayer->crs(), qgisDestinationCrs, QgsProject::instance()->transformContext() );
  mMeshLayer->updateTriangularMesh( tranform );
  if ( mZVerticesDatasetGroup )
    mZVerticesDatasetGroup->setStatisticObsolete();

  activateDataset( mCurrentScalarDatasetId );

  emit repaintRequested();
  mMeshLayer->trigger3DUpdate();
  emit dataChanged();
}

double ReosMeshFrame_p::datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const
{
  return mMeshLayer->datasetValue( QgsMeshDatasetIndex( datasetGroupIndex( datasetId ), 0 ), QgsPointXY( pos ) ).scalar();
}

void ReosMeshFrame_p::datasetGroupMinimumMaximum( const QString &datasetId, double &min, double &max ) const
{
  int groupIndex = datasetGroupIndex( datasetId );
  if ( groupIndex == -1 )
    return;

  const QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( groupIndex, 0 ) );
  min = meta.minimum();
  max = meta.maximum();
}

ReosMeshDataProvider_p *ReosMeshFrame_p::meshProvider() const
{
  return qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
}

QString ReosMeshFrame_p::currentdScalarDatasetId() const
{
  return mCurrentScalarDatasetId;
}

QString ReosMeshFrame_p::currentdVectorDatasetId() const
{
  return mCurrentActiveVectorDatasetId;
}

QString ReosMeshFrame_p::verticalDataset3DId() const
{
  return mVerticalDataset3DId;
}

void ReosMeshFrame_p::setVerticalDataset3DId( const QString &verticalDataset3DId, bool update )
{
  mVerticalDataset3DId = verticalDataset3DId;
  if ( update )
    update3DRenderer();
}

QString ReosMeshFrame_p::verticesElevationDatasetId() const
{
  return mVerticesElevationDatasetId;
}

void ReosMeshFrame_p::setSimulationResults( ReosHydraulicSimulationResults *result, const QString &destinationCrs )
{
  // Before addings results, we have to remove existing dataset groups from the mesh layer
  // But, for now,with QgsMeshLayer, we can only reload all the layer to update the dataset
  // That leads to 2 problems: first is is very costly, second, that clear the triangular mesh that we need to recreate.
  mMeshDataProvider->setDatasetSource( nullptr );
  mMeshLayer->reload();
  if ( result )
  {
    mMeshDataProvider->setDatasetSource( result );
    mMeshLayer->reload();
  }
  QgsCoordinateReferenceSystem qgsCrs = QgsCoordinateReferenceSystem::fromWkt( destinationCrs );
  QgsCoordinateTransform transform( mMeshLayer->crs(), qgsCrs, QgsProject::instance()->transformContext() );
  mMeshLayer->updateTriangularMesh( transform );

  const QStringList ids = mDatasetGroupsIndex.keys();

  for ( const QString &id : ids )
  {
    if ( id != mVerticesElevationDatasetId )
      mDatasetGroupsIndex.remove( id );
  }

  if ( result )
  {
    QList<int> groupIndexes = mMeshLayer->datasetGroupsIndexes();
    int index = -1;
    for ( int i = 0; i < result->groupCount(); ++i )
    {
      for ( int meshIndex : groupIndexes )
      {
        QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( QgsMeshDatasetIndex( meshIndex ) );
        if ( meta.name() == result->groupName( i ) )
        {
          index = meshIndex;
          break;
        }
      }
      const QString groupId = result->groupId( i );
      mDatasetGroupsIndex[groupId] = index;

      if ( !mDatasetScalarSymbologies.contains( groupId ) )
        mDatasetScalarSymbologies.insert( groupId, datasetGroupScalarSymbologyfromLayer( groupId ).bytes() );

      if ( !result->groupIsScalar( i ) )
      {
        if ( !mDatasetVectorSymbologies.contains( groupId ) )
          mDatasetVectorSymbologies.insert( groupId, datasetVectorGroupSymbology( groupId ).bytes() );
      }
    }
  }

  activateDataset( mCurrentScalarDatasetId, false );

  mMeshLayer->triggerRepaint();
  mMeshLayer->trigger3DUpdate();
}

ReosMeshQualityChecker_p::ReosMeshQualityChecker_p( const QgsMesh &mesh,
    ReosMesh::QualityMeshParameters params,
    const QgsDistanceArea &distanceArea,
    ReosMesh::QualityMeshChecks checks, const QgsCoordinateTransform &transform )
  : mMesh( mesh )
  , mMinimumAngle( params.minimumAngle->value() )
  , mMaximumAngle( params.maximumAngle->value() )
  , mConnectionCount( params.connectionCount->value() )
  , mConnectionCountBoundary( params.connectionCountBoundary->value() )
  , mMaximumSlope( params.maximumSlope->value() )
  , mMinimumArea( params.minimumArea->value().valueM2() )
  , mMaximumArea( params.maximumArea->value().valueM2() )
  , mMaximumAreaChange( params.maximumAreaChange->value() )
  , mDistanceArea( distanceArea )
  , mChecks( checks )
  , mTransform( transform )
{
}

static double ccwAngle( const QgsVector &v1, const QgsVector &v2 )
{
  return  std::fmod( v1.angle() / M_PI * 180 + 360.0 - v2.angle() / M_PI * 180, 360.0 );
}

void ReosMeshQualityChecker_p::start()
{
  mIsSuccessful = false;
  QgsTopologicalMesh topologicalMesh = QgsTopologicalMesh::createTopologicalMesh( &mMesh, 3, mError );
  if ( mError != QgsMeshEditingError() )
    return;

  double areaFactor = QgsUnitTypes::fromUnitToUnitFactor( mDistanceArea.areaUnits(), Qgis::AreaUnit::SquareMeters );
  double lenghtFactor = QgsUnitTypes::fromUnitToUnitFactor( mDistanceArea.lengthUnits(), Qgis::DistanceUnit::Meters );
  QSet<int> maxAreaChange;
  QSet<QPair<int, int>> maxSlope;
  QVector<char> facesChecked;
  if ( mChecks & ReosMesh::MaximumAreaChange )
    facesChecked.fill( 0, mMesh.faceCount() );

  setMaxProgression( mMesh.faceCount() );
  setCurrentProgression( 0 );
  setInformation( tr( "Check faces" ) );
  for ( int i = 0; i < mMesh.faceCount(); ++i )
  {
    const QgsMeshFace &face =  mMesh.face( i );
    int size = face.size();
    const QgsGeometry geom = QgsMeshUtils::toGeometry( face, mMesh.vertices );
    bool minAreaCheck = false;
    bool maxAreaCheck = false;
    bool minAngleCheck = false;
    bool maxAngleCheck = false;
    // area check
    if ( mChecks & ( ReosMesh::MinimumArea | ReosMesh::MaximumArea | ReosMesh::MaximumAreaChange ) )
    {
      double area = areaFactor * geom.area();
      if ( mChecks & ReosMesh::MinimumArea && area < mMinimumArea )
        minAreaCheck |= true;

      if ( mChecks & ReosMesh::MaximumArea && area > mMaximumArea )
        maxAreaCheck |= true;

      if ( mChecks & ReosMesh::MaximumAreaChange )
      {
        bool change = false;
        facesChecked[i] = 1;
        const QVector<int> &neighbors = topologicalMesh.neighborsOfFace( i );
        for ( int j = 0; j < neighbors.size(); ++j )
        {
          if ( neighbors.at( j ) == -1 ||
               ( facesChecked.at( i ) == 1 && facesChecked.at( neighbors.at( j ) ) == 1 ) )
            continue;

          const QgsGeometry neighborGeom = QgsMeshUtils::toGeometry( mMesh.face( neighbors.at( j ) ), mMesh.vertices );
          double neighborArea = areaFactor * neighborGeom.area();
          if ( fabs( neighborArea - area ) / area > mMaximumAreaChange )
          {
            change |= true;
            maxAreaChange.insert( neighbors.at( j ) );
          }
          facesChecked[neighbors.at( j )] = 1;
        }
        if ( change )
          maxAreaChange.insert( i );
      }
    }

    // angle and slope checks
    if ( mChecks & ( ReosMesh::MinimumAngle | ReosMesh::MaximumAngle | ReosMesh::MaximumSlope ) )
    {
      for ( int j = 0; j < size; ++j )
      {
        int iv1 = face.at( j );
        int iv2 = face.at( ( j + 1 ) % size );
        int iv3 =  face.at( ( j + 2 ) % size );
        const QgsPointXY p1 = mMesh.vertices.at( iv1 );
        const QgsPointXY p2 = mMesh.vertices.at( iv2 );
        const QgsPointXY p3 = mMesh.vertices.at( iv3 );
        const QgsVector v1 = p1 - p2;
        const QgsVector v2 = p3 - p2;

        double angle = ccwAngle( v1, v2 );
        if ( mChecks & ReosMesh::MinimumAngle )
          minAngleCheck |= angle < mMinimumAngle;
        if ( mChecks & ReosMesh::MaximumAngle )
          maxAngleCheck |= angle > mMaximumAngle;

        if ( mChecks & ReosMesh::MaximumSlope )
        {
          if ( maxSlope.contains( {iv1, iv2} ) || maxSlope.contains( {iv2, iv1} ) )
            continue;

          double dist = mDistanceArea.measureLine( {p1, p2} )*lenghtFactor;
          double slope = std::fabs( ( mMesh.vertices.at( iv1 ).z() - mMesh.vertices.at( iv2 ).z() ) / dist );
          if ( slope > mMaximumSlope )
          {
            QPointF pt1;
            QPointF pt2;
            if ( mTransform.isValid() )
            {
              try
              {
                pt1 = mTransform.transform( p1 ).toQPointF();
                pt2 = mTransform.transform( p2 ).toQPointF();
              }
              catch ( QgsCsException & )
              {
                pt1 = p1.toQPointF();
                pt2 = p2.toQPointF();
              }
            }
            else
            {
              pt1 = p1.toQPointF();
              pt2 = p2.toQPointF();
            }
            mResult.maximumSlope.append( QLineF( pt1, pt2 ) );
            maxSlope.insert( {iv1, iv2} );
          }
        }
      }
    }

    if ( minAreaCheck || maxAreaCheck || minAngleCheck || maxAngleCheck )
    {
      QgsGeometry geomT = geom;
      if ( mTransform.isValid() )
      {
        try
        {
          geomT.transform( mTransform );
        }
        catch ( QgsCsException & )
        {
          geomT = geom;
        }
      }

      QPolygonF poly = geomT.asQPolygonF();

      if ( minAreaCheck )
        mResult.minimumArea.append( poly );
      if ( maxAreaCheck )
        mResult.maximumArea.append( poly );
      if ( minAngleCheck )
        mResult.minimumAngle.append( poly );
      if ( maxAngleCheck )
        mResult.maximumAngle.append( poly );
    }

    if ( isStop() )
      return;
    setCurrentProgression( i );
  }

  for ( int i : maxAreaChange )
  {
    const QgsMeshFace &face =  mMesh.face( i );
    QgsGeometry geom = QgsMeshUtils::toGeometry( face, mMesh.vertices );
    if ( mTransform.isValid() )
    {
      try
      {
        geom.transform( mTransform );
      }
      catch ( QgsCsException & )
      {
        geom = QgsMeshUtils::toGeometry( face, mMesh.vertices );
      }
    }
    mResult.maximumAreaChange.append( geom.asQPolygonF() );

    if ( isStop() )
      return;
  }

  setInformation( tr( "Check vertices" ) );
  setMaxProgression( mMesh.vertices.count() );
  setCurrentProgression( 0 );
  if ( mChecks & ( ReosMesh::ConnectionCount | ReosMesh::ConnectionCountBoundary ) )
  {
    for ( int i = 0; i < mMesh.vertexCount(); ++i )
    {
      bool connCheck = false;
      bool connBoundCheck = false;
      QgsMeshVertexCirculator circulator = topologicalMesh.vertexCirculator( i );

      if ( ( mChecks & ( ReosMesh::ConnectionCount ) ) &&
           circulator.degree() > mConnectionCount )
        connCheck = true;

      if ( ( mChecks & ( ReosMesh::ConnectionCountBoundary ) ) &&
           topologicalMesh.isVertexOnBoundary( i ) )
      {
        if ( circulator.degree() > mConnectionCountBoundary )
          connBoundCheck = true;
      }

      if ( connCheck || connBoundCheck )
      {
        QPointF pt;
        if ( mTransform.isValid() )
        {
          try
          {
            pt = mTransform.transform( QgsPointXY( mMesh.vertex( i ) ) ).toQPointF();
          }
          catch ( QgsCsException & )
          {
            pt = mMesh.vertex( i ).toQPointF();
          }
        }
        else
        {
          pt = mMesh.vertex( i ).toQPointF();
        }

        if ( connCheck )
          mResult.connectionCount.append( pt );
        if ( connBoundCheck )
          mResult.connectionCountBoundary.append( pt );
      }

      setCurrentProgression( i );
    }
  }

  mIsSuccessful = true;

}

ReosMeshQualityChecker::QualityMeshResults ReosMeshQualityChecker_p::result() const
{
  if ( mError != QgsMeshEditingError() )
  {
    switch ( mError.errorType )
    {
      case Qgis::MeshEditingErrorType::NoError:
        break;
      case Qgis::MeshEditingErrorType::InvalidFace:
        mResult.error = QObject::tr( "Invalid face" );
        mResult.errorFace = mError.elementIndex;
        break;
      case Qgis::MeshEditingErrorType::TooManyVerticesInFace:
        mResult.error = QObject::tr( "Too many vertices" );
        mResult.errorFace = mError.elementIndex;
        break;
      case Qgis::MeshEditingErrorType::FlatFace:
        mResult.error = QObject::tr( "Flat face" );
        mResult.errorFace = mError.elementIndex;
        break;
      case Qgis::MeshEditingErrorType::UniqueSharedVertex:
        mResult.error = QObject::tr( "Unique shared vertex" );
        mResult.errorVertex = mError.elementIndex;
        break;
      case Qgis::MeshEditingErrorType::InvalidVertex:
        mResult.error = QObject::tr( "Invalid vertex" );
        mResult.errorVertex = mError.elementIndex;
        break;
      case Qgis::MeshEditingErrorType::ManifoldFace:
        mResult.error = QObject::tr( "Manifold face" );
        mResult.errorFace = mError.elementIndex;
        break;
    }
  }

  return mResult;
}


ReosResultDataset::ReosResultDataset( ReosMeshDatasetSource *simResult, int groupIndex, int index, bool faceSupportActiveFlag )
  : mSimulationResult( simResult )
  , mGroupIndex( groupIndex )
  , mfaceSupportActiveFlag( faceSupportActiveFlag )
  , mDatasetIndex( index )
{}

QgsMeshDatasetValue ReosResultDataset::datasetValue( int valueIndex ) const
{
  if ( !mfaceSupportActiveFlag && mSimulationResult->groupLocation( mGroupIndex ) == ReosMeshDatasetSource::Location::Face )
  {
    if ( mSimulationResult->activeFaces( mGroupIndex ).at( valueIndex ) == 0 )
      return std::numeric_limits<double>::quiet_NaN();
  }

  if ( mSimulationResult->groupIsScalar( mGroupIndex ) )
    return QgsMeshDatasetValue( mSimulationResult->datasetValues( mGroupIndex, mDatasetIndex ).at( valueIndex ) );
  else
  {
    const QVector<double> &vals = mSimulationResult->datasetValues( mGroupIndex, mDatasetIndex );
    return QgsMeshDatasetValue( vals.at( valueIndex * 2 ), vals.at( valueIndex * 2 + 1 ) );
  }
}

QgsMeshDataBlock ReosResultDataset::datasetValues( bool isScalar, int valueIndex, int count ) const
{
  QVector<double> sourceValues = mSimulationResult->datasetValues( mGroupIndex, mDatasetIndex );
  if ( !mfaceSupportActiveFlag && mSimulationResult->groupLocation( mGroupIndex ) == ReosMeshDatasetSource::Location::Face )
  {
    const QVector<int> activefaces = mSimulationResult->activeFaces( mDatasetIndex );

    for ( int i = 0; i < activefaces.count(); ++i )
    {
      if ( activefaces.at( i ) == 0 )
      {
        if ( isScalar )
          sourceValues[i] = std::numeric_limits<double>::quiet_NaN();
        else
        {
          sourceValues[2 * i] = std::numeric_limits<double>::quiet_NaN();
          sourceValues[2 * i + 1] = std::numeric_limits<double>::quiet_NaN();
        }
      }
    }
  }

  int valueCount = std::min( count, mSimulationResult->datasetValuesCount( mGroupIndex, mDatasetIndex ) - valueIndex );
  QgsMeshDataBlock ret( isScalar ? QgsMeshDataBlock::ScalarDouble : QgsMeshDataBlock::Vector2DDouble, valueCount );

  if ( valueCount == mSimulationResult->datasetValuesCount( mGroupIndex, mDatasetIndex ) )
    ret.setValues( sourceValues );
  else
  {
    QVector<double> values( valueCount );
    memcpy( values.data(), &( sourceValues[valueIndex] ), count * sizeof( double ) );
    ret.setValues( values );
  }

  ret.setValid( true );

  return ret;
}

QgsMeshDataBlock ReosResultDataset::areFacesActive( int faceIndex, int count ) const
{
  if ( !mfaceSupportActiveFlag && mSimulationResult->groupLocation( mGroupIndex ) == ReosMeshDatasetSource::Location::Face )
    return QgsMeshDataBlock();

  const QVector<int> &sourceValues = mSimulationResult->activeFaces( mDatasetIndex );

  int valueCount = std::min( count, sourceValues.size() - faceIndex );
  QVector<int> values( valueCount );
  memcpy( values.data(), &( sourceValues[faceIndex] ), count * sizeof( int ) );
  QgsMeshDataBlock ret( QgsMeshDataBlock::ActiveFlagInteger, valueCount );
  ret.setActive( values );

  ret.setValid( true );

  return ret;
}

bool ReosResultDataset::isActive( int faceIndex ) const
{
  return mSimulationResult->activeFaces( mDatasetIndex ).at( faceIndex ) == 1;
}

QgsMeshDatasetMetadata ReosResultDataset::metadata() const
{
  double time = mSimulationResult->datasetRelativeTime( mGroupIndex, mDatasetIndex ).valueHour();
  double min, max;
  mSimulationResult->datasetMinMax( mGroupIndex, mDatasetIndex, min, max );
  return QgsMeshDatasetMetadata( time, true, min, max, 0 );
}

int ReosResultDataset::valuesCount() const
{
  return mSimulationResult->datasetValuesCount( mGroupIndex, mDatasetIndex );
}

ReosResultDatasetGroup::ReosResultDatasetGroup( ReosMeshDatasetSource *simResult, int index, bool faceSupportActiveFlag )
  : mSimulationResult( simResult )
  , mGroupIndex( index )
  , mFacesSupportActiveFlag( faceSupportActiveFlag )
{
  mName = simResult->groupName( index );
  mIsScalar = simResult->groupIsScalar( index );
  setReferenceTime( simResult->groupReferenceTime( index ) );
  switch ( simResult->groupLocation( index ) )
  {
    case ReosMeshDatasetSource::Location::Vertex:
      mDataType = QgsMeshDatasetGroupMetadata::DataOnVertices;
      break;
    case ReosMeshDatasetSource::Location::Face:
      mDataType = QgsMeshDatasetGroupMetadata::DataOnFaces;
      break;
  }
}

void ReosResultDatasetGroup::initialize()
{
  int datasetCount = mSimulationResult->datasetCount( mGroupIndex );

  for ( int i = 0; i < datasetCount; ++i )
    mDatasets.emplace_back( new ReosResultDataset( mSimulationResult, mGroupIndex, i, mFacesSupportActiveFlag ) );
}

QgsMeshDatasetMetadata ReosResultDatasetGroup::datasetMetadata( int datasetIndex ) const
{
  return mDatasets.at( static_cast<size_t>( datasetIndex ) )->metadata();
}

int ReosResultDatasetGroup::datasetCount() const
{
  return static_cast<int>( mDatasets.size() );
}

QgsMeshDataset *ReosResultDatasetGroup::dataset( int index ) const
{
  return mDatasets.at( static_cast<int>( index ) ).get();
}

QgsMeshDatasetGroup::Type ReosResultDatasetGroup::type() const
{
  return QgsMeshDatasetGroup::None;
}

ReosRendererMeshMapTimeStamp_p::ReosRendererMeshMapTimeStamp_p(
  const QgsMeshDatasetIndex &scalarIndex,
  const QgsMeshDatasetIndex &vectorIndex,
  quint64 tracesAges )
  : mScalarIndex( scalarIndex )
  , mVectorIndex( vectorIndex )
  , mTracesAge( tracesAges )
{
}

bool ReosRendererMeshMapTimeStamp_p::equal( ReosRendererObjectMapTimeStamp *other )
{
  ReosRendererMeshMapTimeStamp_p *other_p = dynamic_cast<ReosRendererMeshMapTimeStamp_p *>( other );
  if ( !other_p )
    return false;
  bool testScalar = other_p->mScalarIndex.isValid() || mScalarIndex.isValid();

  if ( testScalar && other_p->mScalarIndex != mScalarIndex )
    return false;

  bool testVector = other_p->mVectorIndex.isValid() || mVectorIndex.isValid();

  if ( testVector &&
       ( other_p->mVectorIndex != mVectorIndex || other_p->mTracesAge != mTracesAge ) )
    return false;

  return true;
}

ReosMeshColorShaderSettings_p::ReosMeshColorShaderSettings_p( ReosMeshFrame_p *mesh )
  : ReosColorShaderSettings_p()
  , mMesh( mesh )
{}

ReosMeshScalarColorShaderSettings_p::ReosMeshScalarColorShaderSettings_p( ReosMeshFrame_p *mesh )
  : ReosMeshColorShaderSettings_p( mesh )
{}

void ReosMeshScalarColorShaderSettings_p::setCurrentSymbology( const ReosEncodedElement &symbology )
{
  QgsMeshRendererScalarSettings settings = getScalarSettingsFromEncoded( symbology );
  mMinClassifiction = settings.classificationMinimum();
  mMaxClassification = settings.classificationMaximum();
  mOpacity = settings.opacity();
  mColorShader = settings.colorRampShader();

  emit settingsChangedFromObject();
}

bool ReosMeshScalarColorShaderSettings_p::isValid() const
{
  if ( mMesh.isNull() )
    return false;

  return mMesh->mCurrentScalarDatasetId != QString();
}


double ReosMeshColorShaderSettings_p::classificationMinimum() const
{
  return mMinClassifiction;
}

void ReosMeshColorShaderSettings_p::setClassificationMinimum( double newClassificationMinimum )
{
  mMinClassifiction = newClassificationMinimum;
}

double ReosMeshColorShaderSettings_p::classificationMaximum() const
{
  return mMaxClassification;
}

void ReosMeshColorShaderSettings_p::setClassificationMaximum( double newClassificationMaximum )
{
  mMaxClassification = newClassificationMaximum;
}

double ReosMeshColorShaderSettings_p::opacity() const
{
  return mOpacity;
}

void ReosMeshColorShaderSettings_p::setOpacity( double opacity )
{
  mOpacity = opacity;
}

bool ReosMeshScalarColorShaderSettings_p::getDirectSourceMinMax( double &min, double &max ) const
{
  mMesh->datasetGroupMinimumMaximum( mMesh->mCurrentScalarDatasetId, min, max );
  return true;
}

void ReosMeshScalarColorShaderSettings_p::onSettingsUpdated()
{
  QgsMeshRendererSettings settings = mMesh->mMeshLayer->rendererSettings();
  int idx = mMesh->mDatasetGroupsIndex.value( mMesh->mCurrentScalarDatasetId, -1 );
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( idx );
  scalarSettings.setClassificationMinimumMaximum( mMinClassifiction, mMaxClassification );
  scalarSettings.setOpacity( mOpacity );
  scalarSettings.setColorRampShader( mColorShader );
  mMesh->applySymbologyOnScalarDataSet( mMesh->mCurrentScalarDatasetId, scalarSettings );
  mMesh->mDatasetScalarSymbologies.insert( mMesh->mCurrentScalarDatasetId, encodedFromScalarSettings( scalarSettings ).bytes() );

  mMesh->update3DRenderer();
  emit mMesh->repaintRequested();
}

QString ReosMeshScalarColorShaderSettings_p::title() const
{
  if ( mMesh )
    return mMesh->datasetName( mMesh->currentdScalarDatasetId() );

  return QString();
}


ReosMeshVectorColorShaderSettings_p::ReosMeshVectorColorShaderSettings_p( ReosMeshFrame_p *mesh )
  : ReosMeshColorShaderSettings_p( mesh )
{}

bool ReosMeshVectorColorShaderSettings_p::isValid() const
{
  if ( mMesh.isNull() )
    return false;

  return mMesh->mCurrentActiveVectorDatasetId != QString();
}

bool ReosMeshVectorColorShaderSettings_p::getDirectSourceMinMax( double &min, double &max ) const
{
  mMesh->datasetGroupMinimumMaximum( mMesh->mCurrentActiveVectorDatasetId, min, max );
  return true;
}

void ReosMeshVectorColorShaderSettings_p::onSettingsUpdated()
{
  QgsMeshRendererSettings settings = mMesh->mMeshLayer->rendererSettings();
  int idx = mMesh->mDatasetGroupsIndex.value( mMesh->mCurrentActiveVectorDatasetId, -1 );

  QgsMeshRendererVectorSettings vectorSettings = settings.vectorSettings( idx );
  vectorSettings.setColorRampShader( mColorShader );
  mMesh->applySymbologyOnVectorDataSet( mMesh->mCurrentActiveVectorDatasetId, vectorSettings );

  mMesh->mDatasetVectorSymbologies.insert( mMesh->mCurrentScalarDatasetId, encodedFromVectorSettings( vectorSettings ).bytes() );

  emit mMesh->repaintRequested();
}

void ReosMeshVectorColorShaderSettings_p::setCurrentSymbology( const ReosEncodedElement &symbology )
{
  QgsMeshRendererVectorSettings settings = getVectorSettingsFromEncoded( symbology );
  mColorShader = settings.colorRampShader();
  mMinClassifiction = mColorShader.minimumValue();
  mMaxClassification = mColorShader.maximumValue();
  mOpacity = -1;

  emit settingsChangedFromObject();
}


ReosMeshTerrainColorShaderSettings_p::ReosMeshTerrainColorShaderSettings_p( ReosMeshFrame_p *mesh )
  : ReosMeshColorShaderSettings_p( mesh )
{}

void ReosMeshTerrainColorShaderSettings_p::setCurrentSymbology( const ReosEncodedElement &symbology )
{
  QgsMeshRendererScalarSettings settings = getScalarSettingsFromEncoded( symbology );
  mMinClassifiction = settings.classificationMinimum();
  mMaxClassification = settings.classificationMaximum();
  mOpacity = settings.opacity();
  mColorShader = settings.colorRampShader();

  emit settingsChangedFromObject();
}

bool ReosMeshTerrainColorShaderSettings_p::isValid() const
{
  if ( mMesh.isNull() )
    return false;

  return mMesh->mVerticesElevationDatasetId != QString();
}

bool ReosMeshTerrainColorShaderSettings_p::getDirectSourceMinMax( double &min, double &max ) const
{
  mMesh->datasetGroupMinimumMaximum( mMesh->mVerticesElevationDatasetId, min, max );
  return true;
}

void ReosMeshTerrainColorShaderSettings_p::onSettingsUpdated()
{
  QgsMeshRendererSettings settings = mMesh->mMeshLayer->rendererSettings();
  int idx = mMesh->mDatasetGroupsIndex.value( mMesh->mVerticesElevationDatasetId, -1 );
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( idx );
  scalarSettings.setClassificationMinimumMaximum( mMinClassifiction, mMaxClassification );
  scalarSettings.setOpacity( mOpacity );
  scalarSettings.setColorRampShader( mColorShader );
  mMesh->mDatasetScalarSymbologies.insert( mMesh->mVerticesElevationDatasetId, encodedFromScalarSettings( scalarSettings ).bytes() );
  mMesh->applySymbologyOnScalarDataSet( mMesh->mVerticesElevationDatasetId, scalarSettings );

  emit mMesh->repaintRequested();
  mMesh->update3DRenderer();
  emit meshTerrainSettingsChanged();
}

QString ReosMeshTerrainColorShaderSettings_p::title() const
{
  return tr( "Terrain elevation" );
}

ReosMeshData_::~ReosMeshData_() {}
