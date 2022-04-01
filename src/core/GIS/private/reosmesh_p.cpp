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

#include "reosmeshdataprovider_p.h"
#include "reosparameter.h"
#include "reosencodedelement.h"

ReosMeshFrame_p::ReosMeshFrame_p( const QString &crs, QObject *parent ): ReosMesh( parent )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );
  QgsCoordinateReferenceSystem qgsiCrs;
  qgsiCrs.createFromWkt( crs );
  mMeshLayer->setCrs( qgsiCrs );
  meshProvider()->overrideCrs( qgsiCrs );

  init();
}

ReosMeshFrame_p::ReosMeshFrame_p( const QString &dataPath )
{
  mMeshLayer.reset( new QgsMeshLayer( "path", "", QStringLiteral( "ReosMesh" ) ) );

  QDir dir( dataPath );
  if ( dir.exists() )
  {
    meshProvider()->loadMeshFrame( dir.filePath( QStringLiteral( "meshFrame.nc" ) ), QStringLiteral( "Ugrid" ) );
    mMeshLayer->reload();
  }

  init();
}

void ReosMeshFrame_p::init()
{
  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererMeshSettings meshSettings = settings.nativeMeshSettings();
  meshSettings.setEnabled( true );
  settings.setNativeMeshSettings( meshSettings );
  mMeshLayer->setRendererSettings( settings );

  QgsCoordinateTransform transform( mMeshLayer->crs(), QgsProject::instance()->crs(), QgsProject::instance() );
  mMeshLayer->updateTriangularMesh( transform );

  connect( mMeshLayer.get(), &QgsMapLayer::repaintRequested, this, &ReosMesh::repaintRequested );
  connect( mMeshLayer.get(), &QgsMeshLayer::layerModified, this, &ReosDataObject::dataChanged );
}

void ReosMeshFrame_p::save( const QString &dataPath ) const
{
  QDir dir( dataPath );

  meshProvider()->setFilePath( dir.filePath( QStringLiteral( "meshFrame.nc" ) ) );
  meshProvider()->setMDALDriver( QStringLiteral( "Ugrid" ) );
  meshProvider()->saveMeshFrameToFile( *mMeshLayer->nativeMesh() );
}

void ReosMeshFrame_p::stopFrameEditing( bool commit )
{
  int activeScalarDatasetIndex = mMeshLayer->rendererSettings().activeScalarDatasetGroup();
  QString activeGroupId;

  for ( auto it = mDatasetGroupsIndex.begin(); it != mDatasetGroupsIndex.end(); ++it )
    if ( it.value() == activeScalarDatasetIndex )
      activeGroupId = it.key();

  QgsCoordinateTransform transform( mMeshLayer->crs(), QgsProject::instance()->crs(), QgsProject::instance() );
  if ( commit )
    mMeshLayer->commitFrameEditing( transform, false );
  else
    mMeshLayer->rollBackFrameEditing( transform, false );

  if ( !mVerticesElevationDatasetId.isEmpty() )
    restoreVertexElevationDataset();

  activateDataset( activeGroupId );
}

ReosEncodedElement ReosMeshFrame_p::meshSymbology() const
{
  QDomDocument doc( QStringLiteral( "mesh-layer" ) );
  QDomElement elem = doc.createElement( QStringLiteral( "symbology" ) );
  QgsReadWriteContext context;
  QString errorMessage;
  mMeshLayer->writeSymbology( elem, doc, errorMessage, context );
  doc.appendChild( elem );

  ReosEncodedElement encodedElem( QStringLiteral( "mesh-symbology" ) );
  encodedElem.addData( "xml-symbology", doc.toString() );

  return encodedElem;
}

void ReosMeshFrame_p::setMeshSymbology( const ReosEncodedElement &symbology )
{

  if ( symbology.description() != QStringLiteral( "mesh-symbology" ) )
    return;

  QString docString;
  symbology.getData( "xml-symbology", docString );

  QDomDocument doc( QStringLiteral( "mesh-layer" ) );
  if ( doc.setContent( docString ) )
  {
    QDomElement domElem = doc.firstChildElement( QStringLiteral( "symbology" ) );
    QgsReadWriteContext context;
    QString errorMessage;
    mMeshLayer->readSymbology( domElem, errorMessage, context );
  }

}

ReosEncodedElement ReosMeshFrame_p::datasetGroupSymbology( const QString &id ) const
{
  QDomDocument doc( QStringLiteral( "dataset-symbology" ) );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( mDatasetGroupsIndex.value( id ) );

  doc.appendChild( scalarSettings.writeXml( doc ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "dataset-symbology" ) );
  QString docString = doc.toString();
  encodedElem.addData( "symbology", docString );

  return encodedElem;
}

ReosObjectRenderer *ReosMeshFrame_p::createRenderer( QGraphicsView *view )
{
  return new ReosMeshRenderer_p( view, mMeshLayer.get() );
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
  return mMeshLayer->meshVertexCount();
}

QPointF ReosMeshFrame_p::vertexPosition( int vertexIndex, const QString &destinationCrs )
{
  if ( destinationCrs.isEmpty() )
    return mMeshLayer->nativeMesh()->vertices.at( vertexIndex ).toQPointF();

  QgsCoordinateReferenceSystem crs;
  crs.fromWkt( destinationCrs );
  QgsCoordinateTransform transform( mMeshLayer->crs(), crs, QgsProject::instance() );
  QgsPointXY vert = mMeshLayer->nativeMesh()->vertices.at( vertexIndex );
  if ( transform.isValid() )
  {
    try
    {
      return transform.transform( vert ).toQPointF();
    }
    catch ( QgsCsException &e )
    {
      return vert.toQPointF();
    }
  }

  return vert.toQPointF();
}

QVector<int> ReosMeshFrame_p::face( int faceIndex ) const
{
  return mMeshLayer->nativeMesh()->faces.at( faceIndex );
}

int ReosMeshFrame_p::faceCount() const
{
  return mMeshLayer->meshFaceCount();
}


void ReosMeshFrame_p::restoreVertexElevationDataset()
{
  std::unique_ptr<QgsMeshDatasetGroup> group( new QgsMeshVerticesElevationDatasetGroup( mVerticesElevationDatasetName, mMeshLayer->nativeMesh() ) );
  mZVerticesDatasetGroup = group.get();
  mVerticesElevationDatasetId = addDatasetGroup( group.release(), mVerticesElevationDatasetId );
}


QString ReosMeshFrame_p::enableVertexElevationDataset( const QString &name )
{
  mVerticesElevationDatasetName = name;
  restoreVertexElevationDataset();

  int index = mDatasetGroupsIndex.value( mVerticesElevationDatasetId );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( index );
  if ( scalarSettings.classificationMinimum() >= scalarSettings.classificationMaximum() )
  {
    QgsColorRampShader colorRamp = scalarSettings.colorRampShader();
    colorRamp.setMinimumValue( 0 );
    colorRamp.setMaximumValue( 0 );
    scalarSettings.setClassificationMinimumMaximum( 0, 0 );
    scalarSettings.setColorRampShader( colorRamp );
    settings.setScalarSettings( index, scalarSettings );
    mMeshLayer->setRendererSettings( settings );
  }

  return mVerticesElevationDatasetId;
}


QString ReosMeshFrame_p::addDatasetGroup( QgsMeshDatasetGroup *group, const QString &id )
{
  QString name = group->name();

  QString effecticeId = id;
  if ( effecticeId.isEmpty() )
    effecticeId = QUuid::createUuid().toString();

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

  mDatasetGroupsIndex[effecticeId] = index;

  return effecticeId;
}

void ReosMeshFrame_p::firstUpdateOfTerrainScalarSetting()
{
  if ( !mZVerticesDatasetGroup || mDatasetGroupsIndex.contains( mVerticesElevationDatasetId ) )
    return;

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( mDatasetGroupsIndex.value( mVerticesElevationDatasetId ) );
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
      settings.setScalarSettings( mDatasetGroupsIndex.value( mVerticesElevationDatasetId ), scalarSettings );
      mMeshLayer->setRendererSettings( settings );
    }
  }
}

bool ReosMeshFrame_p::activateDataset( const QString &id )
{
  int index = mDatasetGroupsIndex.value( id, -1 );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  settings.setActiveScalarDatasetGroup( index );
  mMeshLayer->setRendererSettings( settings );

  return true;
}

void ReosMeshFrame_p::generateMesh( const ReosMeshFrameData &data )
{
  if ( mMeshLayer->isEditable() )
    stopFrameEditing( false );

  for ( const QVector<int> &boundLine : data.boundaryVertices )
    for ( int i : boundLine )
      mBoundaryVerticesSet.insert( i );

  for ( const QVector<QVector<int>> &hole : data.holesVertices )
    for ( const QVector<int> &holeLine : hole )
      for ( int i : holeLine )
        mBoundaryVerticesSet.insert( i );

  meshProvider()->generateMesh( data );
  mMeshLayer->reload();
  if ( mZVerticesDatasetGroup )
    mZVerticesDatasetGroup->setStatisticObsolete();
  mMeshLayer->trigger3DUpdate();
  emit repaintRequested();
  emit dataChanged();
}

QString ReosMeshFrame_p::crs() const
{
  return mMeshLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
}

QObject *ReosMeshFrame_p::data() const
{
  return mMeshLayer.get();
}

int ReosMeshFrame_p::datasetGroupIndex( const QString &id ) const
{
  return mDatasetGroupsIndex.value( id, -1 );
}

void ReosMeshFrame_p::applyTopographyOnVertices( ReosTopographyCollection *topographyCollection )
{
  if ( mMeshLayer->isEditable() )
    stopFrameEditing( true );

  meshProvider()->applyTopographyOnVertices( topographyCollection );
  mMeshLayer->reload();

  if ( mZVerticesDatasetGroup )
    mZVerticesDatasetGroup->setStatisticObsolete();

  firstUpdateOfTerrainScalarSetting();

  emit repaintRequested();
  mMeshLayer->trigger3DUpdate();
  emit dataChanged();
}

double ReosMeshFrame_p::datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const
{
  return mMeshLayer->datasetValue( QgsMeshDatasetIndex( datasetGroupIndex( datasetId ), 0 ), QgsPointXY( pos ) ).scalar();
}

ReosMeshDataProvider_p *ReosMeshFrame_p::meshProvider() const
{
  return qobject_cast<ReosMeshDataProvider_p *>( mMeshLayer->dataProvider() );
}

QString ReosMeshFrame_p::verticesElevationDatasetId() const
{
  return mVerticesElevationDatasetId;
}


ReosMeshRenderer_p::ReosMeshRenderer_p( QGraphicsView *canvas, QgsMeshLayer *layer )
{
  QgsMapCanvas *mapCanvas = qobject_cast<QgsMapCanvas *>( canvas );
  if ( mapCanvas )
  {
    const QgsMapSettings &settings = mapCanvas->mapSettings();
    mImage = QImage( settings.deviceOutputSize(), settings.outputImageFormat() );
    mImage.setDevicePixelRatio( settings.devicePixelRatio() );
    mImage.setDotsPerMeterX( static_cast<int>( settings.outputDpi() * 39.37 ) );
    mImage.setDotsPerMeterY( static_cast<int>( settings.outputDpi() * 39.37 ) );
    mImage.fill( Qt::transparent );

    mPainter.reset( new QPainter( &mImage ) );
    mRenderContext = QgsRenderContext::fromMapSettings( settings );
    mRenderContext.setPainter( mPainter.get() );
    mLayerRender.reset( layer->createMapRenderer( mRenderContext ) );
  }
}

void ReosMeshRenderer_p::render() const
{
  mLayerRender->render();
}

void ReosMeshRenderer_p::stopRendering()
{
  mRenderContext.setRenderingStopped( true );
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

  double areaFactor = QgsUnitTypes::fromUnitToUnitFactor( mDistanceArea.areaUnits(), QgsUnitTypes::AreaSquareMeters );
  double lenghtFactor = QgsUnitTypes::fromUnitToUnitFactor( mDistanceArea.lengthUnits(), QgsUnitTypes::DistanceMeters );
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
              catch ( QgsCsException &e )
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
        catch ( QgsCsException &e )
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
      catch ( QgsCsException &e )
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
          catch ( QgsCsException &e )
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

