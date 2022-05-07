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

#include "reosmeshdataprovider_p.h"
#include "reosparameter.h"
#include "reosencodedelement.h"
#include "reosmapextent.h"

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

void ReosMeshFrame_p::save( const QString &dataPath )
{
  QDir dir( dataPath );

  bool isEditable = mMeshLayer->isEditable();

  if ( isEditable )
    stopFrameEditing( true, true );

  meshProvider()->setFilePath( dir.filePath( QStringLiteral( "meshFrame.nc" ) ) );
  meshProvider()->setMDALDriver( QStringLiteral( "Ugrid" ) );
  meshProvider()->saveMeshFrameToFile( *mMeshLayer->nativeMesh() );
}

void ReosMeshFrame_p::init()
{
  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererMeshSettings meshSettings = settings.nativeMeshSettings();
  meshSettings.setLineWidth( 0.1 );
  meshSettings.setColor( Qt::gray );
  settings.setNativeMeshSettings( meshSettings );
  mMeshLayer->setRendererSettings( settings );

  QgsCoordinateTransform transform( mMeshLayer->crs(), QgsProject::instance()->crs(), QgsProject::instance() );
  mMeshLayer->updateTriangularMesh( transform );

  connect( mMeshLayer.get(), &QgsMapLayer::repaintRequested, this, &ReosMesh::repaintRequested );
  connect( mMeshLayer.get(), &QgsMeshLayer::layerModified, this, &ReosDataObject::dataChanged );
}

void ReosMeshFrame_p::stopFrameEditing( bool commit, bool continueEditing )
{
  int activeScalarDatasetIndex = mMeshLayer->rendererSettings().activeScalarDatasetGroup();
  QString activeGroupId;

  for ( auto it = mDatasetGroupsIndex.begin(); it != mDatasetGroupsIndex.end(); ++it )
    if ( it.value() == activeScalarDatasetIndex )
      activeGroupId = it.key();

  QgsCoordinateTransform transform( mMeshLayer->crs(), QgsProject::instance()->crs(), QgsProject::instance() );

  if ( mMeshLayer->isEditable() )
  {
    if ( commit )
      mMeshLayer->commitFrameEditing( transform, continueEditing );
    else
      mMeshLayer->rollBackFrameEditing( transform, continueEditing );

    if ( !continueEditing )
      restoreVertexElevationDataset();
  }

  activateDataset( activeGroupId );
}


ReosEncodedElement ReosMeshFrame_p::datasetScalarGroupSymbologyPrivate( int i ) const
{
  QDomDocument doc( QStringLiteral( "dataset-symbology" ) );

  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  QgsMeshRendererScalarSettings scalarSettings = settings.scalarSettings( i );

  doc.appendChild( scalarSettings.writeXml( doc ) ) ;

  ReosEncodedElement encodedElem( QStringLiteral( "dataset-symbology" ) );
  QString docString = doc.toString();
  encodedElem.addData( QStringLiteral( "symbology" ), docString );

  return encodedElem;
}

void ReosMeshFrame_p::applyScalarSymbologyOnMeshDatasetGroup( const QString &id )
{
  if ( !mDatasetScalarSymbologies.contains( id ) )
    return;

  const ReosEncodedElement &encodedSymbology( mDatasetScalarSymbologies.value( id ) );

  QString docString;
  encodedSymbology.getData( QStringLiteral( "symbology" ), docString );

  QDomDocument doc( QStringLiteral( "dataset-symbology" ) );

  if ( doc.setContent( docString ) )
  {
    QDomElement domElem = doc.firstChildElement( QStringLiteral( "scalar-settings" ) );
    QgsReadWriteContext context;
    QgsMeshRendererScalarSettings scalarSettings;
    scalarSettings.readXml( domElem );
    QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
    settings.setScalarSettings( mDatasetGroupsIndex.value( id ), scalarSettings );
    mMeshLayer->setRendererSettings( settings );
  }
}

ReosEncodedElement ReosMeshFrame_p::datasetScalarGroupSymbology( const QString &id ) const
{
  if ( !mDatasetGroupsIndex.contains( id ) || !mDatasetScalarSymbologies.contains( id ) )
    return ReosEncodedElement();

  return ReosEncodedElement( mDatasetScalarSymbologies.value( id ) );
}

void ReosMeshFrame_p::setDatasetScalarGroupSymbology( const ReosEncodedElement &encodedElement, const QString &id )
{
  if ( encodedElement.description() != QStringLiteral( "dataset-symbology" ) )
    return;

  mDatasetScalarSymbologies.insert( id, encodedElement.bytes() );
  applyScalarSymbologyOnMeshDatasetGroup( id );

  update3DRenderer();
}

void ReosMeshFrame_p::activateWireFrame( bool activate )
{
  mWireFrameSettings.enabled = activate;
  updateWireFrameSettings();
}

bool ReosMeshFrame_p::isWireFrameActive() const
{
  return mMeshLayer->rendererSettings().nativeMeshSettings().isEnabled();
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
  addDatasetGroup( group.release(), mVerticesElevationDatasetId );
}

void ReosMeshFrame_p::updateWireFrameSettings()
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

  mMeshLayer->triggerRepaint();
  update3DRenderer();
}

QPointF ReosMeshFrame_p::tolayerCoordinates( const ReosSpatialPosition &position ) const
{
  QgsCoordinateReferenceSystem sourceCrs;
  sourceCrs.fromWkt( position.crs() );

  const QgsCoordinateTransform transform( sourceCrs, mMeshLayer->crs(), QgsProject::instance() );
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

void ReosMeshFrame_p::update3DRenderer()
{
  if ( !mMeshLayer )
    return;

  const QgsMeshRendererScalarSettings scalarSettings =
    mMeshLayer->rendererSettings().scalarSettings( mDatasetGroupsIndex.value( mCurrentdScalarDatasetId ) );

  const QgsMeshRendererMeshSettings meshSettings =
    mMeshLayer->rendererSettings().nativeMeshSettings();

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
  symbol->setWireframeEnabled( meshSettings.isEnabled() );
  symbol->setWireframeLineColor( meshSettings.color() );
  symbol->setWireframeLineWidth( meshSettings.lineWidth() * 2 );
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

void ReosMeshFrame_p::setWireFrameSettings( const WireFrameSettings &wireFrameSettings )
{
  mWireFrameSettings = wireFrameSettings;
  updateWireFrameSettings();
}

//from QGIS src/core/mesh/qgsmeshlayerutils.cpp
static void lamTol( double &lam )
{
  const static double eps = 1e-6;
  if ( ( lam < 0.0 ) && ( lam > -eps ) )
  {
    lam = 0.0;
  }
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
  const QVector<double> datasetValues = datasetSource->datasetValues( sourceGroupindex, datasetIndex );
  const QVector<int> facesActive = datasetSource->activeFaces( datasetIndex );

  bool isScalar = datasetSource->groupIsScalar( sourceGroupindex );

  Q_ASSERT( datasetValues.count() == mMeshLayer->meshVertexCount() * ( isScalar ? 1 : 2 ) );

  QgsPointXY positionInLayer = QgsMeshVertex( tolayerCoordinates( position ) );

  QgsTriangularMesh *triangularMesh = mMeshLayer->triangularMesh();
  const QgsMeshVertex triVert = triangularMesh->nativeToTriangularCoordinates( QgsMeshVertex( positionInLayer ) );
  int faceIndex = triangularMesh->faceIndexForPoint_v2( triVert );

  if ( faceIndex < 0 || faceIndex >= triangularMesh->triangles().count() )
    return std::numeric_limits<double>::quiet_NaN();

  if ( facesActive.at( faceIndex ) == 0 )
    return std::numeric_limits<double>::quiet_NaN();

  const QgsMeshFace &face = triangularMesh->triangles().at( faceIndex );
  const QgsMesh nativeMesh = *mMeshLayer->nativeMesh();

  double i0 = face.at( 0 );
  double i1 = face.at( 1 );
  double i2 = face.at( 2 );
  qDebug() << i0 << i1 << i2;

  bool ok = false;
  double result;

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

  if ( ok )
    return result;

  return std::numeric_limits<double>::quiet_NaN();
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
      mDatasetScalarSymbologies.insert( id, datasetScalarGroupSymbology( id ).bytes() );
      if ( meta.isVector() )
        break;
    }
  }

  mDatasetGroupsIndex[id] = index;
}

void ReosMeshFrame_p::firstUpdateOfTerrainScalarSetting()
{
  if ( !mZVerticesDatasetGroup || !mDatasetGroupsIndex.contains( mVerticesElevationDatasetId ) )
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

bool ReosMeshFrame_p::activateDataset( const QString &id, bool update )
{
  int index = mDatasetGroupsIndex.value( id, -1 );

  mCurrentdScalarDatasetId = id;
  QgsMeshRendererSettings settings = mMeshLayer->rendererSettings();
  settings.setActiveScalarDatasetGroup( index );
  mMeshLayer->setRendererSettings( settings );

  applyScalarSymbologyOnMeshDatasetGroup( id );

  if ( update )
    update3DRenderer();

  return true;
}
  if ( update )
    update3DRenderer();

  return true;
}

QStringList ReosMeshFrame_p::datasetIds() const
{
  QMap<int, QString> mapRet;
  QStringList ids = mDatasetGroupsIndex.keys();
  for ( const QString &id : ids )
    mapRet.insert( mDatasetGroupsIndex.value( id ), id );

  return mapRet.values();
}

QString ReosMeshFrame_p::datasetName( const QString &id ) const
{
  QgsMeshDatasetGroupMetadata meta = mMeshLayer->datasetGroupMetadata( mDatasetGroupsIndex.value( id ) );
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

class ApplyTopopraphyProcess : public ReosProcess
{

  public:
    ApplyTopopraphyProcess( ReosTopographyCollection *topographyCollection, ReosMeshDataProvider_p *meshProvider )
      : mTopographyCollection( topographyCollection )
      , mProvider( meshProvider )
    {

    }
    void start()
    {
      mProvider->applyTopographyOnVertices( mTopographyCollection, this );
    }

  private:
    ReosTopographyCollection *mTopographyCollection = nullptr;
    ReosMeshDataProvider_p *mProvider = nullptr;
};

ReosProcess *ReosMeshFrame_p::applyTopographyOnVertices( ReosTopographyCollection *topographyCollection )
{
  if ( mMeshLayer->isEditable() )
    stopFrameEditing( true );


  std::unique_ptr<ReosProcess> process( new ApplyTopopraphyProcess( topographyCollection, meshProvider() ) );

  connect( process.get(), &ReosProcess::finished, this, [this]
  {
    mMeshLayer->reload();

    if ( mZVerticesDatasetGroup )
      mZVerticesDatasetGroup->setStatisticObsolete();

    firstUpdateOfTerrainScalarSetting();

    emit repaintRequested();
    mMeshLayer->trigger3DUpdate();
    emit dataChanged();
  } );

  return process.release();
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
  return mCurrentdScalarDatasetId;
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

void ReosMeshFrame_p::setSimulationResults( ReosHydraulicSimulationResults *result )
{
  meshProvider()->setDatasetSource( result );
  mMeshLayer->temporalProperties()->setDefaultsFromDataProviderTemporalCapabilities( meshProvider()->temporalCapabilities() );

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
      mDatasetGroupsIndex[result->groupId( i )] = index;
    }
  }

  meshProvider()->reloadData();
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

