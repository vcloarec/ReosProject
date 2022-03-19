/***************************************************************************
  reoshydraulicstructure2d.cpp - ReosHydraulicStructure2D

 ---------------------
 begin                : 9.1.2022
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
#include "reoshydraulicstructure2d.h"
#include "reosmeshgenerator.h"
#include "reosgmshgenerator.h"
#include "reostopographycollection.h"

#include <QProcess>
#include <QDir>

ReosHydraulicStructure2D::ReosHydraulicStructure2D( const QPolygonF &domain, const QString &crs, ReosHydraulicNetwork *parent )
  : ReosHydraulicNetworkElement( parent )
  , mMeshGenerator( new ReosGmshGenerator( this ) )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( domain, crs ) )
  , mMeshResolutionController( new ReosMeshResolutionController( this, crs ) )
  , mTopographyCollecion( ReosTopographyCollection::createTopographyCollection( parent->getGisEngine(), this ) )
  , mMesh( ReosMesh::createMeshFrame( crs ) )
  , mRoughnessStructure( new ReosRoughnessStructure( crs ) )
{
  init();
}


ReosHydraulicStructure2D::ReosHydraulicStructure2D(
  const ReosEncodedElement &encodedElement,
  const ReosHydraulicNetworkContext &context )
  : ReosHydraulicNetworkElement( encodedElement, context.network() )
  , mMeshGenerator( ReosMeshGenerator::createMeshGenerator( encodedElement.getEncodedData( QStringLiteral( "mesh-generator" ) ), this ) )
  , mPolylinesStructures( ReosPolylinesStructure::createPolylineStructure( encodedElement.getEncodedData( QStringLiteral( "structure" ) ) ) )
  , mTopographyCollecion( ReosTopographyCollection::createTopographyCollection( encodedElement.getEncodedData( QStringLiteral( "topography-collection" ) ), context.network()->getGisEngine(), this ) )
  , mRoughnessStructure( new ReosRoughnessStructure( encodedElement.getEncodedData( QStringLiteral( "roughness-structure" ) ) ) )
  , m3dMapSettings( encodedElement.getEncodedData( "3d-map-setings" ) )
{
  if ( encodedElement.hasEncodedData( QStringLiteral( "mesh-resolution-controller" ) ) )
    mMeshResolutionController = new ReosMeshResolutionController( encodedElement.getEncodedData( QStringLiteral( "mesh-resolution-controller" ) ), this );
  else
    mMeshResolutionController = new ReosMeshResolutionController( this );

  QString dataPath = context.projectPath() + '/' + context.projectName() + QStringLiteral( "-hydr-struct" ) + '/' + directory();

  mMesh.reset( ReosMesh::createMeshFrameFromFile( dataPath ) );
  init();
  mMesh->setMeshSymbology( encodedElement.getEncodedData( QStringLiteral( "mesh-frame-symbology" ) ) );
  mMesh->setQualityMeshParameter( encodedElement.getEncodedData( QStringLiteral( "mesh-quality-parameters" ) ) );
}

ReosRoughnessStructure *ReosHydraulicStructure2D::roughnessStructure() const
{
  return mRoughnessStructure.get();
}

Reos3DMapSettings ReosHydraulicStructure2D::map3dSettings() const
{
  return m3dMapSettings;
}

void ReosHydraulicStructure2D::setMap3dSettings( const Reos3DMapSettings &value )
{
  m3dMapSettings = value;
}

Reos3DTerrainSettings ReosHydraulicStructure2D::terrain3DSettings() const
{
  return m3dTerrainSettings;
}

void ReosHydraulicStructure2D::setTerrain3DSettings( const Reos3DTerrainSettings &settings )
{
  m3dTerrainSettings = settings;
}

void ReosHydraulicStructure2D::encodeData( ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  element.addEncodedData( QStringLiteral( "structure" ), mPolylinesStructures->encode() );
  element.addEncodedData( QStringLiteral( "mesh-generator" ), mMeshGenerator->encode() );
  element.addEncodedData( QStringLiteral( "mesh-resolution-controller" ), mMeshResolutionController->encode() );
  element.addEncodedData( QStringLiteral( "topography-collection" ), mTopographyCollecion->encode() );

  QDir dir( context.projectPath() );
  QString hydDir = context.projectName() + QStringLiteral( "-hydr-struct" );
  dir.mkdir( hydDir );
  dir.cd( hydDir );
  dir.mkdir( directory() );
  dir.cd( directory() );

  mMesh->save( dir.path() );
  element.addEncodedData( QStringLiteral( "mesh-frame-symbology" ), mMesh->meshSymbology() );
  element.addEncodedData( QStringLiteral( "mesh-quality-parameters" ), mMesh->qualityMeshParameters().encode() );
  element.addEncodedData( QStringLiteral( "roughness-structure" ), mRoughnessStructure->encode() );

  element.addEncodedData( "3d-map-setings", m3dMapSettings.encode() );
}

ReosTopographyCollection *ReosHydraulicStructure2D::topographyCollecion() const
{
  return mTopographyCollecion;
}

QString ReosHydraulicStructure2D::terrainMeshDatasetId() const
{
  return mTerrainDatasetId;
}

void ReosHydraulicStructure2D::runSimulation()
{
}

void ReosHydraulicStructure2D::init()
{
  mTerrainDatasetId = mMesh->enableVertexElevationDataset( tr( "Terrain elevation" ) );

  connect( mPolylinesStructures.get(), &ReosDataObject::dataChanged, this, [this]
  {
    if ( mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( mMeshResolutionController, &ReosDataObject::dataChanged, this, [this]
  {
    if ( mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( mMeshGenerator, &ReosDataObject::dataChanged, this, [this]
  {
    if ( mMeshGenerator->autoUpdateParameter()->value() )
      generateMeshInPlace();
  } );

  connect( this, &ReosHydraulicStructure2D::meshGenerated, mTopographyCollecion, [this]
  {
    if ( mTopographyCollecion->autoApply()->value() )
      mMesh->applyTopographyOnVertices( mTopographyCollecion );
  } );
}

void ReosHydraulicStructure2D::generateMeshInPlace()
{
  std::unique_ptr<ReosMeshGeneratorProcess> process( getGenerateMeshProcess() );
  process->start();
}

QString ReosHydraulicStructure2D::directory() const
{
  return id().split( ':' ).last();
}

ReosMeshGenerator *ReosHydraulicStructure2D::meshGenerator() const
{
  return mMeshGenerator;
}

QPolygonF ReosHydraulicStructure2D::domain( const QString &crs ) const
{
  return mPolylinesStructures->boundary( crs );
}

ReosPolylinesStructure *ReosHydraulicStructure2D::geometryStructure() const
{
  return mPolylinesStructures.get();
}

ReosMeshResolutionController *ReosHydraulicStructure2D::meshResolutionController() const
{
  return mMeshResolutionController;
}

ReosMesh *ReosHydraulicStructure2D::mesh() const
{
  return mMesh.get();
}

ReosMeshGeneratorProcess *ReosHydraulicStructure2D::getGenerateMeshProcess()
{
  std::unique_ptr<ReosMeshGeneratorProcess> process(
    mMeshGenerator->getGenerateMeshProcess( mPolylinesStructures.get(), mMeshResolutionController, mMesh->crs() ) );
  ReosMeshGeneratorProcess *processP = process.get();

  connect( processP, &ReosProcess::finished, this, [this, processP]
  {
    if ( mMesh && processP->isSuccessful() )
    {
      mMesh->generateMesh( processP->meshResult() );
      emit meshGenerated();
      emit dataChanged();
    }
  } );

  return process.release();
}

void ReosHydraulicStructure2D::activateMeshTerrain()
{
  mMesh->activateDataset( mTerrainDatasetId );
}

void ReosHydraulicStructure2D::deactivateMeshScalar()
{
  mMesh->activateDataset( QString() );
}

ReosHydraulicStructure2D *ReosHydraulicStructure2D::create( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context )
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  return new ReosHydraulicStructure2D( encodedElement, context );

}

ReosHydraulicNetworkElement *ReosHydraulicStructure2dFactory::decodeElement( const ReosEncodedElement &encodedElement, const ReosHydraulicNetworkContext &context ) const
{
  if ( encodedElement.description() != ReosHydraulicStructure2D::staticType() )
    return nullptr;

  return ReosHydraulicStructure2D::create( encodedElement, context );
}

ReosRoughnessStructure::ReosRoughnessStructure( const QString &mCrs )
  : mStructure( ReosPolygonStructure::createPolygonStructure( mCrs ) )
  , mDefaultRoughness( new ReosParameterDouble( tr( "Default roughness" ) ) )
{
  mDefaultRoughness->setDisplayPrecision( 3 );
  mDefaultRoughness->setValue( 0.03 );
}

ReosRoughnessStructure::ReosRoughnessStructure( const ReosEncodedElement &encodedElement )
  : mStructure( ReosPolygonStructure::createPolygonStructure( encodedElement.getEncodedData( QStringLiteral( "structure" ) ) ) )
  , mDefaultRoughness( ReosParameterDouble::decode( encodedElement.getEncodedData( QStringLiteral( "default-roughness" ) ), false, tr( "Default roughness" ), this ) )
{}

ReosEncodedElement ReosRoughnessStructure::encode() const
{
  ReosEncodedElement encodedElement( QStringLiteral( "roughness-structure" ) );

  encodedElement.addEncodedData( QStringLiteral( "structure" ), mStructure->encode() );
  encodedElement.addEncodedData( QStringLiteral( "default-roughness" ), mDefaultRoughness->encode() );

  return encodedElement;
}

ReosParameterDouble *ReosRoughnessStructure::defaultRoughness() const
{
  return mDefaultRoughness;
}

ReosPolygonStructure *ReosRoughnessStructure::structure() const
{
  return mStructure.get();
}
