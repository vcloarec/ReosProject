/***************************************************************************
  reostelemacstructureimportersource.cpp - ReosTelemacStructureImporterSource

 ---------------------
 begin                : 4.10.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#include "reostelemacstructureimportersource.h"

#include <QRegularExpression>


ReosTelemacStructureImporterSource::ReosTelemacStructureImporterSource( const QString &steeringFile, const ReosHydraulicNetworkContext &context )
  : ReosStructureImporterSource()
  , mSteeringFilePath( steeringFile )
  , mNetwork( context.network() )
{}

ReosStructureImporterSource *ReosTelemacStructureImporterSource::clone() const
{
  return new ReosTelemacStructureImporterSource( mSteeringFilePath, mNetwork->context() );
}

ReosStructureImporter *ReosTelemacStructureImporterSource::createImporter() const
{
  return new ReosTelemacStructureImporter( mSteeringFilePath, mNetwork->context(), this );
}

ReosEncodedElement ReosTelemacStructureImporterSource::encode( const ReosHydraulicNetworkContext &context ) const
{}

ReosTelemacStructureImporter::ReosTelemacStructureImporter( const QString &steeringFile, const ReosHydraulicNetworkContext &context, const ReosTelemacStructureImporterSource *source )
  : ReosStructureImporter( context )
  , mDirectory( QFileInfo( steeringFile ).dir() )
  , mSteeringFile( steeringFile )
  , mSource( source )
{
  mSteeringFile.open();
}

ReosHydraulicStructure2D::Structure2DCapabilities ReosTelemacStructureImporter::capabilities() const
{
  return QFlags( ReosHydraulicStructure2D::DefinedExternally ) | ReosHydraulicStructure2D::GeometryEditable;
}

QString ReosTelemacStructureImporter::crs() const
{
  if ( !mGeometryMesh )
    loadGeometry();

  return mGeometryMesh->crs();
}

QPolygonF ReosTelemacStructureImporter::domain() const
{
  if ( !mGeometryMesh )
    loadGeometry();

  return mBoundaries.envelop();
}

static QString unquote( const QString &str )
{
  return str.trimmed().remove( QRegularExpression( "^'|'$" ) );
}

ReosMesh *ReosTelemacStructureImporter::mesh( const QString &destinationCrs ) const
{
  return mGeometryMesh.release();
}

QList<ReosHydraulicStructureBoundaryCondition *> ReosTelemacStructureImporter::createBoundaryConditions( ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const
{
  if ( !mGeometryMesh )
    loadGeometry();

  QList<ReosHydraulicStructureBoundaryCondition::Type> boundaryTypes = mBoundaries.boundaryConditionTypes();
  QList<QSet<int>> liquidDomainSegmentIndex = mBoundaries.liquidDomainSegmentIndex();

  int boundaryCount = boundaryTypes.count();
  ReosPolylinesStructure *geomStructure = structure->geometryStructure();
  QList<ReosGeometryStructureVertex *> vertices = geomStructure->boundaryVertices();

  QList<ReosHydraulicStructureBoundaryCondition *> ret;

  for ( int i = 0; i < boundaryCount; ++i )
  {
    const QString bcId = QUuid::createUuid().toString();
    ReosHydraulicStructureBoundaryCondition *bc = new ReosHydraulicStructureBoundaryCondition( structure, bcId, context );
    bc->setDefaultConditionType( boundaryTypes.at( i ) );
    QList<int> segmentIndexes = QList<int>( liquidDomainSegmentIndex.at( i ).begin(), liquidDomainSegmentIndex.at( i ).end() );
    std::sort( segmentIndexes.begin(), segmentIndexes.end() );

    ReosGeometryStructureVertex *v1 = vertices.at( segmentIndexes.first() );
    ReosGeometryStructureVertex *v2 = vertices.at( ( segmentIndexes.last() + 1 ) % boundaryCount );
    geomStructure->addBoundaryCondition( v1, v2, bcId );

    ret.append( bc );
  }

  return ret;
}

bool ReosTelemacStructureImporter::isValid() const
{
  return mSteeringFile.isValid();
}

void ReosTelemacStructureImporter::loadGeometry() const
{
  QString geometryFile = mSteeringFile.value( QStringLiteral( "GEOMETRY FILE" ) );
  QString boundaryFile = mSteeringFile.value( QStringLiteral( "BOUNDARY CONDITIONS FILE" ) );
  if ( geometryFile.isEmpty() )
    return;

  geometryFile = unquote( geometryFile );
  geometryFile = mDirectory.filePath( geometryFile );
  ReosModule::Message message;
  mGeometryMesh.reset( ReosMesh::createMeshFrameFromFile( geometryFile, geometryFile, message ) );

  boundaryFile = unquote( boundaryFile );
  boundaryFile = mDirectory.filePath( boundaryFile );
  mBoundaries = ReosTelemacBoundaries( mGeometryMesh.get(), boundaryFile );
}
