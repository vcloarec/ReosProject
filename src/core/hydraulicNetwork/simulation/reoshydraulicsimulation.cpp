/***************************************************************************
  reoshydraulicsimulation.cpp - ReosHydraulicSimulation

 ---------------------
 begin                : 19.3.2022
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
#include "reoshydraulicsimulation.h"

#include <qgsmeshdataset.h>
#include <qgsmeshlayer.h>
#include <qgsproviderregistry.h>
#include <qgsmeshtriangulation.h>

#include "reoshydraulicstructure2d.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosparameter.h"


ReosHydraulicSimulation::ReosHydraulicSimulation( QObject *parent ): QObject( parent )
{
  mStartTime = new ReosParameterDateTime( tr( "Start time" ), this );
  mEndTime = new ReosParameterDateTime( tr( "End time" ), this );;
  mTimeStep = new ReosParameterDuration( tr( "Time step" ), false, this );
}

void ReosHydraulicSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure )
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.mkdir( mDirName );
  createSelafinInputGeometry( hydraulicStructure );
  createBoundaryConditionFiles( hydraulicStructure );
  createSteeringFile( hydraulicStructure );
}

void ReosHydraulicSimulation::createSelafinInputGeometry( ReosHydraulicStructure2D *hydraulicStructure )
{
  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *> ( hydraulicStructure->mesh()->data() );
  if ( !meshLayer )
    return;

  const QgsMesh &mesh = *meshLayer->nativeMesh();

  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mGeomFileName );

  QgsProviderMetadata *meta = QgsProviderRegistry::instance()->providerMetadata( QStringLiteral( "mdal" ) );
  if ( !meta )
    return;

  meta->createMeshData( mesh, path, QStringLiteral( "SELAFIN" ), meshLayer->crs() );

  std::unique_ptr<QgsMeshLayer> ouputMesh = std::make_unique < QgsMeshLayer>(
        path,
        QStringLiteral( "temp" ),
        QStringLiteral( "mdal" ) );

  //! Terrain elevation
  QgsMeshZValueDatasetGroup *zValueDatasetGroup = new QgsMeshZValueDatasetGroup( "BOTTOM", mesh );
  ouputMesh->addDatasets( zValueDatasetGroup );

  ouputMesh->saveDataset( path, 0, QStringLiteral( "SELAFIN" ) );

  //! Roughness
  std::unique_ptr<ReosPolygonStructureValues> roughness(
    hydraulicStructure->roughnessStructure()->structure()->values( meshLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT2_2019_SIMPLIFIED ) ) );


  std::shared_ptr<QgsMeshMemoryDataset> roughnessDataset( new QgsMeshMemoryDataset );
  roughnessDataset->values.resize( mesh.vertexCount() );

  int size = mesh.vertexCount();
  double defaultVal = hydraulicStructure->roughnessStructure()->defaultRoughness()->value();
  for ( int i = 0; i < size; ++i )
  {
    const QgsMeshVertex &vert = mesh.vertices.at( i );
    double val = roughness->value( vert.x(), vert.y(), false );
    if ( std::isnan( val ) )
      val = defaultVal;
    roughnessDataset->values[i] = val;
  }

  roughnessDataset->valid = true;
  roughnessDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> roughnessGroup( new QgsMeshMemoryDatasetGroup( "ROUGHNESS", QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  roughnessGroup->addDataset( roughnessDataset );
  roughnessGroup->initialize();

  ouputMesh->addDatasets( roughnessGroup.release() );
  ouputMesh->saveDataset( path, 1, QStringLiteral( "SELAFIN" ) );
}

struct TelemacBoundary
{
  int LIHBOR = 2;
  int LIUBOR = 2;
  int LIVBOR = 2;

  int LITBOR = 2;

  int vertIndex;
};

struct TelemacFlowCondition
{
  int rank = -1;
  QVector<double> timeInSeconds;
  QVector<double> value;
};

void ReosHydraulicSimulation::createBoundaryConditionFiles( ReosHydraulicStructure2D *hydraulicStructure )
{
  QVector<ReosHydraulicStructure2D::BoundaryVertices> boundSegments =  hydraulicStructure->boundaryVertices();
  // constraint for TElEMAC:
  // - Start from the vertices with X+Y is minimum
  // - counter clockwise

  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *> ( hydraulicStructure->mesh()->data() );
  if ( !meshLayer )
    return;

  const QgsMesh &mesh = *meshLayer->nativeMesh();

  //First find the segment that start with min(X+Y) and the Y min (X min if tied) to find the direction
  int startSegmentIndex = 0;
  double xpy = std::numeric_limits<double>::max();
  int minYSgementIndex = 0;
  double yMin = std::numeric_limits<double>::max();
  double assocXMin = std::numeric_limits<double>::max();
  int segCount = boundSegments.count();
  for ( int i = 0; i < segCount; ++i )
  {
    const ReosHydraulicStructure2D::BoundaryVertices &seg = boundSegments.at( i );
    int vertCount = seg.verticesIndex.count();
    if ( vertCount == 0 )
      continue;
    const QgsMeshVertex &vert = mesh.vertices.at( seg.verticesIndex.first() );
    double x = vert.x();
    double y = vert.y();
    double xy = x + y;
    if ( xy < xpy )
    {
      xpy = xy;
      startSegmentIndex = i;
    }

    if ( y < yMin || ( y == yMin && x < assocXMin ) )
    {
      minYSgementIndex = i;
      yMin = y;
      assocXMin = x;
    }
  }

  //find the direction
  const ReosHydraulicStructure2D::BoundaryVertices &seg = boundSegments.at( minYSgementIndex );
  const ReosHydraulicStructure2D::BoundaryVertices &segPrev = boundSegments.at( ( minYSgementIndex + segCount - 1 ) % segCount );
  const ReosHydraulicStructure2D::BoundaryVertices &segNext = boundSegments.at( ( minYSgementIndex +  1 ) % segCount );
  if ( seg.verticesIndex.isEmpty() || segPrev.verticesIndex.isEmpty() || segNext.verticesIndex.isEmpty() )
    return;

  const QgsMeshVertex vert = mesh.vertices.at( seg.verticesIndex.first() );
  const QgsMeshVertex vertPrev = mesh.vertices.at( segPrev.verticesIndex.first() );
  const QgsMeshVertex vertNext = mesh.vertices.at( segNext.verticesIndex.first() );

  const QgsVector vectPrev = vertPrev - vert;
  const QgsVector vectNext = vertNext - vert;

  double crossProduct = vectPrev.x() * vectNext.y() - vectNext.x() * vectPrev.y();

  bool invertDirection = crossProduct > 0;

  QVector<TelemacBoundary> telemacBoundaries;

  for ( int i = 0; i < segCount; ++i )
  {
    int segIndex = invertDirection ? segCount - i - 1 : i;
    const ReosHydraulicStructure2D::BoundaryVertices &seg = boundSegments.at( segIndex );
    int vertCount = seg.verticesIndex.count();

    for ( int j = 0; j < vertCount; ++j )
    {
      int vertIndex = seg.verticesIndex.at( invertDirection ? segCount - j - 1 : j );
      TelemacBoundary bound;
      if ( !seg.boundaryCondition.isNull() )
      {
        switch ( seg.boundaryCondition->conditionType() )
        {
          case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
            bound.LIHBOR = 4;
            bound.LIUBOR = 5;
            bound.LIVBOR = 5;
            bound.LITBOR = 4;
            break;
          case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
            bound.LIHBOR = 4;
            bound.LIUBOR = 4;
            bound.LIVBOR = 4;
            bound.LITBOR = 4;
            break;
        }
      }
      bound.vertIndex = vertIndex + 1;
      telemacBoundaries.append( bound );
    }
  }

  int boundCount = telemacBoundaries.count();
  if ( invertDirection )
  {
    for ( int i = 0; i < ( boundCount - 1 ) / 2 ; ++i )
      std::swap( telemacBoundaries[i + 1], telemacBoundaries[boundCount - 1 - i] );
  }

  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mBoundaryFileName );
  QFile file( path );

  file.open( QIODevice::WriteOnly );

  QTextStream stream( &file );
  const QString templateLine = QStringLiteral( "%1 %2 %3 0.0 0.0 0.0 0.0 %4 0.0 0.0 0.0 %5 %6\n" );

  for ( int i = 0; i < boundCount; ++i )
  {
    const TelemacBoundary &bound = telemacBoundaries.at( i );
    stream << templateLine.arg( QString::number( bound.LIHBOR ), QString::number( bound.LIUBOR ), QString::number( bound.LIVBOR ),
                                QString::number( bound.LITBOR ), QString::number( bound.vertIndex ), QString::number( i + 1 ) );
  }
}

void ReosHydraulicSimulation::createSteeringFile( ReosHydraulicStructure2D *hydraulicStructure )
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mSteeringFileName );
  QFile file( path );

  file.open( QIODevice::WriteOnly );
  QTextStream stream( &file );

  stream << QStringLiteral( "/---------------------------------------------------------------------\n" );
  stream << QStringLiteral( "/ File created by Lekan\n" );
  stream << QStringLiteral( "/---------------------------------------------------------------------\n" );
  stream << QStringLiteral( "\n" );
  stream << QStringLiteral( "BOUNDARY CONDITIONS FILE : '%1'\n" ).arg( mBoundaryFileName );
  stream << QStringLiteral( "LIQUID BOUNDARIES FILE : '%1'\n" ).arg( mBoundaryConditionFileName );
  stream << QStringLiteral( "GEOMETRY FILE : '%1'\n" ).arg( mGeomFileName );
  stream << QStringLiteral( "RESULTS FILE : '%1'\n" ).arg( mResultFileName );
  stream << QStringLiteral( "TITLE : '%1'\n" ).arg( hydraulicStructure->elementName()->value() );
  stream << QStringLiteral( "COMPUTATION CONTINUED = NO\n" );
  stream << QStringLiteral( "INITIAL TIME SET TO ZERO = YES\n" );

  ReosDuration totalDuration( mStartTime->value().msecsTo( mEndTime->value() ), ReosDuration::millisecond );
  int timeStepCount = totalDuration.numberOfFullyContainedIntervals( mTimeStep->value() );

  stream << QStringLiteral( "TIME STEP = %1\n" ).arg( QString::number( totalDuration.valueSecond(), 'f', 2 ) );
  stream << QStringLiteral( "NUMBER OF TIME STEPS = %1\n" ).arg( QString::number( timeStepCount ) );
  stream << QStringLiteral( "GRAPHIC PRINTOUT PERIOD : 10\n" );
  stream << QStringLiteral( "LISTING PRINTOUT PERIOD : 20\n" );

  stream << QStringLiteral( "SOLVER ACCURACY = 1.E-6\n" );
  stream << QStringLiteral( "VARIABLES FOR GRAPHIC PRINTOUTS : 'S,U,V,B,H,W,US,MAXZ,MAXV'\n" );
  stream << QStringLiteral( "LAW OF BOTTOM FRICTION = 4\n" );
}

ReosParameterDateTime *ReosHydraulicSimulation::startTime() const
{
  return mStartTime;
}

ReosParameterDateTime *ReosHydraulicSimulation::endTime() const
{
  return mEndTime;
}

ReosParameterDuration *ReosHydraulicSimulation::timeStep() const
{
  return mTimeStep;
}
