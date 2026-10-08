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

#include "reoshydrograph.h"
#include "reostimeseriesgroup.h"
#include "reostelemac2dsimulation.h"


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
{
  return ReosEncodedElement( QStringLiteral( "Telemac importer" ) );
}

void ReosTelemacStructureImporterSource::setReferenceTime( const QDateTime &referenceTime )
{
  mReferenceTime = referenceTime;
}

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

const ReosMeshFrameData &ReosTelemacStructureImporter::meshData() const
{}

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

  QDateTime referenceTime = mSteeringFile.referenceTime();
  ReosDuration simDuration = mSteeringFile.duration();
  if ( !referenceTime.isValid() )
    referenceTime = mSource->referenceTime();
  QList<double> constantFlowRates = mSteeringFile.prescribedFlowRate();
  QList<double> constantElevations = mSteeringFile.prescibedElevation();

  ReosTelemacLiquidBoundaries liquidBoundaries( unquote( mSteeringFile.value( "LIQUID BOUNDARIES FILE" ) ) );
  for ( qsizetype i = 0; i < ret.count(); ++i )
  {
    ReosHydraulicStructureBoundaryCondition *bc = ret.at( i );

    switch ( bc->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
      {
        ReosTelemacLiquidBoundaries::TelemacLiquidBoundary *liquidBc = liquidBoundaries.boundaryCondition( i + 1, ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
        std::unique_ptr<ReosHydrograph> hydrograph( new ReosHydrograph() );
        hydrograph->setReferenceTime( mSource->referenceTime() );
        if ( liquidBc && !liquidBc->time.isEmpty() && !liquidBc->value.isEmpty() )
        {
          for ( qsizetype j = 0; j < liquidBc->time.count(); ++j )
            hydrograph->setValue( ReosDuration( liquidBc->time.at( j ), ReosDuration::second ), liquidBc->value.at( j ) );
        }
        else
        {
          hydrograph->setValue( ReosDuration( 0, ReosDuration::second ), constantFlowRates.at( i ) );
          hydrograph->setValue( simDuration, constantFlowRates.at( i ) );
        }

        bc->gaugedHydrographsStore()->addHydrograph( hydrograph.release() );
        bc->setInternalHydrographOrigin( ReosHydrographJunction::GaugedHydrograph );
        bc->setGaugedHydrographIndex( 0 );
      }
      break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
      {
        ReosTelemacLiquidBoundaries::TelemacLiquidBoundary *liquidBc = liquidBoundaries.boundaryCondition( i + 1, ReosHydraulicStructureBoundaryCondition::Type::OutputLevel );
        if ( liquidBc && !liquidBc->time.isEmpty() && !liquidBc->value.isEmpty() )
        {
          std::unique_ptr<ReosTimeSeriesVariableTimeStep> levelSeries( new ReosTimeSeriesVariableTimeStep );
          for ( qsizetype j = 0; j < liquidBc->time.count(); ++j )
            levelSeries->setValue( ReosDuration( liquidBc->time.at( j ), ReosDuration::second ), liquidBc->value.at( j ) );

          bc->waterLevelSeriesGroup()->addTimeSeries( levelSeries.release() );
          bc->setWaterLevelSeriesIndex( 0 );
          bc->isWaterLevelConstant()->setValue( false );
        }
        else
        {
          bc->isWaterLevelConstant()->setValue( true );
          bc->constantWaterElevation()->setValue( constantElevations.at( i ) );
        }
      }
      break;
    }
  }

  return ret;
}

QList<ReosHydraulicSimulation *> ReosTelemacStructureImporter::createSimulations( ReosHydraulicStructure2D *parent ) const
{
  ReosTelemac2DSimulation *simulation = new ReosTelemac2DSimulation( parent );

  simulation->setEquation( mSteeringFile.equation() );
  simulation->timeStep()->setValue( mSteeringFile.timeStep() );
  simulation->outputPeriodResult2D()->setValue( mSteeringFile.outputPeriodResult2D() );
  simulation->outputPeriodResultHydrograph()->setValue( mSteeringFile.outputPeriodResultHydrograph() );

  simulation->outputPeriodResult2D()->setValue( mSteeringFile.outputPeriodResult2D() );
  simulation->outputPeriodResultHydrograph()->setValue( mSteeringFile.outputPeriodResultHydrograph() );

  simulation->courantNumber()->setValue( mSteeringFile.courantNumber() );

  simulation->setInitialCondition( mSteeringFile.initialConditionType() );

  simulation->setGeomFileName( mSteeringFile.geomFileName() );
  simulation->setResultFileName( mSteeringFile.resultFileName() );
  simulation->setBoundaryFileName( mSteeringFile.boundaryFileName() );
  simulation->setBoundaryLiquidFileName( mSteeringFile.boundaryFileName() );

  return QList<ReosHydraulicSimulation *>( { simulation } );
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


  // here we should reconstruct the bondary AND the holes to fill ReosMeshFrameData
  QList<QSet<int>> verticesToFaces;
  verticesToFaces.fill( QSet<int>(), mGeometryMesh->vertexCount() );
  QList<QSet<int>> facesToVertices;
  facesToVertices.fill( QSet<int>(), mGeometryMesh->faceCount() );

  const QVector<QVector<int>> &faces = mGeometryMesh->faces();
  for ( qsizetype f = 0; f < faces.count(); ++f )
  {
    const QVector<int> &face = faces.at( f );
    for ( qsizetype v = 0; v < face.count(); ++v )
    {
      int vertexIndex = face.at( v );
      if ( vertexIndex >= 0 && vertexIndex < verticesToFaces.count() )
      {
        verticesToFaces[vertexIndex].insert( f );
        facesToVertices[f].insert( vertexIndex );
      }
    }
  }

  ReosMeshFrameData meshData;

  QList<int> allBoundaryVertices = mBoundaries.boundaryVertexIndexes();

  int first = allBoundaryVertices.first();
  int prev = first;
  allBoundaryVertices.pop_front();
  QList<int> exteriorVertices;
  QVector<QVector<int>> holes;
  bool exteriorFound = false;
  exteriorVertices.append( first );
  int currentVertex = allBoundaryVertices.first();
  allBoundaryVertices.pop_front();
  return;
  while ( !allBoundaryVertices.empty() )
  {
    const QSet<int> &relatedFaces = verticesToFaces.at( prev );
    for ( int f : relatedFaces )
    {
      if ( facesToVertices.at( f ).contains( currentVertex ) )
      {
        prev = currentVertex;
        if ( !exteriorFound )
          exteriorVertices.append( currentVertex );
        else
          holes.last().append( currentVertex );

        currentVertex = allBoundaryVertices.first();
        allBoundaryVertices.pop_front();
        if ( currentVertex == first )
        {
          exteriorFound = true;
          holes.append( QVector<int>() );
          break;
        }
      }
    }
  }

  int a = 1;
}
