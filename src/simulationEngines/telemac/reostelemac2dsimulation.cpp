/***************************************************************************
  reostelemac2dsimulation.cpp - ReosTelemac2DSimulation

 ---------------------
 begin                : 31.3.2022
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
#include "reostelemac2dsimulation.h"

#include <QDir>
#include <QProcess>

#include <qgsmeshlayer.h>
#include <qgsmeshtriangulation.h>

#include <mdal.h>

#include "reoshydraulicstructure2d.h"
#include "reossimulationinitialcondition.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoscalculationcontext.h"
#include "reostelemac2dsimulationresults.h"

ReosTelemac2DSimulation::ReosTelemac2DSimulation( const ReosEncodedElement &element, QObject *parent )
  : ReosHydraulicSimulation( parent )
{
  ReosDataObject::decode( element );
  mTimeStep = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ), false, tr( "Time step" ), this );
  mOutputResultPeriod = ReosParameterInteger::decode( element.getEncodedData( "output-period" ), false, tr( "Output period" ), this );
  mInitialCondition = new ReosSimulationInitialConditions( element.getEncodedData( "initial-condition" ), this );
  int equation = 0;
  element.getData( QStringLiteral( "equation" ), equation );
  mEquation = static_cast<ReosTelemac2DSimulation::Equation>( equation );
}

ReosEncodedElement ReosTelemac2DSimulation::encode() const
{
  ReosEncodedElement element( QStringLiteral( "telemac-2d-simulation" ) );
  element.addData( QStringLiteral( "key" ), key() );

  element.addEncodedData( QStringLiteral( "time-step" ), mTimeStep->encode() );
  element.addEncodedData( QStringLiteral( "output-period" ), mOutputResultPeriod->encode() );
  element.addEncodedData( QStringLiteral( "initial-condition" ), mInitialCondition->encode() );
  element.addData( QStringLiteral( "equation" ), static_cast<int>( mEquation ) );

  ReosDataObject::encode( element );
  return element;
}


ReosTelemac2DSimulation::ReosTelemac2DSimulation( QObject *parent )
  : ReosHydraulicSimulation( parent )
{
  mTimeStep = new ReosParameterDuration( tr( "Time step" ), false, this );
  mTimeStep->setValue( ReosDuration( 30, ReosDuration::second ) );

  mOutputResultPeriod = new ReosParameterInteger( tr( "Output period" ), false, this );
  mOutputResultPeriod->setValue( 5 );


  mInitialCondition = new ReosSimulationInitialConditions( this );
}


REOSEXTERN ReosSimulationEngineFactory *engineSimulationFactory()
{
  return new ReosTelemac2DSimulationEngineFactory();
}

ReosHydraulicSimulation *ReosTelemac2DSimulationEngineFactory::createSimulation( QObject *parent ) const
{
  return new ReosTelemac2DSimulation( parent );
}

ReosHydraulicSimulation *ReosTelemac2DSimulationEngineFactory::createSimulation( const ReosEncodedElement &element, QObject *parent ) const
{
  if ( element.description() == QStringLiteral( "telemac-2d-simulation" ) )
    return new  ReosTelemac2DSimulation( element, parent );
  else
    return new ReosTelemac2DSimulation( parent );
}

ReosParameterInteger *ReosTelemac2DSimulation::outputResultPeriod() const
{
  return mOutputResultPeriod;
}

ReosParameterDuration *ReosTelemac2DSimulation::timeStep() const
{
  return mTimeStep;
}

ReosSimulationInitialConditions *ReosTelemac2DSimulation::initialCondition() const
{
  return mInitialCondition;
}

ReosTelemac2DSimulation::Equation ReosTelemac2DSimulation::equation() const
{
  return mEquation;
}

void ReosTelemac2DSimulation::setEquation( const Equation &equation )
{
  mEquation = equation;
}

bool ReosTelemac2DSimulation::hasResult( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext & ) const
{
  QDir dir = hydraulicStructure->structureDirectory();
  if ( !dir.cd( mDirName ) )
    return false;

  const QFileInfo fileInfo( dir.filePath( mResultFileName ) );

  return fileInfo.exists();
}

ReosHydraulicSimulationResults *ReosTelemac2DSimulation::createResults( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext & ) const
{
  QDir dir = hydraulicStructure->structureDirectory();
  if ( !dir.cd( mDirName ) )
    return nullptr;
  return new ReosTelemac2DSimulationResults( this, hydraulicStructure->mesh(),  dir.filePath( mResultFileName ), hydraulicStructure );
}

void ReosTelemac2DSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &context )
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.mkdir( mDirName );
  dir.cd( mDirName );
  QVector<int> verticesPosInBoundary;
  QList<ReosHydraulicStructureBoundaryCondition *> boundaryCondition = createBoundaryFiles( hydraulicStructure, verticesPosInBoundary );
  createSelafinInputGeometry( hydraulicStructure, verticesPosInBoundary );
  createBoundaryConditionFiles( hydraulicStructure, boundaryCondition, context );
  createSteeringFile( hydraulicStructure, boundaryCondition, context );

  QFileInfo fileInfo( dir.filePath( mResultFileName ) );

  QString fn = dir.filePath( mResultFileName );

  if ( fileInfo.exists() )
    QFile::remove( dir.filePath( mResultFileName ) );
}

ReosSimulationProcess *ReosTelemac2DSimulation::getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );

  return new ReosTelemac2DSimulationProcess( calculationContext, mTimeStep->value(), dir.path() );
}

struct TelemacBoundary
{
  int LIHBOR = 2;
  int LIUBOR = 2;
  int LIVBOR = 2;

  int LITBOR = 2;

  int vertIndex;
};

QList<ReosHydraulicStructureBoundaryCondition *> ReosTelemac2DSimulation::createBoundaryFiles( ReosHydraulicStructure2D *hydraulicStructure, QVector<int> &verticesPosInBoundary )
{
  QVector<ReosHydraulicStructure2D::BoundaryVertices> boundSegments =  hydraulicStructure->boundaryVertices();
  // constraint for TElEMAC:
  // - Start from the vertices with X+Y is minimum
  // - counter clockwise

  ReosMesh *rmesh = hydraulicStructure->mesh();

  if ( !rmesh )
    return QList<ReosHydraulicStructureBoundaryCondition *>();

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
    const QPointF vert = rmesh->vertexPosition( seg.verticesIndex.first() );
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
    return QList<ReosHydraulicStructureBoundaryCondition *>();

  const QPointF vert = rmesh->vertexPosition( seg.verticesIndex.first() );
  const QPointF vertPrev = rmesh->vertexPosition( segPrev.verticesIndex.first() );
  const QPointF vertNext = rmesh->vertexPosition( segNext.verticesIndex.first() );

  const QPointF vectPrev = vertPrev - vert;
  const QPointF vectNext = vertNext - vert;

  double crossProduct = vectPrev.x() * vectNext.y() - vectNext.x() * vectPrev.y();

  bool invertDirection = crossProduct > 0;

  bool prevIsDefined = false;
  QVector<TelemacBoundary> telemacBoundaries;
  QList<ReosHydraulicStructureBoundaryCondition *> ret;
  for ( int i = 0; i < segCount; ++i )
  {
    int segIndex = ( startSegmentIndex + i ) % segCount;
    const ReosHydraulicStructure2D::BoundaryVertices &seg = boundSegments.at( segIndex );
    int vertCount = seg.verticesIndex.count();
    if ( !seg.boundaryCondition.isNull() &&
         ( ret.isEmpty() || seg.boundaryCondition != ret.last() ) )
      ret.append( seg.boundaryCondition );

    for ( int j = 0; j < vertCount; ++j )
    {
      int vertIndex = seg.verticesIndex.at( j );
      TelemacBoundary bound;
      if ( prevIsDefined )
      {
        bound = telemacBoundaries.last();
        prevIsDefined = false;
      }
      else if ( !seg.boundaryCondition.isNull() )
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
            bound.LIHBOR = 5;
            bound.LIUBOR = 4;
            bound.LIVBOR = 4;
            bound.LITBOR = 4;
            break;
          case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
            break;
        }
      }
      bound.vertIndex = vertIndex + 1;
      telemacBoundaries.append( bound );
    }
    prevIsDefined = !seg.boundaryCondition.isNull();
  }

  if ( prevIsDefined ) //need to set the first point as a boundary
  {
    int vertexIndex = telemacBoundaries.first().vertIndex;
    telemacBoundaries.first() = telemacBoundaries.last();
    telemacBoundaries.first().vertIndex = vertexIndex;
  }

  int boundCount = telemacBoundaries.count();
  if ( invertDirection )
  {
    for ( int i = 0; i < ( boundCount - 1 ) / 2 ; ++i )
      std::swap( telemacBoundaries[i + 1], telemacBoundaries[boundCount - 1 - i] );

    for ( int i = 0; i < ret.count() / 2; ++i )
      std::swap( ret[i], ret[ret.count() - 1 - i] );
  }

  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mBoundaryFileName );
  QFile file( path );

  file.open( QIODevice::WriteOnly );

  QTextStream stream( &file );
  const QString templateLine = QStringLiteral( "%1 %2 %3 0.0 0.0 0.0 0.0 %4 0.0 0.0 0.0 %5 %6\n" );

  verticesPosInBoundary = QVector( rmesh->vertexCount(), 0 );
  for ( int i = 0; i < boundCount; ++i )
  {
    const TelemacBoundary &bound = telemacBoundaries.at( i );
    stream << templateLine.arg( QString::number( bound.LIHBOR ), QString::number( bound.LIUBOR ), QString::number( bound.LIVBOR ),
                                QString::number( bound.LITBOR ), QString::number( bound.vertIndex ), QString::number( i + 1 ) );
    verticesPosInBoundary[bound.vertIndex - 1] = i + 1;
  }

  return ret;
}

template<typename T>
void writeValue( T &value, QDataStream &out, bool changeEndianness = false )
{
  T v = value;
  char *const p = reinterpret_cast<char *>( &v );

  if ( changeEndianness )
    std::reverse( p, p + sizeof( T ) );

  out.writeRawData( p, sizeof( T ) );
}

static bool isNativeLittleEndian()
{
  int n = 1;
  return ( *( char * )&n == 1 );
}

template<typename T>
static void writeValue( QDataStream &stream, T value )
{
  writeValue( value, stream, isNativeLittleEndian() );
}

static void writeInt( QDataStream &stream, int i )
{
  writeValue( i, stream, isNativeLittleEndian() );
}

template<typename T>
static void writeValueArrayRecord( QDataStream &stream, const QVector<T> &array )
{
  writeValue( stream, int( array.size()*sizeof( T ) ) );
  for ( const T value : array )
    writeValue( stream, value );
  writeValue( stream, int( array.size()*sizeof( T ) ) );
}

static void writeStringRecord( QDataStream &stream, const QString &str )
{
  writeInt( stream,  str.count() );
  stream.writeRawData( str.toStdString().c_str(), str.count() );
  writeInt( stream, str.count() );
}


static void setCounterClockwise( QVector<int> &triangle, const QPointF &v0, const QPointF &v1, const QPointF &v2 )
{
  //To have consistent clock wise orientation of triangles which is necessary for 3D rendering
  //Check the clock wise, and if it is not counter clock wise, swap indexes to make the oientation counter clock wise
  double ux = v1.x() - v0.x();
  double uy = v1.y() - v0.y();
  double vx = v2.x() - v0.x();
  double vy = v2.y() - v0.y();

  double crossProduct = ux * vy - uy * vx;
  if ( crossProduct < 0 ) //CW -->change the orientation
  {
    std::swap( triangle[1], triangle[2] );
  }
}


void ReosTelemac2DSimulation::createSelafinMeshFrame( ReosHydraulicStructure2D *hydraulicStructure, const QVector<int> &verticesPosInBoundary )
{
  // MDAL does not handle the boundaries. As the parrallel calculation in Telemac need to now about the boundaies vertices,
  // wa can't iuse MDAL to create the mesh frame file. Here we use the same logic as MDAL but we add the boundaries vertices indexes

  ReosMesh *rmesh = hydraulicStructure->mesh();
  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mGeomFileName );

  QFile file( path );
  file.open( QIODevice::WriteOnly );
  QDataStream stream( &file );

  QString header( "Selafin file created by Lekan" );
  int remainingSpace = 72 - header.size();
  QString remainingString;
  remainingString.fill( ' ', remainingSpace );
  header.append( remainingString );
  header.append( "SERAFIND" );
  Q_ASSERT( header.size() == 80 );
  writeStringRecord( stream, header );

  // NBV(1) NBV(2) size
  QVector<int> nbvSize( 2 );
  nbvSize[0] = 0;
  nbvSize[1] = 0;
  writeValueArrayRecord( stream, nbvSize );

  //don't write variable name

  //parameter table, all values are 0
  QVector<int> param( 10, 0 );
  writeValueArrayRecord( stream, param );

  //NELEM,NPOIN,NDP,1
  int verticesPerFace = 3;
  int verticesCount = rmesh->vertexCount();
  int facesCount = rmesh->faceCount();
  QVector<int> elem( 4 );
  elem[0] = facesCount;
  elem[1] = verticesCount;
  elem[2] = verticesPerFace ;
  elem[3] = 1;
  writeValueArrayRecord( stream, elem );

  //connectivity table
  writeInt( stream, facesCount * verticesPerFace * 4 );

  for ( int i = 0; i < facesCount; ++i )
  {
    QVector<int> face = rmesh->face( i );
    setCounterClockwise( face, rmesh->vertexPosition( face.at( 0 ) ), rmesh->vertexPosition( face.at( 1 ) ), rmesh->vertexPosition( face.at( 2 ) ) );
    for ( int f : std::as_const( face ) )
      writeInt( stream, f + 1 );

  }
  writeInt( stream, facesCount * verticesPerFace * 4 );

  writeValueArrayRecord( stream, verticesPosInBoundary );

  //Vertices
  QVector<double> xValues( verticesCount );
  QVector<double>  yValues( verticesCount );
  for ( int i = 0; i < verticesCount; ++i )
  {
    const QPointF vert = rmesh->vertexPosition( i );
    xValues[i] = vert.x();
    yValues[i] = vert.y();
  }

  writeValueArrayRecord( stream, xValues );
  writeValueArrayRecord( stream, yValues );

  file.close();
}

void ReosTelemac2DSimulation::createSelafinInputGeometry( ReosHydraulicStructure2D *hydraulicStructure, const QVector<int> &verticesPosInBoundary )
{
  createSelafinMeshFrame( hydraulicStructure, verticesPosInBoundary );

  // TODO :: replace below by  writing directly on the file without MDAL or QGIS. Indeed, no so much to do more than the method aboce
  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *> ( hydraulicStructure->mesh()->data() );
  if ( !meshLayer )
    return;

  const QgsMesh &mesh = *meshLayer->nativeMesh();

  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mGeomFileName );

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
    roughnessDataset->values[i] = 1 / val;
  }

  roughnessDataset->valid = true;
  roughnessDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> roughnessGroup( new QgsMeshMemoryDatasetGroup( "ROUGHNESS", QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  roughnessGroup->addDataset( roughnessDataset );
  roughnessGroup->initialize();

  ouputMesh->addDatasets( roughnessGroup.release() );
  ouputMesh->saveDataset( path, 1, QStringLiteral( "SELAFIN" ) );
}

struct TelemacBoundaryCondition
{
  int rank = -1;
  QString header;
  QString unit;
  ReosTimeSerieVariableTimeStep *timeSeries = nullptr;
};

void ReosTelemac2DSimulation::createBoundaryConditionFiles( ReosHydraulicStructure2D *hydraulicStructure, QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions, const ReosCalculationContext &context )
{
  QSet<qint64> timeSteps;
  QList<TelemacBoundaryCondition> boundConds;
  const QDateTime startTime = context.simulationStartTime();
  const QDateTime endTime = context.simulationEndTime();

  for ( int i = 0; i < boundaryConditions.count(); ++i )
  {
    ReosHydraulicStructureBoundaryCondition *boundCond = boundaryConditions.at( i );
    TelemacBoundaryCondition bc;

    switch ( boundCond->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        bc.header = QStringLiteral( "Q(%1)" );
        bc.unit = QStringLiteral( "m3/s" );
        bc.timeSeries = boundCond->outputHydrograph();
        bc.rank = i + 1;
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
        bc.header = QStringLiteral( "SL(%1)" );
        bc.unit = QStringLiteral( "m" );
        bc.timeSeries = boundCond->waterLevelSeries();
        bc.rank = i + 1;
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
        continue;
        break;
    }

    if ( bc.timeSeries )
    {
      int valueCount = bc.timeSeries->valueCount();
      int valIndex = 0;
      while ( valIndex < valueCount && bc.timeSeries->timeAt( valIndex ) < startTime )
        valIndex++;

      while ( valIndex < valueCount && bc.timeSeries->timeAt( valIndex ) <= endTime )
      {
        timeSteps.insert( startTime.msecsTo( bc.timeSeries->timeAt( valIndex ) ) );
        valIndex++;
      }

      boundConds.append( bc );
    }
  }

  QList<qint64> timeStepsList = timeSteps.values();
  std::sort( timeStepsList.begin(), timeStepsList.end() );

  if ( !timeStepsList.isEmpty() && timeStepsList.first() == 0 )
    timeStepsList.removeFirst();

  if ( !timeStepsList.isEmpty() && timeStepsList.last() == startTime.msecsTo( endTime ) )
    timeStepsList.removeLast();

  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mBoundaryConditionFileName );
  QFile file( path );
  file.open( QIODevice::WriteOnly );
  QTextStream stream( &file );

  stream << "T";
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    stream << "\t" <<  bc.header.arg( QString::number( bc.rank ) );
  stream << "\n";

  stream << "s";
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    stream << "\t" <<  bc.unit;
  stream << "\n";

  stream << QString::number( 0, 'f', 6 );
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    stream << "\t" << QString::number( bc.timeSeries->valueAtTime( startTime ), 'f', 2 );
  stream << "\n";

  for ( qint64 tms : std::as_const( timeStepsList ) )
  {
    stream << QString::number( tms / 1000.0, 'f', 6 );
    for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
      stream << "\t" <<  QString::number( bc.timeSeries->valueAtTime( startTime.addMSecs( tms ) ), 'f', 2 );
    stream << "\n";
  }

  stream << QString::number( startTime.msecsTo( endTime ) / 1000.0, 'f', 6 );
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    stream <<  "\t" << QString::number( bc.timeSeries->valueAtTime( endTime ), 'f', 2 );
  stream << "\n";

}

void ReosTelemac2DSimulation::createSteeringFile( ReosHydraulicStructure2D *hydraulicStructure, QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions, const ReosCalculationContext &context )
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
  stream << QStringLiteral( "VARIABLES FOR GRAPHIC PRINTOUTS : 'S,U,V,B,H,W,US,MAXZ,MAXV'\n" );

  // Time parameters
  ReosDuration totalDuration( context.simulationStartTime().msecsTo( context.simulationEndTime() ), ReosDuration::millisecond );
  int timeStepCount = totalDuration.numberOfFullyContainedIntervals( mTimeStep->value() );
  stream << QStringLiteral( "COMPUTATION CONTINUED : NO\n" );
  QDate startDate = context.simulationStartTime().date();
  stream << QStringLiteral( "ORIGINAL DATE OF TIME : %1;%2;%3\n" ).arg( QString::number( startDate.year() ),  QString::number( startDate.month() ),  QString::number( startDate.day() ) );
  QTime startTime = context.simulationStartTime().time();
  stream << QStringLiteral( "ORIGINAL HOUR OF TIME : %1;%2;%3\n" ).arg( QString::number( startTime.hour() ),  QString::number( startTime.minute() ),  QString::number( startTime.second() ) );
  stream << QStringLiteral( "INITIAL TIME SET TO ZERO : YES\n" );
  stream << QStringLiteral( "TIME STEP : %1\n" ).arg( QString::number( mTimeStep->value().valueSecond(), 'f', 2 ) );
  stream << QStringLiteral( "NUMBER OF TIME STEPS : %1\n" ).arg( QString::number( timeStepCount ) );
  stream << QStringLiteral( "GRAPHIC PRINTOUT PERIOD : %1\n" ).arg( QString::number( mOutputResultPeriod->value() ) );
  stream << QStringLiteral( "LISTING PRINTOUT PERIOD : %1\n" ).arg( QString::number( mOutputResultPeriod->value() ) );

  //Physical parametres
  stream << QStringLiteral( "LAW OF BOTTOM FRICTION : 3\n" );
  stream << QStringLiteral( "FRICTION COEFFICIENT : 20\n" );

  //Boundary condition
  QStringList prescribedFlow;
  QStringList prescribedElevation;
  QStringList velocityProfile;
  for ( ReosHydraulicStructureBoundaryCondition *bc : boundaryConditions )
  {
    switch ( bc->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
        prescribedFlow.append( QString( '0' ) );
        prescribedElevation.append( QString( '0' ) );
        velocityProfile.append( QString( '1' ) );
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        prescribedFlow.append( QString( '1' ) );
        prescribedElevation.append( QString( '0' ) );
        velocityProfile.append( QString( '4' ) );
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
        prescribedFlow.append( QString( '0' ) );
        prescribedElevation.append( QString( '1' ) );
        velocityProfile.append( QString( '1' ) );
        break;
    }
  }
  stream << QStringLiteral( "PRESCRIBED FLOWRATES : %1\n" ).arg( prescribedFlow.join( ';' ) );
  stream << QStringLiteral( "VELOCITY PROFILES : %1\n" ).arg( velocityProfile.join( ';' ) );
  stream << QStringLiteral( "PRESCRIBED ELEVATIONS : %1\n" ).arg( prescribedElevation.join( ';' ) );

  //Initial condition
  stream << QStringLiteral( "INITIAL CONDITIONS : 'CONSTANT ELEVATION'\n" );
  stream << QStringLiteral( "INITIAL ELEVATION : %1\n" ).arg( QString::number( mInitialCondition->initialWaterLevel()->value(), 'f', 2 ) );

  //Numerical parameters
  switch ( mEquation )
  {
    case ReosTelemac2DSimulation::Equation::FiniteVolume:
      stream << QStringLiteral( "EQUATIONS: 'SAINT-VENANT FV'\n" );
      stream << QStringLiteral( "DESIRED COURANT NUMBER : 0.9\n" );
      stream << QStringLiteral( "VARIABLE TIME-STEP : YES\n" );
      break;
    case ReosTelemac2DSimulation::Equation::FiniteElement:
      stream << QStringLiteral( "EQUATIONS: 'SAINT-VENANT FE'\n" );
      stream << QStringLiteral( "SCHEME FOR ADVECTION OF VELOCITIES : 1\n" );
      stream << QStringLiteral( "IMPLICITATION FOR DEPTH : 0.6 \n" );
      stream << QStringLiteral( "IMPLICITATION FOR VELOCITY : 0.6 \n" );
      stream << QStringLiteral( "MAXIMUM NUMBER OF ITERATIONS FOR ADVECTION SCHEMES : 100 \n" );
      stream << QStringLiteral( "MASS-LUMPING ON H : 1.0\n" );
      stream << QStringLiteral( "MASS-LUMPING ON VELOCITY : 1.0\n" );
      stream << QStringLiteral( "SUPG OPTION : 1;1\n" );
      break;
  }

  stream << QStringLiteral( "DISCRETIZATIONS IN SPACE : 11 ; 11\n" );
  stream << QStringLiteral( "FREE SURFACE GRADIENT COMPATIBILITY : 0.9\n" );
  stream << QStringLiteral( "CONTINUITY CORRECTION : YES\n" );

  stream << QStringLiteral( "TREATMENT OF THE LINEAR SYSTEM : 2\n" );
  stream << QStringLiteral( "SOLVER : 1\n" );
  stream << QStringLiteral( "SOLVER ACCURACY : 1.E-4\n" );
  stream << QStringLiteral( "INFORMATION ABOUT SOLVER : YES\n" );
  stream << QStringLiteral( "MASS-BALANCE : YES\n" );
  stream << QStringLiteral( "MATRIX STORAGE : 3\n" );
}




ReosTelemac2DSimulationProcess::ReosTelemac2DSimulationProcess( const ReosCalculationContext &context, const ReosDuration &timeStep, QString simulationfilePath )
  : mSimulationFilePath( simulationfilePath )
  , mTimeStep( timeStep )
{
  mTotalTime = context.simulationStartTime().msecsTo( context.simulationEndTime() ) / 1000.0;
}

void ReosTelemac2DSimulationProcess::start()
{
  mProcess = new QProcess();
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert( "HOMETE", "/opt/telemac/" );
  env.insert( "PATH", QStringLiteral( "/opt/telemac/scripts/python3:.:%1" ).arg( env.value( "PATH" ) ) );
  env.insert( "SYSTELCFG", "/opt/telemac/configs/systel.vcl-ubuntu.cfg" );
  env.insert( "USETELCFG", "ubugfmpich2" );
  env.insert( "SOURCEFILE", "/opt/telemac/configs/pysource.vcl.sh" );
  env.insert( "PYTHONUNBUFFERED",  "'true'" );
  env.insert( "PYTHONPATH", QStringLiteral( "/opt/telemac/scripts/python3:%1" ).arg( env.value( "PYTHONPATH" ) ) );
  env.insert( "LD_LIBRARY_PATH", QStringLiteral( "/opt/telemac/builds/ubugfmpich2/wrap_api/lib:%1" ).arg( env.value( "LD_LIBRARY_PATH" ) ) );
  env.insert( "PYTHONPATH", QStringLiteral( "/opt/telemac/builds/ubugfmpich2/wrap_api/lib:%1" ).arg( env.value( "PYTHONPATH" ) ) );
  mProcess->setProcessEnvironment( env );

  mProcess->setWorkingDirectory( mSimulationFilePath );

  QString script( QStringLiteral( "python3" ) );
  QStringList arguments;
  arguments << QStringLiteral( "/opt/telemac/scripts/python3/telemac2d.py" )
            << QStringLiteral( "simulation.cas" )
            <<  QStringLiteral( "--ncsize=16" );

  mRegEx = QRegularExpression( QStringLiteral( "(ITERATION +[a-zA-Z0-9]+ +TIME:[\\ 0-9a-zA-Z.()]+)" ) );
  mIsPreparation = true;
  setMaxProgression( 100 );
  setCurrentProgression( 0 );
  connect( mProcess, &QProcess::readyReadStandardOutput, mProcess, [this]
  {
    if ( mProcess )
    {
      addToOutput( mProcess->readAll() );
    }
  } );

  mProcess->start( script, arguments );

  bool resultStart = mProcess->waitForStarted();
  bool finished = false;
  if ( resultStart )
  {
    finished = mProcess->waitForFinished( -1 );
    setCurrentProgression( 100 );
    emit sendInformation( mStandartOutputBuffer );
  }
  else
    emit sendInformation( tr( "Telemac simulation can't start. Check the configuration of the Telemac modele." ) );

  if ( mProcess )
    mProcess->deleteLater();
  mProcess = nullptr;

  mIsSuccessful = finished;// need to check the output ot search for an error
}

void ReosTelemac2DSimulationProcess::stop( bool )
{
  emit askToStop();
}

void ReosTelemac2DSimulationProcess::onStopAsked()
{
  if ( mProcess )
  {
    mProcess->terminate();
  }
}

void ReosTelemac2DSimulationProcess::addToOutput( const QString &txt )
{
  mStandartOutputBuffer.append( txt );
  QRegularExpressionMatch match;
  match = mRegEx.match( mStandartOutputBuffer );
  QString message;

  if ( mIsPreparation )
    emit sendInformation( txt );

  while ( match.hasMatch() )
  {
    mIsPreparation = false;
    message = match.captured();
    extractCurrentTime( message );
    mStandartOutputBuffer = mStandartOutputBuffer.mid( match.capturedEnd() );
    match = mRegEx.match( mStandartOutputBuffer );
  }
}

void ReosTelemac2DSimulationProcess::extractCurrentTime( const QString &message )
{
  QStringList splited = message.split( ' ' );
  double time = 0;
  if ( splited.count() > 1 && splited.last().contains( 'S' ) )
    time = splited.at( splited.count() - 2 ).toDouble();

  if ( time - mCurrentTime > mTimeStep.valueSecond() )
  {
    setCurrentProgression( int( time * 100.0 / mTotalTime ) );
    emit sendInformation( message );
    mCurrentTime = mCurrentTime + mTimeStep.valueSecond();
  }
}
