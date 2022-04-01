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

#include "reoshydraulicstructure2d.h"
#include "reossimulationinitialcondition.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoscalculationcontext.h"

ReosTelemac2DSimulation::ReosTelemac2DSimulation( const ReosEncodedElement &element, QObject *parent )
  : ReosHydraulicSimulation( parent )
{
  ReosDataObject::decode( element );
  mTimeStep = ReosParameterDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ), false, tr( "Time step" ), this );
  mOutputPeriod = ReosParameterInteger::decode( element.getEncodedData( "output-period" ), false, tr( "Output period" ), this );
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
  element.addEncodedData( QStringLiteral( "output-period" ), mOutputPeriod->encode() );
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

  mOutputPeriod = new ReosParameterInteger( tr( "Output period" ), false, this );
  mOutputPeriod->setValue( 5 );


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

ReosParameterInteger *ReosTelemac2DSimulation::outputPeriod() const
{
  return mOutputPeriod;
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

void ReosTelemac2DSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &context )
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.mkdir( mDirName );
  QVector<int> verticesPosInBoundary;
  QList<ReosHydraulicStructureBoundaryCondition *> boundaryCondition = createBoundaryFiles( hydraulicStructure, verticesPosInBoundary );
  createSelafinInputGeometry( hydraulicStructure, verticesPosInBoundary );
  createBoundaryConditionFiles( hydraulicStructure, boundaryCondition, context );
  createSteeringFile( hydraulicStructure, boundaryCondition, context );
}

ReosSimulationProcess *ReosTelemac2DSimulation::getProcess( ReosHydraulicStructure2D *hydraulicStructure ) const
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );

  return new ReosTelemac2DSimulationProcess( dir.path() );
}

struct TelemacFlowCondition
{
  int rank = -1;
  QVector<double> timeInSeconds;
  QVector<double> value;
};

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
          case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
          case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
            bound.LIHBOR = 5;
            bound.LIUBOR = 4;
            bound.LIVBOR = 4;
            bound.LITBOR = 4;
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
    const QVector<int> &face = rmesh->face( i );
    for ( int f : face )
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

void ReosTelemac2DSimulation::createBoundaryConditionFiles( ReosHydraulicStructure2D *hydraulicStructure, QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions, const ReosCalculationContext &context )
{
  QDir dir = hydraulicStructure->structureDirectory();
  dir.cd( mDirName );
  QString path = dir.filePath( mBoundaryConditionFileName );
  QFile file( path );

  file.open( QIODevice::WriteOnly );
  QTextStream stream( &file );

  for ( int i = 0; i < boundaryConditions.count(); ++i )
  {
    ReosHydraulicStructureBoundaryCondition *boundCond = boundaryConditions.at( i );
    QString header;
    QString unit;

    ReosTimeSerieVariableTimeStep timeSeries;

    switch ( boundCond->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        header = QStringLiteral( "Q(%1)" );
        unit = QStringLiteral( "m3/s" );
        timeSeries.copyFrom( boundCond->outputHydrograph() );
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
        //        header = QStringLiteral( "SL(%1]" );
        //        unit = QStringLiteral( "m" );
        continue;
        break;
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
        //        header = QStringLiteral( "SL(%1]" );
        //        unit = QStringLiteral( "m" );
        continue;
        break;
    }
    stream << "#" << boundCond->elementName()->value() << "\n";
    stream << "T" << "\t" <<  header.arg( QString::number( i + 1 ) ) << "\n";
    stream << "s" << "\t" <<  unit << "\n";

    const QDateTime startTime = context.simulationStartTime();
    const QDateTime endTime = context.simulationEndTime();

    stream << QString::number( 0, 'f', 6 )
           << "\t"
           <<  QString::number( timeSeries.valueAtTime( startTime ), 'f', 2 )
           << "\n";

    for ( int i = 0; i < timeSeries.valueCount(); ++i )
    {
      const QDateTime time = timeSeries.timeAt( i );
      if ( time > startTime && time < endTime )
        stream << QString::number( startTime.msecsTo( time ) / 1000.0, 'f', 6 )
               << "\t"
               <<  QString::number( timeSeries.valueAt( i ), 'f', 2 )
               << "\n";
    }

    stream << QString::number( startTime.msecsTo( endTime ) / 1000.0, 'f', 6 )
           << "\t"
           <<  QString::number( timeSeries.valueAtTime( endTime ), 'f', 2 )
           << "\n";

    stream << "\n";
    stream << "\n";
  }
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
  stream << QStringLiteral( "INITIAL TIME SET TO ZERO : YES\n" );
  stream << QStringLiteral( "TIME STEP : %1\n" ).arg( QString::number( mTimeStep->value().valueSecond(), 'f', 2 ) );
  stream << QStringLiteral( "NUMBER OF TIME STEPS : %1\n" ).arg( QString::number( timeStepCount ) );
  stream << QStringLiteral( "GRAPHIC PRINTOUT PERIOD : %1\n" ).arg( QString::number( mOutputPeriod->value() ) );
  stream << QStringLiteral( "LISTING PRINTOUT PERIOD : %1\n" ).arg( QString::number( mOutputPeriod->value() ) );

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




void ReosTelemac2DSimulationProcess::start()
{
  mProcess = new QProcess();
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  env.insert( "HOMETEL", "/opt/telemac/" );
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

  tFile = new QFile( "/home/vincent/telemac_out.txt" );
  mStream = new QTextStream( tFile );

  tFile->open( QIODevice::WriteOnly );
  connect( mProcess, &QProcess::readyReadStandardOutput, mProcess, [this]
  {
    if ( mProcess )
    {
      emit solverMessage( mProcess->readAll() );
    }
  } );


  mProcess->start( script, arguments );

  bool resultStart = mProcess->waitForStarted();

  if ( resultStart )
  {
    bool result = mProcess->waitForFinished( -1 );
  }
  else
  {
    qDebug() << mProcess->readAllStandardError();
  }

  if ( mProcess )
    mProcess->deleteLater();
  mProcess = nullptr;
  tFile->deleteLater();
  delete  mStream;
}

void ReosTelemac2DSimulationProcess::stop( bool )
{
  if ( mProcess )
  {
    mProcess->terminate();
  }
}
