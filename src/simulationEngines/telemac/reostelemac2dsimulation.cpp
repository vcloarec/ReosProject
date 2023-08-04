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

#ifdef _MSC_VER
#include <Windows.h>
#undef max
#undef min
#endif

#include "reostelemac2dsimulation.h"

#include <QDir>
#include <QProcess>

#include <qgsmeshlayer.h>
#include <qgsmeshtriangulation.h>

#include <mdal.h>

#include "reoshydraulicstructure2d.h"
#include "reostelemac2dinitialcondition.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoscalculationcontext.h"
#include "reostelemac2dsimulationresults.h"
#include "reossettings.h"
#include "reoshydraulicscheme.h"
#include "reosgisengine.h"


ReosTelemac2DSimulation::ReosTelemac2DSimulation( ReosHydraulicStructure2D *parent )
  : ReosHydraulicSimulation( parent )
  , mCapabilities( Capability::Hotstart )
{
  init();
}

ReosTelemac2DSimulation::ReosTelemac2DSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent )
  : ReosHydraulicSimulation( parent )
{
  ReosDataObject::decode( element );

  const QList<ReosEncodedElement> encodedInitialConditions = element.getListEncodedData( QStringLiteral( "initial-conditions" ) );
  for ( const ReosEncodedElement &elem : encodedInitialConditions )
  {
    if ( elem.description() == QStringLiteral( "telemac-2d-initial-condition-constant-water-level" ) )
      mInitialConditions.append( new ReosTelemac2DInitialConstantWaterLevel( elem, this ) );

    if ( elem.description() == QStringLiteral( "telemac-2d-initial-condition-from-simulation" ) )
      mInitialConditions.append( new ReosTelemac2DInitialConditionFromSimulation( elem, this ) );

    if ( elem.description() == QStringLiteral( "telemac-2d-initial-condition-water-level-interpolation" ) )
      mInitialConditions.append( new ReosTelemac2DInitialConditionFromInterpolation( elem, this ) );
  }
  init();
}

bool ReosTelemac2DSimulation::hasCapability( Capability cap ) const
{
  return mCapabilities.testFlag( cap );
}

ReosEncodedElement ReosTelemac2DSimulation::encode() const
{
  ReosEncodedElement element( QStringLiteral( "telemac-2d-simulation" ) );
  element.addData( QStringLiteral( "key" ), key() );

  QList<ReosEncodedElement> encodedInitialConditions;
  for ( ReosTelemac2DInitialCondition *ic : mInitialConditions )
    encodedInitialConditions.append( ic->encode() );

  element.addListEncodedData( QStringLiteral( "initial-conditions" ), encodedInitialConditions );

  ReosDataObject::encode( element );
  return element;
}

ReosDuration ReosTelemac2DSimulation::representativeTimeStep() const
{
  return mTimeStep->value() * std::min( mOutputPeriodResult2D->value(), mOutputPeriodResultHyd->value() );
}

ReosDuration ReosTelemac2DSimulation::representative2DTimeStep() const
{
  return mTimeStep->value() * mOutputPeriodResult2D->value();
}

REOSEXTERN ReosSimulationEngineFactory *engineSimulationFactory()
{
  return new ReosTelemac2DSimulationEngineFactory();
}

ReosTelemac2DSimulationEngineFactory::ReosTelemac2DSimulationEngineFactory()
{
  mCapabilities = ReosSimulationEngineFactory::CanBeCreated;
}

ReosHydraulicSimulation *ReosTelemac2DSimulationEngineFactory::createSimulation( ReosHydraulicStructure2D *parent ) const
{
  return new ReosTelemac2DSimulation( parent );
}

ReosHydraulicSimulation *ReosTelemac2DSimulationEngineFactory::createSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent ) const
{
  if ( element.description() == QStringLiteral( "telemac-2d-simulation" ) )
    return new  ReosTelemac2DSimulation( element, parent );
  else
    return new ReosTelemac2DSimulation( parent );
}

void ReosTelemac2DSimulationEngineFactory::initializeSettings()
{
  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "/engine/telemac/telemac-config-file" ) ) )
    return;
  initializeSettingsStatic();
}

void ReosTelemac2DSimulationEngineFactory::initializeSettingsStatic()
{
  ReosSettings settings;

#if 0
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-configuration" ), QString() );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-config-file" ), QString() );
  settings.setValue( QStringLiteral( "/engine/telemac/additional_pathes" ), QString() );
  settings.setValue( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ), QString() );
  settings.setValue( QStringLiteral( "/python_path" ), QString() );
#else

#endif

  QDir telDir = QDir( QString( TELEMAC_PATH ) );
  if ( !telDir.exists() )
  {
    const QString appPath = QCoreApplication::applicationDirPath();
    telDir = QDir( appPath );
    telDir.cdUp();
    if ( !telDir.cd( QStringLiteral( "apps/telemac" ) ) )
      return;
  }

  QDir buildsDir( telDir.filePath( "builds" ) );
  QFileInfo configFileInfo( QString( TELEMAC_CONFIG_FILE ) );
  QDir depsDir( telDir.filePath( "deps" ) );
  QDir scriptsDir( telDir.filePath( "scripts/python3" ) );
  if ( scriptsDir.exists() )
    scriptsDir.cd( QStringLiteral( "python3" ) );

  QString configName( TELEMAC_CONFIG_FILE );
  if ( configName.isEmpty() )
  {
    if ( buildsDir.exists() )
    {
      const QStringList builds = buildsDir.entryList( QDir::Dirs | QDir::NoDotAndDotDot );
      if ( !builds.isEmpty() )
        configName = builds.first();
    }
  }
  if ( configName.isEmpty() )
    settings.setValue( QStringLiteral( "/engine/telemac/telemac-configuration" ), configName );

  if ( configFileInfo.exists() )
  {
    settings.setValue( QStringLiteral( "/engine/telemac/telemac-config-file" ), configFileInfo.filePath() );
  }
  else
  {
    QDir configsDir( telDir.filePath( "configs" ) );
    if ( configsDir.exists() )
    {
      QStringList filters;
      filters << QStringLiteral( "*.cfg" );
      const QStringList configs = configsDir.entryList( filters );
      if ( !configs.isEmpty() )
        settings.setValue( QStringLiteral( "/engine/telemac/telemac-config-file" ), configsDir.filePath( configs.first() ) );
    }
  }

  if ( depsDir.exists() && !depsDir.entryList().isEmpty() )
    settings.setValue( QStringLiteral( "/engine/telemac/additional_pathes" ), depsDir.path() );

  if ( scriptsDir.exists() )
    settings.setValue( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ), scriptsDir.filePath( QStringLiteral( "telemac2d.py" ) ) );

  QDir pythonDir( QCoreApplication::applicationDirPath() );
  pythonDir.cdUp();
  if ( pythonDir.cd( QStringLiteral( "apps/python" ) ) )
    settings.setValue( QStringLiteral( "/python_path" ), pythonDir.path() );
}

ReosParameterInteger *ReosTelemac2DSimulation::outputPeriodResult2D() const
{
  return mOutputPeriodResult2D;
}

ReosParameterDuration *ReosTelemac2DSimulation::timeStep() const
{
  return mTimeStep;
}

ReosTelemac2DInitialCondition *ReosTelemac2DSimulation::initialCondition() const
{
  return mInitialConditions.at( mCurrentInitialCondition );
}

ReosTelemac2DSimulation::Equation ReosTelemac2DSimulation::equation() const
{
  return mEquation;
}

void ReosTelemac2DSimulation::setEquation( const Equation &equation )
{
  mEquation = equation;
}

ReosTelemac2DSimulation::VolumeFiniteScheme ReosTelemac2DSimulation::volumeFiniteScheme( ReosHydraulicScheme *scheme ) const
{
  if ( !scheme )
    return mVFScheme;

  const ReosEncodedElement element = scheme->restoreElementConfig( id() );
  int VFSCheme = 1;
  if ( element.getData( QStringLiteral( "volume-finite-scheme" ), VFSCheme ) )
    return static_cast<VolumeFiniteScheme>( VFSCheme );
  else
    return mVFScheme;
}

void ReosTelemac2DSimulation::setVolumeFiniteEquation( VolumeFiniteScheme VFscheme, ReosHydraulicScheme *hydraulicScheme )
{
  if ( !hydraulicScheme )
  {
    mVFScheme = VFscheme;
    emit dataChanged();
  }
  else
  {
    setVolumeFiniteEquationForScheme( VFscheme, hydraulicScheme );
  }
}

ReosParameterDouble *ReosTelemac2DSimulation::courantNumber() const
{
  return mVfCourantNumber;
}

bool ReosTelemac2DSimulation::hasResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const
{
  const QDir dir = simulationDir( hydraulicStructure, shemeId );
  if ( !dir.exists() )
    return false;

  const QFileInfo fileInfo( dir.filePath( mResultFileName ) );

  if ( !fileInfo.exists() )
    return false;

  // check compatibilty of results with the current mesh
  QByteArray curi = fileInfo.filePath().toUtf8();
  MDAL_MeshH meshH = MDAL_LoadMesh( curi.constData() );

  if ( !meshH )
    return false;

  bool isCompatible = false;

  isCompatible = ( MDAL_M_vertexCount( meshH ) == hydraulicStructure->mesh()->vertexCount() ) &&
                 ( MDAL_M_faceCount( meshH ) == hydraulicStructure->mesh()->faceCount() );

  MDAL_CloseMesh( meshH );

  return isCompatible;
}

void ReosTelemac2DSimulation::saveSimulationResult( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, ReosSimulationProcess *process, bool success ) const
{
  if ( !success )
    return;

  const QDir dir = simulationDir( hydraulicStructure, shemeId );

  QMap<QString, QByteArray> encodedHydrographs;

  QMap<QString, ReosHydrograph *> hyds = process->outputHydrographs();
  ReosEncodeContext encodeContext;
  encodeContext.setBaseDir( dir );

  const QStringList ids = hyds.keys();
  for ( const QString &id : ids )
    encodedHydrographs.insert( id, hyds.value( id )->encode( encodeContext ).bytes() );

  QFile outputHydFile( dir.filePath( QStringLiteral( "outputHydrographs" ) ) );

  if ( outputHydFile.open( QIODevice::WriteOnly ) )
  {
    QDataStream stream( &outputHydFile );
    stream << encodedHydrographs;
  }
}

ReosHydraulicSimulationResults *ReosTelemac2DSimulation::loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, QObject *parent ) const
{
  const QDir dir = simulationDir( hydraulicStructure, shemeId );
  if ( !dir.exists() )
    return nullptr;

  return new ReosTelemac2DSimulationResults( this, hydraulicStructure->mesh(),  dir.filePath( mResultFileName ), parent );
}

void ReosTelemac2DSimulation::removeResults( const ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId ) const
{
  QDir dir = simulationDir( hydraulicStructure, shemeId );
  if ( !dir.exists() )
    return;

  dir.removeRecursively();
}

void ReosTelemac2DSimulation::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );

  element.addEncodedData( QStringLiteral( "time-step" ), mTimeStep->value().encode() );
  element.addData( QStringLiteral( "output-period-2D" ), mOutputPeriodResult2D->value() );
  element.addData( QStringLiteral( "output-period-hydrograph" ), mOutputPeriodResultHyd->value() );
  element.addData( QStringLiteral( "equation" ), static_cast<int>( mEquation ) );
  element.addData( QStringLiteral( "initial-condition-index" ), mCurrentInitialCondition );
  element.addData( QStringLiteral( "vf-courant-number" ), mVfCourantNumber->value() );

  for ( ReosTelemac2DInitialCondition *ic : mInitialConditions )
    ic->saveConfiguration( scheme );

  scheme->saveElementConfig( id(), element );

  setVolumeFiniteEquationForScheme( mVFScheme, scheme );
}

void ReosTelemac2DSimulation::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  const ReosEncodedElement element = scheme->restoreElementConfig( id() );

  if ( element.hasEncodedData( QStringLiteral( "time-step" ) ) )
  {
    ReosDuration timeStep = ReosDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ) );
    mTimeStep->setValue( timeStep );
  }
  int period2D = 5;
  if ( element.getData( QStringLiteral( "output-period-2D" ), period2D ) )
    mOutputPeriodResult2D->setValue( period2D );

  int periodHyd = 1;
  if ( element.getData( QStringLiteral( "output-period-hydrograph" ), periodHyd ) )
    mOutputPeriodResultHyd->setValue( periodHyd );

  int equation = 0;
  if ( element.getData( QStringLiteral( "equation" ), equation ) )
    mEquation = static_cast<Equation>( equation );

  mVFScheme = volumeFiniteScheme( scheme );

  element.getData( QStringLiteral( "initial-condition-index" ), mCurrentInitialCondition );

  double vfCN = 0.9;
  if ( element.getData( QStringLiteral( "vf-courant-number" ), vfCN ) )
    mVfCourantNumber->setValue( vfCN );

  for ( ReosTelemac2DInitialCondition *ic : std::as_const( mInitialConditions ) )
    ic->restoreConfiguration( scheme );
}

QFileInfoList ReosTelemac2DSimulation::cleanScheme( ReosHydraulicStructure2D *hydraulicStructure, ReosHydraulicScheme *scheme )
{
  return QFileInfoList( {QFileInfo( simulationDir( hydraulicStructure, scheme->id() ).path() )} );
}

static ReosTelemac2DInitialConditionFromSimulation *getHotStartCondIni( const QList<ReosTelemac2DInitialCondition *> &initialConditions )
{
  for ( ReosTelemac2DInitialCondition *iniCond : std::as_const( initialConditions ) )
  {
    if ( iniCond->initialConditionType() == ReosTelemac2DInitialCondition::Type::FromOtherSimulation )
      return qobject_cast<ReosTelemac2DInitialConditionFromSimulation *>( iniCond );
  }

  return nullptr;
}

void ReosTelemac2DSimulation::setHotStartSchemeId( const QString &schemeId )
{
  ReosTelemac2DInitialConditionFromSimulation *hotStartIni = getHotStartCondIni( mInitialConditions );
  if ( hotStartIni )
    hotStartIni->setOtherSchemeId( schemeId );
}

void ReosTelemac2DSimulation::setHotStartTimeStepIndex( int index )
{
  ReosTelemac2DInitialConditionFromSimulation *hotStartIni = getHotStartCondIni( mInitialConditions );
  if ( hotStartIni )
    hotStartIni->setTimeStepIndex( index );
}

void ReosTelemac2DSimulation::setHotStartUseLastTimeStep( bool b )
{
  ReosTelemac2DInitialConditionFromSimulation *hotStartIni = getHotStartCondIni( mInitialConditions );
  if ( hotStartIni )
    hotStartIni->setUseLastTimeStep( b );
}

QString ReosTelemac2DSimulation::engineName() const
{
  return QStringLiteral( "TELEMAC" );
}

ReosDuration ReosTelemac2DSimulation::timeStepValueFromScheme( ReosHydraulicScheme *scheme ) const
{
  ReosDuration timeStep( qint64( 0 ) );
  if ( scheme )
  {
    ReosEncodedElement element = scheme->restoreElementConfig( id() );
    if ( element.hasEncodedData( QStringLiteral( "time-step" ) ) )
      timeStep = ReosDuration::decode( element.getEncodedData( QStringLiteral( "time-step" ) ) );

    int period = 1;
    element.getData( QStringLiteral( "output-period-2D" ), period );
    timeStep = timeStep * period;
  }

  return timeStep;
}


ReosParameterInteger *ReosTelemac2DSimulation::outputPeriodResultHydrograph() const
{
  return mOutputPeriodResultHyd;
}

void ReosTelemac2DSimulation::setInitialCondition( ReosTelemac2DInitialCondition::Type type )
{
  if ( initialCondition()->initialConditionType() == type )
    return;

  mCurrentInitialCondition = -1;
  for ( int i = 0; i < mInitialConditions.count(); ++i )
  {
    if ( mInitialConditions.at( i )->initialConditionType() == type )
    {
      mCurrentInitialCondition = i;
      return;
    }
  }
}

void ReosTelemac2DSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosSimulationData &simulationData, const ReosCalculationContext &calculationContext )
{
  const QDir dir = simulationDir( hydraulicStructure, calculationContext.schemeId() );
  prepareInput( hydraulicStructure, simulationData, calculationContext, dir );

  const QFileInfo fileInfo( dir.filePath( mResultFileName ) );

  if ( fileInfo.exists() )
  {
    QFile::remove( dir.filePath( mResultFileName ) );
    QFile::remove( dir.filePath( QStringLiteral( "outputHydrographs" ) ) );
  }
}

void ReosTelemac2DSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosSimulationData &simulationData, const ReosCalculationContext &calculationContext, const QDir &directory )
{
  QVector<int> verticesPosInBoundary;
  QList<ReosHydraulicStructureBoundaryCondition *> boundaryCondition = createBoundaryFiles( hydraulicStructure, simulationData, verticesPosInBoundary, directory );
  createSelafinBaseFile( hydraulicStructure, simulationData, verticesPosInBoundary, directory.filePath( mGeomFileName ) );
  mBoundaries = createBoundaryConditionFiles( boundaryCondition, calculationContext, directory );
  createSteeringFile( hydraulicStructure, simulationData, boundaryCondition, verticesPosInBoundary, calculationContext, directory );
}

ReosSimulationProcess *ReosTelemac2DSimulation::getProcess( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext ) const
{
  QDir dir = simulationDir( hydraulicStructure, calculationContext.schemeId() );
  QMap<int, BoundaryCondition> telemacBounds;

  for ( const TelemacBoundaryCondition &bound : mBoundaries )
    if ( bound.rank > 0 )
      telemacBounds.insert( bound.rank, bound );

  return new ReosTelemac2DSimulationProcess( calculationContext, mTimeStep->value(),  dir.path(), hydraulicStructure->boundaryConditions(), telemacBounds );
}

struct TelemacBoundary
{
  int LIHBOR = 2;
  int LIUBOR = 2;
  int LIVBOR = 2;

  int LITBOR = 2;

  int vertIndex;
};

QList<ReosHydraulicStructureBoundaryCondition *> ReosTelemac2DSimulation::createBoundaryFiles(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosSimulationData &simulationData,
  QVector<int> &verticesPosInBoundary,
  const QDir &directory )
{
  const QVector<ReosSimulationData::BoundaryVertices> &boundSegments =  simulationData.boundaryVertices;
  // constraint for TElEMAC:
  // - Start from the vertices with X+Y is minimum
  // - counterclockwise

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
    const ReosSimulationData::BoundaryVertices &seg = boundSegments.at( i );
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
  const ReosSimulationData::BoundaryVertices &seg = boundSegments.at( minYSgementIndex );
  const ReosSimulationData::BoundaryVertices &segPrev = boundSegments.at( ( minYSgementIndex + segCount - 1 ) % segCount );
  const ReosSimulationData::BoundaryVertices &segNext = boundSegments.at( ( minYSgementIndex +  1 ) % segCount );
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
    const ReosSimulationData::BoundaryVertices &curSeg = boundSegments.at( segIndex );
    int vertCount = curSeg.verticesIndex.count();
    if ( !curSeg.boundaryCondition.isNull() &&
         ( ret.isEmpty() || curSeg.boundaryCondition != ret.last() ) )
      ret.append( curSeg.boundaryCondition );

    for ( int j = 0; j < vertCount; ++j )
    {
      int vertIndex = curSeg.verticesIndex.at( j );
      TelemacBoundary bound;
      if ( prevIsDefined )
      {
        bound = telemacBoundaries.last();
        prevIsDefined = false;
      }
      else if ( !curSeg.boundaryCondition.isNull() )
      {
        switch ( curSeg.boundaryCondition->conditionType() )
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
          case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
            break;
        }
      }
      bound.vertIndex = vertIndex + 1;
      telemacBoundaries.append( bound );
    }
    prevIsDefined = !curSeg.boundaryCondition.isNull();
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

  QString path = directory.filePath( mBoundaryFileName );
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

  //holes
  const QVector<QVector<QVector<int>>> &holesVertices = simulationData.holesVertices;

  int fileIndex = boundCount + 1;
  for ( const QVector<QVector<int>> &holeVertices : holesVertices )
  {
    int lineCount = holeVertices.count();
    // first find the vertex with minimum X
    double minX = std::numeric_limits<double>::max();
    double minY = std::numeric_limits<double>::max();
    int minIndex = -1;
    for ( int i = 0; i < lineCount; ++i )
    {
      if ( holeVertices.at( i ).isEmpty() )
        continue;

      QPointF holeVert = rmesh->vertexPosition( holeVertices.at( i ).at( 0 ) );

      if ( holeVert.x() < minX ||
           ( holeVert.x() == minX && holeVert.y() < minY ) )
      {
        minX = holeVert.x();
        minY = holeVert.y();
        minIndex = i;
      }
    }

    if ( minIndex == -1 )
      continue;

    const QPointF holeVert = rmesh->vertexPosition( holeVertices.at( minIndex ).at( 0 ) );
    const QPointF holeVertPrev = rmesh->vertexPosition( holeVertices.at( ( minIndex - 1 + lineCount ) % lineCount ).last() );
    QPointF holeVertNext;
    if ( holeVertices.at( minIndex ).count() > 1 )
      holeVertNext = rmesh->vertexPosition( holeVertices.at( minIndex ).at( 1 ) );
    else
      holeVertNext = rmesh->vertexPosition( holeVertices.at( ( minIndex + 1 ) % lineCount ).at( 0 ) );

    const QPointF holeVectPrev = holeVertPrev - holeVert;
    const QPointF holeVectNext = holeVertNext - holeVert;

    double holeCrossProduct = holeVectPrev.x() * holeVectNext.y() - holeVectNext.x() * holeVectPrev.y();

    bool holeInvertDirection = holeCrossProduct < 0;

    QVector<int> telemacHole;
    for ( int i = 0; i < lineCount; ++i )
    {
      const QVector<int> line = holeVertices.at( i );
      for ( int vertInd : line )
        telemacHole.append( vertInd + 1 );
    }

    if ( holeInvertDirection )
    {
      for ( int i = 0; i < telemacHole.count() / 2; ++i )
        std::swap( telemacHole[i], telemacHole[telemacHole.count() - 1 - i] );
    }

    for ( int i = 0; i < telemacHole.count(); ++i )
    {
      stream << templateLine.arg( QString::number( 2 ), QString::number( 2 ), QString::number( 2 ),
                                  QString::number( 2 ), QString::number( telemacHole.at( i ) ), QString::number( fileIndex ) );

      verticesPosInBoundary[telemacHole.at( i ) - 1] = fileIndex;
      fileIndex++;
    }
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


void ReosTelemac2DSimulation::createSelafinMeshFrame(
  ReosHydraulicStructure2D *hydraulicStructure,
  const QVector<int> &verticesPosInBoundary,
  const QString &fileName )
{
  // MDAL does not handle the boundaries. As the parrallel calculation in Telemac need to know about the boundaies vertices,
  // wa can't use MDAL to create the mesh frame file. Here we use the same logic as MDAL but we add the boundaries vertices indexes
  ReosMesh *rmesh = hydraulicStructure->mesh();

  QFile file( fileName );
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
    const QPointF vp0 = rmesh->vertexPosition( face.at( 0 ) );
    const QPointF vp1 = rmesh->vertexPosition( face.at( 1 ) );
    const QPointF vp2 = rmesh->vertexPosition( face.at( 2 ) );
    setCounterClockwise( face, vp0, vp1, vp2 );
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

void ReosTelemac2DSimulation::createSelafinBaseFile(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosSimulationData &simulationData,
  const QVector<int> &verticesPosInBoundary,
  const QString &fileName )
{
  createSelafinMeshFrame( hydraulicStructure, verticesPosInBoundary, fileName );

  const QgsMesh mesh = *static_cast<const QgsMesh *>( simulationData.meshData.data() );

  QgsMeshLayer::LayerOptions options;
  options.loadDefaultStyle = false;
  std::unique_ptr<QgsMeshLayer> ouputMesh = std::make_unique < QgsMeshLayer>(
        fileName,
        QStringLiteral( "temp" ),
        QStringLiteral( "mdal" ),
        options );

  //! Terrain elevation
  QgsMeshZValueDatasetGroup *zValueDatasetGroup = new QgsMeshZValueDatasetGroup( "BOTTOM", mesh );
  ouputMesh->addDatasets( zValueDatasetGroup );

  ouputMesh->saveDataset( fileName, 0, QStringLiteral( "SELAFIN" ) );

  //! Roughness
  ReosPolygonStructureValues *roughness = simulationData.roughnessValues.get();

  std::shared_ptr<QgsMeshMemoryDataset> roughnessDataset( new QgsMeshMemoryDataset );
  roughnessDataset->values.resize( mesh.vertexCount() );

  int size = mesh.vertexCount();
  double defaultVal = simulationData.defaultRoughness;
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

  std::unique_ptr<QgsMeshMemoryDatasetGroup> roughnessGroup( new QgsMeshMemoryDatasetGroup( "BOTTOM FRICTION", QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  roughnessGroup->addDataset( roughnessDataset );
  roughnessGroup->initialize();

  ouputMesh->addDatasets( roughnessGroup.release() );
  ouputMesh->saveDataset( fileName, 1, QStringLiteral( "SELAFIN" ) );
}

void ReosTelemac2DSimulation::createSelafinInitialConditionFile(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosSimulationData &simulationData,
  const QVector<int> &verticesPosInBoundary,
  const ReosHydraulicSimulationResults *result,
  int timeStepIndex,
  const QDir &directory )
{
  const QString path = directory.filePath( mInitialConditionFile );

  const QgsMesh mesh = *static_cast<const QgsMesh *>( simulationData.meshData.data() );

  int size = mesh.vertexCount();

  // Water level
  std::shared_ptr<QgsMeshMemoryDataset> waterLevelDataset( new QgsMeshMemoryDataset );

  waterLevelDataset->values.resize( mesh.vertexCount() );
  const QVector<double> waterlevelValue = result->datasetValues( result->groupIndex( ReosHydraulicSimulationResults::DatasetType::WaterLevel ), timeStepIndex );

  if ( waterlevelValue.count() != size )
    return;

  for ( int i = 0; i < size; ++i )
    waterLevelDataset->values[i] = waterlevelValue.at( i );

  waterLevelDataset->valid = true;
  waterLevelDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> waterLevelDatasetGroup( new QgsMeshMemoryDatasetGroup( "FREE SURFACE    M", QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  waterLevelDatasetGroup->addDataset( waterLevelDataset );
  waterLevelDatasetGroup->initialize();

  // Water depth
  std::shared_ptr<QgsMeshMemoryDataset> waterDepthDataset( new QgsMeshMemoryDataset );
  waterDepthDataset->values.resize( mesh.vertexCount() );
  const QVector<double> waterDepthValue = result->datasetValues( result->groupIndex( ReosHydraulicSimulationResults::DatasetType::WaterDepth ), timeStepIndex );

  if ( waterDepthValue.count() != size )
    return;

  for ( int i = 0; i < size; ++i )
  {
    if ( std::isnan( waterDepthValue.at( i ) ) )
      waterDepthDataset->values[i] = 0;
    else
      waterDepthDataset->values[i] = waterDepthValue.at( i );
  }

  waterDepthDataset->valid = true;
  waterDepthDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> waterDepthDatasetGroup( new QgsMeshMemoryDatasetGroup( QStringLiteral( "WATER DEPTH     M" ), QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  waterDepthDatasetGroup->addDataset( waterDepthDataset );
  waterDepthDatasetGroup->initialize();


  // Velocity
  std::shared_ptr<QgsMeshMemoryDataset> velocityDatasetU( new QgsMeshMemoryDataset );
  std::shared_ptr<QgsMeshMemoryDataset> velocityDatasetV( new QgsMeshMemoryDataset );
  velocityDatasetU->values.resize( mesh.vertexCount() );
  velocityDatasetV->values.resize( mesh.vertexCount() );
  const QVector<double>velocityValue = result->datasetValues( result->groupIndex( ReosHydraulicSimulationResults::DatasetType::Velocity ), timeStepIndex );

  if ( velocityValue.count() != size * 2 )
    return;

  for ( int i = 0; i < size; ++i )
  {
    if ( std::isnan( velocityValue.at( 2 * i ) ) )
      velocityDatasetU->values[i] = 0;
    else
      velocityDatasetU->values[i] = velocityValue.at( 2 * i );

    if ( std::isnan( velocityValue.at( 2 * i + 1 ) ) )
      velocityDatasetV->values[i] = 0;
    else
      velocityDatasetV->values[i] = velocityValue.at( 2 * i + 1 );
  }

  velocityDatasetU->valid = true;
  velocityDatasetU->time = 0;
  velocityDatasetV->valid = true;
  velocityDatasetV->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> velocityDatasetGroupU( new QgsMeshMemoryDatasetGroup( QStringLiteral( "VELOCITY U      M/S" ), QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  std::unique_ptr<QgsMeshMemoryDatasetGroup> velocityDatasetGroupV( new QgsMeshMemoryDatasetGroup( QStringLiteral( "VELOCITY V      M/S" ), QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  velocityDatasetGroupU->addDataset( velocityDatasetU );
  velocityDatasetGroupV->addDataset( velocityDatasetV );
  velocityDatasetGroupU->initialize();
  velocityDatasetGroupV->initialize();

  createSelafinInitialConditionFile( path,
                                     hydraulicStructure,
                                     simulationData,
                                     verticesPosInBoundary,
                                     std::move( waterLevelDatasetGroup ),
                                     std::move( waterDepthDatasetGroup ),
                                     std::move( velocityDatasetGroupU ),
                                     std::move( velocityDatasetGroupV ) );
}

void ReosTelemac2DSimulation::createSelafinInitialConditionFile
( ReosHydraulicStructure2D *hydraulicStructure,
  const ReosSimulationData &simulationData,
  const QVector<int> &verticesPosInBoundary,
  const ReosTelemac2DInitialConditionFromInterpolation *interpolation,
  const QDir &directory )
{
  const QString path = directory.filePath( mInitialConditionFile );
  ReosMesh *mesh = hydraulicStructure->mesh();

  int size = hydraulicStructure->mesh()->vertexCount();

  // Water level
  std::shared_ptr<QgsMeshMemoryDataset> waterLevelDataset( new QgsMeshMemoryDataset );

  waterLevelDataset->values.resize( size );

  QPolygonF mInterLine = interpolation->line();
  const QString &crs = interpolation->crs();

  simulationData.coordinateTransformer.transformToCoordinates( crs, mInterLine, mesh->crs() );
  double length = ReosGeometryUtils::length( mInterLine );
  double firstValue = interpolation->firstValue()->value();
  double secondValue = interpolation->secondValue()->value();

  for ( int i = 0; i < size; ++i )
  {
    double position = ReosGeometryUtils::projectedPointDistanceFromBegining( mesh->vertexPosition( i ), mInterLine );
    waterLevelDataset->values[i] = firstValue + ( secondValue - firstValue ) * position / length;
  }

  waterLevelDataset->valid = true;
  waterLevelDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> waterLevelDatasetGroup( new QgsMeshMemoryDatasetGroup( "FREE SURFACE    M", QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  waterLevelDatasetGroup->addDataset( waterLevelDataset );
  waterLevelDatasetGroup->initialize();

  // Water depth
  std::shared_ptr<QgsMeshMemoryDataset> waterDepthDataset( new QgsMeshMemoryDataset );
  waterDepthDataset->values.resize( size );

  for ( int i = 0; i < size; ++i )
  {
    double depth = waterLevelDataset->values.at( i ).scalar() - mesh->vertexElevation( i );
    if ( depth > 0 )
      waterDepthDataset->values[i] = depth;
    else
      waterDepthDataset->values[i] = 0;
  }

  waterDepthDataset->valid = true;
  waterDepthDataset->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> waterDepthDatasetGroup( new QgsMeshMemoryDatasetGroup( QStringLiteral( "WATER DEPTH     M" ), QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  waterDepthDatasetGroup->addDataset( waterDepthDataset );
  waterDepthDatasetGroup->initialize();

  // Velocity
  std::shared_ptr<QgsMeshMemoryDataset> velocityDatasetU( new QgsMeshMemoryDataset );
  std::shared_ptr<QgsMeshMemoryDataset> velocityDatasetV( new QgsMeshMemoryDataset );
  velocityDatasetU->values.resize( size );
  velocityDatasetV->values.resize( size );

  for ( int i = 0; i < size; ++i )
  {
    velocityDatasetU->values[i] = 0;
    velocityDatasetV->values[i] = 0;
  }

  velocityDatasetU->valid = true;
  velocityDatasetU->time = 0;
  velocityDatasetV->valid = true;
  velocityDatasetV->time = 0;

  std::unique_ptr<QgsMeshMemoryDatasetGroup> velocityDatasetGroupU( new QgsMeshMemoryDatasetGroup( QStringLiteral( "VELOCITY U      M/S" ), QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  std::unique_ptr<QgsMeshMemoryDatasetGroup> velocityDatasetGroupV( new QgsMeshMemoryDatasetGroup( QStringLiteral( "VELOCITY V      M/S" ), QgsMeshDatasetGroupMetadata::DataOnVertices ) );
  velocityDatasetGroupU->addDataset( velocityDatasetU );
  velocityDatasetGroupV->addDataset( velocityDatasetV );
  velocityDatasetGroupU->initialize();
  velocityDatasetGroupV->initialize();

  createSelafinInitialConditionFile( path,
                                     hydraulicStructure,
                                     simulationData,
                                     verticesPosInBoundary,
                                     std::move( waterLevelDatasetGroup ),
                                     std::move( waterDepthDatasetGroup ),
                                     std::move( velocityDatasetGroupU ),
                                     std::move( velocityDatasetGroupV ) );
}

void ReosTelemac2DSimulation::createSelafinInitialConditionFile( const QString &path,
    ReosHydraulicStructure2D *hydraulicStructure,
    const ReosSimulationData &simulationData,
    const QVector<int> &verticesPosInBoundary,
    std::unique_ptr<QgsMeshDatasetGroup> waterLevel,
    std::unique_ptr<QgsMeshDatasetGroup> depth,
    std::unique_ptr<QgsMeshDatasetGroup> velocityU,
    std::unique_ptr<QgsMeshDatasetGroup> velocityV )
{
  createSelafinBaseFile( hydraulicStructure, simulationData, verticesPosInBoundary, path );
  QgsMeshLayer::LayerOptions options;
  options.loadDefaultStyle = false;
  std::unique_ptr<QgsMeshLayer> ouputMesh = std::make_unique < QgsMeshLayer>(
        path,
        QStringLiteral( "temp" ),
        QStringLiteral( "mdal" ),
        options );

  ouputMesh->addDatasets( waterLevel.release() );
  ouputMesh->saveDataset( path, 2, QStringLiteral( "SELAFIN" ) );

  ouputMesh->addDatasets( depth.release() );
  ouputMesh->saveDataset( path, 3, QStringLiteral( "SELAFIN" ) );

  ouputMesh->addDatasets( velocityU.release() );
  ouputMesh->saveDataset( path, 4, QStringLiteral( "SELAFIN" ) );
  ouputMesh->addDatasets( velocityV.release() );
  ouputMesh->saveDataset( path, 5, QStringLiteral( "SELAFIN" ) );

}

QList<ReosTelemac2DSimulation::TelemacBoundaryCondition> ReosTelemac2DSimulation::createBoundaryConditionFiles(
  const QList<ReosHydraulicStructureBoundaryCondition *> &boundaryConditions,
  const ReosCalculationContext &context,
  const QDir &directory )
{
  QSet<qint64> timeSteps;
  QList<TelemacBoundaryCondition> boundConds;
  const QDateTime startTime = context.timeWindow().start();
  const QDateTime endTime = context.timeWindow().end();

  for ( int i = 0; i < boundaryConditions.count(); ++i )
  {
    ReosHydraulicStructureBoundaryCondition *boundCond = boundaryConditions.at( i );
    TelemacBoundaryCondition bc;
    bc.type = boundCond->conditionType();
    switch ( bc.type )
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
      case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
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
    }

    bc.boundaryId = boundCond->boundaryConditionId();
    boundConds.append( bc );
  }

  QList<qint64> timeStepsList = timeSteps.values();
  std::sort( timeStepsList.begin(), timeStepsList.end() );

  if ( !timeStepsList.isEmpty() && timeStepsList.first() == 0 )
    timeStepsList.removeFirst();

  if ( !timeStepsList.isEmpty() && timeStepsList.last() == startTime.msecsTo( endTime ) )
    timeStepsList.removeLast();

  QString path = directory.filePath( mBoundaryConditionFileName );
  QFile file( path );
  file.open( QIODevice::WriteOnly );
  QTextStream stream( &file );

  stream << "T";
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    if ( bc.timeSeries )
      stream << "\t" <<  bc.header.arg( QString::number( bc.rank ) );
  stream << "\n";

  stream << "s";
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    if ( bc.timeSeries )
      stream << "\t" <<  bc.unit;
  stream << "\n";

  stream << QString::number( 0, 'f', 6 );
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    if ( bc.timeSeries )
      stream << "\t" << QString::number( bc.timeSeries->valueAtTime( startTime ), 'f', 2 );
  stream << "\n";

  for ( qint64 tms : std::as_const( timeStepsList ) )
  {
    stream << QString::number( tms / 1000.0, 'f', 6 );
    for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
      if ( bc.timeSeries )
        stream << "\t" <<  QString::number( bc.timeSeries->valueAtTime( startTime.addMSecs( tms ) ), 'f', 2 );
    stream << "\n";
  }

  stream << QString::number( startTime.msecsTo( endTime ) / 1000.0, 'f', 6 );
  for ( const TelemacBoundaryCondition &bc : std::as_const( boundConds ) )
    if ( bc.timeSeries )
      stream <<  "\t" << QString::number( bc.timeSeries->valueAtTime( endTime ), 'f', 2 );
  stream << "\n";

  return boundConds;
}

void ReosTelemac2DSimulation::createSteeringFile(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosSimulationData &simulationData,
  const QList<ReosHydraulicStructureBoundaryCondition *> &boundaryConditions,
  const QVector<int> &verticesPosInBoundary,
  const ReosCalculationContext &context,
  const QDir &directory )
{
  QString path = directory.filePath( mSteeringFileName );
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
  stream << QStringLiteral( "TITLE : '%1'\n" ).arg( hydraulicStructure->elementNameParameter()->value() );
  stream << QStringLiteral( "VARIABLES FOR GRAPHIC PRINTOUTS : 'S,U,V,B,H,W,US,MAXZ,MAXV'\n" );

  // Time parameters
  ReosDuration totalDuration( context.timeWindow().start().msecsTo( context.timeWindow().end() ), ReosDuration::millisecond );
  int timeStepCount = totalDuration.numberOfFullyContainedIntervals( mTimeStep->value() );

  switch ( initialCondition()->initialConditionType() )
  {
    case ReosTelemac2DInitialCondition::Type::FromOtherSimulation:
    case ReosTelemac2DInitialCondition::Type::Interpolation:
      stream << QStringLiteral( "COMPUTATION CONTINUED : YES\n" );
      stream << QStringLiteral( "PREVIOUS COMPUTATION FILE : %1\n" ).arg( mInitialConditionFile );
      break;
    case ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity:
      stream << QStringLiteral( "COMPUTATION CONTINUED : NO\n" );
      break;
  }

  QDate startDate = context.timeWindow().start().date();
  stream << QStringLiteral( "ORIGINAL DATE OF TIME : %1;%2;%3\n" ).arg( QString::number( startDate.year() ),  QString::number( startDate.month() ),  QString::number( startDate.day() ) );
  QTime startTime = context.timeWindow().start().time();
  stream << QStringLiteral( "ORIGINAL HOUR OF TIME : %1;%2;%3\n" ).arg( QString::number( startTime.hour() ),  QString::number( startTime.minute() ),  QString::number( startTime.second() ) );
  stream << QStringLiteral( "INITIAL TIME SET TO ZERO : YES\n" );
  stream << QStringLiteral( "TIME STEP : %1\n" ).arg( QString::number( mTimeStep->value().valueSecond(), 'f', 2 ) );
  stream << QStringLiteral( "NUMBER OF TIME STEPS : %1\n" ).arg( QString::number( timeStepCount ) );
  stream << QStringLiteral( "GRAPHIC PRINTOUT PERIOD : %1\n" ).arg( QString::number( mOutputPeriodResult2D->value() ) );
  stream << QStringLiteral( "LISTING PRINTOUT PERIOD : %1\n" ).arg( QString::number( mOutputPeriodResultHyd->value() ) );

  //Physical parameters
  stream << QStringLiteral( "LAW OF BOTTOM FRICTION : 3\n" );
  stream << QStringLiteral( "FRICTION COEFFICIENT : 10\n" );

  //Boundary condition
  QStringList prescribedFlow;
  QStringList prescribedElevation;
  QStringList velocityProfile;
  for ( ReosHydraulicStructureBoundaryCondition *bc : boundaryConditions )
  {
    switch ( bc->conditionType() )
    {
      case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
      case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
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
        if ( bc->isWaterLevelConstant()->value() )
          prescribedElevation.append( QStringLiteral( "%1" ).arg( bc->constantWaterElevation()->value() ) );
        else
          prescribedElevation.append( QString( '1' ) );
        velocityProfile.append( QString( '1' ) );
        break;
    }
  }
  if ( !prescribedFlow.isEmpty() )
    stream << QStringLiteral( "PRESCRIBED FLOWRATES : %1\n" ).arg( prescribedFlow.join( ';' ) );
  if ( !velocityProfile.isEmpty() )
    stream << QStringLiteral( "VELOCITY PROFILES : %1\n" ).arg( velocityProfile.join( ';' ) );
  if ( !prescribedElevation.isEmpty() )
    stream << QStringLiteral( "PRESCRIBED ELEVATIONS : %1\n" ).arg( prescribedElevation.join( ';' ) );

  //Initial condition
  switch ( initialCondition()->initialConditionType() )
  {
    case ReosTelemac2DInitialCondition::Type::FromOtherSimulation:
    {
      ReosTelemac2DInitialConditionFromSimulation *cifs = qobject_cast<ReosTelemac2DInitialConditionFromSimulation *>( initialCondition() );
      if ( hasResult( hydraulicStructure, cifs->otherSchemeId() ) )
      {
        std::unique_ptr<ReosHydraulicSimulationResults> results( loadSimulationResults( hydraulicStructure, cifs->otherSchemeId() ) );
        int waterlevelGroupIndex = results->groupIndex( ReosHydraulicSimulationResults::DatasetType::WaterLevel );
        if ( results->datasetCount( waterlevelGroupIndex ) >  cifs->timeStepIndex() )
        {
          int timeStepIndex = 0;
          if ( cifs->useLastTimeStep() )
            timeStepIndex = results->timeSteps().count() - 1;
          else
            timeStepIndex = cifs->timeStepIndex();

          createSelafinInitialConditionFile( hydraulicStructure, simulationData, verticesPosInBoundary, results.get(), timeStepIndex, directory );
        }
      }
    }
    break;
    case ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity:
    {
      stream << QStringLiteral( "INITIAL CONDITIONS : 'CONSTANT ELEVATION'\n" );
      ReosTelemac2DInitialConstantWaterLevel *ciwl = qobject_cast<ReosTelemac2DInitialConstantWaterLevel *>( initialCondition() );
      stream << QStringLiteral( "INITIAL ELEVATION : %1\n" ).arg( QString::number( ciwl->initialWaterLevel()->value(), 'f', 2 ) );
    }
    break;
    case ReosTelemac2DInitialCondition::Type::Interpolation:
      ReosTelemac2DInitialConditionFromInterpolation *cifi = qobject_cast<ReosTelemac2DInitialConditionFromInterpolation *>( initialCondition() );
      if ( cifi )
        createSelafinInitialConditionFile( hydraulicStructure, simulationData, verticesPosInBoundary, cifi, directory );
      break;
  }

  //Numerical parameters
  switch ( mEquation )
  {
    case ReosTelemac2DSimulation::Equation::FiniteVolume:
    {
      stream << QStringLiteral( "EQUATIONS: 'SAINT-VENANT FV'\n" );
      stream << QStringLiteral( "DESIRED COURANT NUMBER : %1\n" ).arg( QString::number( mVfCourantNumber->value() ) );
      stream << QStringLiteral( "VARIABLE TIME-STEP : YES\n" );
      QString vfScheme( '5' );
      switch ( mVFScheme )
      {
        case VolumeFiniteScheme::Roe:
          vfScheme = QString( '0' );
          break;
        case VolumeFiniteScheme::Kinetic:
          vfScheme =  QString( '1' );
          break;
        case VolumeFiniteScheme::Zokagoa:
          vfScheme =  QString( '3' );
          break;
        case VolumeFiniteScheme::Tchamen:
          vfScheme =  QString( '4' );
          break;
        case VolumeFiniteScheme::HLLC:
          vfScheme =  QString( '5' );
          break;
        case VolumeFiniteScheme::WAF:
          vfScheme =  QString( '6' );
          break;
      }
      stream << QStringLiteral( "FINITE VOLUME SCHEME: %1\n" ).arg( vfScheme );
    }
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

void ReosTelemac2DSimulation::init()
{
  mTimeStep = new ReosParameterDuration( tr( "Time step" ), false, this );
  mTimeStep->setValue( ReosDuration( 30, ReosDuration::second ) );

  mOutputPeriodResult2D = new ReosParameterInteger( tr( "Output period for 2D result" ), false, this );
  mOutputPeriodResult2D->setValue( 5 );

  mOutputPeriodResultHyd = new ReosParameterInteger( tr( "Output period for hydrograph" ), false, this );
  mOutputPeriodResultHyd->setValue( 1 );

  mVfCourantNumber = new ReosParameterDouble( QString(), false, this );
  mVfCourantNumber->setValue( 0.9 );

  connect( mTimeStep, &ReosParameter::valueChanged, this, &ReosHydraulicSimulation::timeStepChanged );
  connect( mOutputPeriodResult2D, &ReosParameter::valueChanged, this, &ReosHydraulicSimulation::timeStepChanged );
  connect( mOutputPeriodResultHyd, &ReosParameter::valueChanged, this, &ReosHydraulicSimulation::timeStepChanged );
  connect( mVfCourantNumber, &ReosParameter::valueChanged, this, &ReosHydraulicSimulation::dataChanged );

  initInitialCondition();
}

void ReosTelemac2DSimulation::initInitialCondition()
{
  QList<ReosTelemac2DInitialCondition::Type> types;

  types << ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity
        << ReosTelemac2DInitialCondition::Type::FromOtherSimulation
        << ReosTelemac2DInitialCondition::Type::Interpolation;

  for ( ReosTelemac2DInitialCondition::Type type : std::as_const( types ) )
  {
    bool found = false;
    for ( ReosTelemac2DInitialCondition *bc : std::as_const( mInitialConditions ) )
      if ( bc->initialConditionType() == type )
      {
        found = true;
        break;
      }

    if ( !found )
    {
      switch ( type )
      {
        case ReosTelemac2DInitialCondition::Type::ConstantLevelNoVelocity:
          mInitialConditions.append( new ReosTelemac2DInitialConstantWaterLevel( this ) );
          break;
        case ReosTelemac2DInitialCondition::Type::FromOtherSimulation:
          mInitialConditions.append( new ReosTelemac2DInitialConditionFromSimulation( this ) );
          break;
        case ReosTelemac2DInitialCondition::Type::Interpolation:
          mInitialConditions.append( new ReosTelemac2DInitialConditionFromInterpolation( this ) );
          break;
      }
    }
  }
}

void ReosTelemac2DSimulation::setVolumeFiniteEquationForScheme( VolumeFiniteScheme VFscheme, ReosHydraulicScheme *hydraulicScheme ) const
{
  ReosEncodedElement element = hydraulicScheme->restoreElementConfig( id() );
  element.addData( QStringLiteral( "volume-finite-scheme" ), static_cast<int>( VFscheme ) );
  hydraulicScheme->saveElementConfig( id(), element );
}


ReosTelemac2DSimulationProcess::ReosTelemac2DSimulationProcess( const ReosCalculationContext &context,
    const ReosDuration &timeStep,
    const QString &simulationfilePath,
    const QList<ReosHydraulicStructureBoundaryCondition *> &boundElem,
    const QMap<int, BoundaryCondition> &boundaries )
  : ReosSimulationProcess( context, boundElem )
  , mSimulationFilePath( simulationfilePath )
  , mTimeStep( timeStep )
  , mBoundaries( boundaries )
{
  mStartTime = context.timeWindow().start();
  mTotalTime = context.timeWindow().start().msecsTo( context.timeWindow().end() ) / 1000.0;
  connect( this, &ReosTelemac2DSimulationProcess::askToStop, this, &ReosTelemac2DSimulationProcess::onStopAsked );
}

void ReosTelemac2DSimulationProcess::start()
{
  mIsSuccessful = false;
  QThread::msleep( 100 ); //just a bit of time to make the connection with the console (TODO: change the logic of process creation to avoid this)
  mProcess = new QProcess();
  QProcessEnvironment env = QProcessEnvironment::systemEnvironment();
  ReosSettings settings;

  env.insert( QStringLiteral( "SYSTELCFG" ), settings.value( QStringLiteral( "/engine/telemac/telemac-config-file" ) ).toString() );
  env.insert( QStringLiteral( "USETELCFG" ), settings.value( QStringLiteral( "/engine/telemac/telemac-configuration" ) ).toString() );

  QString envPath = env.value( QStringLiteral( "PATH" ) );

  QString telemScripts = settings.value( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ) ).toString();
  if ( telemScripts.isEmpty() )
  {
    emit sendInformation( tr( "TELEMAC 2D Python script not set. Please verify TELEMAC settings." ) );
    return;
  }
  QFileInfo telemac2DPythonScript( telemScripts );
  if ( !telemac2DPythonScript.exists() )
  {
    emit sendInformation( tr( "TELEMAC 2D Python script not found. Please verify TELEMAC settings." ) );
    return;
  }

#ifdef _MSC_VER
  const QString pythonPath = settings.value( QStringLiteral( "/python_path" ) ).toString();
  env.insert( QStringLiteral( "PYTHONHOME" ), pythonPath );
  env.insert( QStringLiteral( "PYTHONPATH" ), pythonPath + ';' + telemac2DPythonScript.dir().path() );
  envPath.append( ';' );
  envPath.append( env.value( QStringLiteral( "PYTHONPATH" ) ) );
  envPath.append( ';' );
#else
  if ( envPath.back() != QString( ':' ) )
    envPath.append( ':' );
#endif

  envPath += settings.value( QStringLiteral( "/engine/telemac/additional_pathes" ) ).toString();
  env.insert( QStringLiteral( "PATH" ), envPath );

  mProcess->setProcessEnvironment( env );
  mProcess->setWorkingDirectory( mSimulationFilePath );

#ifdef _MSC_VER
  QString script( pythonPath + '/' + QStringLiteral( "python.exe" ) );
#else
  QString script( QStringLiteral( "python3" ) );
#endif
  QStringList arguments;
  arguments << settings.value( QStringLiteral( "/engine/telemac/telemac-2d-python-script" ) ).toString()
            << QStringLiteral( "simulation.cas" );

  int nbProc = settings.value( QStringLiteral( "/engine/telemac/cpu-usage-count" ) ).toInt();
  if ( nbProc > 1 )
    arguments <<  QStringLiteral( "--ncsize=%1" ).arg( nbProc );

  mBlockRegEx = QRegularExpression( QStringLiteral( "(?s).*?((ITERATION .*?)\\n.*?=====)" ) );
  mBoundaryFlowRegEx = QRegularExpression( QStringLiteral( "(?s).*?FLUX BOUNDARY +([0-9])+: +([\\-0-9.E]+)" ) );

  mTimeRegEx = QRegularExpression( QStringLiteral( "(ITERATION +[a-zA-Z0-9]+ +TIME:[\\ 0-9a-zA-Z.()]+)" ) );

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

    if ( !finished )
    {
      switch ( mProcess->error() )
      {
        case QProcess::FailedToStart:
          emit sendInformation( tr( "Simulation process failed to start" ) );
          break;
        case QProcess::Crashed:
          emit sendInformation( tr( "Simulation process crashed" ) );
          break;
        default:
          emit sendInformation( tr( "Simulation process does not finished for an unknown error" ) );
          break;
      }

      emit sendInformation( mProcess->readAllStandardError() );
    }

    if ( isStop() )
      emit sendInformation( tr( "Simulation canceled by user" ) );
    else
      emit sendInformation( mStandartOutputBuffer );

    if ( mProcess->exitCode() != 0 )
    {
      emit sendInformation( tr( "Simulation process exit with error code %1" ).arg( mProcess->exitCode() ) );
      emit sendInformation( mProcess->readAllStandardError() );
    }
  }
  else
  {
    emit sendInformation( tr( "Telemac simulation can't start in folder \"%1\".\n"
                              "Error: %2\n"
                              "Check the settings of the Telemac engine." )
                          .arg( mProcess->workingDirectory(), QString::number( mProcess->exitCode() ) ) );
  }

  finished = finished && mProcess->exitCode() == 0;

  if ( mProcess )
    delete mProcess;
  mProcess = nullptr;

  mIsSuccessful = finished;
}

void ReosTelemac2DSimulationProcess::stop( bool )
{
  ReosSimulationProcess::stop( true );
  emit askToStop();
}

void ReosTelemac2DSimulationProcess::onStopAsked()
{
  if ( mProcess )
  {
#ifdef _MSC_VER
    QString killCommand( QStringLiteral( "TASKKILL /PID %1 /F /T" ).arg( mProcess->processId() ) );
    system( killCommand.toStdString().c_str() );
#else
    mProcess->terminate();
#endif
  }
}

void ReosTelemac2DSimulationProcess::addToOutput( const QString &txt )
{
  mStandartOutputBuffer.append( txt );
  QRegularExpressionMatch blockMatch = mBlockRegEx.match( mStandartOutputBuffer );
  QString message;

  if ( mIsPreparation )
    emit sendInformation( txt );

  while ( blockMatch.hasMatch() )
  {
    if ( mIsPreparation )
    {
      mIsPreparation = false;
    }
    else
    {
      extractInformation( blockMatch );
    }

    mStandartOutputBuffer = mStandartOutputBuffer.mid( blockMatch.capturedEnd() );
    blockMatch = mTimeRegEx.match( mStandartOutputBuffer );
  }
}

void ReosTelemac2DSimulationProcess::extractInformation( const  QRegularExpressionMatch &blockMatch )
{
  QString timeString = blockMatch.captured( 2 );
  QStringList splited = timeString.split( ' ' );
  double time = 0;
  if ( splited.count() > 1 && splited.last().contains( 'S' ) )
    time = splited.at( splited.count() - 2 ).toDouble();

  if ( time - mCurrentTime > mTimeStep.valueSecond() )
  {
    const QString &blockString = blockMatch.captured( 1 );
    QRegularExpressionMatchIterator it = mBoundaryFlowRegEx.globalMatch( blockString );

    QStringList ids;
    QList<double> flows;
    while ( it.hasNext() )
    {
      const QRegularExpressionMatch match = it.next();
      if ( match.lastCapturedIndex() != 2 )
        continue;
      bool ok;
      int boundRank = match.captured( 1 ).toInt( &ok );
      if ( !ok )
        continue;

      double value = match.captured( 2 ).toDouble( &ok );
      if ( !ok )
        continue;

      BoundaryCondition bc = mBoundaries.value( boundRank );
      ids.append( bc.boundaryId );

      switch ( bc.type )
      {
        case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
        case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
        case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
          break;
        case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
          value = -value;
          break;
      }
      flows.append( value );
    }

    emit sendBoundaryFlow( mStartTime.addMSecs( qint64( time * 1000 ) ), ids, flows );

    int progress = int( time * 100.0 / mTotalTime );
    setCurrentProgression( progress );
    emit sendInformation( QString::number( progress ) + QStringLiteral( " % ," ) + timeString );
    mCurrentTime = mCurrentTime + mTimeStep.valueSecond();
  }
}
