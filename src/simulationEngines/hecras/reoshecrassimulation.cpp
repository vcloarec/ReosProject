/***************************************************************************
  reoshecrassimulation.cpp - ReosHecRasSimulation

 ---------------------
 begin                : 06.10.2022
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

#include "reoshecrassimulation.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoscore.h"
#include "reosdssfile.h"
#include "reosdssutils.h"

#include <QFileInfo>

REOSEXTERN ReosSimulationEngineFactory *engineSimulationFactory()
{
  return new ReosHecRasSimulationEngineFactory();
}

void ReosHecRasSimulationEngineFactory::initializeSettings()
{
}

ReosHecRasStructureImporter::ReosHecRasStructureImporter( const QString &file )
  : ReosStructureImporter()
{
  QFileInfo fileInfo( file );
  mIsValid = fileInfo.exists();
  if ( mIsValid )
  {
    mProject.reset( new ReosHecRasProject( file ) );
    mIsValid = mProject->GeometriesCount() > 0;
    if ( mIsValid )
    {
      QStringList geoms = mProject->geometryIds();
      Q_ASSERT( !geoms.isEmpty() );
      mIsValid = mProject->geometry( geoms.at( 0 ) ).area2dCount() > 0;
    }
  }
}

ReosHecRasStructureImporter::~ReosHecRasStructureImporter() = default;


ReosHydraulicStructure2D::Structure2DCapabilities ReosHecRasStructureImporter::capabilities() const
{
  return ReosHydraulicStructure2D::DefinedExternally;
}

QString ReosHecRasStructureImporter::crs() const
{
  return QString();
}

QPolygonF ReosHecRasStructureImporter::domain() const
{
  if ( mIsValid )
  {
    return mProject->currentGeometry().area2d( 0 ).surface;
  }

  return QPolygonF();
}

ReosMeshResolutionController *ReosHecRasStructureImporter::resolutionController( ReosHydraulicStructure2D *structure ) const
{
  return new ReosMeshResolutionController( structure, crs() );
}

QList<ReosHydraulicStructureBoundaryCondition *> ReosHecRasStructureImporter::createBoundaryConditions(
  ReosHydraulicStructure2D *structure,
  const ReosHydraulicNetworkContext &context ) const
{
  QList<ReosHydraulicStructureBoundaryCondition *> ret;
  const QList<ReosHecRasGeometry::BoundaryCondition> bcs = mProject->currentGeometry().allBoundariesConditions();
  for ( const ReosHecRasGeometry::BoundaryCondition &bc : bcs )
  {
    const ReosSpatialPosition position( bc.middlePosition, crs() );
    const QString id = bc.id();
    std::unique_ptr<ReosHydraulicStructureBoundaryCondition> sbc( new ReosHydraulicStructureBoundaryCondition( structure, id, position, context ) );
    sbc->elementName()->setValue( id );
    sbc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
    ret.append( sbc.get() );
    context.network()->addElement( sbc.release() );
  }

  return ret;
}

QList<ReosHydraulicSimulation *> ReosHecRasStructureImporter::createSimulations( QObject *parent ) const
{
  QList<ReosHydraulicSimulation *> ret;
  std::unique_ptr<ReosHecRasSimulation> sim( new ReosHecRasSimulation( parent ) );
  sim->setName( sim->tr( "HECRAS Simulation" ) );
  sim->setProject( mProject );
  ret.append( sim.release() );
  return ret;
}

ReosHecRasSimulation::ReosHecRasSimulation( QObject *parent )
  : ReosHydraulicSimulation( parent )
  , mMinimumInterval( new ReosParameterDuration( tr( "Minimum Input time Interval" ), false, this ) )
{
  mMinimumInterval->setValue( ReosDuration( 1.0, ReosDuration::minute ) );
}

QString ReosHecRasSimulation::key() const
{return staticKey();}

void ReosHecRasSimulation::prepareInput( ReosHydraulicStructure2D *hydraulicStructure, const ReosCalculationContext &calculationContext )
{
  const ReosHecRasPlan currentPlan = mProject->plan( mCurrentPlan );
  ReosHecRasFlow flow = mProject->flow( currentPlan.flowFile() );

  const QList<ReosHydraulicStructureBoundaryCondition *> bcs = hydraulicStructure->boundaryConditions();

  QList<ReosHecRasFlow::BoundaryFlow> boundaryToModify;
  QString dssFilePath = mProject->directory().filePath( QStringLiteral( "input_%1.dss" ).arg( mCurrentPlan ) );

  const QDateTime startTime = calculationContext.simulationStartTime();
  const QDateTime endTime = calculationContext.simulationEndTime();

  currentPlan.changeSimulationTimeInFile( startTime, endTime );

  for ( ReosHydraulicStructureBoundaryCondition *bc : bcs )
  {
    if ( bc->conditionType() != ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally &&
         bc->conditionType() != ReosHydraulicStructureBoundaryCondition::Type::NotDefined )
    {
      const QString &bcId = bc->boundaryConditionId();
      int hbcCount = flow.boundariesCount();
      for ( int i = 0; i < hbcCount; ++i )
      {
        ReosHecRasFlow::BoundaryFlow hbc = flow.boundary( i );
        if ( bcId == hbc.id() )
        {
          ReosDssPath path;
          path.setGroup( hbc.area );
          path.setLocation( hbc.boundaryConditionLine );

          switch ( bc->conditionType() )
          {
            case ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally:
            case ReosHydraulicStructureBoundaryCondition::Type::NotDefined:
              break;
            case ReosHydraulicStructureBoundaryCondition::Type::InputFlow:
            {
              std::unique_ptr<ReosTimeSerieConstantInterval> constant( new ReosTimeSerieConstantInterval() );
              transformVariableTimeStepToConstant( bc->outputHydrograph(), constant.get() );

              path.setParameter( QStringLiteral( "Flow" ) );
              path.setTimeInterval( constant->timeStep() );
              path.setVersion( QStringLiteral( "INST-VAL" ) );

              QString error;
              writeDssConstantTimeSeries( constant.get(), dssFilePath, path, error );
              hbc.type = ReosHecRasFlow::Type::FlowHydrograph;
            }
            break;
            case ReosHydraulicStructureBoundaryCondition::Type::OutputLevel:
              path.setParameter( QStringLiteral( "Stage" ) );
              hbc.type = ReosHecRasFlow::Type::StageHydrograph;
              break;
          }

          hbc.dssPath = path.string();
          hbc.isDss = true;
          hbc.dssFile = dssFilePath;
          boundaryToModify.append( hbc );
        }
      }
    }

  }

  if ( !boundaryToModify.isEmpty() )
    flow.applyBoudaryFlow( boundaryToModify );
}

void ReosHecRasSimulation::setProject( std::shared_ptr<ReosHecRasProject> newProject )
{
  mProject = newProject;
  mCurrentPlan = newProject->currentPlanId();
}

ReosHecRasProject *ReosHecRasSimulation::project() const
{
  return mProject.get();
}

void ReosHecRasSimulation::transformVariableTimeStepToConstant( ReosTimeSerieVariableTimeStep *variable, ReosTimeSerieConstantInterval *constant ) const
{
  if ( !variable || !constant )
    return;

  if ( variable->valueCount() == 0 )
    return;

  // The approach is to select the valid interval that leads to have the closest volume than the original series
  //Search for maximal time step and calculate volume
  double originalVolume = 0;

  int varCount = variable->valueCount();
  ReosDuration maxInterval( ReosDuration( qint64( 0 ) ) );
  for ( int i = 0; i < varCount - 1; ++i )
  {
    ReosDuration inter = variable->relativeTimeAt( i + 1 ) - variable->relativeTimeAt( i );
    if ( inter > maxInterval )
      maxInterval = inter;
    originalVolume += inter.valueSecond() * ( variable->valueAt( i ) + variable->valueAt( i + 1 ) ) / 2;
  }
  maxInterval = ReosDssUtils::nextValidInterval( maxInterval );
  ReosDuration originalTotalDuration = variable->totalDuration();
  ReosDuration firstRelativeTime = variable->relativeTimeAt( 0 );

  QVector<double> values;
  ReosDuration currentIntervaltest = maxInterval;
  ReosDuration betterInterval;
  QVector<double> betterValues;
  ReosDuration miniInter = mMinimumInterval->value();
  double betterVolDiff = std::numeric_limits<double>::max();
  while ( currentIntervaltest >= miniInter )
  {
    int count = int( originalTotalDuration / currentIntervaltest + 0.5 ) + 1;
    double volumeTest = 0;
    values.clear();
    values.resize( count );
    values[0] = variable->valueAtTime( firstRelativeTime );
    for ( int i = 1; i < count; ++i )
    {
      values[i] = variable->valueAtTime( currentIntervaltest * i + firstRelativeTime );
      volumeTest += currentIntervaltest.valueSecond() * ( values[i] + values[i - 1] ) / 2;
    }

    double diff = std::fabs( originalVolume - volumeTest );
    if ( diff < betterVolDiff )
    {
      betterInterval = currentIntervaltest;
      betterValues = values;
      betterVolDiff = diff;
    }
    currentIntervaltest = ReosDssUtils::previousValidInterval( currentIntervaltest );
  }

  constant->clear();
  constant->setReferenceTime( variable->referenceTime().addMSecs( firstRelativeTime.valueMilliSecond() ) );
  constant->setTimeStep( betterInterval );
  constant->setValues( betterValues );
}

bool ReosHecRasSimulation::writeDssConstantTimeSeries( ReosTimeSerieConstantInterval *series, const QString fileName, const ReosDssPath &path, QString &error ) const
{

  QFileInfo fileInfo( fileName );
  ReosDssFile file( fileName, !fileInfo.exists() );

  if ( !file.isValid() )
  {
    if ( fileInfo.exists() )
      error = QObject::tr( "Unable to open exiting DSS file \"%1\"." ).arg( fileName );
    else
      error = QObject::tr( "Unable to create DSS file \"%1\"." ).arg( fileName );

    return false;
  }

  return file.writeConstantIntervalSeries( path, series->referenceTime(), series->timeStep(), series->constData(), error );
}

