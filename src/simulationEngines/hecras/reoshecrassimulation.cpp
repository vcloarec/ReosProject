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
#include "reoshecrascontroller.h"
#include "reoshydraulicscheme.h"
#include "reossettings.h"

#include <QFileInfo>

REOSEXTERN ReosSimulationEngineFactory *engineSimulationFactory()
{
  return new ReosHecRasSimulationEngineFactory();
}

ReosHecRasSimulationEngineFactory::ReosHecRasSimulationEngineFactory()
{
  mCapabilities = SimulationEngineCapability::ImportStructure2D;
}

ReosHydraulicSimulation *ReosHecRasSimulationEngineFactory::createSimulation( const ReosEncodedElement &element, QObject *parent ) const
{
  std::unique_ptr<ReosHecRasSimulation> sim = std::make_unique<ReosHecRasSimulation>( element, parent );
  sim->setName( displayName() );
  return sim.release();
}

ReosStructureImporter *ReosHecRasSimulationEngineFactory::createImporter( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  return new ReosHecRasStructureImporter( element, context );
}

void ReosHecRasSimulationEngineFactory::initializeSettings()
{
}

ReosHecRasStructureImporter::ReosHecRasStructureImporter( const QString &fileName )
  : ReosStructureImporter()
{
  init( fileName );
}

ReosHecRasStructureImporter::ReosHecRasStructureImporter( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context )
{
  if ( element.description() != QStringLiteral( "hec-ras-importer" ) )
    return;

  QString relativeFileName;
  if ( !element.getData( QStringLiteral( "relative-path-to-project" ), relativeFileName ) )
    return;

  const QDir projectDir( context.projectPath() );
  init( projectDir.filePath( relativeFileName ) );
}

ReosEncodedElement ReosHecRasStructureImporter::encode( const ReosHydraulicNetworkContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "hec-ras-importer" ) );

  element.addData( QStringLiteral( "engine-key" ), ReosHecRasSimulation::staticKey() );

  if ( mIsValid )
  {
    QDir projectDir( context.projectPath() );
    element.addData( QStringLiteral( "relative-path-to-project" ), projectDir.relativeFilePath( mProject->fileName() ) );
  }

  return element;
}

bool ReosHecRasStructureImporter::projectFileExists() const
{
  QFileInfo info( mProject->fileName() );
  return info.exists();
}


void ReosHecRasStructureImporter::init( const QString &fileName )
{
  QFileInfo fileInfo( fileName );
  mIsValid = fileInfo.exists();
  if ( mIsValid )
  {
    mProject.reset( new ReosHecRasProject( fileName ) );
    mIsValid = mProject->GeometriesCount() > 0;
    if ( mIsValid )
    {
      QStringList geoms = mProject->geometryIds();
      Q_ASSERT( !geoms.isEmpty() );
      mIsValid = mProject->geometry( geoms.at( 0 ) ).area2dCount() > 0;

      // During this implementation, the simplest way to obtain the CRS is to load the mesh associated with the geometry
      // But it is far for being te most relevant and efficient. It will be better to get the crs with another way.
      std::unique_ptr<ReosMesh> mesh( ReosMesh::createMeshFrameFromFile( mProject->currentGeometryFileName() + QStringLiteral( ".hdf" ), QString() ) );
      mCrs = mesh->crs();
    }
  }
}

QString ReosHecRasStructureImporter::importerKey() const
{
  return ReosHecRasSimulation::staticKey();
}

ReosHecRasStructureImporter::~ReosHecRasStructureImporter() = default;


ReosHydraulicStructure2D::Structure2DCapabilities ReosHecRasStructureImporter::capabilities() const
{
  return ReosHydraulicStructure2D::DefinedExternally;
}

QString ReosHecRasStructureImporter::crs() const
{
  return mCrs;
}

QPolygonF ReosHecRasStructureImporter::domain() const
{
  if ( mIsValid )
  {
    return mProject->currentGeometry().area2d( 0 ).surface;
  }

  return QPolygonF();
}

ReosMesh *ReosHecRasStructureImporter::mesh( const QString &destinationCrs ) const
{
  if ( mIsValid )
  {
    return ReosMesh::createMeshFrameFromFile( mProject->currentGeometryFileName() + QStringLiteral( ".hdf" ), destinationCrs );
  }

  return nullptr;
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

void ReosHecRasStructureImporter::updateBoundaryConditions( QSet<QString> &currentBoundaryId, ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const
{
  QList<ReosHydraulicStructureBoundaryCondition *> ret;
  const QList<ReosHecRasGeometry::BoundaryCondition> bcs = mProject->currentGeometry().allBoundariesConditions();
  QSet<QString> notExisting = currentBoundaryId;

  QList<ReosHecRasGeometry::BoundaryCondition> toAdd;

  for ( const ReosHecRasGeometry::BoundaryCondition &bc : bcs )
  {
    const QString id = bc.id();
    if ( currentBoundaryId.contains( id ) )
    {
      notExisting.remove( id );
    }
    else
    {
      toAdd.append( bc );
    }
  }

  for ( const ReosHecRasGeometry::BoundaryCondition &bc : std::as_const( toAdd ) )
  {
    const ReosSpatialPosition position( bc.middlePosition, crs() );
    const QString id = bc.id();
    std::unique_ptr<ReosHydraulicStructureBoundaryCondition> sbc( new ReosHydraulicStructureBoundaryCondition( structure, id, position, context ) );
    sbc->elementName()->setValue( id );
    sbc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
    ret.append( sbc.get() );
    context.network()->addElement( sbc.release() );
    currentBoundaryId.insert( sbc->boundaryConditionId() );
  }
}

bool ReosHecRasStructureImporter::isValid() const { return mIsValid; }

ReosHecRasSimulation::ReosHecRasSimulation( QObject *parent )
  : ReosHydraulicSimulation( parent )
  , mMinimumInterval( new ReosParameterDuration( tr( "Minimum Input time Interval" ), false, this ) )
{
  mMinimumInterval->setValue( ReosDuration( 1.0, ReosDuration::minute ) );
}

ReosHecRasSimulation::ReosHecRasSimulation( const ReosEncodedElement &element, QObject *parent )
  : ReosHecRasSimulation( parent )
{
  ReosDataObject::decode( element );
  element.getData( QStringLiteral( "project-file-name" ), mProjectFileName );
  mProject.reset( new ReosHecRasProject( mProjectFileName ) );
}

QString ReosHecRasSimulation::key() const
{
  return staticKey();
}

ReosEncodedElement ReosHecRasSimulation::encode() const
{
  ReosEncodedElement element( QStringLiteral( "hec-ras-simulation" ) );
  element.addData( QStringLiteral( "key" ), key() );
  element.addData( QStringLiteral( "project-file-name" ), mProjectFileName );

  ReosDataObject::encode( element );

  return element;
}

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

ReosSimulationProcess *ReosHecRasSimulation::getProcess(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosCalculationContext &calculationContext ) const
{
  return new ReosHecRasSimulationProcess( *mProject.get(), mCurrentPlan, calculationContext, hydraulicStructure->boundaryConditions());
}

void ReosHecRasSimulation::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );

  element.addEncodedData( QStringLiteral( "minimum-interval" ), mMinimumInterval->value().encode() );
  element.addData( QStringLiteral( "current-plan-id" ), mCurrentPlan );

  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  const ReosEncodedElement element = scheme->restoreElementConfig( id() );

  mMinimumInterval->setValue( ReosDuration::decode( element.getEncodedData( QStringLiteral( "minimum-interval" ) ) ) );
  element.getData( QStringLiteral( "current-plan-id" ), mCurrentPlan );

  accordCurrentPlan();
}

void ReosHecRasSimulation::setProject( std::shared_ptr<ReosHecRasProject> newProject )
{
  mProject = newProject;
  mProjectFileName = newProject->fileName();
  accordCurrentPlan();
}

void ReosHecRasSimulation::setCurrentPlan( const QString &planId )
{
  mCurrentPlan = planId;
}

ReosHecRasProject *ReosHecRasSimulation::project() const
{
  return mProject.get();
}

const QString &ReosHecRasSimulation::currentPlan() const
{
  return mCurrentPlan;
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

void ReosHecRasSimulation::accordCurrentPlan()
{
  QStringList planIds = mProject->planIds();
  if ( !planIds.contains( mCurrentPlan ) )
  {
    if ( planIds.empty() )
      mCurrentPlan.clear();
    else
      mCurrentPlan = planIds.first();
  }
}


ReosHecRasSimulationProcess::ReosHecRasSimulationProcess(
  const ReosHecRasProject &hecRasProject,
  const QString &planId,
  const ReosCalculationContext &context,
  const QList<ReosHydraulicStructureBoundaryCondition *> boundaries )
  : ReosSimulationProcess( context, boundaries )
    , mProject(hecRasProject)
    , mPlan(hecRasProject.plan(planId))
{
  QStringList availableVersions = ReosHecRasController::availableVersion();
  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "/engine/hecras/version" ) ) )
  {
      mControllerVersion = settings.value( QStringLiteral( "/engine/hecras/version" ) ).toString();
    if ( !availableVersions.contains( mControllerVersion ) )
        mControllerVersion.clear();
  }

  if (mControllerVersion.isEmpty() && !availableVersions.isEmpty())
      mControllerVersion = availableVersions.last();
}

void ReosHecRasSimulationProcess::start()
{
  setMaxProgression(100);
  setCurrentProgression(0);
  mIsSuccessful = false;
  if ( mControllerVersion.isEmpty() )
  {
    emit sendInformation( tr( "None version of HEC-RAS found, please verify that HEC-RAS is installed." ) );
    setCurrentProgression(100);
    return;
  }

  std::unique_ptr<ReosHecRasController> controller(new ReosHecRasController(mControllerVersion));

  if (!controller->isValid())
  {
      emit sendInformation(tr("Controller of HEC-RAS found is not valid.\nCalculation cancelled."));
      return;
  }

  emit sendInformation(tr("Start HEC-RAS model calculation with %1.").arg(controller->version()));

  QFileInfo fileProjectInfo(mProject.fileName());
  if (!fileProjectInfo.exists())
  {
      emit sendInformation(tr("HEC-RAS project file \"%1\" not found.\nCalculation cancelled.").arg(mProject.fileName()));
      return;
  }

  if (!controller->openHecrasProject(mProject.fileName()))
  {
      emit sendInformation(tr("UNable to open HEC-RAS project file \"%1\".\nCalculation cancelled.").arg(mProject.fileName()));
      return;
  }

  QStringList plans = controller->planNames();
  
  if (!plans.contains(mPlan.title()))
  {
      emit sendInformation(tr("Plan \"%1\" not found.\nCalculation cancelled.").arg(mPlan.title()));
      return;
  }

  if (!controller->setCurrentPlan(mPlan.title()))
  {
      emit sendInformation(tr("Unable to set plan \"%1\" as current plan.\nCalculation cancelled.").arg(mPlan.title()));
      return;
  }

  controller->showComputationWindow();

  const QStringList returnedMessages = controller->computeCurrentPlan();
  emit sendInformation("kjhskfhjkqsgfjhqsdfjqsdjkfgqsdjkgfjgsd");

  for (const QString& mes : returnedMessages)
  {
      emit sendInformation(mes);
  }

  mIsSuccessful= !returnedMessages.isEmpty() && returnedMessages.last() == QStringLiteral("Computations Completed");
}
