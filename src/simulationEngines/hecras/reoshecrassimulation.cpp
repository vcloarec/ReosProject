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
#include "reoshecrassimulationresults.h"
#include "reostimewindowsettings.h"
#include "reosgriddedrainitem.h"
#include "reosdssprovider.h"

#include <QFileInfo>
#include <QEventLoop>

REOSEXTERN ReosSimulationEngineFactory *engineSimulationFactory()
{
  return new ReosHecRasSimulationEngineFactory();
}

ReosHecRasSimulationEngineFactory::ReosHecRasSimulationEngineFactory()
{
  mCapabilities = SimulationEngineCapability::ImportStructure2D;
}

ReosHecRasSimulation *ReosHecRasSimulationEngineFactory::createSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent ) const
{
  std::unique_ptr<ReosHecRasSimulation> sim = std::make_unique<ReosHecRasSimulation>( element, parent );
  sim->setName( displayName() );

  return sim.release();
}

ReosStructureImporterSource *ReosHecRasSimulationEngineFactory::createImporterSource( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context ) const
{
  return new ReosHecRasStructureImporterSource( element, context );
}

void ReosHecRasSimulationEngineFactory::initializeSettings()
{
}

ReosHecRasStructureImporter::ReosHecRasStructureImporter( const QString &fileName, const ReosHydraulicNetworkContext &context, const ReosHecRasStructureImporterSource *source )
  : ReosStructureImporter( context )
  , mSource( source )
{
  init( fileName );
}


bool ReosHecRasStructureImporter::projectFileExists() const
{
  QFileInfo info( mProject->fileName() );
  return info.exists();
}

int ReosHecRasStructureImporter::planCount() const
{
  return mProject->planIds().count();
}

const ReosStructureImporterSource *ReosHecRasStructureImporter::source() const {return mSource;}

void ReosHecRasStructureImporter::setCreationOption( const CreationOptions &creationOption )
{
  mCreationOption = creationOption;
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
      mIsValid = !geoms.isEmpty() && mProject->geometry( geoms.at( 0 ) ).area2dCount() > 0;
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
  return QFlags( ReosHydraulicStructure2D::DefinedExternally ) | ReosHydraulicStructure2D::GriddedPrecipitation;
}

QString ReosHecRasStructureImporter::crs() const
{
  if ( mProject )
    return mProject->crs();

  return QString();
}

QPolygonF ReosHecRasStructureImporter::domain() const
{
  if ( mIsValid )
  {
    return mProject->currentGeometry().domain();
  }

  return QPolygonF();
}

ReosMesh *ReosHecRasStructureImporter::mesh( const QString &destinationCrs ) const
{
  if ( mIsValid )
  {
    ReosModule::Message message;

    std::unique_ptr<ReosMesh> mesh( mProject->currentGeometry().createMesh( destinationCrs, message ) );
    if ( message.type == ReosModule::Simple )
      return mesh.release();

    mNetWork->message( message, true );
  }

  return nullptr;
}

ReosMesh *ReosHecRasStructureImporter::mesh( ReosHydraulicStructure2D *structure, ReosHydraulicScheme *scheme, const QString &destinationCrs ) const
{
  ReosHecRasSimulation *simulation = qobject_cast<ReosHecRasSimulation *>( structure->currentSimulation() );
  if ( !simulation )
    return nullptr;

  ReosHecRasGeometry geometry = mProject->geometryFromPlan( simulation->currentPlan( scheme ) );
  if ( !geometry.isValid() )
    return nullptr;

  ReosModule::Message message;
  std::unique_ptr<ReosMesh> mesh( geometry.createMesh( destinationCrs, message ) );

  if ( message.type == ReosModule::Simple )
    return mesh.release();

  mNetWork->message( message, true );
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
    sbc->elementName()->setValue( bc.name() + QString( '-' ) + bc.area() );
    sbc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
    ret.append( sbc.get() );
    context.network()->addElement( sbc.release() );
  }

  return ret;
}

QList<ReosHydraulicSimulation *> ReosHecRasStructureImporter::createSimulations( ReosHydraulicStructure2D *parent ) const
{
  QList<ReosHydraulicSimulation *> ret;
  std::unique_ptr<ReosHecRasSimulation> sim( new ReosHecRasSimulation( parent ) );
  sim->setName( QObject::tr( "HECRAS Simulation" ) );

  QStringList schemeNameToKeep;

  if ( mCreationOption.createSchemeWithPlan &&
       parent->network() &&
       parent->network()->hydraulicSchemeCollection() )
  {
    ReosHydraulicSchemeCollection *schemeCollection = parent->network()->hydraulicSchemeCollection();
    int previousSchemeCount = schemeCollection->schemeCount();

    const QStringList planIds = mProject->planIds();
    for ( const QString &id : planIds )
    {
      const QString planTitle = mProject->planTitle( id );
      ReosHydraulicScheme *scheme = nullptr;
      if ( !schemeCollection->schemeByName( planTitle ) )
      {
        std::unique_ptr<ReosHydraulicScheme> sch( new ReosHydraulicScheme( schemeCollection ) );
        sch->schemeName()->setValue( planTitle );
        scheme = sch.release();
        schemeCollection->addScheme( scheme );
      }
      else if ( mCreationOption.removePreviousScheme )
      {
        schemeNameToKeep.append( planTitle );
      }

      const ReosHecRasPlan plan = mProject->plan( id );
      sim->setCurrentPlan( id, scheme );
      sim->setComputeInterval( plan.computeInterval(), scheme );
      sim->setOutputInterval( plan.outputInterval(), scheme );
      sim->setDetailledInterval( plan.detailedOutputInterval(), scheme );
      sim->setMappingInterval( plan.mappingInterval(), scheme );
    }

    int schemeCount = schemeCollection->schemeCount();

    if ( mCreationOption.removePreviousScheme )
    {
      int schemeToRemove = 0;
      while ( schemeCollection->schemeCount() > ( schemeCount - previousSchemeCount ) + schemeToRemove )
      {
        if ( schemeNameToKeep.contains( schemeCollection->scheme( schemeToRemove )->schemeName()->value() ) )
          schemeToRemove++;
        else
          schemeCollection->removeScheme( schemeToRemove );
      }
    }
  }

  sim->setProject( mProject );
  ret.append( sim.release() );
  return ret;
}

void ReosHecRasStructureImporter::updateBoundaryConditions( const QSet<QString> &currentBoundaryId, ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context ) const
{
  ReosHecRasSimulation::updateBoundaryConditions( mProject.get(), currentBoundaryId, structure, context );
}

bool ReosHecRasStructureImporter::isValid() const { return mIsValid; }

ReosHecRasSimulation::ReosHecRasSimulation( ReosHydraulicStructure2D *parent )
  : ReosHydraulicSimulation( parent )
  , mStructure( parent )
{
}

ReosHecRasSimulation::ReosHecRasSimulation( const ReosEncodedElement &element, ReosHydraulicStructure2D *parent )
  : ReosHydraulicSimulation( parent )
  , mStructure( parent )
{
  ReosDataObject::decode( element );
  QString relativeFileName;
  if ( !element.getData( QStringLiteral( "project-file-name" ), relativeFileName ) )
    return;
  if ( parent && parent->network() )
  {
    QDir projectDir( parent->network()->context().projectPath() );
    mProjectFileName = projectDir.filePath( relativeFileName );
  }
  else
  {
    mProjectFileName = relativeFileName;
  }

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

  QString pathToEncode;
  if ( mStructure && mStructure->network() )
  {
    ReosHydraulicNetworkContext context = mStructure->network()->context();
    QDir projectDir( context.projectPath() );
    pathToEncode = projectDir.relativeFilePath( mProjectFileName );
  }
  else
  {
    pathToEncode = mProjectFileName;
  }

  element.addData( QStringLiteral( "project-file-name" ), pathToEncode );

  ReosDataObject::encode( element );

  return element;
}

void ReosHecRasSimulation::prepareInput(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosSimulationData &data,
  const ReosCalculationContext &calculationContext )
{
  const ReosHecRasPlan currentPlan = mProject->plan( mCurrentPlan );
  ReosHecRasFlow flow = mProject->flow( currentPlan.flowFile() );

  const QList<ReosHydraulicStructureBoundaryCondition *> bcs = hydraulicStructure->boundaryConditions();

  QList<ReosHecRasFlow::BoundaryFlow> boundaryToModify;
  QString dssFilePath = mProject->directory().filePath( QStringLiteral( "input_%1.dss" ).arg( mCurrentPlan ) );

  ReosTimeWindow tw = hydraulicStructure->timeWindow();
  const QDateTime startTime = tw.start();
  const QDateTime endTime = tw.end();

  currentPlan.changeSimulationTimeInFile( startTime, endTime, this );

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
          path.setGroup( hbc.area() );
          path.setLocation( hbc.boundaryConditionLine() );

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


  ReosGriddedRainfall *griddedPrecipitation = calculationContext.meteorologicModel()->associatedRainfall( hydraulicStructure );
  bool griddedPrecipitationActive = griddedPrecipitation != nullptr && griddedPrecipitation->gridCount() > 0;
  if ( griddedPrecipitationActive )
  {
    QString gridPrecipDssFile;
    ReosDssPath gridPrecipDssPath;
    QDateTime rainStartFirst, rainEndFirst;
    if ( griddedPrecipitation->dataProvider()->key().contains( ReosDssUtils::dssProviderKey() ) )
    {
      // The gridded rainfall is a dss one, so we just use its uri
      const QString uri = griddedPrecipitation->dataProvider()->dataSource();
      gridPrecipDssFile = ReosDssUtils::dssFileFromUri( uri );
      gridPrecipDssPath = ReosDssUtils::dssPathFromUri( uri );
      rainStartFirst = griddedPrecipitation->startTime( 0 );
      rainEndFirst = griddedPrecipitation->endTime( 0 );
    }
    else
    {
      // The gridded rainfall is not a DSS, we nned to write a DSS one temporarly
      gridPrecipDssFile = mProject->directory().filePath( QStringLiteral( "gridded_precip_%1.dss" ).arg( mCurrentPlan ) );
      ReosDssFile file( gridPrecipDssFile, true );
      gridPrecipDssPath.setGroup( mProject->projectName() );
      gridPrecipDssPath.setLocation( hydraulicStructure->elementName()->value() );
      gridPrecipDssPath.setParameter( QStringLiteral( "PRECIP" ) );
      if ( file.isValid() )
      {
        ReosMapExtent extent = hydraulicStructure->extent();
        file.writeGriddedData( griddedPrecipitation, gridPrecipDssPath, extent, -1, calculationContext.timeWindow() );
      }

      for ( int i = 0; i < griddedPrecipitation->gridCount(); ++i )
      {
        if ( griddedPrecipitation->endTime( i ) > calculationContext.timeWindow().start() )
        {
          rainStartFirst = griddedPrecipitation->startTime( i );
          rainEndFirst = griddedPrecipitation->endTime( i );
          break;
        }
      }
    }

    gridPrecipDssPath.setStartDate( ReosDssUtils::dateToHecRasDate( rainStartFirst.date() ) + ':' + rainStartFirst.time().toString( "HHmm" ) );
    gridPrecipDssPath.setTimeInterval( ReosDssUtils::dateToHecRasDate( rainEndFirst.date() ) + ':' + rainEndFirst.time().toString( "HHmm" ) );

    flow.activeGriddedPrecipitation( gridPrecipDssFile, gridPrecipDssPath );
  }
  else
  {
    flow.deactivateGriddedPrecipitation();
  }

  if ( !boundaryToModify.isEmpty() || griddedPrecipitationActive )
    flow.applyBoudaryFlow( boundaryToModify );
}

ReosSimulationProcess *ReosHecRasSimulation::getProcess(
  ReosHydraulicStructure2D *hydraulicStructure,
  const ReosCalculationContext &calculationContext ) const
{
  return new ReosHecRasSimulationProcess( *mProject.get(), mCurrentPlan, calculationContext, hydraulicStructure->boundaryConditions() );
}

ReosDuration ReosHecRasSimulation::representativeTimeStep() const
{
  return std::min( mOutputInterval, mMappingInterval );
}

ReosDuration ReosHecRasSimulation::representative2DTimeStep() const
{
  return mMappingInterval;
}

void ReosHecRasSimulation::saveSimulationResult( const ReosHydraulicStructure2D *, const QString &, ReosSimulationProcess *, bool ) const
{
  // for HEC-RAS everything is already save, so nothing to do
}

ReosHydraulicSimulationResults *ReosHecRasSimulation::loadSimulationResults( ReosHydraulicStructure2D *hydraulicStructure, const QString &shemeId, QObject *parent ) const
{
  if ( hasResult( hydraulicStructure, shemeId ) )
    return new ReosHecRasSimulationResults( this, hydraulicStructure->mesh(), parent );
  else
    return nullptr;
}

bool ReosHecRasSimulation::hasResult( const ReosHydraulicStructure2D *, const QString & ) const
{
  const ReosHecRasPlan currentPlan = mProject->plan( mCurrentPlan );

  const QString meshFile = currentPlan.fileName() + QStringLiteral( ".hdf" );
  QFileInfo meshInfo( meshFile );
  if ( !meshInfo.exists() )
    return false;

  const QString dssFile = mProject->dssResultFile( mCurrentPlan );
  QFileInfo dssInfo( dssFile );
  if ( !dssInfo.exists() )
    return false;

  return true;
}

void ReosHecRasSimulation::removeResults( const ReosHydraulicStructure2D *, const QString & ) const
{
  const ReosHecRasPlan currentPlan = mProject->plan( mCurrentPlan );
  const QString meshFile = currentPlan.fileName() + QStringLiteral( ".hdf" );
  QFile::remove( meshFile );
}

ReosTimeWindow ReosHecRasSimulation::externalTimeWindow() const
{
  ReosHecRasPlan plan = mProject->plan( mCurrentPlan );
  return ReosTimeWindow( plan.startTime(), plan.endTime() );
}

ReosTimeWindow ReosHecRasSimulation::externalBoundaryConditionTimeWindow( const QString &boundaryId ) const
{
  if ( !mProject )
    return ReosTimeWindow();

  const ReosHecRasFlow flow = mProject->flowFromPlan( mCurrentPlan );

  bool found = false;
  ReosHecRasFlow::BoundaryFlow bcFlow = flow.boundary( boundaryId, found );

  if ( !found )
    return ReosTimeWindow();

  if ( bcFlow.isDss && !bcFlow.dssFile.isEmpty() )
  {
    QFileInfo fileInfo( bcFlow.dssFile );
    if ( !fileInfo.exists() )
    {
      QFileInfo projectFile( mProject->fileName() );
      bcFlow.dssFile = projectFile.dir().filePath( bcFlow.dssFile );

    }
    ReosDssFile dssFile( bcFlow.dssFile );
    if ( dssFile.isValid() )
    {
      ReosDssPath dssPath( bcFlow.dssPath );
      if ( dssPath.isValid() )
      {
        QVector<double> values;
        ReosDuration timeStep;
        QDateTime startTime;
        if ( dssFile.getSeries( dssPath, values, timeStep, startTime ) )
          return ReosTimeWindow( startTime, startTime.addMSecs( ( timeStep * values.count() ).valueMilliSecond() ) );
      }
    }
  }
  else if ( bcFlow.type == ReosHecRasFlow::Type::FlowHydrograph || bcFlow.type == ReosHecRasFlow::Type::StageHydrograph )
  {
    QDateTime startTime;
    if ( bcFlow.useFixedStartTime )
    {
      startTime = bcFlow.startTime;
    }
    else
    {
      const ReosHecRasPlan plan = mProject->plan( mCurrentPlan );
      startTime = plan.startTime();
    }
    return ReosTimeWindow( startTime, startTime.addMSecs( ( bcFlow.interval * bcFlow.values.count() ).valueMilliSecond() ) );
  }

  return ReosTimeWindow();
}

ReosHydraulicNetworkElementCompatibilty ReosHecRasSimulation::checkCompatiblity( ReosHydraulicScheme *scheme ) const
{
  return mProject->checkCompatibility( currentPlan( scheme ), mStructure, nullptr );
}

void ReosHecRasSimulation::saveConfiguration( ReosHydraulicScheme *scheme ) const
{
  setCurrentPlanForScheme( mCurrentPlan, scheme );
  setMinimumIntervalForScheme( mMinimumInterval, scheme );
  setComputeIntervalForScheme( mComputeInterval, scheme );
  setOutputIntervalForScheme( mOutputInterval, scheme );
  setDetailledIntervalForScheme( mDetailledInterval, scheme );
  setMappingIntervalForScheme( mMappingInterval, scheme );
}

void ReosHecRasSimulation::restoreConfiguration( ReosHydraulicScheme *scheme )
{
  if ( !scheme )
    return;
  const ReosEncodedElement element = scheme->restoreElementConfig( id() );

  if ( element.hasEncodedData( QStringLiteral( "minimum-interval" ) ) )
    mMinimumInterval = ReosDuration::decode( element.getEncodedData( QStringLiteral( "minimum-interval" ) ) );

  setCurrentPlan( currentPlan( scheme ) );
  setComputeInterval( computeInterval( scheme ) );
  setOutputInterval( outputInterval( scheme ) );
  setDetailledInterval( detailedInterval( scheme ) );
  setMappingInterval( mappingInterval( scheme ) );
}

void ReosHecRasSimulation::setProject( std::shared_ptr<ReosHecRasProject> newProject )
{
  mProject = newProject;
  mProjectFileName = newProject->fileName();
  accordCurrentPlan();

  const ReosHecRasPlan &plan = mProject->plan( mCurrentPlan );
  mComputeInterval = plan.computeInterval();
  mOutputInterval = plan.outputInterval();
  mDetailledInterval = plan.detailedOutputInterval();
  mMappingInterval = plan.mappingInterval();
}

ReosHecRasProject *ReosHecRasSimulation::project() const
{
  return mProject.get();
}

void ReosHecRasSimulation::setCurrentPlanForScheme( const QString &planId, ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );
  element.addData( QStringLiteral( "current-plan-id" ), planId );
  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::setCurrentPlan( const QString &planId, ReosHydraulicScheme *scheme )
{
  if ( !scheme )
  {
    mCurrentPlan = planId;
    accordCurrentPlan();
    if ( mCurrentPlan.isEmpty() )
    {
      mStructure->geometryStructure()->removeAll();
    }
    else
    {
      const ReosHecRasGeometry &geom = mProject->geometryFromPlan( mCurrentPlan );
      QString geomCrs = geom.crs();
      if ( geomCrs.isEmpty() )
        geomCrs = mProject->crs();

      mStructure->geometryStructure()->reset( geom.polylineStructureData(), geomCrs );

      QString schemeId = mStructure->network()->currentScheme()->id();
      mStructure->clearResults( schemeId );

      ReosModule::Message message;
      geom.resetMesh( mStructure->mesh(), mStructure->network()->gisEngine()->crs(), message );

      const QStringList boundaries = mStructure->boundaryConditionId();
      updateBoundaryConditions( mProject.get(), QSet<QString>( boundaries.constBegin(), boundaries.constEnd() ), mStructure, mStructure->network()->context() );

      mStructure->updateResults( schemeId );
    }

    emit dataChanged();
  }
  else
  {
    setCurrentPlanForScheme( planId, scheme );
  }
}

QString ReosHecRasSimulation::currentPlan( ReosHydraulicScheme *scheme ) const
{
  if ( scheme )
  {
    const ReosEncodedElement element = scheme->restoreElementConfig( id() );
    QString currentPlan;
    if ( element.getData( QStringLiteral( "current-plan-id" ), currentPlan ) )
      return currentPlan;
  }

  return mCurrentPlan;
}

ReosDuration ReosHecRasSimulation::minimumInterval( ReosHydraulicScheme *scheme ) const
{
  if ( scheme )
  {
    const ReosEncodedElement element = scheme->restoreElementConfig( id() );
    if ( element.hasEncodedData( QStringLiteral( "minimum-interval" ) ) )
    {
      ReosDuration ret = ReosDuration::decode( element.getEncodedData( QStringLiteral( "minimum-interval" ) ) );
      return ret;
    }
  }

  return mMinimumInterval;
}

void ReosHecRasSimulation::setMinimumIntervalForScheme( const ReosDuration &newMinimumInterval, ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );
  element.addEncodedData( QStringLiteral( "minimum-interval" ), newMinimumInterval.encode() );
  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::setMinimumInterval( const ReosDuration &newMinimumInterval, ReosHydraulicScheme *scheme )
{
  if ( !scheme )
  {
    mMinimumInterval = newMinimumInterval;
    emit dataChanged();
  }
  else
  {
    setMinimumIntervalForScheme( newMinimumInterval, scheme );
  }
}

ReosDuration ReosHecRasSimulation::computeInterval( ReosHydraulicScheme *scheme ) const
{
  if ( scheme )
  {
    const ReosEncodedElement element = scheme->restoreElementConfig( id() );
    if ( element.hasEncodedData( QStringLiteral( "compute-interval" ) ) )
    {
      ReosDuration ret = ReosDuration::decode( element.getEncodedData( QStringLiteral( "compute-interval" ) ) );
      return ret;
    }
  }

  return mComputeInterval;
}

void ReosHecRasSimulation::setComputeIntervalForScheme( const ReosDuration &newComputeInterval, ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );
  element.addEncodedData( QStringLiteral( "compute-interval" ), newComputeInterval.encode() );
  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::setComputeInterval( const ReosDuration &newComputeInterval, ReosHydraulicScheme *scheme )
{
  if ( !scheme )
  {
    mComputeInterval = newComputeInterval;
    emit dataChanged();
  }
  else
  {
    setComputeIntervalForScheme( newComputeInterval, scheme );
  }
}

ReosDuration ReosHecRasSimulation::outputInterval( ReosHydraulicScheme *scheme ) const
{
  if ( scheme )
  {
    const ReosEncodedElement element = scheme->restoreElementConfig( id() );
    if ( element.hasEncodedData( QStringLiteral( "output-interval" ) ) )
    {
      ReosDuration ret = ReosDuration::decode( element.getEncodedData( QStringLiteral( "output-interval" ) ) );
      return ret;
    }
  }

  return mOutputInterval;
}

void ReosHecRasSimulation::setOutputIntervalForScheme( const ReosDuration &newOutputInterval, ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );
  element.addEncodedData( QStringLiteral( "output-interval" ), newOutputInterval.encode() );
  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::setOutputInterval( const ReosDuration &newOutputInterval, ReosHydraulicScheme *scheme )
{
  if ( !scheme )
  {
    mOutputInterval = newOutputInterval;
    emit dataChanged();
  }
  else
  {
    setOutputIntervalForScheme( newOutputInterval, scheme );
  }
}

ReosDuration ReosHecRasSimulation::detailedInterval( ReosHydraulicScheme *scheme ) const
{
  if ( scheme )
  {
    const ReosEncodedElement element = scheme->restoreElementConfig( id() );
    if ( element.hasEncodedData( QStringLiteral( "detailed-interval" ) ) )
    {
      ReosDuration ret = ReosDuration::decode( element.getEncodedData( QStringLiteral( "detailed-interval" ) ) );
      return ret;
    }
  }

  return mDetailledInterval;
}

void ReosHecRasSimulation::setDetailledIntervalForScheme( const ReosDuration &newDetailledInterval, ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );
  element.addEncodedData( QStringLiteral( "detailed-interval" ), newDetailledInterval.encode() );
  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::setDetailledInterval( const ReosDuration &newDetailledInterval, ReosHydraulicScheme *scheme )
{
  if ( !scheme )
  {
    mDetailledInterval = newDetailledInterval;
    emit dataChanged();
  }
  else
  {
    setDetailledIntervalForScheme( newDetailledInterval, scheme );
  }
}

ReosDuration ReosHecRasSimulation::mappingInterval( ReosHydraulicScheme *scheme ) const
{
  if ( scheme )
  {
    const ReosEncodedElement element = scheme->restoreElementConfig( id() );
    if ( element.hasEncodedData( QStringLiteral( "mapping-interval" ) ) )
    {
      ReosDuration ret = ReosDuration::decode( element.getEncodedData( QStringLiteral( "mapping-interval" ) ) );
      return ret;
    }
  }

  return mMappingInterval;
}

void ReosHecRasSimulation::setMappingIntervalForScheme( const ReosDuration &newMappingInterval, ReosHydraulicScheme *scheme ) const
{
  ReosEncodedElement element = scheme->restoreElementConfig( id() );
  element.addEncodedData( QStringLiteral( "mapping-interval" ), newMappingInterval.encode() );
  scheme->saveElementConfig( id(), element );
}

void ReosHecRasSimulation::setMappingInterval( const ReosDuration &newMappingInterval, ReosHydraulicScheme *scheme )
{
  if ( !scheme )
  {
    mMappingInterval = newMappingInterval;
    emit dataChanged();
  }
  else
  {
    setMappingIntervalForScheme( newMappingInterval, scheme );
  }
}

void ReosHecRasSimulation::updateBoundaryConditions( ReosHecRasProject *project, const QSet<QString> &currentBoundaryId, ReosHydraulicStructure2D *structure, const ReosHydraulicNetworkContext &context )
{
  QList<ReosHydraulicStructureBoundaryCondition *> ret;
  const QList<ReosHecRasGeometry::BoundaryCondition> hecBoundaries = project->currentGeometry().allBoundariesConditions();
  QSet<QString> notHecBc = currentBoundaryId;

  QList<ReosHecRasGeometry::BoundaryCondition> toAdd;

  for ( const ReosHecRasGeometry::BoundaryCondition &hecBc : hecBoundaries )
  {
    const QString hecId = hecBc.id();
    if ( currentBoundaryId.contains( hecId ) )
    {
      notHecBc.remove( hecId );
    }
    else
    {
      toAdd.append( hecBc );
    }
  }

  const QList<ReosHydraulicStructureBoundaryCondition *> allCurrent = structure->boundaryConditions();
  for ( ReosHydraulicStructureBoundaryCondition *bc : allCurrent )
  {
    if ( bc && notHecBc.contains( bc->boundaryConditionId() ) )
    {
      QString bcId = bc->boundaryConditionId();
      context.network()->removeElement( bc );
      structure->onExtrernalBoundaryConditionRemoved( bcId );
    }
  }

  for ( const ReosHecRasGeometry::BoundaryCondition &bc : std::as_const( toAdd ) )
  {
    const ReosSpatialPosition position( bc.middlePosition, project->crs() );
    const QString id = bc.id();
    std::unique_ptr<ReosHydraulicStructureBoundaryCondition> sbc( new ReosHydraulicStructureBoundaryCondition( structure, id, position, context ) );
    sbc->elementName()->setValue( bc.name() + QString( '-' ) + bc.area() );
    sbc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
    ret.append( sbc.get() );
    QString bcId = sbc->boundaryConditionId();
    context.network()->addElement( sbc.release() );
    structure->onExtrernalBoundaryConditionAdded( bcId );
  }
}

ReosHydraulicNetworkElementCompatibilty ReosHecRasSimulation::checkPlanCompability( const QString &planId ) const
{
  if ( mStructure->network() && mStructure->network()->hydraulicSchemeCollection() )
  {
    ReosHydraulicNetworkElementCompatibilty ret;
    ReosHydraulicSchemeCollection *schemeCollection = mStructure->network()->hydraulicSchemeCollection();
    int schemeCount = schemeCollection->schemeCount();
    for ( int i = 0; i < schemeCount; ++i )
      ret.combine( mProject->checkCompatibility( planId, mStructure, schemeCollection->scheme( i ) ) ) ;

    return ret;
  }

  return mProject->checkCompatibility( planId, mStructure, nullptr );
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
  ReosDuration miniInter = mMinimumInterval;
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

bool ReosHecRasSimulation::writeDssConstantTimeSeries( ReosTimeSerieConstantInterval *series, const QString &fileName, const ReosDssPath &path, QString &error ) const
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
  if ( !mProject )
    return;
  QStringList planIds = mProject->planIds();
  if ( !planIds.contains( mCurrentPlan ) )
  {
    if ( planIds.empty() )
      mCurrentPlan.clear();
    else
      mCurrentPlan = planIds.first();
  }

  if ( !mCurrentPlan.isEmpty() )
    mProject->setCurrentPlan( mCurrentPlan );
}

ReosHecRasSimulationProcess::ReosHecRasSimulationProcess( const ReosHecRasProject &hecRasProject,
    const QString &planId,
    const ReosCalculationContext &context,
    const QList<ReosHydraulicStructureBoundaryCondition *> &boundaries )
  : ReosSimulationProcess( context, boundaries )
  , mProject( hecRasProject )
  , mPlan( hecRasProject.plan( planId ) )
{
  QStringList availableVersions = ReosHecRasController::availableVersion();
  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "/engine/hecras/version" ) ) )
  {
    mControllerVersion = settings.value( QStringLiteral( "/engine/hecras/version" ) ).toString();
    if ( !availableVersions.contains( mControllerVersion ) )
      mControllerVersion.clear();
  }

  if ( mControllerVersion.isEmpty() && !availableVersions.isEmpty() )
    mControllerVersion = availableVersions.last();
}

void ReosHecRasSimulationProcess::start()
{
  setMaxProgression( 100 );
  setCurrentProgression( 0 );
  mIsSuccessful = false;
  if ( mControllerVersion.isEmpty() )
  {
    emit sendInformation( tr( "None version of HEC-RAS found, please verify that HEC-RAS is installed." ) );
    setCurrentProgression( 100 );
    return;
  }

  QFileInfo fileProjectInfo( mProject.fileName() );
  if ( !fileProjectInfo.exists() )
  {
    emit sendInformation( tr( "HEC-RAS project file \"%1\" not found.\nCalculation cancelled." ).arg( mProject.fileName() ) );
    return;
  }

  QThread thread;
  std::unique_ptr<ReosHecRasController> controller( new ReosHecRasController( mControllerVersion ) );
  controller->setCurrentPlan( mPlan.title() );

  controller->moveToThread( &thread );

  connect( &thread, &QThread::started, controller.get(), &ReosHecRasController::startComputation );

  QEventLoop loop;
  connect( &thread, &QThread::finished, &loop, &QEventLoop::quit );

  thread.start();

  if ( thread.isRunning() )
    loop.exec();

  mIsSuccessful = controller->isSuccessful();

}

ReosHecRasStructureImporterSource::ReosHecRasStructureImporterSource( const QString &file, const ReosHydraulicNetworkContext &context )
  : mHecRasProjectFile( file )
  , mNetwork( context.network() )
{}

ReosHecRasStructureImporterSource::ReosHecRasStructureImporterSource( const ReosEncodedElement &element, const ReosHydraulicNetworkContext &context )
  : mNetwork( context.network() )
{
  if ( element.description() != QStringLiteral( "hec-ras-importer" ) )
    return;

  QString relativeFileName;
  if ( !element.getData( QStringLiteral( "relative-path-to-project" ), relativeFileName ) )
    return;

  const QDir projectDir( context.projectPath() );
  mHecRasProjectFile = projectDir.filePath( relativeFileName );
}

ReosHecRasStructureImporterSource *ReosHecRasStructureImporterSource::clone() const
{
  return new ReosHecRasStructureImporterSource( mHecRasProjectFile, mNetwork->context() );
}

ReosEncodedElement ReosHecRasStructureImporterSource::encode( const ReosHydraulicNetworkContext &context ) const
{
  ReosEncodedElement element( QStringLiteral( "hec-ras-importer" ) );

  element.addData( QStringLiteral( "engine-key" ), ReosHecRasSimulation::staticKey() );

  QDir projectDir( context.projectPath() );
  element.addData( QStringLiteral( "relative-path-to-project" ), projectDir.relativeFilePath( mHecRasProjectFile ) );

  return element;
}

ReosHecRasStructureImporter *ReosHecRasStructureImporterSource::createImporter() const
{
  return new ReosHecRasStructureImporter( mHecRasProjectFile, mNetwork->context(), this );
}
