/***************************************************************************
                      test_hecras.cpp
                     --------------------------------------
Date                 : 03-10-2022
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include<QtTest/QtTest>
#include <QObject>
#include <filesystem>


#include "reoshecrascontroller.h"

#include "reosgisengine.h"
#include "reoswatershedmodule.h"
#include "reoshecrassimulation.h"
#include "reoshecrasproject.h"
#include "reosdssfile.h"
#include "reosdssprovider.h"
#include "reosdssutils.h"
#include "reoshydrograph.h"
#include "reoshydraulicscheme.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reoshydraulicsimulationresults.h"
#include "reos_testutils.h"
#include "reosgriddedrainitem.h"
#include "reostimewindowsettings.h"


static void copyDirectory( const QDir &source, const QString &newPath )
{
  const QStringList dirEntries = source.entryList( QDir::NoDotAndDotDot | QDir::Dirs );
  for ( const QString &dir : dirEntries )
  {
    QString path = newPath + QDir::separator() + dir;
    source.mkpath( path );
    copyDirectory( source.path() + QDir::separator() + dir, path );
  }

  const QStringList fileEntries = source.entryList( QDir::Files );
  for ( const QString &file : fileEntries )
    QFile::copy( source.path() + QDir::separator() + file, newPath + QDir::separator() + file );
}

class ReosHecrasTesting : public QObject
{
    Q_OBJECT

  private slots:
    void initTestCase();
    void cleanupTestCase();

#ifdef _WIN32
    void availableVersion();
    void createControllerInstance();
    void getControllerPlans();
#endif

    void createDssFile();

    void createTimeSerie();
    void writeGridInDss();

    void hecRasDate();
    void dssInterval();
    void exploreProject();
    void changeBoundaryCondition();

    void importAndLaunchStructure();

    void simulationResults();


  private:
    QString mPathToSimpleToRun;
    void copySimple();

    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;
    ReosWatershedModule *mWatershedModule = nullptr;
};

void ReosHecrasTesting::copySimple()
{
  QDir dir_( mPathToSimpleToRun );
  dir_.removeRecursively();

  QDir dir;
  dir.mkpath( mPathToSimpleToRun );
  copyDirectory( QString( data_path() ) + "/hecras/simple/just_built", mPathToSimpleToRun );
}

void ReosHecrasTesting::initTestCase()
{
  mGisEngine = new ReosGisEngine( &mRootModule );
  mWatershedModule = new ReosWatershedModule( &mRootModule, mGisEngine );
  mPathToSimpleToRun = tempFile( "/hecras/simple" );
  copySimple();
}

void ReosHecrasTesting::cleanupTestCase()
{
  QDir dir( mPathToSimpleToRun );
  dir.removeRecursively();
}


#ifdef _WIN32
void ReosHecrasTesting::availableVersion()
{
  QStringList versions = ReosHecRasController::availableVersion();
  QVERIFY( !versions.isEmpty() );
}

void ReosHecrasTesting::createControllerInstance()
{
  QStringList versions = ReosHecRasController::availableVersion();
  ReosHecRasController controller( versions.last() );

  QVERIFY( controller.isValid() );
}

void ReosHecrasTesting::getControllerPlans()
{
  qDebug() << "!!!!!!!!!!!!!!  Test: createControllerInstance";
  QStringList versions = ReosHecRasController::availableVersion();
  ReosHecRasController controller( versions.last() );

  QVERIFY( controller.isValid() );

  QString path( mPathToSimpleToRun + QStringLiteral( "/simple.prj" ) );
  QVERIFY( controller.openHecrasProject( path ) );

  QStringList plans = controller.planNames();

  QCOMPARE( plans.count(), 2 );
  QCOMPARE( plans.at( 0 ), QStringLiteral( "plan_test" ) );
  QCOMPARE( plans.at( 1 ), QStringLiteral( "plan_test_2" ) );

  QVERIFY( controller.setCurrentPlan( plans.at( 1 ) ) );
  QVERIFY( controller.setCurrentPlan( plans.at( 0 ) ) );

  QVERIFY( !controller.computeCurrentPlan().isEmpty() );
}

#endif
void ReosHecrasTesting::createDssFile()
{
  const QString newDssFile = tempFile( "/dss_file_0" );
  std::unique_ptr<ReosDssFile> dssFile( new ReosDssFile( newDssFile ) );
  QVERIFY( !dssFile->isValid() );

  dssFile.reset( new ReosDssFile( newDssFile, true ) );

  QVERIFY( dssFile->isValid() );
  QVERIFY( dssFile->isOpen() );

  dssFile.reset();

  QFile::remove( newDssFile + QStringLiteral( ".dss" ) );
}

void ReosHecrasTesting::createTimeSerie()
{
  QString stringPath( QStringLiteral( "/GrouP/LoCation/FLOW///ThisVersion/" ) );
  ReosDssPath path( stringPath );
  QVERIFY( path.isValid() );

  QString filePath = tempFile( "/dss_file_1" );

  ReosDssProviderFactory providerFactory;
  QString error;
  bool res = providerFactory.createNewDataSource( filePath, ReosDssProviderTimeSerieConstantTimeStep::dataType(), error );
  QVERIFY( !res ); //path not present
  res = providerFactory.createNewDataSource( "\"" + filePath + "\"::" + path.string(), ReosDssProviderTimeSerieConstantTimeStep::dataType(), error );
  QVERIFY( res );

  std::unique_ptr<ReosTimeSerieConstantTimeStepProvider> provider(
    static_cast<ReosTimeSerieConstantTimeStepProvider *>( providerFactory.createProvider( ReosDssProviderTimeSerieConstantTimeStep::dataType() ) ) );
  provider->setDataSource( "\"" + filePath + ".dss\"::" + path.string() );

  QCOMPARE( provider->valueCount(), 0 );
  QVERIFY( !provider->referenceTime().isValid() );
  QVERIFY( provider->timeStep() == ReosDuration( 1.0, ReosDuration::hour ) );

  const QDateTime refTime = QDateTime( QDate( 2021, 02, 03 ), QTime( 05, 12, 45 ), Qt::UTC );
  provider->setReferenceTime( refTime );
  ReosDuration timeStep( 1.5, ReosDuration::minute );
  QVERIFY( !provider->isTimeStepCompatible( timeStep ) );
  provider->setTimeStep( timeStep );
  QVERIFY( provider->timeStep() == ReosDuration( 1.0, ReosDuration::hour ) ); //Time step not compatible, so not changed

  timeStep = ReosDuration( 10, ReosDuration::minute );
  QVERIFY( provider->isTimeStepCompatible( timeStep ) );
  provider->setTimeStep( timeStep );
  QVERIFY( provider->timeStep() == ReosDuration( 600, ReosDuration::second ) );

  provider->appendValue( 1.23 );
  provider->appendValue( 3.45 );
  provider->appendValue( 6.78 );

  error.clear();
  QVERIFY( provider->persistData( error ) );

  QCOMPARE( provider->referenceTime(), refTime );
  QCOMPARE( provider->timeStep(), timeStep );
  provider.reset();

  filePath = filePath + QStringLiteral( ".dss" );

  ReosTimeSerieConstantInterval newTimeSeries( nullptr, QStringLiteral( "dss" ), "\"" + filePath + "\"::" + path.string() );
  QCOMPARE( newTimeSeries.referenceTime(), refTime );
  QCOMPARE( newTimeSeries.timeStep(), ReosDuration( 600, ReosDuration::second ) );

  QCOMPARE( newTimeSeries.valueCount(), 3 );
  QCOMPARE( newTimeSeries.valueAt( 0 ), 1.23 );
  QCOMPARE( newTimeSeries.valueAt( 1 ), 3.45 );
  QCOMPARE( newTimeSeries.valueAt( 2 ), 6.78 );

  ReosTimeSerieVariableTimeStep variableTimeSeries( nullptr, QStringLiteral( "dss" ), "\"" + filePath + "\"::" + path.string() );
  QCOMPARE( variableTimeSeries.referenceTime(), refTime );

  QCOMPARE( variableTimeSeries.valueCount(), 3 );
  QCOMPARE( variableTimeSeries.valueAt( 0 ), 1.23 );
  QCOMPARE( variableTimeSeries.valueAt( 1 ), 3.45 );
  QCOMPARE( variableTimeSeries.valueAt( 2 ), 6.78 );

  QCOMPARE( variableTimeSeries.relativeTimeAt( 0 ), ReosDuration( 0, ReosDuration::minute ) );
  QCOMPARE( variableTimeSeries.relativeTimeAt( 1 ), ReosDuration( 10, ReosDuration::minute ) );
  QCOMPARE( variableTimeSeries.relativeTimeAt( 2 ), ReosDuration( 20, ReosDuration::minute ) );

  QFile::remove( filePath );
}

void ReosHecrasTesting::writeGridInDss()
{
  QString gribFile( testFile( QStringLiteral( "grib/arome-antilles" ) ) );
  QString variable( QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" ) );
  std::unique_ptr<ReosGriddedRainfall> rainfall(
    new ReosGriddedRainfall( QStringLiteral( "\"%1\"::%2::%3" ).arg( gribFile, variable, "cumulative" ), QStringLiteral( "grib::gridded-rainfall" ) ) );

  QString projCrs = ReosGisEngine::crsFromEPSG( 32620 );
  rainfall->overrideCrs( ReosGisEngine::crsFromEPSG( 4326 ) );

  ReosMapExtent destination( 593806, 1739924, 747444, 1860601 );
  destination.setCrs( projCrs );

  ReosDssFile file( tempFile( "/hecras/es_vcl.dss" ), true );
  ReosDssPath path;
  path.setGroup( "METEOFRANCE" );
  path.setLocation( "ANTILLES" );
  path.setParameter( "PRECIP" );
  path.setVersion( "VCL" );

  QVERIFY( file.writeGriddedData( rainfall.get(), path, destination ) );
}

void ReosHecrasTesting::hecRasDate()
{
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "02JAN2001" ), QDate( 2001, 1, 2 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "03Feb2009" ), QDate( 2009, 2, 3 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "04MAR2008" ), QDate( 2008, 3, 4 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "06Apr2007" ), QDate( 2007, 4, 6 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "07may2006" ), QDate( 2006, 5, 7 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "08jun2006" ), QDate( 2006, 6, 8 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "09Jul2005" ), QDate( 2005, 7, 9 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "12AUG2005" ), QDate( 2005, 8, 12 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "19sep2014" ), QDate( 2014, 9, 19 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "24Oct2004" ), QDate( 2004, 10, 24 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "25nov2003" ), QDate( 2003, 11, 25 ) );
  QCOMPARE( ReosHecRasProject::hecRasDateToDate( "26DEC2002" ), QDate( 2002, 12, 26 ) );
}

void ReosHecrasTesting::exploreProject()
{
  QString path( mPathToSimpleToRun + QStringLiteral( "/simple.prj" ) );
  ReosHecRasProject project( path );

  QStringList planIds = project.planIds();
  QCOMPARE( planIds.count(), 2 );
  QCOMPARE( project.currentPlanId(), planIds.at( 0 ) );
  QCOMPARE( project.planTitle( project.currentPlanId() ), QStringLiteral( "plan_test" ) );
  QCOMPARE( project.planTitle( planIds.at( 1 ) ), QStringLiteral( "plan_test_2" ) );

  QCOMPARE( project.plan( planIds.at( 0 ) ).startTime(), QDateTime( QDate( 2008, 9, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( project.plan( planIds.at( 0 ) ).endTime(), QDateTime( QDate( 2008, 9, 1 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( project.plan( planIds.at( 1 ) ).startTime(), QDateTime( QDate( 2016, 1, 7 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( project.plan( planIds.at( 1 ) ).endTime(), QDateTime( QDate( 2016, 1, 8 ), QTime( 1, 0, 0 ), Qt::UTC ) );

  QCOMPARE( project.plan( planIds.at( 0 ) ).computeInterval(), ReosDuration( 60.0, ReosDuration::second ) );
  QCOMPARE( project.plan( planIds.at( 0 ) ).outputIntevall(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( project.plan( planIds.at( 0 ) ).detailedOutputInteval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( project.plan( planIds.at( 0 ) ).mappingInteval(), ReosDuration( 5.0, ReosDuration::minute ) );

  QCOMPARE( project.currentGeometry().title(), QStringLiteral( "simple_2D_geometry" ) );

  QCOMPARE( project.GeometriesCount(), 2 );

  QStringList geometryIds = project.geometryIds();
  QCOMPARE( geometryIds.count(), 2 );

  ReosHecRasGeometry geometry = project.geometry( geometryIds.at( 0 ) );
  QCOMPARE( geometry.title(), QStringLiteral( "simple_2D_geometry" ) );
  QCOMPARE( geometry.area2dCount(), 1 );

  QString areaName = geometry.area2dName( 0 );
  QCOMPARE( areaName, QStringLiteral( "Perimeter 1" ) );
  QList<ReosHecRasGeometry::BoundaryCondition> boundaries = geometry.boundariesConditions( areaName );
  QCOMPARE( boundaries.count(), 2 );
  QCOMPARE( boundaries.at( 0 ).area, areaName );
  QCOMPARE( boundaries.at( 0 ).name, QStringLiteral( "Upstream limit" ) );
  QCOMPARE( boundaries.at( 1 ).area, areaName );
  QCOMPARE( boundaries.at( 1 ).name, QStringLiteral( "Downstream limit" ) );

  geometry = project.geometry( geometryIds.at( 1 ) );
  QCOMPARE( geometry.title(), QStringLiteral( "simle_2D_geometry_other" ) );
  QCOMPARE( geometry.area2dCount(), 1 );

  ReosHecRasFlow currentFlow = project.currentFlow();
  QCOMPARE( currentFlow.title(), QStringLiteral( "flow_1" ) );
  QCOMPARE( currentFlow.boundariesCount(), 2 );
  QVERIFY( currentFlow.boundary( 0 ).type == ReosHecRasFlow::Type::FlowHydrograph );
  QVERIFY( currentFlow.boundary( 0 ).isDss );
  QCOMPARE( currentFlow.boundary( 0 ).area, areaName );
  QCOMPARE( currentFlow.boundary( 0 ).boundaryConditionLine, boundaries.at( 0 ).name );
  QVERIFY( currentFlow.boundary( 1 ).type == ReosHecRasFlow::Type::NormalDepth );
  QVERIFY( !currentFlow.boundary( 1 ).isDss );
  QCOMPARE( currentFlow.boundary( 1 ).area, areaName );
  QCOMPARE( currentFlow.boundary( 1 ).boundaryConditionLine, boundaries.at( 1 ).name );
}

#define WAITING_TIME_FOR_LOOP 100

void ReosHecrasTesting::changeBoundaryCondition()
{
  QString path( mPathToSimpleToRun + QStringLiteral( "/simple.prj" ) );
  ReosHecRasProject project( path );

  ReosHecRasFlow currentFlow = project.currentFlow();
  QVERIFY( currentFlow.boundariesCount() == 2 );

  ReosHecRasFlow::BoundaryFlow bc1 = currentFlow.boundary( 0 );
  bc1.isDss = true;
  bc1.dssFile = QStringLiteral( "/my/dss/file" );
  bc1.dssPath = QStringLiteral( "/path/dss/FLOW" );

  ReosHecRasFlow::BoundaryFlow bc2 = currentFlow.boundary( 1 );
  bc2.type = ReosHecRasFlow::Type::StageHydrograph;
  bc2.isDss = true;
  bc2.dssFile = QStringLiteral( "/my/dss/file" );
  bc2.dssPath = QStringLiteral( "/path/dss/STAGE" );

  QList<ReosHecRasFlow::BoundaryFlow> bcs;
  bcs << bc1 << bc2;

  currentFlow.applyBoudaryFlow( bcs );

  ReosHecRasProject project2( path );
  ReosHecRasFlow currentFlow2 = project2.currentFlow();
  QCOMPARE( currentFlow2.title(), QStringLiteral( "flow_1" ) );
  QCOMPARE( currentFlow2.boundariesCount(), 2 );
  QVERIFY( currentFlow2.boundary( 0 ).type == ReosHecRasFlow::Type::FlowHydrograph );
  QVERIFY( currentFlow2.boundary( 0 ).isDss );
  QCOMPARE( currentFlow2.boundary( 0 ).dssFile, QStringLiteral( "/my/dss/file" ) );
  QCOMPARE( currentFlow2.boundary( 0 ).dssPath, QStringLiteral( "/path/dss/FLOW" ) );
  QCOMPARE( currentFlow2.boundary( 0 ).area, bc1.area );
  QCOMPARE( currentFlow2.boundary( 0 ).boundaryConditionLine, bc1.boundaryConditionLine );
  QVERIFY( currentFlow2.boundary( 1 ).type == ReosHecRasFlow::Type::StageHydrograph );
  QVERIFY( currentFlow2.boundary( 1 ).isDss );
  QCOMPARE( currentFlow2.boundary( 1 ).dssFile, QStringLiteral( "/my/dss/file" ) );
  QCOMPARE( currentFlow2.boundary( 1 ).dssPath, QStringLiteral( "/path/dss/STAGE" ) );
  QCOMPARE( currentFlow2.boundary( 1 ).area, bc2.area );
  QCOMPARE( currentFlow2.boundary( 1 ).boundaryConditionLine, bc2.boundaryConditionLine );

  copySimple();
}

void ReosHecrasTesting::dssInterval()
{
  QVERIFY( ReosDuration( 1, ReosDuration::hour ) == ReosDssUtils::dssIntervalToDuration( QStringLiteral( "1HOUR" ) ) );
  QVERIFY( ReosDuration( 1, ReosDuration::minute ) == ReosDssUtils::dssIntervalToDuration( QStringLiteral( "1MINUTE" ) ) );
  QVERIFY( ReosDuration( 6, ReosDuration::minute ) == ReosDssUtils::dssIntervalToDuration( QStringLiteral( "6MINUTES" ) ) );
  QVERIFY( ReosDuration( 1, ReosDuration::day ) == ReosDssUtils::dssIntervalToDuration( QStringLiteral( "1DAY" ) ) );

  QCOMPARE( ReosDssUtils::durationToDssInterval( ReosDuration( 1, ReosDuration::hour ) ), QStringLiteral( "1Hour" ) ) ;
  QCOMPARE( ReosDssUtils::durationToDssInterval( ReosDuration( 1, ReosDuration::minute ) ), QStringLiteral( "1Minute" ) ) ;
  QCOMPARE( ReosDssUtils::durationToDssInterval( ReosDuration( 6, ReosDuration::minute ) ), QStringLiteral( "6Minute" ) ) ;
  QCOMPARE( ReosDssUtils::durationToDssInterval( ReosDuration( 1, ReosDuration::day ) ), QStringLiteral( "1Day" ) ) ;

  QVERIFY( ReosDuration( 1, ReosDuration::minute ) == ReosDssUtils::closestValidInterval( ReosDuration( 62, ReosDuration::second ) ) );
  QVERIFY( ReosDuration::minute == ReosDssUtils::closestValidInterval( ReosDuration( 62, ReosDuration::second ) ).unit() );

  QVERIFY( ReosDuration( 1, ReosDuration::hour ) == ReosDssUtils::closestValidInterval( ReosDuration( 3662, ReosDuration::second ) ) );
  QVERIFY( ReosDuration::hour == ReosDssUtils::closestValidInterval( ReosDuration( 3662, ReosDuration::second ) ).unit() );

  QVERIFY( ReosDuration( 1, ReosDuration::day ) == ReosDssUtils::closestValidInterval( ReosDuration( 72, ReosDuration::hour ) ) );
  QVERIFY( ReosDuration::day == ReosDssUtils::closestValidInterval( ReosDuration( 72, ReosDuration::hour ) ).unit() );

  QVERIFY( ReosDuration( 1, ReosDuration::week ) == ReosDssUtils::closestValidInterval( ReosDuration( 120, ReosDuration::hour ) ) );
  QVERIFY( ReosDuration::week == ReosDssUtils::closestValidInterval( ReosDuration( 120, ReosDuration::hour ) ).unit() );

  QString inter = ReosDssFile::getEPart( ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( inter, QStringLiteral( "1Minute" ) );

  inter = ReosDssFile::getEPart( ReosDuration( 3600, ReosDuration::second ) );
  QCOMPARE( inter, QStringLiteral( "1Hour" ) );
}

void ReosHecrasTesting::importAndLaunchStructure()
{
  QString path( mPathToSimpleToRun + QStringLiteral( "/simple.prj" ) );

  ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &mRootModule, mGisEngine, mWatershedModule );
  network->setCurrentScheme( 0 );
  ReosHydraulicScheme *scheme = network->currentScheme();
  QVERIFY( scheme );

  std::unique_ptr<ReosHecRasStructureImporter> importer( new ReosHecRasStructureImporter( path, network->context() ) );
  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.release(), network->context() );
  QVERIFY( structure != nullptr );
  QVERIFY( structure->domain().count() > 0 );
  QVERIFY( structure->structureImporter() );
  QVERIFY( structure->structureImporter()->isValid() );

  QVERIFY( structure->mesh() );
  QCOMPARE( structure->mesh()->vertexCount(), 1900 );

  ReosHydraulicSimulation *simulation = structure->currentSimulation();
  QVERIFY( simulation );
  ReosHecRasSimulation *hecSim = qobject_cast <ReosHecRasSimulation *>( simulation );
  QVERIFY( hecSim );
  QCOMPARE( hecSim->currentPlan(), QStringLiteral( "p01" ) );
  QCOMPARE( hecSim->computeInterval(), ReosDuration( 60.0, ReosDuration::second ) );
  QCOMPARE( hecSim->outputInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( hecSim->detailedInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( hecSim->mappingInterval(), ReosDuration( 5.0, ReosDuration::minute ) );

  QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions = structure->boundaryConditions();
  QCOMPARE( boundaryConditions.count(), 2 );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "Perimeter 1-Upstream limit" ) );
  QVERIFY( boundaryConditions.at( 1 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
  QCOMPARE( boundaryConditions.at( 0 )->boundaryConditionId(), QStringLiteral( "Perimeter 1-Downstream limit" ) );
  QVERIFY( boundaryConditions.at( 0 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );

  ReosHydraulicStructureBoundaryCondition *upstreamBc = boundaryConditions.at( 1 );
  upstreamBc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( upstreamBc->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::InputFlow );

  structure->timeWindowSettings()->automaticallyDefined()->setValue( false );
  structure->timeWindowSettings()->userStartTime()->setValue( QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  structure->timeWindowSettings()->userEndTime()->setValue( QDateTime( QDate( 2000, 01, 01 ), QTime( 12, 0, 0 ), Qt::UTC ) );

  // Setup an hydrograph
  std::unique_ptr<ReosHydrograph> hydrograph( new ReosHydrograph );
  hydrograph->setReferenceTime( QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  hydrograph->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph->setValue( ReosDuration( 5, ReosDuration::minute ), 10 );
  hydrograph->setValue( ReosDuration( 8.3, ReosDuration::minute ), 12 );
  hydrograph->setValue( ReosDuration( 10, ReosDuration::minute ), 15 );
  hydrograph->setValue( ReosDuration( 30, ReosDuration::minute ), 35 );
  hydrograph->setValue( ReosDuration( 60, ReosDuration::minute ), 1 );
  hydrograph->setValue( ReosDuration( 2, ReosDuration::day ), 0 );
  QVERIFY( hydrograph->valueCount() == 7 );
  upstreamBc->gaugedHydrographsStore()->addHydrograph( hydrograph.release() );
  upstreamBc->setInternalHydrographOrigin( ReosHydrographJunction::GaugedHydrograph );
  upstreamBc->setGaugedHydrographIndex( 0 );
  QVERIFY( upstreamBc->outputHydrograph()->valueCount() == 0 );
  //We need the end of the current task to have updated output hydrograph

  simulateEventLoop( WAITING_TIME_FOR_LOOP );

  QVERIFY( upstreamBc->outputHydrograph()->valueCount() == 7 );

  // setup time interval simulation
  hecSim->setComputeInterval( ReosDuration( 10, ReosDuration::second ) );
  hecSim->setOutputInterval( ReosDuration( 1, ReosDuration::minute ) );
  hecSim->setDetailledInterval( ReosDuration( 1, ReosDuration::minute ) );
  hecSim->setMappingInterval( ReosDuration( 1, ReosDuration::minute ) );

  ReosSimulationData simulationData;
  simulation->prepareInput( structure, simulationData, scheme->calculationContext() );

  ReosHecRasProject projectAfterPreparation( path );
  ReosHecRasPlan planAfterPreparation = projectAfterPreparation.currentPlan();

  QCOMPARE( planAfterPreparation.startTime(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( planAfterPreparation.endTime(), QDateTime( QDate( 2000, 01, 01 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QCOMPARE( planAfterPreparation.computeInterval(), ReosDuration( 10.0, ReosDuration::second ) );
  QCOMPARE( planAfterPreparation.outputIntevall(), ReosDuration( 1.0, ReosDuration::minute ) );
  QCOMPARE( planAfterPreparation.detailedOutputInteval(), ReosDuration( 1.0, ReosDuration::minute ) );
  QCOMPARE( planAfterPreparation.mappingInteval(), ReosDuration( 1.0, ReosDuration::minute ) );

  ReosHecRasFlow flowAfterPreparation = projectAfterPreparation.currentFlow();
  QVERIFY( flowAfterPreparation.boundariesCount() == 2 );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).id(), QStringLiteral( "Perimeter 1-Upstream limit" ) );
  QVERIFY( flowAfterPreparation.boundary( 0 ).type == ReosHecRasFlow::Type::FlowHydrograph );
  QVERIFY( flowAfterPreparation.boundary( 0 ).isDss );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).dssFile, mPathToSimpleToRun + QStringLiteral( "/input_p01.dss" ) );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).dssPath, QStringLiteral( "/Perimeter 1/Upstream limit/Flow//1Minute/INST-VAL/" ) );


  QStringList versions = ReosHecRasController::availableVersion();
  if ( !versions.isEmpty() )
  {
    ReosHecRasController controller( versions.last() );

    QVERIFY( controller.isValid() );

    QVERIFY( controller.openHecrasProject( path ) );

    QStringList plans = controller.planNames();

    QCOMPARE( plans.count(), 2 );
    QCOMPARE( plans.at( 0 ), QStringLiteral( "plan_test" ) );
    QCOMPARE( plans.at( 1 ), QStringLiteral( "plan_test_2" ) );

    QVERIFY( controller.setCurrentPlan( plans.at( 1 ) ) );
    QVERIFY( controller.setCurrentPlan( plans.at( 0 ) ) );

    QVERIFY( !controller.computeCurrentPlan().isEmpty() );
  }

  ReosEncodedElement encodedNetwork = network->encode( QFileInfo( path ).dir().path(), "project.lkn" );

  network->clear();
  network->decode( encodedNetwork, QFileInfo( path ).dir().path(), "project.lkn" );

  structure = qobject_cast<ReosHydraulicStructure2D *>( network->hydraulicNetworkElements( ReosHydraulicStructure2D::staticType() ).at( 0 ) );
  QVERIFY( structure );
  QVERIFY( structure->boundaryConditions().count() == 2 );
  QVERIFY( structure->structureImporter() );
  QVERIFY( structure->structureImporter()->isValid() );

  boundaryConditions = structure->boundaryConditions();
  QCOMPARE( boundaryConditions.count(), 2 );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "Perimeter 1-Upstream limit" ) );
  QVERIFY( boundaryConditions.at( 1 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QCOMPARE( boundaryConditions.at( 0 )->boundaryConditionId(), QStringLiteral( "Perimeter 1-Downstream limit" ) );
  QVERIFY( boundaryConditions.at( 0 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );

  QVERIFY( structure->mesh() );
  QCOMPARE( structure->mesh()->vertexCount(), 1900 );
}

void ReosHecrasTesting::simulationResults()
{
  QString projectPath = data_path() + QStringLiteral( "/hecras/simple/calculated/simple.prj" );

  ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &mRootModule, mGisEngine, mWatershedModule );
  network->setCurrentScheme( 0 );
  ReosHydraulicScheme *scheme = network->currentScheme();
  QVERIFY( scheme );

  std::unique_ptr<ReosHecRasStructureImporter> importer( new ReosHecRasStructureImporter( projectPath, network->context() ) );
  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.release(), network->context() );
  QVERIFY( structure );

  std::shared_ptr<ReosHecRasProject> project = std::make_shared<ReosHecRasProject>( projectPath );

  ReosHydraulicSimulation *simulation = structure->currentSimulation();
  ReosHydraulicScheme *currentScheme = network->currentScheme();

  QVERIFY( simulation->hasResult( structure, currentScheme->id() ) );

  std::unique_ptr<ReosHydraulicSimulationResults> simResult( simulation->loadSimulationResults( structure, scheme->id() ) );

  QMap<QString, ReosHydrograph *> outputHydrographs = simResult->outputHydrographs();
  QVERIFY( outputHydrographs.count() == 2 );
  QVERIFY( outputHydrographs.contains( QStringLiteral( "Perimeter 1-Downstream limit" ) ) );
  ReosHydrograph *hyd = outputHydrographs.value( QStringLiteral( "Perimeter 1-Downstream limit" ) );
  QDateTime refTime = hyd->referenceTime();
  QCOMPARE( refTime, QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QVERIFY( hyd->valueCount() > 0 );

  QCOMPARE( simResult->groupCount(), 2 );
  QCOMPARE( simResult->datasetCount( 0 ), 121 );
  QCOMPARE( simResult->datasetType( 0 ), ReosHydraulicSimulationResults::DatasetType::WaterLevel );
  QCOMPARE( simResult->datasetType( 1 ), ReosHydraulicSimulationResults::DatasetType::Velocity );
  QCOMPARE( simResult->groupLocation( 0 ), ReosHydraulicSimulationResults::Location::Face );
  QCOMPARE( simResult->groupLocation( 1 ), ReosHydraulicSimulationResults::Location::Face );
  QCOMPARE( simResult->groupIsScalar( 1 ), false );
  QCOMPARE( simResult->datasetValuesCount( 0, 0 ), 1746 ) ;
  QCOMPARE( simResult->datasetValues( 0, 0 ).at( 1258 ), 2.000097513198853 ) ;

  simulateEventLoop( WAITING_TIME_FOR_LOOP );

  ReosTimeWindow tw = structure->timeWindow();
  QCOMPARE( tw.start(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( tw.end(), QDateTime( QDate( 2000, 01, 01 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QVERIFY( tw == mGisEngine->mapTimeWindow() );

}

QTEST_MAIN( ReosHecrasTesting )
#include "test_hecras.moc"
