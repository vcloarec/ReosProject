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

#ifdef _WIN32
#include "reoshecrascontroller.h"
#endif

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

static QString tmp_file( std::string basename )
{
  std::string path( TESTHECDATA + std::string( "/tmp" ) );
  std::filesystem::path tmpPath( path );
  if ( !std::filesystem::exists( path ) )
    std::filesystem::create_directory( tmpPath );
  path += basename;
  return QString::fromStdString( path );
}

static QString test_path()
{
  return TESTHECDATA + QString( '/' );
}

static void copyDirectory( const QDir &source, const QString newPath )
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

#ifdef _MSC_VER
    void availableVersion();
    void createControllerInstance();
#endif
    void hecRasDate();
    void dssInterval();
    void exploreProject();
    void importStructure();
    void changeBoundaryCondition();

    void createDssFile();
    void createTimeSerie();

  private:
    QString mPathToSimpleToRun;

    void copySimple();
};

void ReosHecrasTesting::copySimple()
{
  QDir dir_( mPathToSimpleToRun );
  dir_.removeRecursively();

  QDir dir;
  dir.mkpath( mPathToSimpleToRun );
  copyDirectory( test_path() + "/simple/just_built", mPathToSimpleToRun );
}

void ReosHecrasTesting::initTestCase()
{
  mPathToSimpleToRun = tmp_file( "/simple" );
  copySimple();
}

void ReosHecrasTesting::cleanupTestCase()
{
  QDir dir_( mPathToSimpleToRun );
  dir_.removeRecursively();
}

#ifdef _MSC_VER

void ReosHecrasTesting::availableVersion()
{
  QStringList versions = ReosHecrasController::availableVersion();
  QVERIFY( !versions.isEmpty() );
}
void ReosHecrasTesting::createControllerInstance()
{
  QStringList versions = ReosHecrasController::availableVersion();
  ReosHecrasController controller( versions.last() );

  QVERIFY( controller.isValid() );
}
#endif

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

void ReosHecrasTesting::importStructure()
{
  QString path( mPathToSimpleToRun + QStringLiteral( "/simple.prj" ) );

  ReosHecRasStructureImporter importer( path );

  ReosModule rootModule;
  ReosGisEngine *gisEngine = new ReosGisEngine( &rootModule );
  ReosWatershedModule *watershedModule = new ReosWatershedModule( &rootModule, gisEngine );
  ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &rootModule, gisEngine, watershedModule );
  network->setCurrentScheme( 0 );
  ReosHydraulicScheme *scheme = network->currentScheme();
  QVERIFY( scheme );
  scheme->startTime()->setValue( QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  scheme->endTime()->setValue( QDateTime( QDate( 2000, 01, 01 ), QTime( 11, 0, 0 ), Qt::UTC ) );

  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( &importer, network->context() );
  QVERIFY( structure != nullptr );
  QVERIFY( structure->domain().count() > 0 );

  QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions = structure->boundaryConditions();
  QCOMPARE( boundaryConditions.count(), 2 );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "Perimeter 1-Upstream limit" ) );
  QVERIFY( boundaryConditions.at( 1 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
  QCOMPARE( boundaryConditions.at( 0 )->boundaryConditionId(), QStringLiteral( "Perimeter 1-Downstream limit" ) );
  QVERIFY( boundaryConditions.at( 1 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );

  ReosHydraulicStructureBoundaryCondition *upstreamBc = boundaryConditions.at( 1 );
  upstreamBc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( upstreamBc->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::InputFlow );

  // Setup an hydrograph
  std::unique_ptr<ReosHydrograph> hydrograph( new ReosHydrograph );
  hydrograph->setReferenceTime( scheme->startTime()->value() );
  hydrograph->setValue( ReosDuration( 0, ReosDuration::minute ), 0 );
  hydrograph->setValue( ReosDuration( 5, ReosDuration::minute ), 10 );
  hydrograph->setValue( ReosDuration( 8.3, ReosDuration::minute ), 12 );
  hydrograph->setValue( ReosDuration( 10, ReosDuration::minute ), 15 );
  hydrograph->setValue( ReosDuration( 30, ReosDuration::minute ), 35 );
  hydrograph->setValue( ReosDuration( 60, ReosDuration::minute ), 1 );
  hydrograph->setValue( ReosDuration( 2, ReosDuration::day ), 0 );
  //QVERIFY( hydrograph->valueCount() == 6 );
  upstreamBc->gaugedHydrographsStore()->addHydrograph( hydrograph.release() );
  upstreamBc->setInternalHydrographOrigin( ReosHydrographJunction::GaugedHydrograph );
  upstreamBc->setGaugedHydrographIndex( 0 );
  QVERIFY( upstreamBc->outputHydrograph()->valueCount() == 0 );
  //We need the end of the current task to have updated output hydrograph
  QTimer timer;
  QEventLoop loop;
  connect( &timer, &QTimer::timeout, &loop, &QEventLoop::quit );
  timer.start( WAITING_TIME_FOR_LOOP );
  loop.exec();
  //QVERIFY( upstreamBc->outputHydrograph()->valueCount() == 6 );

  ReosHydraulicSimulation *simulation = structure->currentSimulation();
  QVERIFY( simulation );
  simulation->prepareInput( structure, scheme->calculationContext() );

  ReosHecRasProject projectAfterPreparation( path );
  ReosHecRasFlow flowAfterPreparation = projectAfterPreparation.currentFlow();
  QVERIFY( flowAfterPreparation.boundariesCount() == 2 );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).id(), QStringLiteral( "Perimeter 1-Upstream limit" ) );
  QVERIFY( flowAfterPreparation.boundary( 0 ).type == ReosHecRasFlow::Type::FlowHydrograph );
  QVERIFY( flowAfterPreparation.boundary( 0 ).isDss );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).dssFile, mPathToSimpleToRun + QStringLiteral( "/input_p01.dss" ) );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).dssPath, QStringLiteral( "/Perimeter 1/Upstream limit/Flow//1Minute/INST-VAL/" ) );
}

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

void ReosHecrasTesting::createDssFile()
{
  const QString newDssFile = tmp_file( "/dss_file_0" );
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
  QString stringPath( QStringLiteral( "/GrouP/LoCation/FLOW/01jan2000/1Hour/ThisVersion/" ) );
  ReosDssPath path( stringPath );
  QVERIFY( path.isValid() );

  const QString filePath = tmp_file( "/dss_file_1" );

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
  QVERIFY( provider->timeStep() == ReosDuration() );

  provider->setReferenceTime( QDateTime( QDate( 2021, 02, 03 ), QTime( 05, 12, 45 ), Qt::UTC ) );
  ReosDuration timeStep( 1.5, ReosDuration::minute );
  QVERIFY( !provider->isTimeStepCompatible( timeStep ) );
  provider->setTimeStep( timeStep );
  QVERIFY( provider->timeStep() == ReosDuration() ); //Time step not compatible, so not changed

  timeStep = ReosDuration( 10, ReosDuration::minute );
  QVERIFY( provider->isTimeStepCompatible( timeStep ) );
  provider->setTimeStep( timeStep );
  QVERIFY( provider->timeStep() == ReosDuration( 600, ReosDuration::second ) );

  provider->appendValue( 1.23 );
  provider->appendValue( 3.45 );
  provider->appendValue( 6.78 );

  error.clear();
  QVERIFY( provider->persistData( error ) );

  QFile::remove( filePath + QStringLiteral( ".dss" ) );
}


QTEST_MAIN( ReosHecrasTesting )
#include "test_hecras.moc"
