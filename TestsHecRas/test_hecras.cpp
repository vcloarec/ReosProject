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
#include "reoshydrograph.h"
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

class ReosHecrasTesting : public QObject
{
    Q_OBJECT

  private slots:
#ifdef _MSC_VER
    void availableVersion();
    void createControllerInstance();
#endif
    void hecRasDate();
    void exploreProject();
    void importStructure();

    void validInterval();
    void createDssFile();
    void createTimeSerie();
};

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

void ReosHecrasTesting::exploreProject()
{
  QString path( test_path() + QStringLiteral( "simple/simple.prj" ) );
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
  QCOMPARE( boundaries.at( 0 ).name, QStringLiteral( "Upstream limit" ) );
  QCOMPARE( boundaries.at( 1 ).name, QStringLiteral( "Downstream limit" ) );

  geometry = project.geometry( geometryIds.at( 1 ) );
  QCOMPARE( geometry.title(), QStringLiteral( "simle_2D_geometry_other" ) );
  QCOMPARE( geometry.area2dCount(), 1 );
}


void ReosHecrasTesting::importStructure()
{
  QString path( test_path() + QStringLiteral( "simple/simple.prj" ) );

  ReosHecRasStructureImporter importer( path );

  ReosModule rootModule;
  ReosGisEngine *gisEngine = new ReosGisEngine( &rootModule );
  ReosWatershedModule *watershedModule = new ReosWatershedModule( &rootModule, gisEngine );
  ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &rootModule, gisEngine, watershedModule );

  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( &importer, network->context() );
  QVERIFY( structure != nullptr );
  QVERIFY( structure->domain().count() > 0 );

  QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions = structure->boundaryConditions();
  QCOMPARE( boundaryConditions.count(), 2 );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "Upstream limit" ) );
  QCOMPARE( boundaryConditions.at( 0 )->boundaryConditionId(), QStringLiteral( "Downstream limit" ) );
}


void ReosHecrasTesting::validInterval()
{
  QVERIFY( ReosDuration( 1, ReosDuration::minute ) == ReosDssFile::closestValidInterval( ReosDuration( 62, ReosDuration::second ) ) );
  QVERIFY( ReosDuration::minute == ReosDssFile::closestValidInterval( ReosDuration( 62, ReosDuration::second ) ).unit() );

  QVERIFY( ReosDuration( 1, ReosDuration::hour ) == ReosDssFile::closestValidInterval( ReosDuration( 3662, ReosDuration::second ) ) );
  QVERIFY( ReosDuration::hour == ReosDssFile::closestValidInterval( ReosDuration( 3662, ReosDuration::second ) ).unit() );

  QVERIFY( ReosDuration( 1, ReosDuration::day ) == ReosDssFile::closestValidInterval( ReosDuration( 72, ReosDuration::hour ) ) );
  QVERIFY( ReosDuration::day == ReosDssFile::closestValidInterval( ReosDuration( 72, ReosDuration::hour ) ).unit() );

  QVERIFY( ReosDuration( 1, ReosDuration::week ) == ReosDssFile::closestValidInterval( ReosDuration( 120, ReosDuration::hour ) ) );
  QVERIFY( ReosDuration::week == ReosDssFile::closestValidInterval( ReosDuration( 120, ReosDuration::hour ) ).unit() );

  QString inter = ReosDssFile::getEPart( ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( inter, QStringLiteral( "1Minute" ) );

  inter = ReosDssFile::getEPart( ReosDuration( 3600, ReosDuration::second ) );
  QCOMPARE( inter, QStringLiteral( "1Hour" ) );
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

  QVERIFY( provider->persistData( error ) );

  QFile::remove( filePath + QStringLiteral( ".dss" ) );
}

QTEST_MAIN( ReosHecrasTesting )
#include "test_hecras.moc"
