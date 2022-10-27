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
#include "reoshecrassimulation.h"
#include "reoshydraulicstructure2d.h"
#include "reosgisengine.h"
#include "reoswatershedmodule.h"
#endif

#include "reoshecrasproject.h"
#include "reosdssfile.h"
#include "reosdssprovider.h"
#include "reoshydrograph.h"

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

    void importStructure();
#endif
    void exploreProject();

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

void ReosHecrasTesting::importStructure()
{
  QString path( "C:\\dev\\sources\\ReosProject\\TestsHecRas\\testData\\simple\\simple.prj" );

  QStringList versions = ReosHecrasController::availableVersion();
  ReosHecRasStructureImporter importer( versions.last(), path );

  ReosModule rootModule;
  ReosGisEngine *gisEngine = new ReosGisEngine( &rootModule );
  ReosWatershedModule *watershedModule = new ReosWatershedModule( &rootModule, gisEngine );
  ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &rootModule, gisEngine, watershedModule );

  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( &importer, network->context() );
  QVERIFY( structure != nullptr );
  QVERIFY( structure->domain().count() > 0 );
}
#endif

void ReosHecrasTesting::exploreProject()
{
  QString path( test_path() + QStringLiteral( "simple/simple.prj" ) );
  ReosHecRasProject project( path );

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
