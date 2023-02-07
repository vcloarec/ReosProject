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
#include "reoshydrographrouting.h"
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
#include "reoshdf5.h"
#include "reostestrenderedobject.h"
#include "reosgriddedrainfallprovider.h"


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

    void findTerrainFiles();

    void manipulateDssFile();

    void createTimeSerie();
    void writeGridInDss();
    void createAndWriteGridFromScratch();

    void hecRasDate();
    void dssInterval();
    void exploreProject();
    void changeBoundaryCondition();

    void importAndLaunchStructure();

    void simulationResults();

    void planCompatibility();

    void importCreatingScheme();

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
  mPathToSimpleToRun = tempFile( "hecras/simple" );
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

void ReosHecrasTesting::findTerrainFiles()
{
  QDir dir( mPathToSimpleToRun );
  QString path = dir.filePath( QStringLiteral( "simple.g01.hdf" ) );

  ReosHdf5File file( path );
  QVERIFY( file.isValid() );
  QVERIFY( file.pathExists( QStringLiteral( "/Geometry" ) ) );

  ReosHdf5Group group = file.createGroup( QStringLiteral( "/Geometry" ) );
  QVERIFY( group.isValid() );

  ReosHdf5Attribute terrainFileAttr = group.attribute( QStringLiteral( "Terrain Filename" ) );
  QVERIFY( terrainFileAttr.isValid() );
  QString terrainFilePath = terrainFileAttr.readString();

  if ( terrainFilePath.contains( QStringLiteral( "\\" ) ) )
    terrainFilePath.replace( QLatin1String( "\\" ), QString( '/' ) );

  QString terrainFileName = dir.filePath( terrainFilePath );

  QFileInfo terrainFileInfo( terrainFileName );
  QVERIFY( terrainFileInfo.exists() );

  QString vrtFile = terrainFileInfo.dir().filePath( terrainFileInfo.baseName() + QStringLiteral( ".vrt" ) );
  QFileInfo vertFileInfo( vrtFile );
  QVERIFY( vertFileInfo.exists() );
}


void ReosHecrasTesting::manipulateDssFile()
{
  const QString newDssFile = tempFile( "/dss_file_0" );
  QFile::remove( newDssFile + QStringLiteral( ".dss" ) ); // to be sure th

  std::unique_ptr<ReosDssFile> dssFile( new ReosDssFile( newDssFile ) );
  QVERIFY( !dssFile->isValid() );

  dssFile.reset( new ReosDssFile( newDssFile, true ) );

  QVERIFY( dssFile->isValid() );
  QVERIFY( dssFile->isOpen() );

  QList<ReosDssPath> pathes = dssFile->allPathes();
  QVERIFY( pathes.isEmpty() );

  ReosDssPath pathSeries( QStringLiteral( "/group1/location1/FLOW///version1/" ) );

  int valueCount = 120;
  QVector<double> values( valueCount );
  QDateTime startTime( QDateTime( QDate( 2020, 02, 02 ), QTime( 05, 00, 00 ), Qt::UTC ) );
  for ( int i = 0; i < valueCount; ++i )
    values[i] = static_cast<double>( i * 2 );
  QString error;
  dssFile->writeConstantIntervalSeries( pathSeries, startTime, ReosDuration( 5, ReosDuration::minute ), values, error );
  QVERIFY( error.isEmpty() );
  pathes = dssFile->allPathes();
  QCOMPARE( pathes.count(), 1 );
  QCOMPARE( pathes.at( 0 ).string(), QStringLiteral( "/group1/location1/FLOW/02Feb2020/5Minute/version1/" ) );
  dssFile.reset();

  dssFile.reset( new ReosDssFile( newDssFile, true ) ); // create==true, ovewrite the file so, must be empty
  pathes = dssFile->allPathes();
  QVERIFY( pathes.isEmpty() );
  dssFile->writeConstantIntervalSeries( pathSeries, startTime, ReosDuration( 5, ReosDuration::minute ), values, error );
  QVERIFY( error.isEmpty() );
  pathes = dssFile->allPathes();
  QCOMPARE( pathes.count(), 1 );
  QCOMPARE( pathes.at( 0 ).string(), QStringLiteral( "/group1/location1/FLOW/02Feb2020/5Minute/version1/" ) );

  dssFile.reset();
  dssFile.reset( new ReosDssFile( newDssFile, false ) ); // create==false, we just open the file
  pathes = dssFile->allPathes();
  QCOMPARE( pathes.count(), 1 );
  QCOMPARE( pathes.at( 0 ).string(), QStringLiteral( "/group1/location1/FLOW/02Feb2020/5Minute/version1/" ) );

  values.clear();
  ReosDuration timeStep;
  startTime = QDateTime();
  pathSeries.setTimeInterval( ReosDuration( 5, ReosDuration::minute ) );
  QVERIFY( dssFile->getSeries( pathSeries, values, timeStep, startTime ) );
  QCOMPARE( startTime, QDateTime( QDate( 2020, 02, 02 ), QTime( 05, 00, 00 ), Qt::UTC ) );
  QCOMPARE( values.count(), 120 );
  for ( int i = 0; i < valueCount; ++i )
    QCOMPARE( values.at( i ), static_cast<double>( i * 2 ) );

  values.clear();
  startTime = QDateTime();
  ReosTimeWindow timeWindow( QDateTime( QDate( 2020, 02, 02 ), QTime( 06, 00, 00 ), Qt::UTC ),
                             QDateTime( QDate( 2020, 02, 02 ), QTime( 06, 30, 00 ), Qt::UTC ) );
  QVERIFY( dssFile->getSeries( pathSeries, timeWindow, values, timeStep, startTime ) );
  QCOMPARE( startTime, QDateTime( QDate( 2020, 02, 02 ), QTime( 06, 00, 00 ), Qt::UTC ) );
  QCOMPARE( values.count(), 7 );
  for ( int i = 0; i < 7; ++i )
    QCOMPARE( values.at( i ), static_cast<double>( ( i + 12 ) * 2 ) );

  values.clear();
  startTime = QDateTime();
  timeWindow = ReosTimeWindow( QDateTime( QDate( 2020, 02, 02 ), QTime( 04, 00, 00 ), Qt::UTC ),
                               QDateTime( QDate( 2020, 02, 02 ), QTime( 05, 45, 00 ), Qt::UTC ) );
  QVERIFY( dssFile->getSeries( pathSeries, timeWindow, values, timeStep, startTime ) );
  QCOMPARE( startTime, QDateTime( QDate( 2020, 02, 02 ), QTime( 05, 00, 00 ), Qt::UTC ) );
  QCOMPARE( values.count(), 10 );
  for ( int i = 0; i < 10; ++i )
    QCOMPARE( values.at( i ), static_cast<double>( ( i ) * 2 ) );

  values.clear();
  startTime = QDateTime();
  timeWindow = ReosTimeWindow( QDateTime( QDate( 2020, 02, 02 ), QTime( 14, 00, 00 ), Qt::UTC ),
                               QDateTime( QDate( 2020, 02, 02 ), QTime( 15, 30, 00 ), Qt::UTC ) );
  QVERIFY( dssFile->getSeries( pathSeries, timeWindow, values, timeStep, startTime ) );
  QCOMPARE( startTime, QDateTime( QDate( 2020, 02, 02 ), QTime( 14, 00, 00 ), Qt::UTC ) );
  QCOMPARE( values.count(), 12 );
  for ( int i = 0; i < 12; ++i )
    QCOMPARE( values.at( i ), static_cast<double>( ( i + 12 * 9 ) * 2 ) );

  dssFile.reset();

  std::unique_ptr<ReosHydrograph> hydrograph( new ReosHydrograph(
        nullptr, QStringLiteral( "dss" ),
        "\"" + newDssFile + QStringLiteral( ".dss" ) +
        "\"::/group1/location1/FLOW/02Feb2020/5Minute/version1/" + "::" +
        "2020:02:02T06:00:00Z::2020:02:02T08:00:00Z" ) );

  QCOMPARE( hydrograph->valueCount(), 25 );
  QCOMPARE( hydrograph->referenceTime(), QDateTime( QDate( 2020, 02, 02 ), QTime( 6, 00, 00 ), Qt::UTC ) );
  QCOMPARE( hydrograph->timeExtent().second, QDateTime( QDate( 2020, 02, 02 ), QTime( 8, 00, 00 ), Qt::UTC ) );
  for ( int i = 0; i < 25; ++i )
    QCOMPARE( hydrograph->valueAt( i ), static_cast<double>( ( i + 12 ) * 2 ) );

  hydrograph.reset( new ReosHydrograph(
                      nullptr, QStringLiteral( "dss" ),
                      "\"" + newDssFile + QStringLiteral( ".dss" ) +
                      "\"::/group1/location1/FLOW/02Feb2020/5Minute/version1/" + "::" +
                      "2020:02:02T02:00:00Z::2020:02:02T07:00:00Z" ) );

  QCOMPARE( hydrograph->valueCount(), 25 );
  QCOMPARE( hydrograph->timeExtent().first, QDateTime( QDate( 2020, 02, 02 ), QTime( 5, 00, 00 ), Qt::UTC ) );
  QCOMPARE( hydrograph->timeExtent().second, QDateTime( QDate( 2020, 02, 02 ), QTime( 7, 00, 00 ), Qt::UTC ) );
  for ( int i = 0; i < 25; ++i )
    QCOMPARE( hydrograph->valueAt( i ), static_cast<double>( ( i ) * 2 ) );

  hydrograph.reset();

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
    new ReosGriddedRainfall( QStringLiteral( "\"%1\"::%2::%3" ).arg( gribFile, variable, "cumulative" ), QStringLiteral( "grib::gridded-precipitation" ) ) );

  QString projCrs = ReosGisEngine::crsFromEPSG( 32620 );
  rainfall->overrideCrs( ReosGisEngine::crsFromEPSG( 4326 ) );

  ReosMapExtent destination( 593806, 1739924, 747444, 1860601 );
  destination.setCrs( projCrs );

  ReosDssFile file( tempFile( "/hecras/es_vcl.dss" ), true );
  ReosDssPath path;
  path.setGroup( "METEOFRANCE" );
  path.setLocation( "ANTILLES" );
  path.setVersion( "VCL" );

  QVERIFY( file.writeGriddedData( rainfall.get(), path, destination ) );
}

void ReosHecrasTesting::createAndWriteGridFromScratch()
{
  QString filePath( tempFile( "/hecras/gridded_jarry.dss" ) );
  ReosDssPath path_1;
  path_1.setGroup( "HOME_MADE" );
  path_1.setLocation( "JARRY" );
  path_1.setVersion( "VCL" );

  {
    ReosGriddedRainfallMemoryProvider memoryRainfallProvider;

    ReosRasterMemory<double> frame1( 30, 30 );
    frame1.reserveMemory();
    frame1.fill( 0 );

    for ( int i = 0; i < 2; ++i )
      for ( int j = 0; j < 2; ++j )
      {
        frame1.setValue( i, j, 5 );
        frame1.setValue( i, 28 + j, 10 );
        frame1.setValue( 28 + i, 28 + j, 15 );
        frame1.setValue( 28 + i, j, 20 );
      }

    memoryRainfallProvider.addFrame( frame1,
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 30, 0 ), Qt::UTC ),
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 35, 0 ), Qt::UTC ) );

    ReosRasterMemory<double> frame2( 30, 30 );
    frame2.reserveMemory();
    frame2.fill( 0 );
    for ( int i = 5; i < 25; ++i )
      for ( int j = 5; j < 25; j++ )
        frame2.setValue( i, j, std::abs( 15 - std::max( i, j ) ) );
    memoryRainfallProvider.addFrame( frame2,
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 35, 0 ), Qt::UTC ),
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 40, 0 ), Qt::UTC ) );

    ReosRasterMemory<double> frame3( 30, 30 );
    frame3.reserveMemory();
    frame3.fill( 0 );
    for ( int i = 5; i < 25; ++i )
      for ( int j = 5; j < 25; j++ )
        frame3.setValue( i, j, std::abs( 10 - std::max( i, j ) ) );
    memoryRainfallProvider.addFrame( frame3,
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 40, 0 ), Qt::UTC ),
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 45, 0 ), Qt::UTC ) );

    QPolygonF polyExtent;
    polyExtent << QPointF( 652991.48222804930992424, 1796819.82771771727129817 )
               << QPointF( 652991.48222804884426296, 1797504.95862683397717774 )
               << QPointF( 653726.54979237192310393, 1797504.95862683444283903 )
               << QPointF( 653726.54979237180668861, 1796819.82771771727129817 );

    ReosMapExtent extent( polyExtent, ReosGisEngine::crsFromEPSG( 32620 ) );

    memoryRainfallProvider.setExtent( ReosRasterExtent( extent, 30, 30 ) );

    ReosGriddedRainfall rainfall;
    rainfall.copyFrom( &memoryRainfallProvider );

    ReosDssFile file( filePath, true );
    file.writeGriddedData( &rainfall, path_1 );
    file.close();
  }

  ReosDssPath path_2;
  path_2.setGroup( "HOME_MADE_2" );
  path_2.setLocation( "JARRY_2" );
  path_2.setVersion( "VCL" );
  {
    ReosGriddedRainfallMemoryProvider memoryRainfallProvider;

    ReosRasterMemory<double> frame1( 30, 30 );
    frame1.reserveMemory();
    frame1.fill( 0 );

    for ( int i = 0; i < 2; ++i )
      for ( int j = 0; j < 2; ++j )
      {
        frame1.setValue( i, j, 25 );
        frame1.setValue( i, 28 + j, 30 );
        frame1.setValue( 28 + i, 28 + j, 65 );
        frame1.setValue( 28 + i, j, 70 );
      }

    memoryRainfallProvider.addFrame( frame1,
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 30, 0 ), Qt::UTC ),
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 35, 0 ), Qt::UTC ) );

    ReosRasterMemory<double> frame2( 30, 30 );
    frame2.reserveMemory();
    frame2.fill( 0 );
    for ( int i = 5; i < 25; ++i )
      for ( int j = 5; j < 25; j++ )
        frame2.setValue( i, j, std::abs( 15 - std::max( i, j ) ) );
    memoryRainfallProvider.addFrame( frame2,
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 35, 0 ), Qt::UTC ),
                                     QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 40, 0 ), Qt::UTC ) );

    QPolygonF polyExtent;
    polyExtent << QPointF( 652991.48222804930992424, 1796819.82771771727129817 )
               << QPointF( 652991.48222804884426296, 1797504.95862683397717774 )
               << QPointF( 653726.54979237192310393, 1797504.95862683444283903 )
               << QPointF( 653726.54979237180668861, 1796819.82771771727129817 );

    ReosMapExtent extent( polyExtent, ReosGisEngine::crsFromEPSG( 32620 ) );

    memoryRainfallProvider.setExtent( ReosRasterExtent( extent, 30, 30 ) );

    ReosGriddedRainfall rainfall;
    rainfall.copyFrom( &memoryRainfallProvider );

    ReosDssFile file( filePath, false );
    file.writeGriddedData( &rainfall, path_2 );
    file.close();
  }

  // Now, we try to open it
  std::unique_ptr<ReosDssProviderGriddedRainfall> dssProvider = std::make_unique<ReosDssProviderGriddedRainfall>();
  QVERIFY( dssProvider->canReadUri( filePath ) );

  dssProvider.reset( static_cast<ReosDssProviderGriddedRainfall *>(
                       ReosDataProviderRegistery::instance()->createCompatibleProvider( filePath, ReosGriddedRainfall::staticType() ) ) );

  QVERIFY( dssProvider );
  ReosModule::Message message;
  QList<ReosDssPath> griddedPathes = dssProvider->griddedRainfallPathes( filePath, message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( griddedPathes.count(), 2 );

  ReosGriddedRainfallProvider::FileDetails details = dssProvider->details( filePath, message );
  QVERIFY( message.type == ReosModule::Simple );
  QCOMPARE( details.availableVariables.count(), 2 );
  QCOMPARE( details.availableVariables.at( 0 ), QStringLiteral( "/HOME_MADE/JARRY/PRECIP///VCL/" ) );
  QCOMPARE( details.availableVariables.at( 1 ), QStringLiteral( "/HOME_MADE_2/JARRY_2/PRECIP///VCL/" ) );

  ReosDssPath gridPath = griddedPathes.at( 0 );
  QVERIFY( path_1.isEquivalent( gridPath ) );

  QString uri = ReosDssUtils::uri( filePath, gridPath );
  QCOMPARE( uri, "\"" + filePath + "\"::/HOME_MADE/JARRY/PRECIP///VCL/" );

  std::unique_ptr<ReosGriddedRainfall> griddedPrecipitation = std::make_unique<ReosGriddedRainfall>( uri, dssProvider->key() );

  QCOMPARE( griddedPrecipitation->gridCount(), 3 );

  QCOMPARE( griddedPrecipitation->startTime( 0 ),  QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 30, 0 ), Qt::UTC ) );
  QCOMPARE( griddedPrecipitation->endTime( 0 ),  QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 35, 0 ), Qt::UTC ) );
  QCOMPARE( griddedPrecipitation->startTime( 1 ),  QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 35, 0 ), Qt::UTC ) );
  QCOMPARE( griddedPrecipitation->endTime( 1 ),  QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 40, 0 ), Qt::UTC ) );
  QCOMPARE( griddedPrecipitation->startTime( 2 ),  QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 40, 0 ), Qt::UTC ) );
  QCOMPARE( griddedPrecipitation->endTime( 2 ),  QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 45, 0 ), Qt::UTC ) );

  ReosRasterExtent griddedExtent = griddedPrecipitation->rasterExtent();

  QCOMPARE( griddedExtent.xCellCount(), 32 );
  QCOMPARE( griddedExtent.yCellCount(), 30 );

  QVector<double> values = griddedPrecipitation->intensityValues( 0 );

  QCOMPARE( values.count(), 32 * 30 );
  QCOMPARE( values.at( 0 ), 5.0 );
  QCOMPARE( values.at( 30 ), 10.0 );
  QCOMPARE( values.at( 29 * 32 + 31 ), 15.0 );
  QCOMPARE( values.at( 29 * 32 ), 20.0 );

  double min = -20;
  double max = -20;
  griddedPrecipitation->calculateMinMaxValue( min, max );

  QCOMPARE( min, 0 );
  QCOMPARE( max, 20 );

  QVERIFY( dssProvider->hasData( uri ) );

  QVERIFY( dssProvider->hasData( uri, ReosTimeWindow(
                                   QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 00, 0 ), Qt::UTC ),
                                   QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 32, 0 ), Qt::UTC ) ) ) );

  QVERIFY( dssProvider->hasData( uri, ReosTimeWindow(
                                   QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 32, 0 ), Qt::UTC ),
                                   QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 42, 0 ), Qt::UTC ) ) ) );

  QVERIFY( !dssProvider->hasData( uri, ReosTimeWindow(
                                    QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 00, 0 ), Qt::UTC ),
                                    QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 28, 0 ), Qt::UTC ) ) ) );

  QVERIFY( !dssProvider->hasData( uri, ReosTimeWindow(
                                    QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 50, 0 ), Qt::UTC ),
                                    QDateTime( QDate( 2005, 05, 01 ), QTime( 12, 51, 0 ), Qt::UTC ) ) ) );

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
  QCOMPARE( project.plan( planIds.at( 0 ) ).outputInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( project.plan( planIds.at( 0 ) ).detailedOutputInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( project.plan( planIds.at( 0 ) ).mappingInterval(), ReosDuration( 5.0, ReosDuration::minute ) );

  QCOMPARE( project.currentGeometry().title(), QStringLiteral( "simple_2D_geometry" ) );

  QCOMPARE( project.GeometriesCount(), 2 );

  QStringList geometryIds = project.geometryIds();
  QCOMPARE( geometryIds.count(), 2 );

  ReosHecRasGeometry geometry = project.geometry( geometryIds.at( 0 ) );
  QCOMPARE( geometry.title(), QStringLiteral( "simple_2D_geometry" ) );
  QCOMPARE( geometry.area2dCount(), 1 );

  QCOMPARE( geometry.crs(), "PROJCS[\"WGS_1984_UTM_Zone_20N\",GEOGCS[\"GCS_WGS_1984\",DATUM[\"D_WGS_1984\",SPHEROID[\"WGS_1984\",6378137.0,298.257223563]],PRIMEM[\"Greenwich\",0.0],UNIT[\"Degree\",0.0174532925199433]],PROJECTION[\"Transverse_Mercator\"],PARAMETER[\"False_Easting\",500000.0],PARAMETER[\"False_Northing\",0.0],PARAMETER[\"Central_Meridian\",-63.0],PARAMETER[\"Scale_Factor\",0.9996],PARAMETER[\"Latitude_Of_Origin\",0.0],UNIT[\"Meter\",1.0]]" );
  ReosGisEngine::crsIsValid( geometry.crs() );

  QPolygonF domain;
  domain << QPointF( 653204.178513767, 1797219.8459956 )
         << QPointF( 653499.89032075, 1797219.72312258 )
         << QPointF( 653499.500009407, 1797130.63683779 )
         << QPointF( 653203.35825702, 1797130.98806042 );

  QCOMPARE( domain, geometry.domain() );

  ReosModule::Message message;
  std::unique_ptr<ReosMesh> mesh( geometry.createMesh( QString(), message ) );
  QVERIFY( message.type == ReosModule::Simple );
  QVERIFY( mesh );
  QCOMPARE( mesh->vertexCount(), 1900 );
  QVERIFY( mesh->vertexElevation( 0 ) != 0.0 );

  QString areaName = geometry.area2dName( 0 );
  QCOMPARE( areaName, QStringLiteral( "Perimeter 1" ) );
  QList<ReosHecRasGeometry::BoundaryCondition> boundaries = geometry.boundariesConditions( areaName );
  QCOMPARE( boundaries.count(), 2 );
  QCOMPARE( boundaries.at( 0 ).area(), areaName );
  QCOMPARE( boundaries.at( 0 ).name(), QStringLiteral( "Upstream limit" ) );
  QCOMPARE( boundaries.at( 1 ).area(), areaName );
  QCOMPARE( boundaries.at( 1 ).name(), QStringLiteral( "Downstream limit" ) );

  geometry = project.geometry( geometryIds.at( 1 ) );
  QCOMPARE( geometry.title(), QStringLiteral( "simle_2D_geometry_other" ) );
  QCOMPARE( geometry.area2dCount(), 2 );

  domain.clear();
  domain << QPointF( 653202.534063554, 1797130.66817971 )
         << QPointF( 653202.050994364, 1797219.71445612 )
         << QPointF( 653306.971839022, 1797219.72312258 )
         << QPointF( 653490.227515079, 1797213.58963957 )
         << QPointF( 653490.227515079, 1797137.18285738 )
         << QPointF( 653307.021942612, 1797131.13594369 );

  QPolygonF actualDomain = geometry.domain();
  QCOMPARE( geometry.domain(), domain );

  ReosHecRasFlow currentFlow = project.currentFlow();
  QCOMPARE( currentFlow.title(), QStringLiteral( "flow_1" ) );
  QCOMPARE( currentFlow.boundariesCount(), 2 );
  QVERIFY( currentFlow.boundary( 0 ).type == ReosHecRasFlow::Type::FlowHydrograph );
  QVERIFY( currentFlow.boundary( 0 ).isDss );
  QCOMPARE( currentFlow.boundary( 0 ).area(), areaName );
  QCOMPARE( currentFlow.boundary( 0 ).boundaryConditionLine(), boundaries.at( 0 ).name() );
  QVERIFY( currentFlow.boundary( 1 ).type == ReosHecRasFlow::Type::NormalDepth );
  QVERIFY( !currentFlow.boundary( 1 ).isDss );
  QCOMPARE( currentFlow.boundary( 1 ).area(), areaName );
  QCOMPARE( currentFlow.boundary( 1 ).boundaryConditionLine(), boundaries.at( 1 ).name() );
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
  QCOMPARE( currentFlow2.boundary( 0 ).area(), bc1.area() );
  QCOMPARE( currentFlow2.boundary( 0 ).boundaryConditionLine(), bc1.boundaryConditionLine() );
  QVERIFY( currentFlow2.boundary( 1 ).type == ReosHecRasFlow::Type::StageHydrograph );
  QVERIFY( currentFlow2.boundary( 1 ).isDss );
  QCOMPARE( currentFlow2.boundary( 1 ).dssFile, QStringLiteral( "/my/dss/file" ) );
  QCOMPARE( currentFlow2.boundary( 1 ).dssPath, QStringLiteral( "/path/dss/STAGE" ) );
  QCOMPARE( currentFlow2.boundary( 1 ).area(), bc2.area() );
  QCOMPARE( currentFlow2.boundary( 1 ).boundaryConditionLine(), bc2.boundaryConditionLine() );

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

  QVERIFY( network->gisEngine()->crs().isEmpty() );

  std::unique_ptr<ReosHecRasStructureImporterSource> importerSource( new ReosHecRasStructureImporterSource( path, network->context() ) );
  std::unique_ptr<ReosStructureImporter> importer( importerSource->createImporter() );

  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.get(), network->context() );
  QVERIFY( structure != nullptr );
  QVERIFY( structure->domain().count() > 0 );
  QVERIFY( structure->structureImporterSource() );

  QVERIFY( !network->gisEngine()->crs().isEmpty() );

  QVERIFY( structure->mesh() );
  QCOMPARE( structure->mesh()->vertexCount(), 1900 );
  QPolygonF domain_1;
  domain_1 << QPointF( 653204.178513767, 1797219.8459956 )
           << QPointF( 653499.89032075, 1797219.72312258 )
           << QPointF( 653499.500009407, 1797130.63683779 )
           << QPointF( 653203.35825702, 1797130.98806042 );
  QCOMPARE( structure->domain(), domain_1 );

  ReosHydraulicSimulation *simulation = structure->currentSimulation();
  QVERIFY( simulation );
  ReosHecRasSimulation *hecSim = qobject_cast <ReosHecRasSimulation *>( simulation );
  QVERIFY( hecSim );
  QCOMPARE( hecSim->currentPlan(), QStringLiteral( "p01" ) );
  QCOMPARE( hecSim->computeInterval(), ReosDuration( 60.0, ReosDuration::second ) );
  QCOMPARE( hecSim->outputInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( hecSim->detailedInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( hecSim->mappingInterval(), ReosDuration( 5.0, ReosDuration::minute ) );

  hecSim->setCurrentPlan( "p02" );

  QVERIFY( structure->mesh() );
  QCOMPARE( structure->mesh()->vertexCount(), 1330 );

  QCOMPARE( hecSim->computeInterval(), ReosDuration( 60.0, ReosDuration::second ) );
  QCOMPARE( hecSim->outputInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( hecSim->detailedInterval(), ReosDuration( 5.0, ReosDuration::minute ) );
  QCOMPARE( hecSim->mappingInterval(), ReosDuration( 5.0, ReosDuration::minute ) );

  QVERIFY( structure->domain() != domain_1 );

  hecSim->setCurrentPlan( "p01" );

  QVERIFY( structure->domain() == domain_1 );

  QList<ReosHydraulicStructureBoundaryCondition *> boundaryConditions = structure->boundaryConditions();
  QCOMPARE( boundaryConditions.count(), 2 );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "\"Perimeter 1\"::\"Upstream limit\"" ) );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "\"Perimeter 1\"::\"Upstream limit\"" ) );
  QVERIFY( boundaryConditions.at( 1 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );
  QCOMPARE( boundaryConditions.at( 0 )->boundaryConditionId(), QStringLiteral( "\"Perimeter 1\"::\"Downstream limit\"" ) );
  QVERIFY( boundaryConditions.at( 0 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );

  ReosHydraulicStructureBoundaryCondition *upstreamBc = boundaryConditions.at( 1 );
  upstreamBc->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QVERIFY( upstreamBc->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::InputFlow );

  structure->timeWindowSettings()->useExternalDefinedTimeWindow()->setValue( false );
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

  // Setup a gridded precipitation
  QString rainUri =  QStringLiteral( "\"" ) + QString( data_path() ) + "/hecras/dss/gridded_jarry.dss" +
                     QStringLiteral( "\"::/HOME_MADE/JARRY/PRECIP///VCL/" );
  std::unique_ptr<ReosGriddedRainItem> griddedItem(
    new ReosGriddedRainItem( "gridded precipitaton", "", new ReosGriddedRainfall( rainUri, "dss" ) ) );
  scheme->meteoModel()->associate( structure, griddedItem.get() );
  ReosGriddedRainfall *rain = scheme->meteoModel()->associatedRainfall( structure );
  QVERIFY( rain );
  QCOMPARE( rain->gridCount(), 3 );
  QCOMPARE( rain->startTime( 0 ), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( rain->endTime( 0 ), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 40, 0 ), Qt::UTC ) );
  QCOMPARE( rain->endTime( 2 ), QDateTime( QDate( 2000, 01, 01 ), QTime( 12, 0, 0 ), Qt::UTC ) );

  // setup time interval simulation
  hecSim->setComputeInterval( ReosDuration( 10, ReosDuration::second ) );
  hecSim->setOutputInterval( ReosDuration( 1, ReosDuration::minute ) );
  hecSim->setDetailledInterval( ReosDuration( 1, ReosDuration::minute ) );
  hecSim->setMappingInterval( ReosDuration( 1, ReosDuration::minute ) );

  ReosSimulationData simulationData = structure->simulationData();
  simulation->prepareInput( structure, simulationData, scheme->calculationContext() );

  ReosHecRasProject projectAfterPreparation( path );
  ReosHecRasPlan planAfterPreparation = projectAfterPreparation.currentPlan();

  QCOMPARE( planAfterPreparation.startTime(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( planAfterPreparation.endTime(), QDateTime( QDate( 2000, 01, 01 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QCOMPARE( planAfterPreparation.computeInterval(), ReosDuration( 10.0, ReosDuration::second ) );
  QCOMPARE( planAfterPreparation.outputInterval(), ReosDuration( 1.0, ReosDuration::minute ) );
  QCOMPARE( planAfterPreparation.detailedOutputInterval(), ReosDuration( 1.0, ReosDuration::minute ) );
  QCOMPARE( planAfterPreparation.mappingInterval(), ReosDuration( 1.0, ReosDuration::minute ) );

  ReosHecRasFlow flowAfterPreparation = projectAfterPreparation.currentFlow();
  QVERIFY( flowAfterPreparation.boundariesCount() == 2 );
  QCOMPARE( flowAfterPreparation.boundary( 0 ).id(), QStringLiteral( "\"Perimeter 1\"::\"Upstream limit\"" ) );
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
  QVERIFY( structure->structureImporterSource() );

  boundaryConditions = structure->boundaryConditions();
  QCOMPARE( boundaryConditions.count(), 2 );
  QCOMPARE( boundaryConditions.at( 1 )->boundaryConditionId(), QStringLiteral( "\"Perimeter 1\"::\"Upstream limit\"" ) );
  QVERIFY( boundaryConditions.at( 1 )->conditionType() == ReosHydraulicStructureBoundaryCondition::Type::InputFlow );
  QCOMPARE( boundaryConditions.at( 0 )->boundaryConditionId(), QStringLiteral( "\"Perimeter 1\"::\"Downstream limit\"" ) );
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

  std::unique_ptr<ReosStructureImporterSource> importerSource( new ReosHecRasStructureImporterSource( projectPath, network->context() ) );
  std::unique_ptr<ReosStructureImporter> importer( importerSource->createImporter() );
  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.get(), network->context() );
  QVERIFY( structure );

  QList<ReosHydraulicStructureBoundaryCondition * > boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 2 );
  QPointF pos = boundaries.at( 0 )->position( QString() );
  QCOMPARE( pos, QPointF( 653500.105543859, 1797175.01204072 ) );
  pos = boundaries.at( 1 )->position( QString() );
  QCOMPARE( pos, QPointF( 653203.343995325, 1797175.15557276 ) );

  std::shared_ptr<ReosHecRasProject> project = std::make_shared<ReosHecRasProject>( projectPath );
  QVERIFY( project );

  //we use dynamic cast because there are some issue with qobject_cast, maybe due to crossing dynamic library
  ReosHecRasSimulation *simulation = dynamic_cast<ReosHecRasSimulation *>( structure->currentSimulation() );
  ReosHydraulicScheme *currentScheme = network->currentScheme();

  QVERIFY( simulation->hasResult( structure, currentScheme->id() ) );

  ReosTestRenderedObject renderer;
  QVERIFY( renderer.compareRendering(
             structure->mesh(), structure->timeWindow().start(),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan1_terrrain.png" ) ) );

  QStringList datasetIds = structure->mesh()->datasetIds();
  QCOMPARE( datasetIds.count(), 4 );
  structure->activateResultDatasetGroup( datasetIds.at( 1 ) );

  QVERIFY( renderer.compareRendering(
             structure->mesh(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan1_ws.png" ) ) );

  ReosSpatialPosition position( 653361, 1797175 );
  double value = structure->resultsValueAt( QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ),
                 position,
                 ReosHydraulicSimulationResults::DatasetType::WaterLevel,
                 currentScheme->id() );

  QVERIFY( equal( value, 2.00, 0.001 ) );

  structure->deactivateMeshScalar();

  QVERIFY( renderer.compareRendering(
             structure->mesh(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan1_void.png" ), 50 ) );


  std::unique_ptr<ReosHydraulicSimulationResults> simResult( simulation->loadSimulationResults( structure, scheme->id() ) );

  QMap<QString, ReosHydrograph *> outputHydrographs = simResult->outputHydrographs();
  QVERIFY( outputHydrographs.count() == 2 );
  QVERIFY( outputHydrographs.contains( QStringLiteral( "\"Perimeter 1\"::\"Downstream limit\"" ) ) );
  ReosHydrograph *hyd = outputHydrographs.value( QStringLiteral( "\"Perimeter 1\"::\"Downstream limit\"" ) );
  QDateTime refTime = hyd->referenceTime();
  QCOMPARE( refTime, QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QVERIFY( hyd->valueCount() > 0 );

  QCOMPARE( simResult->groupCount(), 3 );
  QCOMPARE( simResult->datasetCount( 0 ), 121 );
  QCOMPARE( simResult->datasetType( 0 ), ReosHydraulicSimulationResults::DatasetType::WaterLevel );
  QCOMPARE( simResult->datasetType( 1 ), ReosHydraulicSimulationResults::DatasetType::WaterDepth );
  QCOMPARE( simResult->datasetType( 2 ), ReosHydraulicSimulationResults::DatasetType::Velocity );
  QCOMPARE( simResult->groupLocation( 0 ), ReosHydraulicSimulationResults::Location::Face );
  QCOMPARE( simResult->groupLocation( 1 ), ReosHydraulicSimulationResults::Location::Vertex );
  QCOMPARE( simResult->groupLocation( 2 ), ReosHydraulicSimulationResults::Location::Face );
  QCOMPARE( simResult->groupIsScalar( 0 ), true );
  QCOMPARE( simResult->groupIsScalar( 1 ), true );
  QCOMPARE( simResult->groupIsScalar( 2 ), false );
  QCOMPARE( simResult->datasetValuesCount( 0, 0 ), 1746 ) ;
  QCOMPARE( simResult->datasetValues( 0, 0 ).at( 1258 ), 2.000097513198853 ) ;
  QCOMPARE( simResult->datasetValues( 1, 0 ).at( 1258 ), 0.0704100131989 ) ;
  QCOMPARE( simResult->datasetValues( 2, 0 ).at( 1258 ), 0.0 ) ;
  QCOMPARE( simResult->datasetValues( 0, 1 ).at( 1258 ), 1.0190500020980835 ) ;
  QCOMPARE( simResult->datasetValues( 1, 1 ).at( 1258 ), 0.08105674386024475 ) ;
  QCOMPARE( simResult->datasetValues( 2, 1 ).at( 1258 ), 0.5631286203861237 ) ;

  simulateEventLoop( WAITING_TIME_FOR_LOOP );

  ReosTimeWindow tw = structure->timeWindow();
  QCOMPARE( tw.start(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( tw.end(), QDateTime( QDate( 2000, 01, 01 ), QTime( 12, 0, 0 ), Qt::UTC ) );
  QVERIFY( tw == mGisEngine->mapTimeWindow() );

  simulation->setCurrentPlan( "p02" );

  QVERIFY( renderer.compareRendering(
             structure->mesh(), structure->timeWindow().start(),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan2_void.png" ) ) );

  datasetIds = structure->mesh()->datasetIds();
  QCOMPARE( datasetIds.count(), 1 );
  structure->activateResultDatasetGroup( datasetIds.at( 0 ) );

  QVERIFY( renderer.compareRendering(
             structure->mesh(), structure->timeWindow().start(),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan2_terrain.png" ) ) );

  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 1 );

  ReosEncodedElement simEncoded = simulation->encode();
  ReosEncodedElement encodedNetwork = network->encode( QFileInfo( projectPath ).dir().path(),  "project.lkn" );

  structure->deactivateMeshScalar();

  QVERIFY( renderer.compareRendering(
             structure->mesh(), structure->timeWindow().start(),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan2_void.png" ) ) );

  simulation->setCurrentPlan( "p01" );

  QVERIFY( renderer.compareRendering(
             structure->mesh(), structure->timeWindow().start(),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan1_void.png" ), 50 ) );

  datasetIds = structure->mesh()->datasetIds();
  QCOMPARE( datasetIds.count(), 4 );
  structure->activateResultDatasetGroup( datasetIds.at( 1 ) );

  QVERIFY( renderer.compareRendering(
             structure->mesh(), structure->timeWindow().start(),
             data_path() + QStringLiteral( "/hecras/simple/control_images/simple_plan1_ws.png" ) ) );

  //*********************** project clear and reloaded
  network->clear();
  network->decode( encodedNetwork, QFileInfo( projectPath ).dir().path(),  "project.lkn" );

  QList<ReosHydraulicNetworkElement *> elements = network->hydraulicNetworkElements( ReosHydraulicStructure2D::staticType() );
  QCOMPARE( elements.count(), 1 );

  structure = qobject_cast<ReosHydraulicStructure2D *>( elements.at( 0 ) );
  QVERIFY( structure );
  simulation = dynamic_cast<ReosHecRasSimulation *>( structure->currentSimulation() );

  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 1 );

  QVERIFY( simulation );
  simulation->setCurrentPlan( "p01" );

  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 2 );

  pos = boundaries.at( 0 )->position( QString() );
  QCOMPARE( pos, QPointF( 653500.105543859, 1797175.01204072 ) );
  pos = boundaries.at( 1 )->position( QString() );
  QCOMPARE( pos, QPointF( 653203.343995325, 1797175.15557276 ) );

  // We change the time window settings to fix it from the input of the model
  structure->timeWindowSettings()->useExternalDefinedTimeWindow()->setValue( false );
  tw = structure->timeWindow();
  QCOMPARE( tw.start(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( tw.end(), QDateTime( QDate( 2000, 01, 03 ), QTime( 10, 1, 0 ), Qt::UTC ) );
  QVERIFY( tw == mGisEngine->mapTimeWindow() );

  structure->timeWindowSettings()->setOriginEnd( ReosTimeWindowSettings::Begin );
  structure->timeWindowSettings()->endOffset()->setValue( ReosDuration( 3.0, ReosDuration::hour ) );

  tw = structure->timeWindow();
  QCOMPARE( tw.start(), QDateTime( QDate( 2000, 01, 01 ), QTime( 10, 0, 0 ), Qt::UTC ) );
  QCOMPARE( tw.end(), QDateTime( QDate( 2000, 01, 01 ), QTime( 13, 0, 0 ), Qt::UTC ) );
  QVERIFY( tw == mGisEngine->mapTimeWindow() );

}

void ReosHecrasTesting::planCompatibility()
{
  QString projectPath = data_path() + QStringLiteral( "/hecras/simple/calculated/simple.prj" );

  ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &mRootModule, mGisEngine, mWatershedModule );
  network->changeScheme( 0 );
  ReosHydraulicScheme *scheme = network->currentScheme();
  QVERIFY( scheme );

  std::unique_ptr<ReosStructureImporterSource> importerSource( new ReosHecRasStructureImporterSource( projectPath, network->context() ) );
  std::unique_ptr<ReosStructureImporter> importer( importerSource->createImporter() );
  ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.get(), network->context() );
  QVERIFY( structure );

  QList<ReosHydraulicStructureBoundaryCondition * > boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 2 );

  //we use dynamic cast because there are some issue with qobject_cast, maybe due to crossing dynamic library
  ReosHecRasSimulation *simulation = dynamic_cast<ReosHecRasSimulation *>( structure->currentSimulation() );
  QVERIFY( simulation );

  QCOMPARE( simulation->project()->planIds().count(), 2 );
  const QString planId_1 = simulation->project()->planIds().at( 0 );
  const QString planId_2 = simulation->project()->planIds().at( 1 );

  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  simulation->setCurrentPlan( planId_2 );
  boundaries = structure->boundaryConditions();

  QCOMPARE( boundaries.count(), 1 );
  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  boundaries.at( 0 )->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );

  QVERIFY( !simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  boundaries.at( 0 )->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::DefinedExternally );

  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  ReosHydrographJunction *junction = new ReosHydrographJunction( ReosSpatialPosition(), network );
  network->addElement( junction );

  QPointer<ReosHydrographRoutingLink> link = new ReosHydrographRoutingLink( junction, boundaries.at( 0 ), network );
  network->addElement( link );

  QVERIFY( !simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  network->removeElement( link );
  simulateEventLoop( WAITING_TIME_FOR_LOOP );
  QVERIFY( link.isNull() );

  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  simulation->setCurrentPlan( planId_1 );

  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 2 );
  link = new ReosHydrographRoutingLink( boundaries.at( 1 ), junction, network );
  network->addElement( link );

  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( !simulation->checkPlanCompability( planId_2 ).isCompatible );

  QVERIFY( !link.isNull() );

  simulation->setCurrentPlan( planId_2 );

  QVERIFY( simulation->checkPlanCompability( planId_1 ).isCompatible );
  QVERIFY( simulation->checkPlanCompability( planId_2 ).isCompatible );

  simulateEventLoop( WAITING_TIME_FOR_LOOP );
  QVERIFY( link.isNull() ); //link was removed because planId_2 is not compatible with

  network->hydraulicSchemeCollection()->addScheme( new ReosHydraulicScheme( network->hydraulicSchemeCollection() ) );
  network->hydraulicSchemeCollection()->addScheme( new ReosHydraulicScheme( network->hydraulicSchemeCollection() ) );
  QCOMPARE( network->hydraulicSchemeCollection()->schemeCount(), 3 );
  ReosHydraulicScheme *scheme_1 = network->hydraulicSchemeCollection()->scheme( 0 );
  ReosHydraulicScheme *scheme_2 = network->hydraulicSchemeCollection()->scheme( 1 );
  ReosHydraulicScheme *scheme_3 = network->hydraulicSchemeCollection()->scheme( 2 );
  QVERIFY( scheme_1 );
  QVERIFY( scheme_2 );
  QVERIFY( scheme_3 );

  QCOMPARE( simulation->currentPlan(), planId_2 );
  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 1 );

  simulation->setCurrentPlan( planId_1 );
  QCOMPARE( simulation->currentPlan(), planId_1 );
  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 2 );

  network->changeScheme( 2 ); //First time this scheme is current, so it take config of previous scheme
  network->changeScheme( 1 );//First time this scheme is current, so it take config of previous scheme

  QVERIFY( network->checkSchemeCompatibility( scheme_1 ).isCompatible );
  QVERIFY( network->checkSchemeCompatibility( scheme_2 ).isCompatible );
  QVERIFY( network->checkSchemeCompatibility( scheme_3 ).isCompatible );

  simulation->checkPlanCompability( planId_2 );
  simulation->setCurrentPlan( planId_2 );
  boundaries = structure->boundaryConditions();
  QCOMPARE( boundaries.count(), 1 );
  boundaries.at( 0 )->setDefaultConditionType( ReosHydraulicStructureBoundaryCondition::Type::InputFlow );

  QVERIFY( !network->checkSchemeCompatibility( scheme_1 ).isCompatible );
  QVERIFY( network->checkSchemeCompatibility( scheme_2 ).isCompatible );
  QVERIFY( !network->checkSchemeCompatibility( scheme_3 ).isCompatible );
}

void ReosHecrasTesting::importCreatingScheme()
{
  {
    QString projectPath = data_path() + QStringLiteral( "/hecras/simple/calculated/simple.prj" );

    ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &mRootModule, mGisEngine, mWatershedModule );
    network->changeScheme( 0 );
    ReosHydraulicScheme *scheme = network->currentScheme();
    QVERIFY( scheme );

    std::unique_ptr<ReosHecRasStructureImporterSource> importerSource( new ReosHecRasStructureImporterSource( projectPath, network->context() ) );
    std::unique_ptr<ReosHecRasStructureImporter> importer( importerSource->createImporter() );
    QVERIFY( importer );
    importer->setCreationOption( {true, false} );
    ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.get(), network->context() );
    QVERIFY( structure );

    QCOMPARE( network->hydraulicSchemeCollection()->schemeCount(), 3 );

    scheme = network->hydraulicSchemeCollection()->scheme( 1 );
    QCOMPARE( scheme->schemeName()->value(), QStringLiteral( "plan_test" ) );
    network->setCurrentScheme( 1 );

    //we use dynamic cast because there are some issue with qobject_cast, maybe due to crossing dynamic library
    ReosHecRasSimulation *simulation = dynamic_cast<ReosHecRasSimulation *>( structure->currentSimulation() );
    QVERIFY( simulation );
    QCOMPARE( simulation->currentPlan(), QStringLiteral( "p01" ) );

    scheme = network->hydraulicSchemeCollection()->scheme( 2 );
    QCOMPARE( scheme->schemeName()->value(), QStringLiteral( "plan_test_2" ) );
    network->setCurrentScheme( 2 );

    QCOMPARE( simulation->currentPlan(), QStringLiteral( "p02" ) );
  }

  {
    QString projectPath = data_path() + QStringLiteral( "/hecras/simple/calculated/simple.prj" );

    ReosHydraulicNetwork *network = new ReosHydraulicNetwork( &mRootModule, mGisEngine, mWatershedModule );
    network->changeScheme( 0 );
    ReosHydraulicScheme *scheme = network->currentScheme();
    QVERIFY( scheme );

    std::unique_ptr<ReosHecRasStructureImporterSource> importerSource( new ReosHecRasStructureImporterSource( projectPath, network->context() ) );
    std::unique_ptr<ReosHecRasStructureImporter> importer( importerSource->createImporter() );
    QVERIFY( importer );
    importer->setCreationOption( {true, true} );
    ReosHydraulicStructure2D *structure = ReosHydraulicStructure2D::create( importer.get(), network->context() );
    QVERIFY( structure );

    QCOMPARE( network->hydraulicSchemeCollection()->schemeCount(), 2 );

    scheme = network->hydraulicSchemeCollection()->scheme( 0 );
    QCOMPARE( scheme->schemeName()->value(), QStringLiteral( "plan_test" ) );
    network->setCurrentScheme( 0 );

    //we use dynamic cast because there are some issue with qobject_cast, maybe due to crossing dynamic library
    ReosHecRasSimulation *simulation = dynamic_cast<ReosHecRasSimulation *>( structure->currentSimulation() );
    QVERIFY( simulation );
    QCOMPARE( simulation->currentPlan(), QStringLiteral( "p01" ) );

    scheme = network->hydraulicSchemeCollection()->scheme( 1 );
    QCOMPARE( scheme->schemeName()->value(), QStringLiteral( "plan_test_2" ) );
    network->setCurrentScheme( 1 );
    QCOMPARE( simulation->currentPlan(), QStringLiteral( "p02" ) );
  }
}

QTEST_MAIN( ReosHecrasTesting )
#include "test_hecras.moc"
