/***************************************************************************
                      reos_hydraulic_structure_2d_test.cpp
                     --------------------------------------
Date                 : Januay-2022
Copyright            : (C) 2022 by Vincent Cloarec
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
#include <QModelIndexList>

#include "reoshydraulicstructure2d.h"
#include "reospolygonstructure.h"
#include "reoshydraulicstructureprofile.h"
#include "reosgisengine.h"
#include "reosmapextent.h"
#include "reostopographycollection.h"
#include "reos_testutils.h"
#include "reosgriddedrainitem.h"
#include "reosrainfallmodel.h"
#include "reosrainfallregistery.h"

class ReoHydraulicStructure2DTest: public QObject
{
    Q_OBJECT
  private slots:

    void initTestCase();
    void createAndEditPolylineStructure();
    void createAndEditPolygonStructure();

    void createHydraulicStructure();
    void profile();

    void meteoModel();

  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosModule *mRootModule = nullptr;
    ReosGisEngine *mGisEngine = nullptr;
    ReosHydraulicStructure2D *mHydraulicStructure = nullptr;
    ReosRainfallModel *mRainfallModel = nullptr;
    ReosZoneItem *mRainZone1 = nullptr;
};

void ReoHydraulicStructure2DTest::initTestCase()
{
  mRootModule = new ReosModule( QStringLiteral("root"), this );
  mGisEngine = new ReosGisEngine( this );
  mNetwork = new ReosHydraulicNetwork( mRootModule, mGisEngine, nullptr );

  mRainfallModel = ReosRainfallRegistery::instance()->rainfallModel();
  mRainZone1 = mRainfallModel->addZone( QStringLiteral("Zone 1"), "", QModelIndex() );
}

void ReoHydraulicStructure2DTest::createAndEditPolylineStructure()
{
  QPolygonF domain;
  domain << QPointF( 0, 0 )
         << QPointF( 0, 0.5 )
         << QPointF( 0, 1 )
         << QPointF( 0.5, 1 )
         << QPointF( 1, 1 )
         << QPointF( 1, 0.5 )
         << QPointF( 1, 0.5 ) //dupplicate vertex
         << QPointF( 1, 0 )
         << QPointF( 0.5, 0 );

  std::unique_ptr < ReosHydraulicStructure2D> structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork->context() );

  domain.removeAt( 6 ); //remove the dupplicte vertex that has to be removed in the structure

  ReosPolylinesStructure *geomStructure = structure2D->geometryStructure();

  QCOMPARE( structure2D->domain(), domain );
  geomStructure->undoStack()->undo();
  QCOMPARE( structure2D->domain(), domain );
  ReosPolylinesStructure::Data data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  ReosMapExtent searchZone( -0.1, 0.4, -0.1, 0.6 );
  ReosGeometryStructureVertex *vert = geomStructure->searchForVertex( searchZone );
  QVERIFY( !vert );

  searchZone = ReosMapExtent( -0.1, 0.4, 0.1, 0.6 );
  vert = geomStructure->searchForVertex( searchZone );
  QVERIFY( vert );

  QVERIFY( !geomStructure->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 0.5, 1.5 ) ) ) ); //lines are crossing
  QVERIFY( geomStructure->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 0.991, 0.491 ) ) ) ); //just outside the tolerance
  QVERIFY( geomStructure->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 0.5, 0.5 ) ) ) );

  geomStructure->moveVertex( vert, ReosSpatialPosition( QPointF( 0.5, 0.5 ) ) );
  QPolygonF expectded = structure2D->domain();

  QVERIFY( domain != structure2D->domain() );
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  geomStructure->undoStack()->undo();

  data = geomStructure->structuredLinesData();
  QVERIFY( domain == structure2D->domain() );
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  geomStructure->undoStack()->redo();

  domain[1] = QPointF( 0.5, 0.5 );
  expectded = structure2D->domain();

  qint64 lineId;
  QVERIFY( geomStructure->searchForLine( ReosMapExtent( 0.70, 0.9, 0.8, 1.1 ), lineId ) );

  geomStructure->insertVertex( ReosSpatialPosition( QPointF( 0.75, 1.1 ) ), lineId );
  QVERIFY( domain != structure2D->domain() );
  geomStructure->undoStack()->undo();
  data = geomStructure->structuredLinesData();
  QVERIFY( domain == structure2D->domain() );
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  geomStructure->undoStack()->redo();
  domain.insert( 4, QPointF( 0.75, 1.0 ) ); //it is the projected point that is inserted
  QVERIFY( domain == structure2D->domain() );
  geomStructure->undoStack()->undo();
  QVERIFY( domain != structure2D->domain() );

  geomStructure->undoStack()->redo();
  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  searchZone = ReosMapExtent( -0.1, 0.9, 0.1, 1.1 );
  vert = geomStructure->searchForVertex( searchZone );
  QVERIFY( vert );

  geomStructure->removeVertex( vert );
  QCOMPARE( structure2D->domain().count(), 8 );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  geomStructure->undoStack()->undo();
  QCOMPARE( structure2D->domain().count(), 9 );
  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() );
  QCOMPARE( data.internalLines.count(), 0 );

  geomStructure->undoStack()->redo();
  QVERIFY( domain != structure2D->domain() );
  domain.removeAt( 2 );
  QVERIFY( domain == structure2D->domain() );

  QPolygonF lines;
  lines << QPointF( 0.749, 1.01 )
        << QPointF( 0.8, 0.6 )
        << QPointF( 0.6, 0.6 );
  geomStructure->addPolylines( lines );

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() - 2 );
  QCOMPARE( data.internalLines.count(), 2 );


  // insert vertex in internal lines
  searchZone = ReosMapExtent( 0.74, 0.59, 0.76, 0.61 );
  geomStructure->searchForLine( searchZone, lineId );
  geomStructure->insertVertex( ReosSpatialPosition( QPointF( 0.75, 0.59 ) ), lineId );

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() - data.internalLines.count() );
  QCOMPARE( data.internalLines.count(), 3 );

  geomStructure->undoStack()->undo();

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() - 2 );
  QCOMPARE( data.internalLines.count(), 2 );

  geomStructure->undoStack()->redo();

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() - data.internalLines.count() );
  QCOMPARE( data.internalLines.count(), 3 );

  // add new simple line intersecting the boundary with first vertex under tolerance from the boundary line
  lines.clear();
  lines << QPointF( 0.995, 0.25 )
        << QPointF( 0.8, 0.25 );
  geomStructure->addPolylines( lines );

  QVERIFY( domain != structure2D->domain() );
  data = geomStructure->structuredLinesData();

  domain.insert( 6, QPointF( 1, 0.25 ) );
  QVERIFY( domain == structure2D->domain() );
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() - data.internalLines.count() );
  QCOMPARE( data.internalLines.count(), 4 );

  //link two boundary line by internal link and add a vertex in the new line
  lines.clear();
  lines << QPointF( 0.5, 0.5 )
        << QPointF( 1.0, 0.0 );
  geomStructure->addPolylines( lines );
  QVERIFY( geomStructure->searchForLine( ReosMapExtent( 0.74, 0.24, 0.76, 0.26 ), lineId ) );
  QVERIFY( geomStructure->insertVertex( ReosSpatialPosition( 0.75, 0.25 ), lineId ) );

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.internalLines.count(), 6 );

  while ( geomStructure->undoStack()->canUndo() )
    geomStructure->undoStack()->undo();

  while ( geomStructure->undoStack()->canRedo() )
    geomStructure->undoStack()->redo();

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.internalLines.count(), 6 );

  geomStructure->removeAll();
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 0 );
  QCOMPARE( data.internalLines.count(), 0 );
  QCOMPARE( data.vertices.count(), 0 );

  geomStructure->undoStack()->undo();

  QCOMPARE( data.boundaryPointCount, 0 );
  QCOMPARE( data.internalLines.count(), 0 );
  QCOMPARE( data.vertices.count(), 0 );

  // restart with a new structure
  domain.clear();
  domain << QPointF( 1, 1 )
         << QPointF( 0, 1 )
         << QPointF( 0, 0 )
         << QPointF( 1, 0 );

  structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork->context() );
  geomStructure = structure2D->geometryStructure();

  geomStructure->searchForLine( ReosMapExtent( 0.99, 0.4, 1.01, 0.5 ), lineId );
  geomStructure->insertVertex( QPointF( 1, 0.7 ), lineId );

  geomStructure->searchForLine( ReosMapExtent( 0.99, 0.4, 1.01, 0.5 ), lineId );
  geomStructure->insertVertex( QPointF( 1, 0.3 ), lineId );

  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 6 );
  QCOMPARE( data.internalLines.count(), 0 );
  QCOMPARE( data.vertices.count(), 6 );

  geomStructure->undoStack()->undo();
  geomStructure->undoStack()->undo();

  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 4 );
  QCOMPARE( data.internalLines.count(), 0 );
  QCOMPARE( data.vertices.count(), 4 );

  geomStructure->undoStack()->redo();
  geomStructure->undoStack()->redo();

  domain.clear();
  domain << QPointF( 1, 1 )
         << QPointF( 0, 1 )
         << QPointF( 0, 0 )
         << QPointF( 1, 0 )
         << QPointF( 1, 0.3 )
         << QPointF( 1, 0.7 );

  QVERIFY( domain == structure2D->domain() );

  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 6 );
  QCOMPARE( data.internalLines.count(), 0 );
  QCOMPARE( data.vertices.count(), 6 );

  // restart with a new structure
  domain.clear();
  domain << QPointF( 1, 1 )
         << QPointF( 0.5, 1.0 )
         << QPointF( 0.0, 1.0 )
         << QPointF( 0.0, 0.5 )
         << QPointF( 0.0, 0.0 )
         << QPointF( 0.5, 0.0 )
         << QPointF( 1.0, 0.0 )
         << QPointF( 1.0, 0.5 );

  structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork->context() );
  geomStructure = structure2D->geometryStructure();
  QCOMPARE( structure2D->domain(), domain );

  lines.clear();
  lines << QPointF( 1.0, 1.0 )
        << QPointF( 0.5, 0.5 )
        << QPointF( 0.0, 0.0 );

  geomStructure->addPolylines( lines );

  QCOMPARE( structure2D->domain(), domain );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.internalLines.count(), 2 );
  QCOMPARE( data.vertices.count(), 9 );

  lines.clear();
  lines << QPointF( 1.0, 0.0 )
        << QPointF( 0.5, 0.5 );

  geomStructure->addPolylines( lines );

  QCOMPARE( structure2D->domain(), domain );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.internalLines.count(), 3 );
  QCOMPARE( data.vertices.count(), 9 );

  geomStructure->removeVertex( geomStructure->searchForVertex( ReosMapExtent( 0.99, -0.01, 1.01, 0.01 ) ) );
  QVERIFY( structure2D->domain() != domain );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 7 );
  QCOMPARE( data.internalLines.count(), 2 );
  QCOMPARE( data.vertices.count(), 8 );

  geomStructure->undoStack()->undo();
  data = geomStructure->structuredLinesData();
  QCOMPARE( structure2D->domain(), domain );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.internalLines.count(), 3 );
  QCOMPARE( data.vertices.count(), 9 );

  lines.clear();
  lines << QPointF( 0.1, 0.75 )
        << QPointF( 0.2, 0.75 );
  geomStructure->addPolylines( lines );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.internalLines.count(), 4 );
  QCOMPARE( data.vertices.count(), 11 );

  vert = geomStructure->searchForVertex( ReosMapExtent( 0.19, 0.74, 0.21, 0.76 ) );
  geomStructure->moveVertex( vert, QPointF( 0.5, 0.5 ) );

  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.internalLines.count(), 4 );
  QCOMPARE( data.vertices.count(), 10 );

  geomStructure->undoStack()->undo();

  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, 8 );
  QCOMPARE( data.internalLines.count(), 4 );
  QCOMPARE( data.vertices.count(), 11 );
}

void ReoHydraulicStructure2DTest::createAndEditPolygonStructure()
{
  std::unique_ptr<ReosPolygonStructure> polygonStructure = ReosPolygonStructure::createPolygonStructure();
  polygonStructure->addClass( "class1", 123 );
  QPolygonF polygon;
  polygon << QPointF( 15, 5 ) << QPointF( 15, 15 ) << QPointF( 5, 15 );
  polygonStructure->addPolygon( polygon, "class1" );

  ReosSpatialPosition position( 12, 10 );
  std::unique_ptr<ReosPolygonStructureValues> values( polygonStructure->values( QString() ) );
  QCOMPARE( values->value( position.position().x(), position.position().y() ), 123 );

}

void ReoHydraulicStructure2DTest::createHydraulicStructure()
{
  QPolygonF domain;
  domain << QPointF( 0, 0 )
         << QPointF( 10, 0 )
         << QPointF( 10, 10 )
         << QPointF( 20, 10 )
         << QPointF( 20, 0 )
         << QPointF( 30, 0 )
         << QPointF( 30, 20 )
         << QPointF( 0, 20 );

  mHydraulicStructure = new ReosHydraulicStructure2D( domain, QString(), mNetwork->context() );
  mHydraulicStructure->meshResolutionController()->defaultSize()->setValue( 1 );

  std::unique_ptr<ReosMeshGeneratorProcess> meshGenerator( mHydraulicStructure->getGenerateMeshProcess() );
  meshGenerator->start();
  Q_ASSERT( meshGenerator->isSuccessful() );
#ifdef _MSC_VER
  QCOMPARE( mHydraulicStructure->mesh()->vertexCount(), 715 );
#else
  QCOMPARE( mHydraulicStructure->mesh()->vertexCount(), 714 );
#endif

  QPolygonF hole;
  hole << QPointF( 2.5, 2.5 )
       << QPointF( 7.5, 2.5 )
       << QPointF( 7.5, 7.5 )
       << QPointF( 2.5, 7.5 )
       << QPointF( 2.5, 2.5 );
  mHydraulicStructure->geometryStructure()->addPolylines( hole );
  mHydraulicStructure->geometryStructure()->addHolePoint( QPointF( 5, 5 ) );

  meshGenerator.reset( mHydraulicStructure->getGenerateMeshProcess() );
  meshGenerator->start();
  QVERIFY( meshGenerator->isSuccessful() );

#ifdef _MSC_VER
  QCOMPARE( mHydraulicStructure->mesh()->vertexCount(), 685 );
#else
  QCOMPARE( mHydraulicStructure->mesh()->vertexCount(), 687 );
#endif

  QString demId = mGisEngine->addRasterLayer( test_file( "dem_for_mesh.tif" ).c_str() );
  mGisEngine->registerLayerAsDigitalElevationModel( demId );
  mHydraulicStructure->topographyCollecion()->insertTopography( 0, demId );

  ModuleProcessControler controler( mHydraulicStructure->mesh()->applyTopographyOnVertices( mHydraulicStructure->topographyCollecion() ) );
  controler.waitForFinished();

  QVERIFY( equal( mHydraulicStructure->terrainElevationAt( QPointF( 15.0, 15.0 ) ), 1, 0.01 ) );
  QVERIFY( equal( mHydraulicStructure->terrainElevationAt( QPointF( 10.0, 15.0 ) ), 0.666, 0.01 ) );
  QVERIFY( equal( mHydraulicStructure->terrainElevationAt( QPointF( 20.0, 15.0 ) ), 1.333, 0.01 ) );
  QVERIFY( std::isnan( mHydraulicStructure->terrainElevationAt( QPointF( 5.0, 5.0 ) ) ) );
  QVERIFY( std::isnan( mHydraulicStructure->terrainElevationAt( QPointF( 15.0, 5.0 ) ) ) );
}

void ReoHydraulicStructure2DTest::profile()
{
  QPolygonF profileGeom;
  profileGeom << QPointF( -5, 5 )
              << QPointF( 5, 5 )
              << QPointF( 15, 5 )
              << QPointF( 15, 5 )
              << QPointF( 25, 15 );

  int profileIndex = mHydraulicStructure->createProfile( QStringLiteral( "profile 1" ), profileGeom, QString() );
  ReosHydraulicStructureProfile *profile = mHydraulicStructure->profile( profileIndex );

  QCOMPARE( QStringLiteral( "profile 1" ), profile->name() );
  QCOMPARE( 1, mHydraulicStructure->profilesCount() );

  const QList<QPolygonF> partsList = profile->parts().values();

  QCOMPARE( partsList.count(), 3 );
  QPolygonF part1;
  part1 << QPointF( 0.0, 5.0 ) << QPointF( 2.5, 5.0 );
  QCOMPARE( part1, partsList.at( 0 ) );
  QPolygonF part2;
  part2 << QPointF( 7.5, 5.0 ) << QPointF( 10.0, 5.0 );
  QCOMPARE( part2, partsList.at( 1 ) );
  QPolygonF part3;
  part3 << QPointF( 20.0, 10.0 ) << QPointF( 25.0, 15.0 );
  QCOMPARE( part3, partsList.at( 2 ) );

  const QList<QList<ReosMeshPointValue>> allPointValues = profile->pointValues().values();

  QCOMPARE( allPointValues.count(), 3 );

  for ( const QList<ReosMeshPointValue> &pointValues : std::as_const( allPointValues ) )
  {
    int count = pointValues.count();
    for ( int i = 0; i < count; ++i )
    {
      double value1 = pointValues.at( i ).terrainElevation( mHydraulicStructure->mesh() );
      double value2 = mHydraulicStructure->terrainElevationAt( pointValues.at( i ).position() );
      QVERIFY( equal( value1, value2, 0.00000001 ) || std::isnan( value2 ) );
    }
  }

  QPolygonF terrainProfile = profile->terrainProfile();

  for ( int i = 0; i < terrainProfile.count(); ++i )
  {
    if ( i < terrainProfile.count() - 1 )
      QVERIFY( terrainProfile.at( i ).x() < terrainProfile.at( i + 1 ).x() );
    double abs = terrainProfile.at( i ).x();
    if ( abs < 15 )
    {
      double zValue = terrainProfile.at( i ).y();
      double meshValue = mHydraulicStructure->terrainElevationAt( QPointF( abs - 5, 5.0 ) );
      QVERIFY( equal( zValue, meshValue, 0.00000001 ) || std::isnan( meshValue ) );
    }
  }
}

void ReoHydraulicStructure2DTest::meteoModel()
{
  mHydraulicStructure->mCapabilities.setFlag( ReosHydraulicStructure2D::GriddedPrecipitation, true );

  std::unique_ptr<ReosMeteorologicStructureItemModel> meteoStructureItemModel( new ReosMeteorologicStructureItemModel( mNetwork ) );
  std::unique_ptr<ReosMeteorologicModel> meteoModel( new ReosMeteorologicModel( "meteo_model" ) );
  meteoStructureItemModel->setCurrentMeteoModel( meteoModel.get() );

  QCOMPARE( meteoStructureItemModel->rowCount( QModelIndex() ), 0 );
  mNetwork->addElement( mHydraulicStructure, false );
  QCOMPARE( meteoStructureItemModel->rowCount( QModelIndex() ), 1 );


  QString filePath = testFile( QStringLiteral( "/grib/W_fr-meteofrance,MODEL,AROME+0025+SP1+00H06H_C_LFPW_202211161200--.grib2" ) );
  QString variableName = QStringLiteral( "Total precipitation rate [kg/(m^2*s)]" );
  ReosGriddedRainItem *rainItem = mRainfallModel->addGriddedRainfall( "gridded rain", "", mRainfallModel->itemToIndex( mRainZone1 ),
                                  new ReosGriddedRainfall( filePath + "::" + variableName + "::" + "cumulative", QStringLiteral( "grib" ) ) ) ;

  QVERIFY( rainItem->data() );

  QModelIndexList indexes;
  indexes << mRainfallModel->itemToIndex( rainItem );
  std::unique_ptr<QMimeData> mimeData( mRainfallModel->mimeData( indexes ) );

  QVERIFY( meteoModel->associatedRainfallItem( mHydraulicStructure ) == nullptr );

  QVERIFY( meteoStructureItemModel->canDropMimeData( mimeData.get(), Qt::DropAction::CopyAction, 0, 0, meteoStructureItemModel->structureToIndex( mHydraulicStructure ) ) );
  QVERIFY( meteoStructureItemModel->dropMimeData( mimeData.get(), Qt::DropAction::CopyAction, 0, 0, meteoStructureItemModel->structureToIndex( mHydraulicStructure ) ) );

  QVERIFY( meteoModel->associatedRainfallItem( mHydraulicStructure ) == rainItem );

  meteoModel->disassociate( mHydraulicStructure );
  QVERIFY( meteoModel->associatedRainfallItem( mHydraulicStructure ) == nullptr );

  QVERIFY( meteoStructureItemModel->dropMimeData( mimeData.get(), Qt::DropAction::CopyAction, 0, 0, meteoStructureItemModel->structureToIndex( mHydraulicStructure ) ) );
  QVERIFY( meteoModel->associatedRainfallItem( mHydraulicStructure ) == rainItem );

  ReosGriddedRainfall *rainfall = meteoModel->associatedRainfall( mHydraulicStructure );
  QVERIFY( rainfall );
  QCOMPARE( rainfall->gridCount(), 6 );

  ReosEncodedElement encodedMeteoModel = meteoModel->encode( nullptr );

  std::unique_ptr<ReosMeteorologicModel> otherModel( new ReosMeteorologicModel( encodedMeteoModel, nullptr, ReosRainfallRegistery::instance() ) );
  QVERIFY( otherModel->associatedRainfallItem( mHydraulicStructure ) == rainItem );
}



QTEST_MAIN( ReoHydraulicStructure2DTest )
#include "reos_hydraulic_structure_2D_test.moc"
