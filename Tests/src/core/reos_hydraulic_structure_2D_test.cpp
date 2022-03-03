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

#include "reoshydraulicstructure2d.h"
#include "reospolygonstructure.h"
#include "reosgisengine.h"
#include "reosmapextent.h"

class ReoHydraulicStructure2DTest: public QObject
{
    Q_OBJECT
  private slots:

    void init();
    void createAndEditPolylineStructure();
    void createAndEditPolygonStructure();
  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosModule *mRootModule = nullptr;
    ReosGisEngine engine;
};

void ReoHydraulicStructure2DTest::init()
{
  mRootModule = new ReosModule( this );
  mNetwork = new ReosHydraulicNetwork( nullptr, nullptr, nullptr );
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
         << QPointF( 1, 0 )
         << QPointF( 0.5, 0 );

  std::unique_ptr < ReosHydraulicStructure2D> structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork );

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

  structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork );
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

  structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork );
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

QTEST_MAIN( ReoHydraulicStructure2DTest )
#include "reos_hydraulic_structure_2D_test.moc"
