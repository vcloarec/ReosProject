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
#include "reosgisengine.h"
#include "reosmapextent.h"

class ReoHydraulicStructure2DTest: public QObject
{
    Q_OBJECT
  private slots:

    void init();
    void createAndAditGeometry();
  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosModule *mRootModule = nullptr;
    ReosGisEngine engine;
};

void ReoHydraulicStructure2DTest::init()
{
  mRootModule = new ReosModule( this );
  mNetwork = new ReosHydraulicNetwork( nullptr, nullptr );
}

void ReoHydraulicStructure2DTest::createAndAditGeometry()
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
  QVERIFY( !geomStructure->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 1, 0.495 ) ) ) ); //too close of another vertex
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
  geomStructure->addPolylines( lines, 0.01 );

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
  geomStructure->addPolylines( lines, 0.01 );

  QVERIFY( domain != structure2D->domain() );
  data = geomStructure->structuredLinesData();

  domain.insert( 6, QPointF( 1, 0.25 ) );
  QVERIFY( domain == structure2D->domain() );
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.boundaryPointCount, data.vertices.count() - data.internalLines.count() );
  QCOMPARE( data.internalLines.count(), 4 );

  //link two boundary line by internal link and add a verter in the new line
  lines.clear();
  lines << QPointF( 0.5, 0.5 )
        << QPointF( 1.0, 0.0 );
  geomStructure->addPolylines( lines, 0.01 );
  QVERIFY( geomStructure->searchForLine( ReosMapExtent( 0.74, 0.24, 0.76, 0.26 ), lineId ) );
  QVERIFY( geomStructure->insertVertex( ReosSpatialPosition( 0.75, 0.25 ), lineId ) );

  QVERIFY( domain == structure2D->domain() );
  data = geomStructure->structuredLinesData();
  QCOMPARE( data.boundaryPointCount, domain.count() );
  QCOMPARE( data.internalLines.count(), 6 );

//test remove vertex if resulting line cross another line



}

QTEST_MAIN( ReoHydraulicStructure2DTest )
#include "reos_hydraulic_structure_2D_test.moc"
