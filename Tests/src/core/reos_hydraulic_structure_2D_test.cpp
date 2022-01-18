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

  QCOMPARE( structure2D->domain(), domain );
  structure2D->geometryStructure()->undoStack()->undo();
  QCOMPARE( structure2D->domain(), domain );

  ReosMapExtent searchZone( -0.1, 0.4, -0.1, 0.6 );
  ReosGeometryStructureVertex *vert = structure2D->geometryStructure()->searchForVertex( searchZone );
  QVERIFY( !vert );

  searchZone = ReosMapExtent( -0.1, 0.4, 0.1, 0.6 );
  vert = structure2D->geometryStructure()->searchForVertex( searchZone );
  QVERIFY( vert );

  QVERIFY( !structure2D->geometryStructure()->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 0.5, 1.5 ) ) ) ); //lines are crossing
  QVERIFY( !structure2D->geometryStructure()->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 1, 0.495 ) ) ) ); //too close of another vertex
  QVERIFY( structure2D->geometryStructure()->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 0.991, 0.491 ) ) ) ); //just outside the tolerance
  QVERIFY( structure2D->geometryStructure()->vertexCanBeMoved( vert, ReosSpatialPosition( QPointF( 0.5, 0.5 ) ) ) );

  structure2D->geometryStructure()->moveVertex( vert, ReosSpatialPosition( QPointF( 0.5, 0.5 ) ) );
  QPolygonF expectded = structure2D->domain();

  QVERIFY( domain != structure2D->domain() );

  structure2D->geometryStructure()->undoStack()->undo();

  QVERIFY( domain == structure2D->domain() );

  structure2D->geometryStructure()->undoStack()->redo();

  domain[1] = QPointF( 0.5, 0.5 );
  expectded = structure2D->domain();
  QVERIFY( domain == structure2D->domain() );
}

QTEST_MAIN( ReoHydraulicStructure2DTest )
#include "reos_hydraulic_structure_2D_test.moc"
