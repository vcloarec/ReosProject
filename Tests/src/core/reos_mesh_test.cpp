/***************************************************************************
                      reos_mesh_test.cpp
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

#include "reosgisengine.h"
#include "reosmesh.h"
#include "reosmeshgenerator.h"
#include "reospolylinesstructure.h"
#include "reosgmshgenerator.h"

class ReosMeshTest: public QObject
{
    Q_OBJECT
  private slots:
    void GmshGenerator();
    void memoryMesh();

  private:

};


void ReosMeshTest::GmshGenerator()
{
  QPolygonF domain;
  domain << QPointF( 0, 0 ) << QPointF( 0, 20 ) << QPointF( 20, 20 ) << QPointF( 20, 0 );

  std::unique_ptr<ReosPolylinesStructure> structure =
    ReosPolylinesStructure::createPolylineStructure( domain, QString() );

  ReosGmshGenerator generator;
  generator.setGeometryStructure( structure.get(), QString() );

  bool ok;
  ReosMeshFrameData frameData = generator.generatedMesh( &ok );
  QVERIFY( ok );

  ReosGmshResolutionController controler;
  generator.setResolutionController( &controler );

  controler.defaultSize()->setValue( 1 );
  frameData = generator.generatedMesh( &ok );
  QCOMPARE( frameData.facesIndexes.count(), 832 );

  controler.defaultSize()->setValue( 10 );
  frameData = generator.generatedMesh( &ok );
  QCOMPARE( frameData.facesIndexes.count(), 162 );

  QVERIFY( ok );

}

void ReosMeshTest::memoryMesh()
{
  ReosGisEngine engine;
  std::unique_ptr<ReosMesh> mesh( ReosMesh::createMemoryMesh() );
  QVERIFY( mesh->isValid() );

  QCOMPARE( mesh->vertexCount(), 0 );
  QCOMPARE( mesh->faceCount(), 0 );

  ReosMeshGeneratorPoly2Tri generator;

  QPolygonF domain;
  domain << QPointF( 0, 0 ) << QPointF( 0, 20 ) << QPointF( 20, 20 ) << QPointF( 20, 0 );
  generator.setDomain( domain );

  QVERIFY( mesh->generateMesh( generator ) );

  QCOMPARE( mesh->vertexCount(), 4 );
  QCOMPARE( mesh->faceCount(), 2 );
}

QTEST_MAIN( ReosMeshTest )
#include "reos_mesh_test.moc"
