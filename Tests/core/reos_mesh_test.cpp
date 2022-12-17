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
    ReosGisEngine engine;
};


void ReosMeshTest::GmshGenerator()
{
  QPolygonF domain;
  domain << QPointF( 0, 0 ) << QPointF( 0, 20 ) << QPointF( 20, 20 ) << QPointF( 20, 0 );

  std::unique_ptr<ReosPolylinesStructure> structure =
    ReosPolylinesStructure::createPolylineStructure( domain, QString() );

  ReosGmshGenerator generator;

  std::unique_ptr<ReosMeshGeneratorProcess> process;
  process.reset( generator.getGenerateMeshProcess( structure.get(), nullptr ) );
  process->start();
  QVERIFY( process->isSuccessful() );
  ReosMeshFrameData frameData = process->meshResult();

  ReosMeshResolutionController controler;
  controler.defaultSize()->setValue( 1 );
  process.reset( generator.getGenerateMeshProcess( structure.get(), &controler ) );
  process->start();
  QVERIFY( process->isSuccessful() );
  frameData = process->meshResult();
#ifdef _MSC_VER
  QCOMPARE( frameData.facesIndexes.count(), 1022 );
#else
  QCOMPARE( frameData.facesIndexes.count(), 1026 );
#endif
  controler.defaultSize()->setValue( 10 );
  process.reset( generator.getGenerateMeshProcess( structure.get(), &controler ) );
  process->start();
  QVERIFY( process->isSuccessful() );
  frameData = process->meshResult();
  QCOMPARE( frameData.facesIndexes.count(), 16 );
}

void ReosMeshTest::memoryMesh()
{
  std::unique_ptr<ReosMesh> mesh( ReosMesh::createMeshFrame() );
  QVERIFY( mesh->isValid() );

  QCOMPARE( mesh->vertexCount(), 0 );
  QCOMPARE( mesh->faceCount(), 0 );

  ReosMeshGeneratorPoly2Tri generator;

  QPolygonF domain;
  domain << QPointF( 0, 0 ) << QPointF( 0, 20 ) << QPointF( 20, 20 ) << QPointF( 20, 0 );
  generator.setDomain( domain );

  std::unique_ptr<ReosPolylinesStructure> structure = ReosPolylinesStructure::createPolylineStructure( domain, QString() );

  std::unique_ptr<ReosMeshGeneratorProcess> process;
  process.reset( generator.getGenerateMeshProcess( structure.get(), nullptr ) );
  process->start();
  QVERIFY( process->isSuccessful() );
  mesh->generateMesh( process->meshResult() );

  QCOMPARE( mesh->vertexCount(), 4 );
  QCOMPARE( mesh->faceCount(), 2 );
}

QTEST_MAIN( ReosMeshTest )
#include "reos_mesh_test.moc"
