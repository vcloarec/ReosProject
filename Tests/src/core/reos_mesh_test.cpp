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

class ReosMeshTest: public QObject
{
    Q_OBJECT
  private slots:
    void memoryMesh();
  private:

};

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

  mesh->generateMesh( generator );

  QCOMPARE( mesh->vertexCount(), 4 );
  QCOMPARE( mesh->faceCount(), 2 );
}

QTEST_MAIN( ReosMeshTest )
#include "reos_mesh_test.moc"
