/***************************************************************************
  reos_tin_test.cpp

 ---------------------
 begin                : 12.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
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

#include "reostriangularirregularnetwork.h"
#include "reosgisengine.h"

class ReosTinTest: public QObject
{
    Q_OBJECT
  private slots:
    void initTestCase();
    void addPoint();

  private:
    std::unique_ptr<ReosGisEngine> gisEngine;

};

void ReosTinTest::initTestCase()
{
  gisEngine = std::make_unique<ReosGisEngine>();
}

void ReosTinTest::addPoint()
{
  QString tinLayerid = gisEngine->createTinEditor( "TIN Editor" );
  QVERIFY( !tinLayerid.isEmpty() );
  ReosTriangularIrregularNetwork *tin = gisEngine->triangularIrregularNetWork( tinLayerid );

  QVERIFY( tin );

  QVERIFY( tin->addVertex( { 0, 0, 0 } ) );
  QVERIFY( tin->addVertex( { 0, 1, 1 } ) );
  QVERIFY( tin->addVertex( { 1, 1, 2 } ) );

  QCOMPARE( tin->vertexCount(), 3 );

}

QTEST_MAIN( ReosTinTest )
#include "reos_tin_test.moc"
