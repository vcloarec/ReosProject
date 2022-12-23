/***************************************************************************
                      reos_raster_test.cpp
                     --------------------------------------
Date                 : 04-09-2020
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

#include "reos_testutils.h"
#include "reosgriddedrainitem.h"
#include "reoscomephoresprovider.h"

class ReosComephoresbTest: public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();
};

void ReosComephoresbTest::createProvider()
{
  std::unique_ptr<ReosGriddedRainfallProvider> provider( new ReosComephoresProvider );

  QString comephoresPath( testFile( QStringLiteral( "comephores/tif.tif" ) ) );
  provider->setDataSource( comephoresPath );
  QVERIFY( !provider->isValid() );

  comephoresPath = QString( testFile( QStringLiteral( "comephores/tif" ) ) );
  provider->setDataSource( comephoresPath );
  QVERIFY( provider->isValid() );

  QCOMPARE( provider->count(), 73 );
  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 2018, 02, 18 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 50 ), QDateTime( QDate( 2018, 02, 20 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 50 ), QDateTime( QDate( 2018, 02, 20 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();

  QCOMPARE( extent.xCellSize(), 1000.0 );
  QCOMPARE( extent.yCellSize(), -1000.0 );
  QCOMPARE( extent.xCellCount(), 101 );
  QCOMPARE( extent.yCellCount(), 170 );

  QVector<double> values = provider->data( 70 );
  QCOMPARE( values.at( 8581 ), 2.2 );
  QCOMPARE( values.at( 8584 ), 3.0 );
  QVERIFY( std::isnan( values.last() ) );
}


QTEST_MAIN( ReosComephoresbTest )
#include "reos_comephores_test.moc"
