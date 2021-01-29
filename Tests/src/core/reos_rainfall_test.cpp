/***************************************************************************
                      reos_watershed_test.cpp
                     --------------------------------------
Date                 : October-2020
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
#include "reosrainfallmodel.h"
#include "reosrainfallitem.h"

class ReosRainfallTest: public QObject
{
    Q_OBJECT
  private slots:

    void addingItem();

  private:

};

void ReosRainfallTest::addingItem()
{
  ReosRainfallModel rainfallModel;

  QVERIFY( rainfallModel.addZone( QStringLiteral( "Somewhere" ), QString() ) );

  QCOMPARE( rainfallModel.rowCount( QModelIndex() ), 1 );

  // try to add a zone with same name
  QVERIFY( ! rainfallModel.addZone( QStringLiteral( "Somewhere" ), QString() ) );
  QCOMPARE( rainfallModel.rowCount( QModelIndex() ), 1 );

  QVERIFY( rainfallModel.addZone( QStringLiteral( "Elsewhere" ), QString() ) );
  QCOMPARE( rainfallModel.rowCount( QModelIndex() ), 2 );

  QModelIndex index = rainfallModel.index( 0, 0, QModelIndex() );
  ReosRainfallItem *item = rainfallModel.indexToItem( index );
  QVERIFY( item );
  QCOMPARE( item->name(), QStringLiteral( "Somewhere" ) );
  QCOMPARE( QStringLiteral( "Somewhere" ), rainfallModel.data( index, Qt::DisplayRole ).toString() );

  index = rainfallModel.index( 1, 0, QModelIndex() );
  item = rainfallModel.indexToItem( index );
  QVERIFY( item );
  QCOMPARE( item->name(), QStringLiteral( "Elsewhere" ) );
  QCOMPARE( QStringLiteral( "Elsewhere" ), rainfallModel.data( index, Qt::DisplayRole ).toString() );

  index = rainfallModel.index( 2, 0, QModelIndex() );
  item = rainfallModel.indexToItem( index );
  QCOMPARE( item, nullptr );
  QVERIFY( !rainfallModel.data( index, Qt::DisplayRole ).isValid() );

  index = rainfallModel.index( 1, 0, QModelIndex() );
  rainfallModel.addZone( QStringLiteral( "Elsewhere in somewhere" ), QString(), index );
  QCOMPARE( rainfallModel.rowCount( QModelIndex() ), 2 );
  QCOMPARE( rainfallModel.rowCount( rainfallModel.index( 1, 0, QModelIndex() ) ), 1 );
  index = rainfallModel.index( 0, 0, rainfallModel.index( 1, 0, QModelIndex() ) );
  QVERIFY( index.isValid() );
  item = rainfallModel.indexToItem( index );
  QVERIFY( item );
  QCOMPARE( item->name(), QStringLiteral( "Elsewhere in somewhere" ) );
  QCOMPARE( item->name(), rainfallModel.data( index, Qt::DisplayRole ).toString() );

  QString tempFileName( tmp_file( QStringLiteral( "rainfallFile" ).toStdString() ).c_str() );

  rainfallModel.saveToFile( tempFileName, QStringLiteral( "test" ) );

  ReosRainfallModel otherModel;

  otherModel.loadFromFile( tempFileName, QStringLiteral( "test" ) );
  index = otherModel.index( 0, 0, otherModel.index( 1, 0, QModelIndex() ) );
  QVERIFY( index.isValid() );
  item = otherModel.indexToItem( index );
  QVERIFY( item );
  QCOMPARE( item->name(), QStringLiteral( "Elsewhere in somewhere" ) );
  QCOMPARE( item->name(), otherModel.data( index, Qt::DisplayRole ).toString() );
}

QTEST_MAIN( ReosRainfallTest )
#include "reos_rainfall_test.moc"
