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

#include "reostimeserie.h"

class ReosDataTesting: public QObject
{
    Q_OBJECT
  private slots:
    void _variable_time_step_time_model();

};

void ReosDataTesting::_variable_time_step_time_model()
{
  ReosTimeSerieVariableTimeStep timeSerie;
  timeSerie.setReferenceTime( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 0, 0, Qt::UTC ) ) );
  ReosTimeSerieVariableTimeStepModel variableTimeStepModel;
  variableTimeStepModel.setSerie( &timeSerie );

  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 1 ); //row for new value
  QCOMPARE( variableTimeStepModel.columnCount( QModelIndex() ), 2 );
  QVERIFY( !( variableTimeStepModel.flags( variableTimeStepModel.index( 0, 0, QModelIndex() ) ) & Qt::ItemIsEditable ) );
  QVERIFY( ( variableTimeStepModel.flags( variableTimeStepModel.index( 0, 1, QModelIndex() ) ) & Qt::ItemIsEditable ) );

  variableTimeStepModel.setNewRowWithFixedTimeStep( false );
  variableTimeStepModel.setVariableTimeStepUnit( ReosDuration::minute );
  QCOMPARE( variableTimeStepModel.columnCount( QModelIndex() ), 3 );

  QVERIFY( !( variableTimeStepModel.flags( variableTimeStepModel.index( 0, 0, QModelIndex() ) ) & Qt::ItemIsEditable ) );
  QVERIFY( ( variableTimeStepModel.flags( variableTimeStepModel.index( 0, 1, QModelIndex() ) ) & Qt::ItemIsEditable ) );
  QVERIFY( ( variableTimeStepModel.flags( variableTimeStepModel.index( 0, 2, QModelIndex() ) ) & Qt::ItemIsEditable ) );

  QVERIFY( !variableTimeStepModel.setData( variableTimeStepModel.index( 0, 1, QModelIndex() ), QVariant( "dfsdf" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 1 );
  QCOMPARE( timeSerie.valueCount(), 0 );

  QVERIFY( !variableTimeStepModel.setData( variableTimeStepModel.index( 0, 0, QModelIndex() ), QVariant( "3.3" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 1 );
  QCOMPARE( timeSerie.valueCount(), 0 );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 0, 1, QModelIndex() ), QVariant( "3.5" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 2 );
  QCOMPARE( timeSerie.valueCount(), 1 );
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 3, 30, Qt::UTC ) ), timeSerie.timeAt( 0 ) );
  QCOMPARE( 0, timeSerie.valueAt( 0 ) );

  QVERIFY( !variableTimeStepModel.setData( variableTimeStepModel.index( 0, 2, QModelIndex() ), QVariant( "dfsdf" ), Qt::EditRole ) );
  QCOMPARE( 0, timeSerie.valueAt( 0 ) );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 0, 2, QModelIndex() ), QVariant( "10.200" ), Qt::EditRole ) );
  QCOMPARE( 10.2, timeSerie.valueAt( 0 ) );

  // insert at end a relative time lesser than the value before
  QVERIFY( !variableTimeStepModel.setData( variableTimeStepModel.index( 1, 1, QModelIndex() ), QVariant( "3.0" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 2 );
  QCOMPARE( timeSerie.valueCount(), 1 );

  // change the value before
  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 0, 1, QModelIndex() ), QVariant( "2.5" ), Qt::EditRole ) );
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 2, 30, Qt::UTC ) ), timeSerie.timeAt( 0 ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 2 );
  QCOMPARE( timeSerie.valueCount(), 1 );

  //retry
  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 1, 1, QModelIndex() ), QVariant( "3.0" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 3 );
  QCOMPARE( timeSerie.valueCount(), 2 );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 2, 2, QModelIndex() ), QVariant( "9.99" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 4 );
  QCOMPARE( timeSerie.valueCount(), 3 );
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 3, 30, Qt::UTC ) ), timeSerie.timeAt( 2 ) );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 1, 2, QModelIndex() ), QVariant( "5.200" ), Qt::EditRole ) );
  QCOMPARE( 5.2, timeSerie.valueAt( 1 ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 4 );
  QCOMPARE( timeSerie.valueCount(), 3 );

  variableTimeStepModel.setNewRowWithFixedTimeStep( true );
  variableTimeStepModel.setFixedTimeStep( ReosDuration( 12, ReosDuration::minute ) );
  // now constant time steps are set automatically, the column for vcalue is the column1
  QVERIFY( !variableTimeStepModel.setData( variableTimeStepModel.index( 3, 2, QModelIndex() ), QVariant( "10.99" ), Qt::EditRole ) );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 3, 1, QModelIndex() ), QVariant( "10.99" ), Qt::EditRole ) );
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 15, 30, Qt::UTC ) ), timeSerie.timeAt( 3 ) );
  QCOMPARE( 10.99, timeSerie.valueAt( 3 ) );
  QCOMPARE( 10.99, timeSerie.valueAtTime( ReosDuration( 15.5, ReosDuration::minute ) ) );


  QList<QVariantList> data;

  data << ( QVariantList() << QVariant( QString( "5.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "dfgdfg" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "10.5" ) ) << QVariant( "1111" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 4 );

  data.clear();
  data << ( QVariantList() << QVariant( QString( "1.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "7.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "10.5" ) ) << QVariant( "1111" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 4 );

  data.clear();
  data << ( QVariantList() << QVariant( QString( "25.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "24.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "20.5" ) ) << QVariant( "1111" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 4 );

  data.clear();
  data << ( QVariantList() << QVariant( QString( "20.5" ) ) << QVariant( "11.0" ) );
  data << ( QVariantList() << QVariant( QString( "24.5" ) ) << QVariant( "12.2" ) );
  data << ( QVariantList() << QVariant( QString( "25.5" ) ) << QVariant( "13.2" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 7 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 10.2 );

}

QTEST_MAIN( ReosDataTesting )
#include "reos_data_test.moc"
