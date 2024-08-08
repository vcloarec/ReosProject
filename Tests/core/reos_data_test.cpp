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

#include "reostimeseries.h"
#include "reosmemoryraster.h"
#include "reos_testutils.h"
#include "reoshydrograph.h"

class ReosDataTesting: public QObject
{
    Q_OBJECT
  private slots:
    void variable_time_step_time_model();
    void encode_variable_time_step();
    void hydrograph();

};

void ReosDataTesting::variable_time_step_time_model()
{
  ReosTimeSeriesVariableTimeStep timeSerie;
  timeSerie.setReferenceTime( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  ReosTimeSeriesVariableTimeStepModel variableTimeStepModel;
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
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 3, 30 ), Qt::UTC ), timeSerie.timeAt( 0 ) );
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
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 2, 30 ), Qt::UTC ), timeSerie.timeAt( 0 ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 2 );
  QCOMPARE( timeSerie.valueCount(), 1 );

  //retry
  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 1, 1, QModelIndex() ), QVariant( "3.0" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 3 );
  QCOMPARE( timeSerie.valueCount(), 2 );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 2, 2, QModelIndex() ), QVariant( "9.99" ), Qt::EditRole ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 4 );
  QCOMPARE( timeSerie.valueCount(), 3 );
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 3, 30 ), Qt::UTC ), timeSerie.timeAt( 2 ) );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 1, 2, QModelIndex() ), QVariant( "5.200" ), Qt::EditRole ) );
  QCOMPARE( 5.2, timeSerie.valueAt( 1 ) );
  QCOMPARE( variableTimeStepModel.rowCount( QModelIndex() ), 4 );
  QCOMPARE( timeSerie.valueCount(), 3 );

  variableTimeStepModel.setNewRowWithFixedTimeStep( true );
  variableTimeStepModel.setFixedTimeStep( ReosDuration( 12, ReosDuration::minute ) );
  // now constant time steps are set automatically, the column for vcalue is the column1
  QVERIFY( !variableTimeStepModel.setData( variableTimeStepModel.index( 3, 2, QModelIndex() ), QVariant( "10.99" ), Qt::EditRole ) );

  QVERIFY( variableTimeStepModel.setData( variableTimeStepModel.index( 3, 1, QModelIndex() ), QVariant( "10.99" ), Qt::EditRole ) );
  QCOMPARE( QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 15, 30 ), Qt::UTC ), timeSerie.timeAt( 3 ) );
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

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 1, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 4 );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 4 );

  data.clear();
  data << ( QVariantList() << QVariant( QString( "25.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "24.5" ) ) << QVariant( "1111" ) );
  data << ( QVariantList() << QVariant( QString( "20.5" ) ) << QVariant( "1111" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 4 );

  data.clear();
  data << ( QVariantList() << QVariant( QString( "0.5" ) ) << QVariant( "11.0" ) );
  data << ( QVariantList() << QVariant( QString( "0.75" ) ) << QVariant( "12.2" ) );
  data << ( QVariantList() << QVariant( QString( "1.0" ) ) << QVariant( "13.2" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 7 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 13.2 );

  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 10.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 5.2 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 9.99 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 10.99 );

  data.clear();
  data << ( QVariantList() << QVariant( QString( "20.5" ) ) << QVariant( "11.0" ) );
  data << ( QVariantList() << QVariant( QString( "20.75" ) ) << QVariant( "12.2" ) );
  data << ( QVariantList() << QVariant( QString( "21.0" ) ) << QVariant( "13.2" ) );

  variableTimeStepModel.insertValues( variableTimeStepModel.index( 7, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 10 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 10.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 5.2 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 9.99 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 10.99 );

  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 13.2 );


  data.clear();
  data << ( QVariantList() << QVariant( "11.0" ) );
  data << ( QVariantList() << QVariant( "12.2" ) );
  variableTimeStepModel.insertValues( variableTimeStepModel.index( 1, 2, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 12 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );

  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );

  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 10.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.2 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 9.99 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 10.99 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );


  data.clear();
  data << ( QVariantList() << QVariant( "12.0" ) );
  data << ( QVariantList() << QVariant( "14.2" ) );
  variableTimeStepModel.insertValues( variableTimeStepModel.index( 12, 2, QModelIndex() ), data );

  QCOMPARE( timeSerie.valueCount(), 14 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 10.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.2 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 9.99 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 10.99 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );

  QVERIFY( timeSerie.relativeTimeAt( 12 ) == ReosDuration( 33, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 12 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 13 ) == ReosDuration( 45, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 13 ), 14.2 );

  data.clear();
  data << ( QVariantList() << QVariant( "12.0" ) );
  data << ( QVariantList() << QVariant( "14.2" ) );
  data << ( QVariantList() << QVariant( "15.0" ) );
  data << ( QVariantList() << QVariant( "12.23" ) );
  data << ( QVariantList() << QVariant( "1.23" ) );

  variableTimeStepModel.setValues( variableTimeStepModel.index( 4, 0, QModelIndex() ), data );

  QCOMPARE( timeSerie.valueCount(), 14 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 14.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 15.0 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 12.23 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 1.23 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 12 ) == ReosDuration( 33, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 12 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 13 ) == ReosDuration( 45, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 13 ), 14.2 );


  // data with not valid time regarding the existing one
  data.clear();
  data << ( QVariantList() <<  QVariant( QString( "0.5" ) ) << QVariant( "2.0" ) );
  data << ( QVariantList() << QVariant( QString( "21.5" ) ) << QVariant( "4.2" ) );
  data << ( QVariantList() << QVariant( QString( "22.5" ) ) << QVariant( "5.0" ) );
  data << ( QVariantList() << QVariant( QString( "23.5" ) ) << QVariant( "2.23" ) );
  data << ( QVariantList() << QVariant( QString( "24.5" ) ) << QVariant( "0.23" ) );

  variableTimeStepModel.setValues( variableTimeStepModel.index( 4, 0, QModelIndex() ), data );

  QCOMPARE( timeSerie.valueCount(), 14 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 14.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 15.0 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 12.23 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 1.23 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 12 ) == ReosDuration( 33, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 12 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 13 ) == ReosDuration( 45, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 13 ), 14.2 );

  // data with not valid time regarding the existing one
  data.clear();
  data << ( QVariantList() <<  QVariant( QString( "2.0" ) ) << QVariant( "2.0" ) );
  data << ( QVariantList() << QVariant( QString( "1.75" ) ) << QVariant( "4.2" ) );
  data << ( QVariantList() << QVariant( QString( "1.5" ) ) << QVariant( "5.0" ) );
  data << ( QVariantList() << QVariant( QString( "1.75" ) ) << QVariant( "2.23" ) );
  data << ( QVariantList() << QVariant( QString( "1.2" ) ) << QVariant( "0.23" ) );

  variableTimeStepModel.setValues( variableTimeStepModel.index( 4, 0, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 14 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 150, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 14.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 3, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 15.0 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 3.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 12.23 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 15.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 1.23 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 12 ) == ReosDuration( 33, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 12 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 13 ) == ReosDuration( 45, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 13 ), 14.2 );

// data time valid
  data.clear();
  data << ( QVariantList() <<  QVariant( QString( "1.2" ) ) << QVariant( "2.0" ) );
  data << ( QVariantList() << QVariant( QString( "1.25" ) ) << QVariant( "4.2" ) );
  data << ( QVariantList() << QVariant( QString( "1.5" ) ) << QVariant( "5.0" ) );
  data << ( QVariantList() << QVariant( QString( "1.75" ) ) << QVariant( "2.23" ) );
  data << ( QVariantList() << QVariant( QString( "2.0" ) ) << QVariant( "0.23" ) );

  variableTimeStepModel.setValues( variableTimeStepModel.index( 4, 0, QModelIndex() ), data );

  QCOMPARE( timeSerie.valueCount(), 14 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 72, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 75, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 1.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.0 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 1.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 2.23 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 2.0, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 0.23 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 12 ) == ReosDuration( 33, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 12 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 13 ) == ReosDuration( 45, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 13 ), 14.2 );

  // set value from end
  data.clear();
  data << ( QVariantList() <<  QVariant( QString( "46" ) ) << QVariant( "2.0" ) );
  data << ( QVariantList() << QVariant( QString( "47" ) ) << QVariant( "4.2" ) );
  data << ( QVariantList() << QVariant( QString( "48" ) ) << QVariant( "5.0" ) );

  variableTimeStepModel.setValues( variableTimeStepModel.index( 14, 0, QModelIndex() ), data );

  QCOMPARE( timeSerie.valueCount(), 17 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 72, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 75, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 1.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.0 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 1.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 2.23 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 2.0, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 0.23 );
  QVERIFY( timeSerie.relativeTimeAt( 9 ) == ReosDuration( 20.5, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 9 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 10 ) == ReosDuration( 20.75, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 10 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 11 ) == ReosDuration( 21, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 11 ), 13.2 );
  QVERIFY( timeSerie.relativeTimeAt( 12 ) == ReosDuration( 33, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 12 ), 12.0 );
  QVERIFY( timeSerie.relativeTimeAt( 13 ) == ReosDuration( 45, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 13 ), 14.2 );
  QVERIFY( timeSerie.relativeTimeAt( 14 ) == ReosDuration( 46, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 14 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 15 ) == ReosDuration( 47, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 15 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 16 ) == ReosDuration( 48, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 16 ), 5.0 );

  variableTimeStepModel.deleteRows( variableTimeStepModel.index( 5, 0, QModelIndex() ), 10 );
  QCOMPARE( timeSerie.valueCount(), 7 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 35, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 11.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 40, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 72, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 2.0 );

  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 47, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 48, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.0 );

  variableTimeStepModel.deleteRows( variableTimeStepModel.index( 0, 1, QModelIndex() ), 3 );
  QCOMPARE( timeSerie.valueCount(), 4 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 72, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 47, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 48, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 5.0 );

  variableTimeStepModel.insertRows( variableTimeStepModel.index( 0, 1, QModelIndex() ), 3 );
  QCOMPARE( timeSerie.valueCount(), 7 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 0, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 0. );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 15, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 4.0666666666666666667 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 8.1333333333333333333 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 72, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 47, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 48, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.0 );
  //interpolted value
  QCOMPARE( timeSerie.valueAtTime( ReosDuration( 47.5, ReosDuration::minute ) ), 4.6 );

  variableTimeStepModel.insertRows( variableTimeStepModel.index( 7, 1, QModelIndex() ), 2 );
  QCOMPARE( timeSerie.valueCount(), 9 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( 0, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 0. );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( 15, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 4.0666666666666666667 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( 30, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 8.1333333333333333333 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 45, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 12.2 );
  QVERIFY( timeSerie.relativeTimeAt( 4 ) == ReosDuration( 72, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 4 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 5 ) == ReosDuration( 47, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 5 ), 4.2 );
  QVERIFY( timeSerie.relativeTimeAt( 6 ) == ReosDuration( 48, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 6 ), 5.0 );
  QVERIFY( timeSerie.relativeTimeAt( 7 ) == ReosDuration( 60, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 7 ), 0.0 );
  QVERIFY( timeSerie.relativeTimeAt( 8 ) == ReosDuration( 72, ReosDuration::minute ) );
  QCOMPARE( timeSerie.valueAt( 8 ), 0.0 );

  //not possible to insert with no value from start if first existing time time is 0
  variableTimeStepModel.insertRows( variableTimeStepModel.index( 0, 1, QModelIndex() ), 10 );
  QCOMPARE( timeSerie.valueCount(), 9 );

  //same way, not possible to insert only values without time if first existing time is 0
  data.clear();
  data << ( QVariantList() << QVariant( QString( "1.0" ) ) );
  data << ( QVariantList() << QVariant( QString( "2.0" ) ) );
  data << ( QVariantList() << QVariant( QString( "3.0" ) ) );
  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 1, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 9 );

  //but possible if there is time values

  data.clear();
  data << ( QVariantList() << QVariant( QString( "-3" ) ) << QVariant( QString( "1.0" ) ) );
  data << ( QVariantList() << QVariant( QString( "-2" ) ) << QVariant( QString( "2.0" ) ) );
  data << ( QVariantList() << QVariant( QString( "-1" ) ) << QVariant( QString( "3.0" ) ) );
  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 1, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 12 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( -180, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 1.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( -120, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( -60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 3.0 );
  QVERIFY( timeSerie.relativeTimeAt( 3 ) == ReosDuration( 0, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 3 ), 0.0 );

  timeSerie.clear();
  variableTimeStepModel.insertValues( variableTimeStepModel.index( 0, 1, QModelIndex() ), data );
  QCOMPARE( timeSerie.valueCount(), 3 );
  QVERIFY( timeSerie.relativeTimeAt( 0 ) == ReosDuration( -180, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 0 ), 1.0 );
  QVERIFY( timeSerie.relativeTimeAt( 1 ) == ReosDuration( -120, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 1 ), 2.0 );
  QVERIFY( timeSerie.relativeTimeAt( 2 ) == ReosDuration( -60, ReosDuration::second ) );
  QCOMPARE( timeSerie.valueAt( 2 ), 3.0 );

  ReosEncodeContext encodeContext;
  ReosEncodedElement elem = timeSerie.encode( encodeContext );

  std::unique_ptr<ReosTimeSeriesVariableTimeStep> otherTimeSerie( ReosTimeSeriesVariableTimeStep::decode( elem, encodeContext ) );
  QVERIFY( otherTimeSerie );
  QCOMPARE( otherTimeSerie->valueCount(), 3 );
  QVERIFY( otherTimeSerie->relativeTimeAt( 0 ) == ReosDuration( -180, ReosDuration::second ) );
  QCOMPARE( otherTimeSerie->valueAt( 0 ), 1.0 );
  QVERIFY( otherTimeSerie->relativeTimeAt( 1 ) == ReosDuration( -120, ReosDuration::second ) );
  QCOMPARE( otherTimeSerie->valueAt( 1 ), 2.0 );
  QVERIFY( otherTimeSerie->relativeTimeAt( 2 ) == ReosDuration( -60, ReosDuration::second ) );
  QCOMPARE( otherTimeSerie->valueAt( 2 ), 3.0 );
}

void ReosDataTesting::encode_variable_time_step()
{
  // build a variable time step serie as old version(<= 2.2.95)
  ReosEncodedElement providerElement( QStringLiteral( "variable-time-step-serie-memory-provider" ) );
  providerElement.addData( QStringLiteral( "reference-time" ), QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );

  QVector<double> values;
  QVector<ReosDuration> timeValues;
  values << 1.23 << 3.45 << 6.78 << 9.12;
  timeValues << ReosDuration( 0.0, ReosDuration::minute )
             << ReosDuration( 1.0, ReosDuration::hour )
             << ReosDuration( 7200, ReosDuration::second )
             << ReosDuration( 1.0, ReosDuration::day );

  providerElement.addData( QStringLiteral( "values" ), values );
  QList<ReosEncodedElement> encodedTimeValues;
  encodedTimeValues.reserve( timeValues.count() );

  for ( const ReosDuration &time : std::as_const( timeValues ) )
    encodedTimeValues.append( time.encode() );

  providerElement.addListEncodedData( QStringLiteral( "timeValues" ), encodedTimeValues );

  ReosEncodedElement tsElement( QStringLiteral( "time-serie-variable-time-step" ) );
  tsElement.addData( QStringLiteral( "provider-key" ), QStringLiteral( "variable-time-step-memory" ) );
  tsElement.addEncodedData( QStringLiteral( "provider-data" ), providerElement );
  ReosEncodeContext encodeContext;

  std::unique_ptr<ReosTimeSeriesVariableTimeStep> timeSerie( ReosTimeSeriesVariableTimeStep::decode( tsElement, encodeContext ) );

  QVERIFY( timeSerie );
  QCOMPARE( timeSerie->valueCount(), 4 );
  QCOMPARE( timeSerie->timeAt( 0 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QVERIFY( timeSerie->relativeTimeAt( 0 ) == ReosDuration( 0, ReosDuration::second ) );
  QCOMPARE( timeSerie->valueAt( 0 ), 1.23 );
  QCOMPARE( timeSerie->timeAt( 1 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QVERIFY( timeSerie->relativeTimeAt( 1 ) == ReosDuration( 3600, ReosDuration::second ) );
  QCOMPARE( timeSerie->valueAt( 1 ), 3.45 );
  QCOMPARE( timeSerie->timeAt( 2 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 4, 0, 0 ), Qt::UTC ) );
  QVERIFY( timeSerie->relativeTimeAt( 2 ) == ReosDuration( 120, ReosDuration::minute ) );
  QCOMPARE( timeSerie->valueAt( 2 ), 6.78 );
  QCOMPARE( timeSerie->timeAt( 3 ), QDateTime( QDate( 2020, 01, 02 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QVERIFY( timeSerie->relativeTimeAt( 3 ) == ReosDuration( 24, ReosDuration::hour ) );
  QCOMPARE( timeSerie->valueAt( 3 ), 9.12 );

  ReosEncodedElement elem = timeSerie->encode( encodeContext );
  std::unique_ptr<ReosTimeSeriesVariableTimeStep> otherTimeSerie( ReosTimeSeriesVariableTimeStep::decode( elem, encodeContext ) );
  QVERIFY( otherTimeSerie );

  QCOMPARE( otherTimeSerie->valueCount(), 4 );
  QCOMPARE( otherTimeSerie->timeAt( 0 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QVERIFY( otherTimeSerie->relativeTimeAt( 0 ) == ReosDuration( 0, ReosDuration::second ) );
  QCOMPARE( otherTimeSerie->valueAt( 0 ), 1.23 );
  QCOMPARE( otherTimeSerie->timeAt( 1 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 3, 0, 0 ), Qt::UTC ) );
  QVERIFY( otherTimeSerie->relativeTimeAt( 1 ) == ReosDuration( 3600, ReosDuration::second ) );
  QCOMPARE( otherTimeSerie->valueAt( 1 ), 3.45 );
  QCOMPARE( otherTimeSerie->timeAt( 2 ), QDateTime( QDate( 2020, 01, 01 ), QTime( 4, 0, 0 ), Qt::UTC ) );
  QVERIFY( otherTimeSerie->relativeTimeAt( 2 ) == ReosDuration( 120, ReosDuration::minute ) );
  QCOMPARE( otherTimeSerie->valueAt( 2 ), 6.78 );
  QCOMPARE( otherTimeSerie->timeAt( 3 ), QDateTime( QDate( 2020, 01, 02 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QVERIFY( otherTimeSerie->relativeTimeAt( 3 ) == ReosDuration( 24, ReosDuration::hour ) );
  QCOMPARE( otherTimeSerie->valueAt( 3 ), 9.12 );
}

void ReosDataTesting::hydrograph()
{
  QString file( testFile( QStringLiteral( "hydroportail/J261401002_Q_2000.csv" ) ) );
  ReosHydrograph hydrograph( nullptr, "hydroportail", file );

  QCOMPARE( hydrograph.valueCount(), 1304 );

  QDateTime timeMax;
  double max = 0;
  for ( int i = 0; i < hydrograph.valueCount(); ++i )
  {
    if ( hydrograph.valueAt( i ) > max )
    {
      max = hydrograph.valueAt( i );
    }
  }

  double min;

  hydrograph.valuesExtent( min, max, true );

  ReosFloat64GridBlock data = hydrograph.toConstantTimeStep( ReosDuration( 0.1, ReosDuration::hour ) );
  QVector<double> values = data.values();

  QDateTime refTime = hydrograph.referenceTime();

  QCOMPARE( values.count(), 87812 );
  QVERIFY( equal( values.at( 0 ), 6.66, 0.001 ) );
  QVERIFY( equal( values.at( 500 ), 5.1281, 0.001 ) );
  QVERIFY( equal( values.at( 1000 ), 4.4908, 0.001 ) );

  QDateTime newRef = QDateTime( refTime.date(), QTime( refTime.time().hour(), 0, 0 ), Qt::UTC );

  data = hydrograph.toConstantTimeStep( ReosDuration( 0.1, ReosDuration::hour ), newRef );
  values = data.values();

  QVERIFY( equal( values.at( 0 ), 0.0, 0.001 ) );
  QVERIFY( equal( values.at( 1 ), 0.0, 0.001 ) );
  QVERIFY( equal( values.at( 2 ), 0.0, 0.001 ) );
  QVERIFY( equal( values.at( 3 ), 6.66, 0.001 ) );

  QVERIFY( equal( values.at( 0 ), hydrograph.valueAtTime( newRef ), 0.001 ) );
  QVERIFY( equal( values.at( 1 ), hydrograph.valueAtTime( newRef.addSecs( 360 ) ), 0.001 ) );
  QVERIFY( equal( values.at( 2 ), hydrograph.valueAtTime( newRef.addSecs( 720 ) ), 0.001 ) );
  QVERIFY( equal( values.at( 3 ), hydrograph.valueAtTime( newRef.addSecs( 1080 ) ), 0.001 ) );

  newRef = QDateTime( refTime.date(), QTime( refTime.time().hour() + 1, 0, 0 ), Qt::UTC );
  data = hydrograph.toConstantTimeStep( ReosDuration( 0.1, ReosDuration::hour ), QDateTime( refTime.date(), QTime( refTime.time().hour() + 1, 0, 0 ), Qt::UTC ) );
  values = data.values();

  QVERIFY( equal( values.at( 0 ), 6.626, 0.001 ) );
  QVERIFY( equal( values.at( 1 ), 6.621, 0.001 ) );
  QVERIFY( equal( values.at( 2 ), 6.616, 0.001 ) );
  QVERIFY( equal( values.at( 3 ), 6.612, 0.001 ) );

  QVERIFY( equal( values.at( 0 ), hydrograph.valueAtTime( newRef ), 0.001 ) );
  QVERIFY( equal( values.at( 1 ), hydrograph.valueAtTime( newRef.addSecs( 360 ) ), 0.001 ) );
  QVERIFY( equal( values.at( 2 ), hydrograph.valueAtTime( newRef.addSecs( 720 ) ), 0.001 ) );
  QVERIFY( equal( values.at( 3 ), hydrograph.valueAtTime( newRef.addSecs( 1080 ) ), 0.001 ) );


  ReosTimeWindow tw = hydrograph.timeWindow();
  ReosHydrograph hyrographToComplete;
  hyrographToComplete.copyFrom( &hydrograph );
  ReosHydrograph hydroForCompleting_1;
  hydroForCompleting_1.setValue( tw.end().addMSecs( -3600 ), 1.0 );
  hydroForCompleting_1.setValue( tw.end(), 2.0 );
  hydroForCompleting_1.setValue( tw.end().addMSecs( 3600 ), 3.0 );
  hydroForCompleting_1.setValue( tw.end().addMSecs( 4152 ), 4.0 );

  hyrographToComplete.completeAfter( &hydroForCompleting_1 );

  QCOMPARE( hyrographToComplete.valueCount(), 1306 );

  double val = hyrographToComplete.valueAt( 1304 );
  QDateTime time = hyrographToComplete.timeAt( 1304 );

  QVERIFY( equal( val, 3.0, 0.001 ) );
  QCOMPARE( tw.end().addMSecs( 3600 ), time );

  val = hyrographToComplete.valueAt( 1305 );
  time = hyrographToComplete.timeAt( 1305 );

  QVERIFY( equal( val, 4.0, 0.001 ) );
  QCOMPARE( tw.end().addMSecs( 4152 ), time );

}

QTEST_MAIN( ReosDataTesting )
#include "reos_data_test.moc"
