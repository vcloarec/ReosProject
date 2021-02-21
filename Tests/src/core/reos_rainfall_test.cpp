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
#include "reosidfcurves.h"

class ReosRainfallTest: public QObject
{
    Q_OBJECT
  private slots:

    void addingItem();
    void IDFCurves();

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

void ReosRainfallTest::IDFCurves()
{
  ReosIdfFormulaRegistery *formulaRegistery = ReosIdfFormulaRegistery::instance();
  formulaRegistery->registerFormula( new ReosIdfFormulaMontana );

  ReosIdfFormula *formula = formulaRegistery->formula( QStringLiteral( "XXX" ) );
  QVERIFY( !formula );
  formula = formulaRegistery->formula( QStringLiteral( "Montana" ) );
  QVERIFY( formula );

  ReosIntensityDurationCurve curve;
  ReosDuration duration_1( 10, ReosDuration::minute );
  ReosDuration duration_2( 20, ReosDuration::minute );
  ReosDuration duration_3( 40, ReosDuration::minute );
  ReosDuration duration_4( 70, ReosDuration::minute );

  QVERIFY( !curve.isFormulaValid() );
  curve.setupFormula( formulaRegistery );
  QVERIFY( !curve.isFormulaValid() );
  curve.setCurrentFormula( QStringLiteral( "dfdsf" ) );
  QVERIFY( !curve.isFormulaValid() );
  curve.setupFormula( formulaRegistery );
  QVERIFY( !curve.isFormulaValid() );
  curve.setCurrentFormula( QStringLiteral( "Montana" ) );
  QVERIFY( !curve.isFormulaValid() );
  curve.setupFormula( formulaRegistery );
  QVERIFY( curve.isFormulaValid() );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( curve.height( duration_2 ) < 0 );

  QVERIFY( curve.addInterval( ReosDuration( 15, ReosDuration::minute ), ReosDuration( 30, ReosDuration::minute ) ) );
  QVERIFY( !curve.addInterval( ReosDuration( 10, ReosDuration::minute ), ReosDuration( 25, ReosDuration::minute ) ) );
  QVERIFY( !curve.addInterval( ReosDuration( 20, ReosDuration::minute ), ReosDuration( 35, ReosDuration::minute ) ) );
  QVERIFY( !curve.addInterval( ReosDuration( 20, ReosDuration::minute ), ReosDuration( 22, ReosDuration::minute ) ) );
  QVERIFY( curve.addInterval( ReosDuration( 30, ReosDuration::minute ), ReosDuration( 60, ReosDuration::minute ) ) );

  QCOMPARE( curve.intervalCount(), 2 );

  QVERIFY( curve.timeInterval( 0 ).first == ReosDuration( 15, ReosDuration::minute ) );
  QVERIFY( curve.timeInterval( 0 ).second == ReosDuration( 30, ReosDuration::minute ) );
  QVERIFY( curve.timeInterval( 1 ).first == ReosDuration( 30, ReosDuration::minute ) );
  QVERIFY( curve.timeInterval( 1 ).second == ReosDuration( 60, ReosDuration::minute ) );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( curve.height( duration_2 ) < 0 );
  QVERIFY( curve.height( duration_3 ) < 0 );
  QVERIFY( curve.height( duration_4 ) < 0 );

  ReosIdfParameters *parameters_1 = curve.createParameters( 0, formula );
  QVERIFY( parameters_1 );
  QCOMPARE( parameters_1->formulaName, QStringLiteral( "Montana" ) );
  QCOMPARE( parameters_1->parametersCount(), 2 );
  parameters_1->parameter( 0 )->setValue( 4.782 );
  parameters_1->parameter( 1 )->setValue( 0.31928 );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( equal( curve.height( duration_2 ), 36.749, 0.001 ) );
  QVERIFY( curve.height( duration_3 ) < 0 );
  QVERIFY( curve.height( duration_4 ) < 0 );

  ReosIdfParameters *parameters_2 = curve.createParameters( 1, formula );
  QVERIFY( parameters_2 );
  QCOMPARE( parameters_2->formulaName, QStringLiteral( "Montana" ) );
  QCOMPARE( parameters_2->parametersCount(), 2 );
  parameters_2->parameter( 0 )->setValue( 4.680 );
  parameters_2->parameter( 1 )->setValue( 0.313964 );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( equal( curve.height( duration_2 ), 36.749, 0.001 ) );
  QVERIFY( equal( curve.height( duration_3 ), 58.791, 0.001 ) );
  QVERIFY( curve.height( duration_4 ) < 0 );

  delete formulaRegistery;
}


QTEST_MAIN( ReosRainfallTest )
#include "reos_rainfall_test.moc"
