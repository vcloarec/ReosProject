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
#include "reosrainfallregistery.h"

class ReosRainfallTest: public QObject
{
    Q_OBJECT
  private slots:
    void initTestCase();
    void addingItem();
    void IDFCurvesMontana();
    void IDFCurvesSherman();
    void loadRainfallData();

  private:
    ReosModule mRootModule;

};



void ReosRainfallTest::initTestCase()
{
  ReosIdfFormulaRegistery::instantiate( &mRootModule );
}

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

void ReosRainfallTest::IDFCurvesMontana()
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

  curve.setCurrentParameterTimeUnit( ReosDuration::minute );
  curve.setCurrentResultTimeUnit( ReosDuration::minute );

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

  ReosIdfParameters *parameters_1 = curve.createParameters( 0, formula, curve.currentParameterTimeUnit(), curve.currentResultTimeUnit() );
  QVERIFY( parameters_1 );
  QCOMPARE( parameters_1->formulaName, QStringLiteral( "Montana" ) );
  QCOMPARE( parameters_1->parametersCount(), 2 );
  parameters_1->parameter( 0 )->setValue( 4.782 );
  parameters_1->parameter( 1 )->setValue( 0.31928 );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( equal( curve.height( duration_2 ), 36.749, 0.001 ) );
  QVERIFY( curve.height( duration_3 ) < 0 );
  QVERIFY( curve.height( duration_4 ) < 0 );

  ReosIdfParameters *parameters_2 = curve.createParameters( 1, formula, curve.currentParameterTimeUnit(), curve.currentResultTimeUnit() );
  QVERIFY( parameters_2 );
  QCOMPARE( parameters_2->formulaName, QStringLiteral( "Montana" ) );
  QCOMPARE( parameters_2->parametersCount(), 2 );
  parameters_2->parameter( 0 )->setValue( 4.680 );
  parameters_2->parameter( 1 )->setValue( 0.313964 );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( equal( curve.height( duration_2 ), 36.749, 0.001 ) );
  QVERIFY( equal( curve.height( duration_3 ), 58.791, 0.001 ) );
  QVERIFY( curve.height( duration_4 ) < 0 );

}

void ReosRainfallTest::IDFCurvesSherman()
{
  ReosIdfFormulaRegistery *formulaRegistery = ReosIdfFormulaRegistery::instance();
  ReosIdfFormulaRegistery::instance()->registerFormula( new ReosIdfFormulaMontana );

  ReosIdfFormula *formula = formulaRegistery->formula( QStringLiteral( "XXX" ) );
  QVERIFY( !formula );
  formula = formulaRegistery->formula( QStringLiteral( "Sherman" ) );
  QVERIFY( !formula );

  formulaRegistery->registerFormula( new ReosIdfFormulaSherman );
  formula = formulaRegistery->formula( QStringLiteral( "Sherman" ) );
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
  curve.setCurrentFormula( QStringLiteral( "Sherman" ) );
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
  QCOMPARE( parameters_1->formulaName, QStringLiteral( "Sherman" ) );
  QCOMPARE( parameters_1->parametersCount(), 3 );
  parameters_1->parameter( 0 )->setValue( 1750.134 );
  parameters_1->parameter( 1 )->setValue( 20 );
  parameters_1->parameter( 2 )->setValue( 0.74 );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( equal( curve.height( duration_2 ), 38.056, 0.01 ) );
  QVERIFY( curve.height( duration_3 ) < 0 );
  QVERIFY( curve.height( duration_4 ) < 0 );

  ReosIdfParameters *parameters_2 = curve.createParameters( 1, formula );
  QVERIFY( parameters_2 );
  QCOMPARE( parameters_2->formulaName, QStringLiteral( "Sherman" ) );
  QCOMPARE( parameters_2->parametersCount(), 3 );
  parameters_2->parameter( 0 )->setValue( 1750.134 );
  parameters_2->parameter( 1 )->setValue( 20 );
  parameters_2->parameter( 2 )->setValue( 0.74 );

  QVERIFY( curve.height( duration_1 ) < 0 );
  QVERIFY( equal( curve.height( duration_2 ), 38.056, 0.01 ) );
  QVERIFY( equal( curve.height( duration_3 ), 56.383, 0.001 ) );
  QVERIFY( curve.height( duration_4 ) < 0 );
}

void ReosRainfallTest::loadRainfallData()
{
  ReosRainfallModel *rainfallModel = ReosRainfallRegistery::instance()->rainfallModel();
  ReosIdfFormulaRegistery::instance()->registerFormula( new ReosIdfFormulaMontana );

  std::vector<std::string> paths;

  paths.push_back( "rainfallData_before-2-2.rrf" );
  paths.push_back( "rainfallData.rrf" );

  for ( const std::string &path : std::as_const( paths ) )
  {

    QVERIFY( rainfallModel->loadFromFile( test_file( path ).c_str(), QString() ) );
    // root, only one region item
    QCOMPARE( rainfallModel->rowCount( QModelIndex() ), 1 );
    ReosRainfallItem *rootItem = rainfallModel->indexToItem( rainfallModel->index( 0, 0, QModelIndex() ) );
    Q_ASSERT( rootItem );
    QCOMPARE( rootItem->type(), ReosRainfallItem::Zone );
    QCOMPARE( rootItem->name(), QStringLiteral( "Somewhere" ) );

    // sub region item
    QCOMPARE( rainfallModel->rowCount( rainfallModel->itemToIndex( rootItem ) ), 1 );
    ReosRainfallItem *subRegionItem = rainfallModel->indexToItem( rainfallModel->index( 0, 0, rainfallModel->itemToIndex( rootItem ) ) );
    Q_ASSERT( subRegionItem );
    QCOMPARE( subRegionItem->type(), ReosRainfallItem::Zone );
    QCOMPARE( subRegionItem->name(), QStringLiteral( "little zone" ) );

    //Station item
    QCOMPARE( rainfallModel->rowCount( rainfallModel->itemToIndex( subRegionItem ) ), 1 );
    ReosRainfallItem *stationItem = rainfallModel->indexToItem( rainfallModel->index( 0, 0, rainfallModel->itemToIndex( subRegionItem ) ) );
    Q_ASSERT( stationItem );
    QCOMPARE( stationItem->type(), ReosRainfallItem::Station );
    QCOMPARE( stationItem->name(), QStringLiteral( "station" ) );

    //data in station
    QCOMPARE( rainfallModel->rowCount( rainfallModel->itemToIndex( stationItem ) ), 5 );

    //****** IDF curves
    ReosRainfallIdfCurvesItem *idfCurvesItem = qobject_cast<ReosRainfallIdfCurvesItem *>(
          rainfallModel->indexToItem( rainfallModel->index( 0, 0, rainfallModel->itemToIndex( stationItem ) ) ) );
    Q_ASSERT( idfCurvesItem );
    QCOMPARE( idfCurvesItem->type(), ReosRainfallItem::Data );
    QCOMPARE( idfCurvesItem->name(), QStringLiteral( "IDF Curves" ) );
    ReosIntensityDurationFrequencyCurves *curves = idfCurvesItem->data();
    QVERIFY( !curves );
    //need to setupData
    idfCurvesItem->setupData();
    curves = idfCurvesItem->data();
    QVERIFY( curves );

    // id curve
    ReosIntensityDurationCurve *curve = idfCurvesItem->curve( 0 );
    Q_ASSERT( curves );
    Q_ASSERT( curve );
    QCOMPARE( rainfallModel->rowCount( rainfallModel->itemToIndex( idfCurvesItem ) ), 1 );
    ReosRainfallIntensityDurationCurveItem *idCurveItem = qobject_cast<ReosRainfallIntensityDurationCurveItem *>(
          rainfallModel->indexToItem( rainfallModel->index( 0, 0, rainfallModel->itemToIndex( idfCurvesItem ) ) ) );
    Q_ASSERT( idCurveItem );
    QCOMPARE( idCurveItem->name(), QStringLiteral( "10 years" ) );
    QCOMPARE( idCurveItem->data(), curve );
    QCOMPARE( curve->currentFormula(), "Montana" );
    QCOMPARE( curve->intervalCount(), 4 );
    QPair<ReosDuration, ReosDuration> interval = {ReosDuration( 6, ReosDuration::minute ), ReosDuration( 15, ReosDuration::minute )};
    QVERIFY( curve->timeInterval( 0 ) == interval );
    interval = {ReosDuration( 15, ReosDuration::minute ), ReosDuration( 30, ReosDuration::minute )};
    QVERIFY( curve->timeInterval( 1 ) == interval );
    interval = {ReosDuration( 30, ReosDuration::minute ), ReosDuration( 60, ReosDuration::minute )};
    QVERIFY( curve->timeInterval( 2 ) == interval );
    interval = {ReosDuration( 60, ReosDuration::minute ), ReosDuration( 120, ReosDuration::minute )};
    QVERIFY( curve->timeInterval( 3 ) == interval );

    ReosIdfParameters *param = curve->currentParameters( 0 );
    QCOMPARE( param->parametersCount(), 2 );
    QCOMPARE( param->parameter( 0 )->value(), 4.677 );
    QCOMPARE( param->parameter( 1 )->value(), 0.314 );
    param = curve->currentParameters( 1 );
    QCOMPARE( param->parametersCount(), 2 );
    QCOMPARE( param->parameter( 0 )->value(), 4.782 );
    QCOMPARE( param->parameter( 1 )->value(), 0.321 );
    param = curve->currentParameters( 2 );
    QCOMPARE( param->parametersCount(), 2 );
    QCOMPARE( param->parameter( 0 )->value(), 9.34 );
    QCOMPARE( param->parameter( 1 )->value(), 0.5189 );
    param = curve->currentParameters( 3 );
    QCOMPARE( param->parametersCount(), 2 );
    QCOMPARE( param->parameter( 0 )->value(), 11.722 );
    QCOMPARE( param->parameter( 1 )->value(), 0.574 );

    //****** Chicago rainfall
    ReosRainfallChicagoItem *chicagoItem = qobject_cast<ReosRainfallChicagoItem *>(
        rainfallModel->indexToItem( rainfallModel->index( 1, 0, rainfallModel->itemToIndex( stationItem ) ) ) );
    QVERIFY( chicagoItem );
    QCOMPARE( chicagoItem->name(), "Chicago 10 ans" );
    chicagoItem->setupData();
    chicagoItem->resolveDependencies();
    ReosChicagoRainfall *chicagoRainfall = chicagoItem->data();
    QVERIFY( chicagoRainfall );
    QVERIFY( chicagoRainfall->intensityDurationCurve() == curve );
    QVERIFY( chicagoRainfall->totalDuration()->value() == ReosDuration( 2, ReosDuration::hour ) );
    QVERIFY( chicagoRainfall->timeStep()->value() == ReosDuration( 5, ReosDuration::minute ) );
    QVERIFY( chicagoRainfall->centerCoefficient()->value() == 0.5 );
    QVERIFY( chicagoRainfall->referenceTime()->value() == QDateTime( QDate( 2013, 02, 01 ), QTime( 1, 2, 3 ), Qt::UTC ) );

    //****** Double triangle rainfall
    ReosRainfallDoubleTriangleItem *doubleTriangleItem = qobject_cast<ReosRainfallDoubleTriangleItem *>(
          rainfallModel->indexToItem( rainfallModel->index( 2, 0, rainfallModel->itemToIndex( stationItem ) ) ) );
    QVERIFY( doubleTriangleItem );
    QCOMPARE( doubleTriangleItem->name(), "Double triangle" );
    doubleTriangleItem->setupData();
    ReosDoubleTriangleRainfall *doubleTriangle = doubleTriangleItem->data();
    QVERIFY( doubleTriangle );
    QVERIFY( doubleTriangle->intensityDurationCurveTotal() == curve );
    QVERIFY( doubleTriangle->intensityDurationCurveIntensePeriod() == curve );
    QVERIFY( doubleTriangle->totalDuration()->value() == ReosDuration( 2, ReosDuration::hour ) );
    QVERIFY( doubleTriangle->intenseDuration()->value() == ReosDuration( 10, ReosDuration::minute ) );
    QVERIFY( doubleTriangle->timeStep()->value() == ReosDuration( 2, ReosDuration::minute ) );
    QVERIFY( doubleTriangle->centerCoefficient()->value() == 0.5 );
    QVERIFY( doubleTriangle->referenceTime()->value() == QDateTime( QDate( 2010, 02, 03 ), QTime( 5, 6, 7 ), Qt::UTC ) );

    //****** Aletrnate rainfall
    ReosRainfallAlternatingBlockItem *alternateItem = qobject_cast<ReosRainfallAlternatingBlockItem *>(
          rainfallModel->indexToItem( rainfallModel->index( 4, 0, rainfallModel->itemToIndex( stationItem ) ) ) );
    QVERIFY( alternateItem );
    QCOMPARE( alternateItem->name(), "Alternate" );
    alternateItem->setupData();
    alternateItem->resolveDependencies();
    ReosAlternatingBlockRainfall *alternateRainfall = alternateItem->data();
    QVERIFY( alternateRainfall );
    QVERIFY( alternateRainfall->intensityDurationCurve() == curve );
    QVERIFY( alternateRainfall->totalDuration()->value() == ReosDuration( 1, ReosDuration::hour ) );
    QVERIFY( alternateRainfall->timeStep()->value() == ReosDuration( 5, ReosDuration::minute ) );
    QVERIFY( alternateRainfall->centerCoefficient()->value() == 0.5 );
    QVERIFY( alternateRainfall->referenceTime()->value() == QDateTime( QDate( 2021, 01, 01 ), QTime( 0, 0, 0 ), Qt::UTC ) );

    //****** Aletrnate rainfall
    ReosRainfallGaugedRainfallItem *gaugedItem = qobject_cast<ReosRainfallGaugedRainfallItem *>(
          rainfallModel->indexToItem( rainfallModel->index( 3, 0, rainfallModel->itemToIndex( stationItem ) ) ) );
    QVERIFY( gaugedItem );
    QCOMPARE( gaugedItem->name(), "gauged one" );
    gaugedItem->setupData();
    ReosSerieRainfall *gaugedRainfall = gaugedItem->data();
    QVERIFY( gaugedRainfall );
    QCOMPARE( gaugedRainfall->valueCount(), 3 );
    QVERIFY( gaugedRainfall->timeStep()->value() == ReosDuration( 10, ReosDuration::minute ) );
    QCOMPARE( gaugedRainfall->referenceTime()->value(), QDateTime( QDate( 2013, 02, 01 ), QTime( 2, 3, 4 ), Qt::UTC ) );
    QCOMPARE( gaugedRainfall->valueAt( 0 ), 4.0 );
    QCOMPARE( gaugedRainfall->valueAt( 1 ), 5.0 );
    QCOMPARE( gaugedRainfall->valueAt( 2 ), 6.0 );
  }
}

QTEST_MAIN( ReosRainfallTest )
#include "reos_rainfall_test.moc"
