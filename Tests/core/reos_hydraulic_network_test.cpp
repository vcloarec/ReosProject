/***************************************************************************
                      reos_hydraulic_structure_2d_test.cpp
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
#include <QModelIndexList>

#include "reoshydraulicnetwork.h"
#include "reoshydraulicelementmodel.h"
#include "reosgisengine.h"
#include "reoshydrographsource.h"
#include "reoshydrographrouting.h"
#include "reoshydraulicscheme.h"

class ReoHydraulicNetworkTest: public QObject
{
    Q_OBJECT
  private slots:

    void initTestCase();
    void addRemoveElement();

  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosModule *mRootModule = nullptr;
    ReosGisEngine *mGisEngine = nullptr;
    ReosHydraulicElementModel *mElementModel = nullptr;

};

void ReoHydraulicNetworkTest::initTestCase()
{
  mRootModule = new ReosModule( this );
  mGisEngine = new ReosGisEngine( this );
  mNetwork = new ReosHydraulicNetwork( mRootModule, mGisEngine, nullptr );
  mElementModel = new ReosHydraulicElementModel( mNetwork );

  QVERIFY( mNetwork->hydraulicNetworkElements().isEmpty() );
  QCOMPARE( mNetwork->hydraulicSchemeCollection()->schemeCount(), 1 );
}

void ReoHydraulicNetworkTest::addRemoveElement()
{
  QCOMPARE( mElementModel->rowCount( QModelIndex() ), 0 );
  ReosHydraulicNetworkElement *junctionNode1 = mNetwork->addElement( new ReosHydrographJunction( ReosSpatialPosition( QPointF( 10, 10 ), mGisEngine->crs() ), mNetwork ) );
  ReosHydraulicNetworkElement *junctionNode2 = mNetwork->addElement( new ReosHydrographJunction( ReosSpatialPosition( QPointF( 10, 20 ), mGisEngine->crs() ), mNetwork ) );
  QCOMPARE( mElementModel->rowCount( QModelIndex() ), 2 );

  mNetwork->removeElement( junctionNode1 );
  QCOMPARE( mElementModel->rowCount( QModelIndex() ), 1 );
  junctionNode1 = mNetwork->addElement( new ReosHydrographJunction( ReosSpatialPosition( QPointF( 10, 10 ), mGisEngine->crs() ), mNetwork ) );
  QCOMPARE( mElementModel->rowCount( QModelIndex() ), 2 );

  ReosHydraulicNetworkElement *link = mNetwork->addElement(
                                        new ReosHydrographRoutingLink( qobject_cast<ReosHydrographJunction *>( junctionNode1 ),
                                            qobject_cast<ReosHydrographJunction *>( junctionNode1 ),
                                            mNetwork ) );

  QCOMPARE( mElementModel->rowCount( QModelIndex() ), 3 );
  QCOMPARE( mNetwork->networkExtent(), ReosMapExtent( 10.0, 10.0, 10.0, 20.0 ) );

  QList<ReosHydraulicNetworkElement *> allElements = mNetwork->hydraulicNetworkElements();
  QVERIFY( allElements.contains( junctionNode1 ) );
  QVERIFY( allElements.contains( junctionNode2 ) );
  QVERIFY( allElements.contains( link ) );

  QModelIndex index = mElementModel->elementToIndex( junctionNode1 );
  QCOMPARE( junctionNode1, mElementModel->indexToElement( index ) );
  index = mElementModel->elementToIndex( junctionNode2 );
  QCOMPARE( junctionNode2, mElementModel->indexToElement( index ) );
  index = mElementModel->elementToIndex( link );
  QCOMPARE( link, mElementModel->indexToElement( index ) );

  mNetwork->removeElement( junctionNode1 );
  QCOMPARE( mElementModel->rowCount( QModelIndex() ), 1 );
  allElements = mNetwork->hydraulicNetworkElements();
  QVERIFY( !allElements.contains( junctionNode1 ) );
  QVERIFY( allElements.contains( junctionNode2 ) );
  QVERIFY( !allElements.contains( link ) );
  QCOMPARE( mNetwork->networkExtent(), ReosMapExtent( 10.0, 20.0, 10.0, 20.0 ) );
}



QTEST_MAIN( ReoHydraulicNetworkTest )
#include "reos_hydraulic_network_test.moc"
