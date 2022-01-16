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

#include "reoshydraulicstructure2d.h"
#include "reosgisengine.h"

class ReoHydraulicStructure2DTest: public QObject
{
    Q_OBJECT
  private slots:

    void init();
    void create();
  private:
    ReosHydraulicNetwork *mNetwork = nullptr;
    ReosModule *mRootModule = nullptr;
    ReosGisEngine engine;
};

void ReoHydraulicStructure2DTest::init()
{
  mRootModule = new ReosModule( this );
  mNetwork = new ReosHydraulicNetwork( nullptr, nullptr );
}

void ReoHydraulicStructure2DTest::create()
{
  QPolygonF domain;
  domain << QPointF( 0, 0 ) << QPointF( 0, 1 ) << QPointF( 1, 1 ) << QPointF( 1, 0 );
  std::unique_ptr < ReosHydraulicStructure2D> structure2D = std::make_unique<ReosHydraulicStructure2D>( domain, QString(), mNetwork );
}

QTEST_MAIN( ReoHydraulicStructure2DTest )
#include "reos_hydraulic_structure_2D_test.moc"
