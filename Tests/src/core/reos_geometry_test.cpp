/***************************************************************************
                      reos_geometry_test.cpp
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

#include "reosgeometryutils.h"

class ReosGeometryTest: public QObject
{
    Q_OBJECT
  private slots:
    void polygonInteractions();
    void polygonPolylineInteractions();

};
void ReosGeometryTest::polygonInteractions()
{
  QPolygonF polygon1;
  QPolygonF polygon2;
  QPolygonF polygon3;
  polygon1 << QPointF( 0, 0 ) << QPointF( 0, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon2 << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon3 << QPointF( 0, 0 ) << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 0, 2 );

  QCOMPARE( ReosInclusionType::Partial, ReosGeometryUtils::polygonIsInsidePolygon( polygon1, polygon2 ) );
  QCOMPARE( ReosInclusionType::Partial, ReosGeometryUtils::polygonIsInsidePolygon( polygon1, polygon3 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon2, polygon1 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon3, polygon1 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polygonIsInsidePolygon( polygon2, polygon3 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polygonIsInsidePolygon( polygon3, polygon2 ) );

  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon1, polygon1 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polygonIsInsidePolygon( polygon2, polygon2 ) );
}


void ReosGeometryTest::polygonPolylineInteractions()
{
  QPolygonF polygon1;
  QPolygonF polygon2;
  QPolygonF polygon3;
  polygon1 << QPointF( 0, 0 ) << QPointF( 0, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon2 << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 2, 2 ) << QPointF( 2, 0 );
  polygon3 << QPointF( 0, 0 ) << QPointF( 1, 0 ) << QPointF( 1, 2 ) << QPointF( 0, 2 );

  QPolygonF polyline1;
  QPolygonF polyline2;
  QPolygonF polyline3;
  polyline1 << QPointF( -1, 1 ) << QPointF( 3, 1 );
  polyline2 << QPointF( 0.25, 1 ) << QPointF( .75, 1 );
  polyline3 << QPointF( 0, 1 ) << QPointF( 1, 1 );

  QCOMPARE( ReosInclusionType::Partial, ReosGeometryUtils::polylineIsInsidePolygon( polyline1, polygon1 ) );

  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline2, polygon1 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polylineIsInsidePolygon( polyline2, polygon2 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline2, polygon3 ) );

  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline3, polygon1 ) );
  QCOMPARE( ReosInclusionType::None, ReosGeometryUtils::polylineIsInsidePolygon( polyline3, polygon2 ) );
  QCOMPARE( ReosInclusionType::Total, ReosGeometryUtils::polylineIsInsidePolygon( polyline3, polygon3 ) );
}


QTEST_MAIN( ReosGeometryTest )
#include "reos_geometry_test.moc"
