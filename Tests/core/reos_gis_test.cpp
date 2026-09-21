/***************************************************************************
                      reos_gis_test.cpp
                     --------------------------------------
Date                 : 27-08-2026
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
#include <QtTest/QtTest>
#include <QObject>

#include <memory.h>

#include <QPolygonF>

#include "reospolygonwatershed.h"
#include "reosgisengine.h"

class ReosGisTesting : public QObject
{
    Q_OBJECT
  private slots:
    void test_polygon_watershed();
};

void ReosGisTesting::test_polygon_watershed()
{
  std::unique_ptr<ReosPolygonWatershed> pw( ReosPolygonWatershed::createPolygonWatershed( ReosGisEngine::crsFromEPSG( 2154 ) ) );

  QPolygonF poly(
    { QPointF( -2.49724733804582133, 48.29513350483178868 ),
      QPointF( -2.2437196895132514, 48.24442797512527648 ),
      QPointF( -2.2690724543665084, 47.97399848335719952 ),
      QPointF( -2.31555252326414651, 47.76695090372226815 ),
      QPointF( -2.64513846635648697, 47.78807820776665238 ),
      QPointF( -2.94092072297781826, 48.10498776843236612 ),
      QPointF( -2.49724733804582133, 48.29513350483178868 ) }
  );

  pw->addWatershed( poly, ReosGisEngine::crsFromEPSG( 4326 ), QStringLiteral( "watershed1" ) );

  ReosMapExtent extent = pw->extent( ReosGisEngine::crsFromEPSG( 4326 ) );

  QCOMPARE( extent.xMapMin(), -2.9598565098558787 );
  QCOMPARE( extent.xMapMax(), -2.1971336384458029 );
  QCOMPARE( extent.yMapMin(), 47.738912149221541 );
  QCOMPARE( extent.yMapMax(), 48.306381821846529 );

  pw->removeWatershed( "watershed" );

  extent = pw->extent( ReosGisEngine::crsFromEPSG( 4326 ) );

  QCOMPARE( extent.xMapMin(), -2.9598565098558787 );
  QCOMPARE( extent.xMapMax(), -2.1971336384458029 );
  QCOMPARE( extent.yMapMin(), 47.738912149221541 );
  QCOMPARE( extent.yMapMax(), 48.306381821846529 );

  pw->removeWatershed( "watershed1" );

  extent = pw->extent( ReosGisEngine::crsFromEPSG( 2154 ) );

  QVERIFY( !extent.isValid() );
}

QTEST_MAIN( ReosGisTesting )
#include "reos_gis_test.moc"