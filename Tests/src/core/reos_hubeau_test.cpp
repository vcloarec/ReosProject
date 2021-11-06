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


#include "reosmapextent.h"
#include "reosmap.h"
#include "reosgisengine.h"

class ReosHubEauTest: public QObject
{
    Q_OBJECT
  private slots:
    void connection();

    void stations();

  private:

};


void ReosHubEauTest::connection()
{
//  ReosHubEauServer server;
//  QVERIFY( server.testConnection() );
}

void ReosHubEauTest::stations()
{
//  ReosGisEngine gisEngine;
//  ReosMap map( &gisEngine, nullptr );
//  ReosHubEauServer server( &map );
//  ReosMapExtent extent( -5.283, 47.036, -1.7500, 49.0311 );
//  map.setExtent( extent );

}

QTEST_MAIN( ReosHubEauTest )
#include "reos_hubeau_test.moc"
