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

#include "reospython.h"

class ReosPythonTesting: public QObject
{
    Q_OBJECT
  private slots:
    void initTestCase();

    void reosCoreModule();
  private:

    ReosPython python;
};


void ReosPythonTesting::initTestCase()
{
  python.initialize();
  QString errMsg;
  QString version;
  bool success = python.runString( QStringLiteral( "import sys" ), errMsg );
  QVERIFY( success );
  QVERIFY( python.evalString( QStringLiteral( "sys.version" ), version ) );
  QVERIFY( !version.isEmpty() );

  QVERIFY( QCoreApplication::instance() );

  success = python.runString( QStringLiteral( "from reos.core import *" ), errMsg );
  QVERIFY( success );
}


void ReosPythonTesting::reosCoreModule()
{
  QString errMsg;
  QVERIFY( python.runString( QStringLiteral( "core_module=ReosCoreModule()" ), errMsg ) );

  QVERIFY( python.runString( QStringLiteral( "gis_engine=core_module.gisEngine()" ), errMsg ) );

  QVERIFY( python.runString( QStringLiteral( "crs=gis_engine.crs()" ), errMsg ) );

  QString ret;
  QVERIFY( python.evalString( QStringLiteral( "crs!=''" ), ret ) );
  QCOMPARE( ret, QStringLiteral( "True" ) );

  QVERIFY( python.evalString( QStringLiteral( "crs==ReosGisEngine.crsFromEPSG(4326)" ), ret ) );
  QCOMPARE( ret, QStringLiteral( "True" ) );

  QVERIFY( python.runString( QStringLiteral( "hydrograph = ReosHydrograph(None, 'hub-eau-hydrometry', 'J881301001')" ), errMsg ) );

}

QTEST_MAIN( ReosPythonTesting )
#include "reos_test_python.moc"
