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
#include <QtTest/QtTest>
#include <QObject>

#include "reos_testutils.h"
#include "reosgisengine.h"
#include "reoswatershed.h"
#include "reoslandusedata.h"


class ReosCdslccTest : public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();

  private:
    ReosModule mRootModule;
    ReosGisEngine *mGisEngine = nullptr;
};

void ReosCdslccTest::createProvider()
{
    std::unique_ptr<ReosDataProvider> compatibleProvider(
    ReosDataProviderRegistery::instance()->createCompatibleProvider( testFile("/cdslcc/finistere.nc"), ReosLandUseData::staticType() ));

  QVERIFY( compatibleProvider );
}


QTEST_MAIN( ReosCdslccTest )
#include "reos_cdslcc_test.moc"
