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

#include "reos_testutils.h"
#include "reosgriddedrainitem.h"
#include "reosgriddedrainfallprovider.h"
#include "reosera5provider.h"
#include "reosnetcdfutils.h"
#include "reosgisengine.h"

class ReosEra5Test: public QObject
{
    Q_OBJECT

  private slots:
    void createProvider();
};

void ReosEra5Test::createProvider()
{
  std::unique_ptr<ReosDataProvider> compatibleProvider( ReosDataProviderRegistery::instance()->createCompatibleProvider(
        ERA5_FILES_PATH + QStringLiteral( "/nc.nc" ), ReosGriddedRainfall::staticType() ) );
  QVERIFY( !compatibleProvider );

  const QString &variabbleShortName = QStringLiteral( "tp" );
  const QVariantMap &uriParamDescription = ReosDataProviderRegistery::instance()->uriParameters( QStringLiteral( "era5" ), ReosEra5Provider::dataType() );

  QVariantMap uriParam;
  uriParam[QStringLiteral( "file-or-dir-path" )] = QString( ERA5_FILES_PATH );
  uriParam[QStringLiteral( "var-short-name" )] = QStringLiteral( "tp" );

  bool ok = false;
  const QString &uri = ReosDataProviderRegistery::instance()->buildUri( QStringLiteral( "era5" ), ReosEra5Provider::dataType(), uriParam, ok );
  QVERIFY( ok );

  compatibleProvider.reset( ReosDataProviderRegistery::instance()->createCompatibleProvider( uri, ReosGriddedData::staticType() ) );
  QVERIFY( compatibleProvider );

  ReosGriddedDataProvider *provider = qobject_cast<ReosGriddedDataProvider *>( compatibleProvider.get() );
  provider->setDataSource( uri );
  QVERIFY( provider->isValid() );

  QCOMPARE( provider->count(), 2880 );
  QCOMPARE( provider->startTime( 0 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 0, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 0 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 1 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 1, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 1 ), QDateTime( QDate( 1990, 1, 1 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->startTime( 50 ), QDateTime( QDate( 1990, 1, 3 ), QTime( 2, 0, 0 ), Qt::UTC ) );
  QCOMPARE( provider->endTime( 50 ), QDateTime( QDate( 1990, 1, 3 ), QTime( 3, 0, 0 ), Qt::UTC ) );

  ReosRasterExtent extent = provider->extent();

  QCOMPARE( extent.xCellSize(), 0.25 );
  QCOMPARE( extent.yCellSize(), -0.25 );
  QCOMPARE( extent.xCellCount(), 63 );
  QCOMPARE( extent.yCellCount(), 41 );

  QVector<double> values = provider->data( 0 );
  QCOMPARE( values.at( 50 ), 1.112854089824677e-05 );
  QCOMPARE( values.at( 60 ), 3.561133087438655e-06 );

  values = provider->data( 50 );
  QCOMPARE( values.at( 50 ), 5.341699631158416e-06 );
  QCOMPARE( values.at( 60 ), 6.343268312000456e-06 );

  double min = 0, max = 0;
  QVERIFY( !provider->getDirectMinMax( min, max ) );
  provider->calculateMinMax( min, max );
  QCOMPARE( min, 0.0 );
  QCOMPARE( max, 0.0072928667068481445 );
}


QTEST_MAIN( ReosEra5Test )
#include "reos_era5_test.moc"
