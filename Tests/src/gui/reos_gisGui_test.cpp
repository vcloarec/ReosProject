/***************************************************************************
  reos_gisGui_test.cpp - %{Cpp:License:ClassName}

 ---------------------
 begin                : 17.1.2021
 copyright            : (C) 2021 by Vincent Cloarec
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
#include <QPointer>

#include "reosmapitem.h"
#include "reosmaptool.h"
#include "reosgisengine.h"
#include "reosmap.h"

class ReosGisGuiTest: public QObject
{
    Q_OBJECT
  private slots:
    void initTestCase();
    void mapItemToolCreationDeletation();

  private:
    std::unique_ptr<ReosGisEngine> gisEngine;
    ReosMap *map;
    QWidget placeHolderWidget;
};


void ReosGisGuiTest::initTestCase()
{
  gisEngine = std::make_unique<ReosGisEngine>();
}

void ReosGisGuiTest::mapItemToolCreationDeletation()
{
  map = new ReosMap( gisEngine.get(), &placeHolderWidget );

  // Destruct map before item or tool
  ReosMapMarkerFilledCircle *markerP = new ReosMapMarkerFilledCircle( map );
  QPointer<ReosMapToolDrawExtent> toolExtent = new ReosMapToolDrawExtent( map );
  delete map;

  // ReosMapTools are QObject children of ReosMap, so no need to delete.
  QVERIFY( toolExtent == nullptr );
  // ReosMapItem not QObject and not children of ReosMap, still exist but can't be drawn anymore withour map... and need to be deleted
  markerP->setColor( QColor() );
  delete markerP;

  // Destruct item or tool before map
  map = new ReosMap( gisEngine.get(), &placeHolderWidget );
  markerP = new ReosMapMarkerFilledCircle( map );
  toolExtent = new ReosMapToolDrawExtent( map );

  delete markerP;
  delete toolExtent;
  delete map;
}


QTEST_MAIN( ReosGisGuiTest )
#include "reos_gisGui_test.moc"
