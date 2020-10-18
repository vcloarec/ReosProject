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

};
void ReosGeometryTest::polygonInteractions()
{
  QPolygonF polygon1;
  QPolygonF polygon2;

}


QTEST_MAIN( ReosGeometryTest )
#include "reos_geometry_test.moc"
