/***************************************************************************
                      test_hecras.cpp
                     --------------------------------------
Date                 : 03-10-2022
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

#include "reoshecrascontroller.h"

class ReosHecrasTesting : public QObject
{
    Q_OBJECT
private slots:
    void createControllerInstance();

};


void ReosHecrasTesting::createControllerInstance()
{
    ReosHecrasController controller;

    QVERIFY(controller.isValid());
}

QTEST_MAIN(ReosHecrasTesting)
#include "test_hecras.moc"