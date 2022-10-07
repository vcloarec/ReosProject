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
#include "reoshecrassimulation.h"
#include "reoshydraulicstructure2d.h"
#include "reosgisengine.h"
#include "reoswatershedmodule.h"

class ReosHecrasTesting : public QObject
{
    Q_OBJECT

private slots:
    void availableVersion();
    void createControllerInstance();
    void exploreProject();

    void importStructure();

};

void ReosHecrasTesting::availableVersion()
{
    QStringList versions = ReosHecrasController::availableVersion();
    QVERIFY(!versions.isEmpty());
}
void ReosHecrasTesting::createControllerInstance()
{
    QStringList versions = ReosHecrasController::availableVersion();
    ReosHecrasController controller(versions.last());

    QVERIFY(controller.isValid());
}

void ReosHecrasTesting::exploreProject()
{
    QString path("C:\\dev\\sources\\ReosProject\\TestsHecRas\\testData\\simple\\simple.prj");

    QStringList versions = ReosHecrasController::availableVersion();
    ReosHecrasController controller(versions.last());

    QVERIFY(controller.openHecrasProject(path));

    QStringList plans= controller.planNames();

    QCOMPARE(plans.count(), 2);
    QCOMPARE(plans.at(0), QStringLiteral("plan_simple_1"));
    QCOMPARE(plans.at(1), QStringLiteral("plan_simple_2"));

    QStringList flow2DAreasNames = controller.flowAreas2D();
    QCOMPARE(flow2DAreasNames.count(), 1);
    QCOMPARE(flow2DAreasNames.at(0), QStringLiteral("Perimeter 1     "));

    QPolygonF domain = controller.flow2DAreasDomain(flow2DAreasNames.first());
    QVERIFY(domain.count(), 4);
}

void ReosHecrasTesting::importStructure()
{
    QString path("C:\\dev\\sources\\ReosProject\\TestsHecRas\\testData\\simple\\simple.prj");

    QStringList versions = ReosHecrasController::availableVersion();
    ReosHecRasStructureImporter importer(versions.last(), path);
    
    ReosModule rootModule;
    ReosGisEngine *gisEngine=new ReosGisEngine(&rootModule);
    ReosWatershedModule* watershedModule = new ReosWatershedModule(&rootModule, gisEngine);
    ReosHydraulicNetwork *network=new ReosHydraulicNetwork(&rootModule,gisEngine,watershedModule);

    ReosHydraulicStructure2D* structure=ReosHydraulicStructure2D::create(&importer, network->context());
    QVERIFY(structure!=nullptr);
    QVERIFY(structure->domain().count() > 0);
}

QTEST_MAIN(ReosHecrasTesting)
#include "test_hecras.moc"