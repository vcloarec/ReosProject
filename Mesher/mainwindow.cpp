/***************************************************************************
                      mainwindow.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "mainwindow.h"
#include "ui_mainwindow.h"

MainWindow::MainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::MainWindow)
{
    ui->setupUi(this);
    ui->setupUi(this);


    setDockNestingEnabled(true);


    HdMap *map=new HdMap(this);
    centralWidget()->setLayout(new QVBoxLayout);
    centralWidget()->layout()->addWidget(map->getMapCanvas());
    statusBar()->addPermanentWidget(map->getCursorPosition());


    HdManagerSIG *gisManager=new HdManagerSIG(map);
    QDockWidget* dockSIG=new QDockWidget(tr("Panneau de contrôle SIG"));
    dockSIG->setWidget(gisManager->getWidget());
    statusBar()->addPermanentWidget(gisManager->createCRSDisplay(this));
    dockSIG->setObjectName(QStringLiteral("Dock GIS"));
    addDockWidget(Qt::LeftDockWidgetArea,dockSIG);


    QgsProviderRegistry::instance()->registerProvider(new HdMeshEditorProviderMetaData());


    editor=new HdTinEditorUI(gisManager,this);
    QToolBar *toolBar=addToolBar("TIN Editor");
    toolBar->addActions(editor->getActions());

}

MainWindow::~MainWindow()
{
    delete ui;
}
