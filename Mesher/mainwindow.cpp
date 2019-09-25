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
    ui(new Ui::MainWindow),
    actionTinEditor(new QAction(QPixmap("://toolbar/MeshTinEditor.png"),tr("Edit TIN"),this))
{
    ui->setupUi(this);

    setDockNestingEnabled(true);


    ReosMap *map=new ReosMap(this);
    centralWidget()->setLayout(new QVBoxLayout);
    centralWidget()->layout()->addWidget(map->getMapCanvas());
    statusBar()->addPermanentWidget(map->getCursorPosition());


    ReosGisManager *gisManager=new ReosGisManager(map);
    QDockWidget* dockSIG=new QDockWidget(tr("GIS panel"));
    dockSIG->setWidget(gisManager->getWidget());
    statusBar()->addPermanentWidget(gisManager->createCRSDisplay(this));
    dockSIG->setObjectName(QStringLiteral("Dock GIS"));
    addDockWidget(Qt::LeftDockWidgetArea,dockSIG);

    QgsProviderRegistry::instance()->registerProvider(new HdTinEditorProviderMetaData());

    QMenu *fileMenu=new QMenu(tr("Files"));
    menuBar()->addMenu(fileMenu);

    ui->mainToolBar->addAction(actionTinEditor);
    actionTinEditor->setCheckable(true);
    tinEditor=new ReosTinEditorUi(gisManager,this);
    QMenu *tinMenu=new QMenu(tr("Triangulated Irregular Network DEM"));
    tinMenu->addActions(tinEditor->getActions());
    menuBar()->addMenu(tinMenu);
    connect(actionTinEditor,&QAction::triggered,this,&MainWindow::showTinEditor);
    connect(tinEditor,&ReosModule::widgetVisibility,actionTinEditor,&QAction::setChecked);
    connect(tinEditor,&ReosModule::activeUndoStack,this,&MainWindow::activeUndoStack);


    mUndoGroup=new QUndoGroup(this);
    ui->mainToolBar->addAction(mUndoGroup->createUndoAction(this));
    ui->mainToolBar->addAction(mUndoGroup->createRedoAction(this));


    //Active Tin editor
    tinEditor->showWidget();
    actionTinEditor->setChecked(true);

}

MainWindow::~MainWindow()
{
    delete ui;
}
