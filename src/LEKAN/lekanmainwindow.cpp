/***************************************************************************
                      lekanmainwindow.cpp
                     --------------------------------------
Date                 : 18-11-2018
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec@gmail.com projetreos@gmail.com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "lekanmainwindow.h"
#include "ui_lekanmainwindow.h"

LekanMainWindow::LekanMainWindow(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::LekanMainWindow),rootReosModule(new ReosModule(this)),
    groupActionFile(new QActionGroup(this)),
    actionNewProject(new QAction(QPixmap("://toolbar/NouveauDoc.png"),tr("Nouveau projet"),this)),
    actionOpenFile(new QAction(QPixmap("://toolbar/ouvrirDoc.png"),tr("Ouvrir fichier"),this)),
    actionSaveFile(new QAction(QPixmap("://toolbar/enregister.png"),tr("Enregistrer"),this)),
    actionSaveFileAs(new QAction(QPixmap("://toolbar/enregisterSous.png"),tr("Enregister sous ..."),this)),
    groupActionEdit(new QActionGroup(this)),
    groupActionOption(new QActionGroup(this)),
    groupActionInterrogation(new QActionGroup(this)),
    actionLanguageSelection(new QAction(tr("Sélection de la langue"),this)),
    actionAPropos(new QAction(tr("A propos ..."),this)),
    actionNewVersionAvailable(new QAction(tr("Vérifier si nouvelle version"),this)),
    actionDocumentation(new QAction(tr("Documentation"),this)),
    undoStack(new QUndoStack(this))
{
    ui->setupUi(this);
    setDockNestingEnabled(true);

    ReosSettings settings;


    //****************************************************************

    map=new ReosMap(rootReosModule);
    centralWidget()->setLayout(new QVBoxLayout);
    centralWidget()->layout()->addWidget(map->getMapCanvas());
    statusBar()->addPermanentWidget(map->getCursorPosition());


    gisManager=new ReosGisManager(map,rootReosModule);
    dockSIG=new QDockWidget(tr("Panneau de contrôle SIG"));
    dockSIG->setWidget(gisManager->getWidget());
    statusBar()->addPermanentWidget(gisManager->createCRSDisplay(this));
    dockSIG->setObjectName(QStringLiteral("Dock GIS"));

    demManager=new HdDEMManager(gisManager,rootReosModule);
    dockDEM=new QDockWidget(tr("Panneau de contrôle MNT"));
    dockDEM->setWidget(demManager->getWidget());
    dockDEM->setObjectName(QStringLiteral("Dock DEM"));

    watershedManager=new HlgWatershedManager(map,gisManager,demManager,rootReosModule);
    dockWatershed=new QDockWidget(tr("Panneau de contrôle Bassin versant"));
    dockWatershed->setWidget(watershedManager->getWidget());
    dockWatershed->setObjectName(QStringLiteral("Dock watershed"));


    rainfallManager=new HlgRainfallManager(map->getMapCanvas(),watershedManager);
    runoffManager=new HlgRunoffManager(map->getMapCanvas(),watershedManager,rainfallManager);

    messageBox=new ReosMessageBox(this);
    dockMessageBox=new QDockWidget(tr("Message"));
    dockMessageBox->setWidget(messageBox);
    dockMessageBox->setObjectName(QStringLiteral("Dock message"));

    //****************************************************************

    groupActionFile->addAction(actionNewProject);
    groupActionFile->addAction(actionOpenFile);
    groupActionFile->addAction(actionSaveFile);
    groupActionFile->addAction(actionSaveFileAs);

    toolBarFile=addToolBar(tr("Fichier"));
    toolBarFile->addActions(groupActionFile->actions());
    menuFile=menuBar()->addMenu(tr("Fichier"));
    menuFile->addActions(groupActionFile->actions());



    QAction *actionUndo=undoStack->createUndoAction(this);
    actionUndo->setIcon(QPixmap("://toolbar/undo.png"));

    groupActionEdit->addAction(actionUndo);
    QAction *actionRedo=undoStack->createRedoAction(this);
    actionRedo->setIcon(QPixmap("://toolbar/redo.png"));
    groupActionEdit->addAction(actionRedo);

    toolBarEdit=addToolBar(tr("Edition"));
    toolBarEdit->addActions(groupActionEdit->actions());
    menuEdit=menuBar()->addMenu(tr("Edition"));
    menuEdit->addActions(groupActionEdit->actions());

    menuBar()->addMenu(watershedManager->getMenu());
    addToolBar(watershedManager->getToolBar());

    addToolBar(rainfallManager->getToolBar());
    toolBarRainfallRunoffModel=addToolBar(tr("Modèle pluie/débit"));
//    toolBarRainsFaillRunoffModel->addActions(rainfallManager->getToolBar()->actions());
//    toolBarRainsFaillRunoffModel->addSeparator();
    toolBarRainfallRunoffModel->addActions(runoffManager->getToolBar()->actions());

    menuRainFallRunoffModel=menuBar()->addMenu(tr("Modèle pluie/débit"));
    menuRainFallRunoffModel->addActions(rainfallManager->getMenu()->actions());
    menuRainFallRunoffModel->addSeparator();
    menuRainFallRunoffModel->addActions(runoffManager->getMenu()->actions());


    groupActionOption->addAction(actionLanguageSelection);
    menuOption=menuBar()->addMenu(tr("Options"));
    menuOption->addActions(groupActionOption->actions());

    groupActionInterrogation->addAction(actionAPropos);
    groupActionInterrogation->addAction(actionNewVersionAvailable);
    groupActionInterrogation->addAction(actionDocumentation);
    menuInterrogation=menuBar()->addMenu(tr("?"));
    menuInterrogation->addActions(groupActionInterrogation->actions());

    reosDocumentation=new ReosDocumentation(lekanVersion,this);
    //****************************************************************


    connect(rootReosModule,&ReosModule::newCommandToUndoStack,this,&LekanMainWindow::newUndoCommand);

    connect(actionNewProject,&QAction::triggered,this,&LekanMainWindow::newProject);
    connect(actionOpenFile,&QAction::triggered,this,&LekanMainWindow::open);
    connect(actionSaveFileAs,&QAction::triggered,this,&LekanMainWindow::saveProjectAs);
    connect(actionSaveFile,&QAction::triggered,this,&LekanMainWindow::saveProject);

    connect(actionLanguageSelection,&QAction::triggered,this,&LekanMainWindow::languageSelection);
    connect(actionAPropos,&QAction::triggered,this,&LekanMainWindow::aPropos);
    connect(actionNewVersionAvailable,&QAction::triggered,this,&LekanMainWindow::newVersionAvailable);
    connect(actionDocumentation,&QAction::triggered,reosDocumentation,&ReosDocumentation::call);

    connect(rootReosModule,&ReosModule::messageEmited,messageBox,&ReosMessageBox::receiveMessage);

}

LekanMainWindow::~LekanMainWindow()
{
    delete ui;
}

bool LekanMainWindow::open()
{
    ReosSettings settings;
    QString path=settings.value(QStringLiteral("/Path/Project")).toString();

    QString fileName=QFileDialog::getOpenFileName(this,tr("Ouvrir un projet"),path,"*.lkn");

    if (fileName=="")
        return false;

    return openProject(fileName);
}

bool LekanMainWindow::openProject(QString fileName)
{
    ReosSettings settings;
    QFileInfo fileInfo(fileName);

    QFile file(fileName);
    QDataStream stream(&file);

    if (!file.open(QIODevice::ReadOnly))
    {
        QMessageBox::warning(this,tr("Erreur de fichier"),tr("Impossible d'ouvrir le fichier"));
        return openBackFile(fileName);
    }

    if (fileName.right(3)=="lkn")
        fileNameCurrentProject=fileName;
    else {
        fileNameCurrentProject="";
    }

    settings.setValue(QStringLiteral("/Path/Project"),fileInfo.path());



    //*************************************************
    QString gisFileName=getGISFileName();
    QFileInfo gisFileInfo(gisFileName);
    if (gisFileInfo.exists())
        gisManager->setGISFileName(gisFileName);
    //*************************************************



    //*************************************************
    QByteArray byteArrayLekan;
    stream>>byteArrayLekan;
    bool succesOpen=decode(byteArrayLekan);
    //*************************************************

    if (!succesOpen)
    {
        QMessageBox::critical(this,tr("Ouverture"),tr("Echec de l'ouverture du fichier"));
        return openBackFile(fileName);
    }

    return true;
}

bool LekanMainWindow::openBackFile(QString filename)
{
    filename.append(".bak");
    QFileInfo fileInfo(filename);

    if (!fileInfo.exists())
    {
        return false;
    }

    if(QMessageBox::information(this,tr("Ouverture"),tr("Un fichier de sauvegarde a été trouvé, voulez-vous essayer de l'ouvrir ?"),
                                QMessageBox::Yes|QMessageBox::No,QMessageBox::Yes)==QMessageBox::Yes)
    {
        return openProject(filename);
    }
    else {
        return false;
    }
}



void LekanMainWindow::addDock()
{
    addDockWidget(Qt::LeftDockWidgetArea,dockSIG);
    addDockWidget(Qt::LeftDockWidgetArea,dockDEM);
    addDockWidget(Qt::RightDockWidgetArea,dockWatershed);
    addDockWidget(Qt::RightDockWidgetArea,dockMessageBox);
}

QString LekanMainWindow::getGISFileName()
{
    QString gisFileName;

    if (fileNameCurrentProject.right(4)==".lkn")
        gisFileName=fileNameCurrentProject.left(fileNameCurrentProject.count()-4);
    else
        gisFileName=fileNameCurrentProject;

    gisFileName.append(".qgs");

    return gisFileName;
}

bool LekanMainWindow::saveProject()
{
    if (fileNameCurrentProject=="")
        return saveProjectAs();

    QFile file(fileNameCurrentProject);
    QString backupFile=fileNameCurrentProject;
    backupFile.append(".bak");
    file.copy(backupFile);
    QDataStream stream(&file);

    if (!file.open(QIODevice::WriteOnly))
    {
        QMessageBox::warning(this,tr("Erreur de fichier"),tr("Impossible d'ouvrir le fichier"));
        return saveProjectAs();
    }

    //*************************************************
    QString gisFileName=getGISFileName();
    gisManager->setGISFileName(gisFileName);
    //*************************************************


    //*************************************************
    try {
        stream<<encode();
    } catch (...) {
        QMessageBox::critical(this,tr("Erreur critique"),tr("Erreur lors de l'enregistrement"));
        file.close();
        file.remove();
        QFile backup(backupFile);
        backup.copy(fileNameCurrentProject);
        return false;

    }
    //*************************************************

    QFile::remove(file.fileName().append(".bak"));
    file.close();

    return true;
}

bool LekanMainWindow::saveProjectAs()
{
    ReosSettings settings;
    QString path=settings.value(QStringLiteral("/Path/Project")).toString();

    fileNameCurrentProject=QFileDialog::getSaveFileName(this,tr("Enregistrer le projet sous ..."),path,"*.lkn");

    if (fileNameCurrentProject=="")
        return false;

    QFileInfo fileInfo(fileNameCurrentProject);
    settings.setValue(QStringLiteral("/Path/Project"),fileInfo.path());

    return saveProject();

}

void LekanMainWindow::newProject()
{
    int returnButton=QMessageBox::warning(this,tr("Nouveau projet"),tr("Sauvegarder le projet actuel ?"),QMessageBox::Save|QMessageBox::No|QMessageBox::Cancel,QMessageBox::Cancel);

    if (returnButton==QMessageBox::Cancel)
        return;

    if (returnButton==QMessageBox::Ok)
        saveProject();

    fileNameCurrentProject.clear();
    clearProject();
}

QByteArray LekanMainWindow::encode() const
{
    std::vector<int> list;

    ReosEncodedElement encodedLekan(QStringLiteral("Lekan"));
    encodedLekan.addData(QStringLiteral("SIG Manager"),gisManager->encode());
    encodedLekan.addData(QStringLiteral("Dem Manager"),demManager->encode());
    encodedLekan.addData(QStringLiteral("Watershed Manager"),watershedManager->encode());
    encodedLekan.addData(QStringLiteral("Map"),map->encode());
    encodedLekan.addData(QStringLiteral("Rainfall manager"),rainfallManager->encode());
    encodedLekan.addData(QStringLiteral("Runoff manager"),runoffManager->encode());
    return encodedLekan.encode();
}

bool LekanMainWindow::decode(const QByteArray &byteArray)
{
    clearProject();

    ReosEncodedElement codedLekan(byteArray);
    if (codedLekan.selfDescription()!=QStringLiteral("Lekan"))
        return false;

    QByteArray ba;
    if (codedLekan.getData(QStringLiteral("SIG Manager"),ba))
    {
        gisManager->decode(ba);
    }
    else {
        gisManager->loadGISProject();
    }

    if (codedLekan.getData(QStringLiteral("Dem Manager"),ba))
    {
        demManager->decode(ba);
    }
    ba.clear();

    if (codedLekan.getData(QStringLiteral("Watershed Manager"),ba))
    {
        watershedManager->decode(ba);
    }

    ba.clear();

    if (codedLekan.getData(QStringLiteral("Map"),ba))
    {
        map->decode(ba);
    }

    if (codedLekan.getData(QStringLiteral("Rainfall manager"),ba))
    {
        rainfallManager->decode(ba);
    }

    if (codedLekan.getData(QStringLiteral("Runoff manager"),ba))
    {
        runoffManager->decode(ba);
    }


    return true;
}

void LekanMainWindow::languageSelection()
{

    ReosSettings settings;
    DialogChoixLangue dial(settings.value(QStringLiteral("Locale")).toLocale());

    if (dial.exec())
    {
        settings.setValue(QStringLiteral("Locale"),dial.getChoixLangue());
    }

}

void LekanMainWindow::aPropos()
{
    APropos *apropos=new APropos(this);

    apropos->setBan(QPixmap("://titre_Lekan.png"));
    apropos->setVersion(lekanVersion.softwareNameWithVersion());
    apropos->setAdresseWeb(webSite);
    apropos->addBibliotheque("Qt","5.11","www.qt.io/");
    apropos->addBibliotheque("QGis","3.4.12","www.qgis.org/");
    apropos->addBibliotheque("GDAL","2.4","www.gdal.org/");
     apropos->addBibliotheque("Qwt","6.1.4","qwt.sourceforge.io");

    QString licenceTxt;

    licenceTxt.append("Overview: \n");
    licenceTxt.append("1. Lekan\n");
    licenceTxt.append("2. ECW Raster Plugin for GDAL\n");
    licenceTxt.append("3. MrSID Raster Plugin for GDAL\n\n\n");

    licenceTxt.append("1. Lekan\n");
    QFile licenceFileLekan("../LICENSE_LEKAN.txt");
    QTextStream streamLekan(&licenceFileLekan);
    licenceFileLekan.open(QIODevice::ReadOnly);
    licenceTxt.append(streamLekan.readAll());

    licenceTxt.append("\n\n\n\n****************************\n\n\n\n");

    licenceTxt.append("2. ECW Raster Plugin for GDAL\n");
    QFile licenceFileECW("../ECWLicense.txt");
    QTextStream streamECW(&licenceFileECW);
    licenceFileECW.open(QIODevice::ReadOnly);
    licenceTxt.append(streamECW.readAll());

    licenceTxt.append("\n\n\n\n****************************\n\n\n\n");

    licenceTxt.append("3. MrSID Raster Plugin for GDAL\n");
    QFile licenceFileMrSID("../MRSIDLicense.txt");
    QTextStream streamMrSID(&licenceFileMrSID);
    licenceFileMrSID.open(QIODevice::ReadOnly);
    licenceTxt.append(streamMrSID.readAll());

    apropos->setLicenceText(licenceTxt);

    apropos->exec();
}

void LekanMainWindow::clearProject()
{
    runoffManager->clear();
    rainfallManager->clear();
    watershedManager->clear();
    gisManager->clear();
    demManager->clear();
    messageBox->clean();
}

void LekanMainWindow::closeEvent(QCloseEvent *event)
{
    ReosSettings settings;
    settings.setValue(QStringLiteral("Windows/MainWindow/geometry"),saveGeometry());
    settings.setValue(QStringLiteral("Windows/MainWindow/state"),saveState());
    QMainWindow::closeEvent(event);
}

void LekanMainWindow::keyPressEvent(QKeyEvent *event)
{
    if (event->matches(QKeySequence::Undo))
    {
        undoStack->undo();
        event->accept();
    }

    if (event->matches((QKeySequence::Redo)))
    {
        undoStack->redo();
        event->accept();
    }

    QWidget::keyPressEvent(event);
}

void LekanMainWindow::newUndoCommand(QUndoCommand *command)
{
    undoStack->push(command);
}
