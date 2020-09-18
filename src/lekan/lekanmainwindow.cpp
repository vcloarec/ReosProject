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

#include <QKeyEvent>
#include <QMenu>
#include <QStatusBar>
#include <QDockWidget>

#include "reossettings.h"
#include "reosmodule.h"
#include "reosmap.h"
#include "reosgisengine.h"
#include "reosgislayerswidget.h"

LekanMainWindow::LekanMainWindow( QWidget *parent ) :
  ReosMainWindow( parent ),
  mGisEngine( new ReosGisEngine( rootModule() ) ),
  mMap( new ReosMap( mGisEngine, this ) )
{


  //****************************************************************

//  map = new ReosMap( rootReosModule );
//  centralWidget()->setLayout( new QVBoxLayout );
//  centralWidget()->layout()->addWidget( map->getMapCanvas() );


//  gisManager = new ReosGisManager( map, rootReosModule );
//  dockSIG = new QDockWidget( tr( "Panneau de contrôle SIG" ) );
//  dockSIG->setWidget( gisManager->getWidget() );
//  statusBar()->addPermanentWidget( gisManager->createCRSDisplay( this ) );
//  dockSIG->setObjectName( QStringLiteral( "Dock GIS" ) );

//  demManager = new HdDEMManager( gisManager, rootReosModule );
//  dockDEM = new QDockWidget( tr( "Panneau de contrôle MNT" ) );
//  dockDEM->setWidget( demManager->getWidget() );
//  dockDEM->setObjectName( QStringLiteral( "Dock DEM" ) );

//  watershedManager = new HlgWatershedManager( map, gisManager, demManager, rootReosModule );
//  dockWatershed = new QDockWidget( tr( "Panneau de contrôle Bassin versant" ) );
//  dockWatershed->setWidget( watershedManager->getWidget() );
//  dockWatershed->setObjectName( QStringLiteral( "Dock watershed" ) );


//  rainfallManager = new HlgRainfallManager( map->getMapCanvas(), watershedManager );
//  runoffManager = new HlgRunoffManager( map->getMapCanvas(), watershedManager, rainfallManager );

//  messageBox = new ReosMessageBox( this );
//  dockMessageBox = new QDockWidget( tr( "Message" ) );
//  dockMessageBox->setWidget( messageBox );
//  dockMessageBox->setObjectName( QStringLiteral( "Dock message" ) );

//  //****************************************************************

  //  menuBar()->addMenu( watershedManager->getMenu() );
  //  addToolBar( watershedManager->getToolBar() );

  //  addToolBar( rainfallManager->getToolBar() );
  //toolBarRainfallRunoffModel = addToolBar( tr( "Modèle pluie/débit" ) );
  //    toolBarRainsFaillRunoffModel->addActions(rainfallManager->getToolBar()->actions());
  //    toolBarRainsFaillRunoffModel->addSeparator();
  // toolBarRainfallRunoffModel->addActions( runoffManager->getToolBar()->actions() );




//  groupActionInterrogation->addAction( actionNewVersionAvailable );
//  groupActionInterrogation->addAction( actionDocumentation );
//  menuInterrogation = menuBar()->addMenu( tr( "?" ) );
//  menuInterrogation->addActions( groupActionInterrogation->actions() );

  //reosDocumentation = new ReosDocumentation( lekanVersion, this );
  //****************************************************************

//  connect( actionNewProject, &QAction::triggered, this, &LekanMainWindow::newProject );
//  connect( actionOpenFile, &QAction::triggered, this, &LekanMainWindow::open );
//  connect( actionSaveFileAs, &QAction::triggered, this, &LekanMainWindow::saveProjectAs );
//  connect( actionSaveFile, &QAction::triggered, this, &LekanMainWindow::saveProject );

//  connect( actionLanguageSelection, &QAction::triggered, this, &LekanMainWindow::languageSelection );
//  connect( actionAPropos, &QAction::triggered, this, &LekanMainWindow::aPropos );
//  connect( actionNewVersionAvailable, &QAction::triggered, this, &LekanMainWindow::newVersionAvailable );
  //connect( actionDocumentation, &QAction::triggered, reosDocumentation, &ReosDocumentation::call );

  //connect( rootReosModule, &ReosModule::messageEmited, messageBox, &ReosMessageBox::receiveMessage );

  init();

  statusBar()->addPermanentWidget( new ReosMapCursorPosition( mMap, this ) );
  centralWidget()->layout()->addWidget( mMap->mapCanvas() );
  mGisDock = new QDockWidget( tr( "GIS Layers" ) );
  mGisDock->setWidget( new ReosGisLayersWidget( mGisEngine, mMap, this ) );
  addDockWidget( Qt::LeftDockWidgetArea, mGisDock );
}


QString LekanMainWindow::projectFileFilter() const
{
  return QStringLiteral( "Lekan file (*.lkn)" );
}

QList<QMenu *> LekanMainWindow::specificMenus()
{
  QList<QMenu *> menusList;

  menuRainFallRunoffModel = new QMenu( tr( "Modèle pluie/débit" ), this );

  //menuRainFallRunoffModel->addActions( rainfallManager->getMenu()->actions() );
  menuRainFallRunoffModel->addSeparator();
  //menuRainFallRunoffModel->addActions( runoffManager->getMenu()->actions() );
  menusList << menuRainFallRunoffModel;

  return menusList;
}

//bool LekanMainWindow::open()
//{
//  ReosSettings settings;
//  QString path = settings.value( QStringLiteral( "/Path/Project" ) ).toString();

//  QString fileName = QFileDialog::getOpenFileName( this, tr( "Ouvrir un projet" ), path, "*.lkn" );

//  if ( fileName == "" )
//    return false;

//  return openProject( fileName );
//}

//bool LekanMainWindow::openProject( QString fileName )
//{
//  ReosSettings settings;
//  QFileInfo fileInfo( fileName );

//  QFile file( fileName );
//  QDataStream stream( &file );

//  if ( !file.open( QIODevice::ReadOnly ) )
//  {
//    QMessageBox::warning( this, tr( "Erreur de fichier" ), tr( "Impossible d'ouvrir le fichier" ) );
//    return openBackFile( fileName );
//  }

//  if ( fileName.right( 3 ) == "lkn" )
//    fileNameCurrentProject = fileName;
//  else
//  {
//    fileNameCurrentProject = "";
//  }

//  settings.setValue( QStringLiteral( "/Path/Project" ), fileInfo.path() );



//  //*************************************************
//  QString gisFileName = getGISFileName();
//  QFileInfo gisFileInfo( gisFileName );
//  if ( gisFileInfo.exists() )
//    gisManager->setGISFileName( gisFileName );
//  //*************************************************



//  //*************************************************
//  QByteArray byteArrayLekan;
//  stream >> byteArrayLekan;
//  bool succesOpen = decode( byteArrayLekan );
//  //*************************************************

//  if ( !succesOpen )
//  {
//    QMessageBox::critical( this, tr( "Ouverture" ), tr( "Echec de l'ouverture du fichier" ) );
//    return openBackFile( fileName );
//  }

//  return true;
//}

//bool LekanMainWindow::openBackFile( QString filename )
//{
//  filename.append( ".bak" );
//  QFileInfo fileInfo( filename );

//  if ( !fileInfo.exists() )
//  {
//    return false;
//  }

//  if ( QMessageBox::information( this, tr( "Ouverture" ), tr( "Un fichier de sauvegarde a été trouvé, voulez-vous essayer de l'ouvrir ?" ),
//                                 QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes ) == QMessageBox::Yes )
//  {
//    return openProject( filename );
//  }
//  else
//  {
//    return false;
//  }
//}



//void LekanMainWindow::addDock()
//{
//  addDockWidget( Qt::LeftDockWidgetArea, dockSIG );
//  addDockWidget( Qt::LeftDockWidgetArea, dockDEM );
//  addDockWidget( Qt::RightDockWidgetArea, dockWatershed );
//  addDockWidget( Qt::RightDockWidgetArea, dockMessageBox );
//}

//QString LekanMainWindow::getGISFileName()
//{
//  QString gisFileName;

//  if ( fileNameCurrentProject.right( 4 ) == ".lkn" )
//    gisFileName = fileNameCurrentProject.left( fileNameCurrentProject.count() - 4 );
//  else
//    gisFileName = fileNameCurrentProject;

//  gisFileName.append( ".qgs" );

//  return gisFileName;
//}

//bool LekanMainWindow::saveProject()
//{
//  if ( fileNameCurrentProject == "" )
//    return saveProjectAs();

//  QFile file( fileNameCurrentProject );
//  QString backupFile = fileNameCurrentProject;
//  backupFile.append( ".bak" );
//  file.copy( backupFile );
//  QDataStream stream( &file );

//  if ( !file.open( QIODevice::WriteOnly ) )
//  {
//    QMessageBox::warning( this, tr( "Erreur de fichier" ), tr( "Impossible d'ouvrir le fichier" ) );
//    return saveProjectAs();
//  }

//  //*************************************************
//  QString gisFileName = getGISFileName();
//  gisManager->setGISFileName( gisFileName );
//  //*************************************************


//  //*************************************************
//  try
//  {
//    stream << encode();
//  }
//  catch ( ... )
//  {
//    QMessageBox::critical( this, tr( "Erreur critique" ), tr( "Erreur lors de l'enregistrement" ) );
//    file.close();
//    file.remove();
//    QFile backup( backupFile );
//    backup.copy( fileNameCurrentProject );
//    return false;

//  }
//  //*************************************************

//  QFile::remove( file.fileName().append( ".bak" ) );
//  file.close();

//  return true;
//}

//bool LekanMainWindow::saveProjectAs()
//{
//  ReosSettings settings;
//  QString path = settings.value( QStringLiteral( "/Path/Project" ) ).toString();

//  fileNameCurrentProject = QFileDialog::getSaveFileName( this, tr( "Enregistrer le projet sous ..." ), path, "*.lkn" );

//  if ( fileNameCurrentProject == "" )
//    return false;

//  QFileInfo fileInfo( fileNameCurrentProject );
//  settings.setValue( QStringLiteral( "/Path/Project" ), fileInfo.path() );

//  return saveProject();

//}

//void LekanMainWindow::newProject()
//{
//  int returnButton = QMessageBox::warning( this, tr( "Nouveau projet" ), tr( "Sauvegarder le projet actuel ?" ), QMessageBox::Save | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Cancel );

//  if ( returnButton == QMessageBox::Cancel )
//    return;

//  if ( returnButton == QMessageBox::Ok )
//    saveProject();

//  fileNameCurrentProject.clear();
//  clearProject();
//}

//QByteArray LekanMainWindow::encode() const
//{
//  std::vector<int> list;

//  ReosEncodedElement encodedLekan( QStringLiteral( "Lekan" ) );
//  encodedLekan.addData( QStringLiteral( "SIG Manager" ), gisManager->encode() );
//  encodedLekan.addData( QStringLiteral( "Dem Manager" ), demManager->encode() );
//  encodedLekan.addData( QStringLiteral( "Watershed Manager" ), watershedManager->encode() );
//  encodedLekan.addData( QStringLiteral( "Map" ), map->encode() );
//  encodedLekan.addData( QStringLiteral( "Rainfall manager" ), rainfallManager->encode() );
//  encodedLekan.addData( QStringLiteral( "Runoff manager" ), runoffManager->encode() );
//  return encodedLekan.encode();
//}

//bool LekanMainWindow::decode( const QByteArray &byteArray )
//{
//  clearProject();

//  ReosEncodedElement codedLekan( byteArray );
//  if ( codedLekan.selfDescription() != QStringLiteral( "Lekan" ) )
//    return false;

//  QByteArray ba;
//  if ( codedLekan.getData( QStringLiteral( "SIG Manager" ), ba ) )
//  {
//    gisManager->decode( ba );
//  }
//  else
//  {
//    gisManager->loadGISProject();
//  }

//  if ( codedLekan.getData( QStringLiteral( "Dem Manager" ), ba ) )
//  {
//    demManager->decode( ba );
//  }
//  ba.clear();

//  if ( codedLekan.getData( QStringLiteral( "Watershed Manager" ), ba ) )
//  {
//    watershedManager->decode( ba );
//  }

//  ba.clear();

//  if ( codedLekan.getData( QStringLiteral( "Map" ), ba ) )
//  {
//    map->decode( ba );
//  }

//  if ( codedLekan.getData( QStringLiteral( "Rainfall manager" ), ba ) )
//  {
//    rainfallManager->decode( ba );
//  }

//  if ( codedLekan.getData( QStringLiteral( "Runoff manager" ), ba ) )
//  {
//    runoffManager->decode( ba );
//  }


//  return true;
//}

//void LekanMainWindow::languageSelection()
//{

//  ReosSettings settings;
//  DialogChoixLangue dial( settings.value( QStringLiteral( "Locale" ) ).toLocale() );

//  if ( dial.exec() )
//  {
//    settings.setValue( QStringLiteral( "Locale" ), dial.getChoixLangue() );
//  }

//}

//void LekanMainWindow::aPropos()
//{
//  APropos *apropos = new APropos( this );

//  apropos->setBan( QPixmap( "://titre_Lekan.png" ) );
//  apropos->setVersion( lekanVersion.softwareNameWithVersion() );
//  apropos->setAdresseWeb( webSite );
//  apropos->addBibliotheque( "Qt", "5.11", "www.qt.io/" );
//  apropos->addBibliotheque( "QGis", "3.4.12", "www.qgis.org/" );
//  apropos->addBibliotheque( "GDAL", "2.4", "www.gdal.org/" );
//  apropos->addBibliotheque( "Qwt", "6.1.4", "qwt.sourceforge.io" );

//  QString licenceTxt;

//  licenceTxt.append( "Overview: \n" );
//  licenceTxt.append( "1. Lekan\n" );
//  licenceTxt.append( "2. ECW Raster Plugin for GDAL\n" );
//  licenceTxt.append( "3. MrSID Raster Plugin for GDAL\n\n\n" );

//  licenceTxt.append( "1. Lekan\n" );
//  QFile licenceFileLekan( "../LICENSE_LEKAN.txt" );
//  QTextStream streamLekan( &licenceFileLekan );
//  licenceFileLekan.open( QIODevice::ReadOnly );
//  licenceTxt.append( streamLekan.readAll() );

//  licenceTxt.append( "\n\n\n\n****************************\n\n\n\n" );

//  licenceTxt.append( "2. ECW Raster Plugin for GDAL\n" );
//  QFile licenceFileECW( "../ECWLicense.txt" );
//  QTextStream streamECW( &licenceFileECW );
//  licenceFileECW.open( QIODevice::ReadOnly );
//  licenceTxt.append( streamECW.readAll() );

//  licenceTxt.append( "\n\n\n\n****************************\n\n\n\n" );

//  licenceTxt.append( "3. MrSID Raster Plugin for GDAL\n" );
//  QFile licenceFileMrSID( "../MRSIDLicense.txt" );
//  QTextStream streamMrSID( &licenceFileMrSID );
//  licenceFileMrSID.open( QIODevice::ReadOnly );
//  licenceTxt.append( streamMrSID.readAll() );

//  apropos->setLicenceText( licenceTxt );

//  apropos->exec();
//}

//void LekanMainWindow::clearProject()
//{
//  runoffManager->clear();
//  rainfallManager->clear();
//  watershedManager->clear();
//  gisManager->clear();
//  demManager->clear();
//  messageBox->clean();
//}

