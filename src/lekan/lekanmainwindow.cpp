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
#include <QFileInfo>
#include <QDir>

#include "reossettings.h"
#include "reosmodule.h"
#include "reosmap.h"
#include "reosgisengine.h"
#include "reosgislayerswidget.h"
#include "reosmaptool.h"
#include "reoswatershedmodule.h"
#include "reosdelineatingwatershedwidget.h"
#include "reoswatershedwidget.h"
#include "reosrainfallmanager.h"
#include "reosrainfallregistery.h"
#include "reosrunoffmanager.h"
#include "reosrunoffmodel.h"


LekanMainWindow::LekanMainWindow( QWidget *parent ) :
  ReosMainWindow( parent ),
  mGisEngine( new ReosGisEngine( rootModule() ) ),
  mMap( new ReosMap( mGisEngine, this ) )
{
  init();

  ReosRainfallRegistery::instantiate( rootModule() );
  ReosRunoffModelRegistery::instantiate( rootModule() );

  ReosPlotItemFactories::instantiate( rootModule() );
  ReosFormWidgetFactories::instantiate( rootModule() );

  mRainFallManagerWidget = new ReosRainfallManager( ReosRainfallRegistery::instance()->rainfallModel(), this );
  mActionRainfallManager->setCheckable( true );
  mRainFallManagerWidget->setAction( mActionRainfallManager );
  mRainFallManagerWidget->loadDataFile();

  mRunoffManagerWidget = new ReosRunoffManager( ReosRunoffModelRegistery::instance()->model(), this );
  mActionRunoffManager->setCheckable( true );
  mRunoffManagerWidget->setAction( mActionRunoffManager );
  mRunoffManagerWidget->loadDataFile();

  statusBar()->addPermanentWidget( new ReosMapCursorPosition( mMap, this ) );
  centralWidget()->layout()->addWidget( mMap->mapCanvas() );

  mGisDock = new QDockWidget( tr( "GIS Layers" ) );
  mGisDock->setWidget( new ReosGisLayersWidget( mGisEngine, mMap, this ) );
  addDockWidget( Qt::LeftDockWidgetArea, mGisDock );

  mDockWatershed = new QDockWidget( tr( "Watershed" ), this );
  mWatershedModule = new ReosWatershedModule( rootModule(), mGisEngine );
  ReosWatershedWidget *watersehdWidget = new  ReosWatershedWidget( mMap, mWatershedModule, this );
  mDockWatershed->setWidget( watersehdWidget );
  addDockWidget( Qt::RightDockWidgetArea, mDockWatershed );

  mMap->setDefaultMapTool();
}

bool LekanMainWindow::openProject()
{
  QString filePath = currentProjectFilePath();
  QString path = currentProjectPath();
  QString baseName = currentProjectBaseName();

  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  QDataStream stream( &file );
  QByteArray byteArray;
  stream >> byteArray;

  ReosEncodedElement lekanProject( byteArray );
  if ( lekanProject.description() != QStringLiteral( "Lekan-project" ) )
    return false;

  QByteArray gisEngineData;
  if ( !lekanProject.getData( QStringLiteral( "GIS-engine" ), gisEngineData ) )
    return false;
  ReosEncodedElement encodedGisEngine( gisEngineData );
  if ( !mGisEngine->decode( lekanProject.getEncodedData( QStringLiteral( "GIS-engine" ) ), path, baseName ) )
    return false;

  mWatershedModule->decode( lekanProject.getEncodedData( QStringLiteral( "watershed-module" ) ) );

  return true;
}

bool LekanMainWindow::saveProject()
{
  QString filePath = currentProjectFilePath();
  QString path = currentProjectPath();
  QString baseName = currentProjectBaseName();

  ReosEncodedElement lekanProject( QStringLiteral( "Lekan-project" ) );

  ReosEncodedElement encodedGisEngine = mGisEngine->encode( path, baseName );
  lekanProject.addEncodedData( QStringLiteral( "GIS-engine" ), encodedGisEngine );
  lekanProject.addEncodedData( QStringLiteral( "watershed-module" ), mWatershedModule->encode() );

  QFileInfo fileInfo( filePath );
  if ( fileInfo.suffix().isEmpty() )
    filePath.append( QStringLiteral( ".lkn" ) );

  QFile file( filePath );
  if ( !file.open( QIODevice::WriteOnly ) )
    return false;
  QDataStream stream( &file );
  stream << lekanProject.bytes();
  return true;
}


QString LekanMainWindow::projectFileFilter() const
{
  return QStringLiteral( "Lekan file (*.lkn)" );
}

QFileInfo LekanMainWindow::gisFileInfo() const
{
  QString gisFileName = currentProjectBaseName();
  gisFileName.prepend( QStringLiteral( "lkn_" ) );
  gisFileName.append( QStringLiteral( ".qgz" ) );
  QDir dir( currentProjectPath() );
  return QFileInfo( dir, gisFileName );
}

QList<QMenu *> LekanMainWindow::specificMenus()
{
  QList<QMenu *> menusList;

  QMenu *hydrologyMenu = new QMenu( tr( "Hydrology" ), this );
  mActionRainfallManager = hydrologyMenu->addAction( QPixmap( QStringLiteral( ":/images/rainfall.svg" ) ), tr( "Rainfall manager" ) );
  mActionRunoffManager = hydrologyMenu->addAction( QPixmap( QStringLiteral( ":/images/runoff.svg" ) ), tr( "Runoff manager" ) );

  menusList << hydrologyMenu;

  //menuRainFallRunoffModel = new QMenu( tr( "Modèle pluie/débit" ), this );

  //menuRainFallRunoffModel->addActions( rainfallManager->getMenu()->actions() );
// menuRainFallRunoffModel->addSeparator();
  //menuRainFallRunoffModel->addActions( runoffManager->getMenu()->actions() );
  //menusList << menuRainFallRunoffModel;

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

