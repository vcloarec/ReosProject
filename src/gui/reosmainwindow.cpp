/***************************************************************************
                      reosmainwindow.cpp
                     --------------------------------------
Date                 : 07-09-2018
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

#include "reosmainwindow.h"

#include <QKeyEvent>
#include <QMenuBar>
#include <QFileDialog>
#include <QDockWidget>

#include <qgsmapcanvas.h>

#include "reossettings.h"
#include "reosmodule.h"
#include "reosmessagebox.h"
#include "reoslanguageselectionwidget.h"
#include "reosaboutwidget.h"
#include "reosdocumentation.h"
#include "reosversionmessagebox.h"


ReosMainWindow::ReosMainWindow( QWidget *parent ) :
  QMainWindow( parent ),
  mRootModule( new ReosModule( this ) ),
  mGroupActionFile( new QActionGroup( this ) ),
  mGroupActionEdit( new QActionGroup( this ) ),
  mGroupActionOption( new QActionGroup( this ) ),
  mGroupActionInterrogation( new QActionGroup( this ) ),
  mActionNewProject( new QAction( QPixmap( "://images/mActionNew.png" ), tr( "New Project" ), this ) ),
  mActionOpenFile( new QAction( QPixmap( "://images/mActionOpen.png" ), tr( "Open file" ), this ) ),
  mActionSaveFile( new QAction( QPixmap( "://images/mActionSave.png" ), tr( "Save" ), this ) ),
  mActionSaveFileAs( new QAction( QPixmap( "://images/mActionSaveAs.png" ), tr( "Save as ..." ), this ) ),
  mActionLanguageSelection( new QAction( tr( "Select language" ), this ) ),
  mActionAbout( new QAction( tr( "About ..." ), this ) ),
  mActionNewVersionAvailable( new QAction( tr( "Check new version" ), this ) ),
  mActionDocumentation( new QAction( tr( "Documentation" ), this ) ),
  mUndoStack( new QUndoStack( this ) )
{
  setDockNestingEnabled( true );

  QWidget *centralWidget = new QWidget( this );
  setCentralWidget( centralWidget );
  QGridLayout *centralLayout = new QGridLayout( centralWidget );
  centralWidget->setLayout( centralLayout );
  centralLayout->setContentsMargins( 0, 0, 0, 0 );
}

void ReosMainWindow::init()
{
  //****************************************************************
  messageBox = new ReosMessageBox( this );
  mDockMessageBox = new QDockWidget( tr( "Message" ) );
  mDockMessageBox->setWidget( messageBox );
  mDockMessageBox->setObjectName( QStringLiteral( "Dock message" ) );
  addDockWidget( Qt::RightDockWidgetArea, mDockMessageBox );
  //****************************************************************

  mGroupActionFile->addAction( mActionNewProject );
  mGroupActionFile->addAction( mActionOpenFile );
  mGroupActionFile->addAction( mActionSaveFile );
  mGroupActionFile->addAction( mActionSaveFileAs );

  mToolBarFile = addToolBar( tr( "File" ) );
  mToolBarFile->addActions( mGroupActionFile->actions() );
  mMenuFile = menuBar()->addMenu( tr( "File" ) );
  mMenuFile->addActions( mGroupActionFile->actions() );

  QAction *actionUndo = mUndoStack->createUndoAction( this );
  actionUndo->setIcon( QPixmap( "://images/mActionUndo.png" ) );

  mGroupActionEdit->addAction( actionUndo );
  QAction *actionRedo = mUndoStack->createRedoAction( this );
  actionRedo->setIcon( QPixmap( "://images/mActionRedo.png" ) );
  mGroupActionEdit->addAction( actionRedo );

  mToolBarEdit = addToolBar( tr( "Edit" ) );
  mToolBarEdit->addActions( mGroupActionEdit->actions() );
  mMenuEdit = menuBar()->addMenu( tr( "Edit" ) );
  mMenuEdit->addActions( mGroupActionEdit->actions() );

  const QList<QMenu *> &sm = specificMenus();
  for ( QMenu *menu : sm )
    menuBar()->addMenu( menu );

  mGroupActionOption->addAction( mActionLanguageSelection );
  mMenuOption = menuBar()->addMenu( tr( "Options" ) );
  mMenuOption->addActions( mGroupActionOption->actions() );

  mGroupActionInterrogation->addAction( mActionAbout );
  mGroupActionInterrogation->addAction( mActionNewVersionAvailable );
  mGroupActionInterrogation->addAction( mActionDocumentation );
  mMenuInterrogation = menuBar()->addMenu( tr( "?" ) );
  mMenuInterrogation->addActions( mGroupActionInterrogation->actions() );

  mDocumentation = new ReosDocumentation( version(), this );
  //****************************************************************

  connect( mRootModule, &ReosModule::newCommandToUndoStack, this, &ReosMainWindow::newUndoCommand );

  connect( mActionNewProject, &QAction::triggered, this, &ReosMainWindow::newProject );
  connect( mActionOpenFile, &QAction::triggered, this, &ReosMainWindow::open );
  connect( mActionSaveFile, &QAction::triggered, this, &ReosMainWindow::save );
  connect( mActionSaveFileAs, &QAction::triggered, this, &ReosMainWindow::saveAs );

  connect( mActionLanguageSelection, &QAction::triggered, this, &ReosMainWindow::languageSelection );
  connect( mActionAbout, &QAction::triggered, this, &ReosMainWindow::about );
  connect( mActionNewVersionAvailable, &QAction::triggered, this, &ReosMainWindow::newVersionAvailable );
  connect( mActionDocumentation, &QAction::triggered, mDocumentation, &ReosDocumentation::call );

  connect( mRootModule, &ReosModule::emitMessage, messageBox, &ReosMessageBox::receiveMessage );
}

void ReosMainWindow::addActionsFile( const QList<QAction *> actions )
{
  for ( QAction *action : actions )
    mGroupActionFile->addAction( action );
}

void ReosMainWindow::addActionEdit( const QList<QAction *> actions )
{
  for ( QAction *action : actions )
    mGroupActionEdit->addAction( action );
}

void ReosMainWindow::addActionOption( const QList<QAction *> actions )
{
  for ( QAction *action : actions )
    mGroupActionOption->addAction( action );
}

void ReosMainWindow::addActionInterrogation( const QList<QAction *> actions )
{
  for ( QAction *action : actions )
    mGroupActionInterrogation->addAction( action );
}

bool ReosMainWindow::open()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "/Path/Project" ) ).toString();

  QString filter = projectFileFilter();
  QString fileName = QFileDialog::getOpenFileName( this, tr( "Open a project" ), path, projectFileFilter() );

  if ( fileName.isEmpty() )
    return false;

  return openProject( fileName );
}

bool ReosMainWindow::save()
{
  if ( mFileNameCurrentProject.isEmpty() )
    return saveAs();

  return saveProject();
}

bool ReosMainWindow::saveAs()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "/Path/Project" ) ).toString();

  mFileNameCurrentProject = QFileDialog::getSaveFileName( this, tr( "Save project as" ), path, projectFileFilter() );

  if ( mFileNameCurrentProject.isEmpty() )
    return false;

  QFileInfo fileInfo( mFileNameCurrentProject );
  settings.setValue( QStringLiteral( "/Path/Project" ), fileInfo.path() );

  return saveProject();
}


void ReosMainWindow::newProject()
{
  int returnButton = QMessageBox::warning( this, tr( "New project" ), tr( "Save current project?" ), QMessageBox::Save | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Cancel );

  if ( returnButton == QMessageBox::Cancel )
    return;

  if ( returnButton == QMessageBox::Ok )
    saveProject();

  mFileNameCurrentProject.clear();
  clearProject();
}

void ReosMainWindow::languageSelection()
{

  ReosSettings settings;
  ReosLanguageSelectionWidget dial( settings.value( QStringLiteral( "Locale" ) ).toLocale() );

  if ( dial.exec() )
  {
    settings.setValue( QStringLiteral( "Locale" ), dial.language() );
  }

}

void ReosMainWindow::newVersionAvailable()
{
  new ReosVersionMessageBox( this, version(), false );
}

ReosModule *ReosMainWindow::rootModule() const
{
  return mRootModule;
}


void ReosMainWindow::about()
{
  ReosAboutWidget *about = new ReosAboutWidget( this );

  about->setBan( QPixmap( "://titre_Lekan.png" ) );
  about->setVersion( version().softwareNameWithVersion() );
  about->setWebAddress( webSite );
  about->addLibrary( "Qt", "5.11", "www.qt.io/" );
  about->addLibrary( "QGis", "3.4.12", "www.qgis.org/" );
  about->addLibrary( "GDAL", "2.4", "www.gdal.org/" );
  about->addLibrary( "Qwt", "6.1.4", "qwt.sourceforge.io" );

  QString licenceTxt;

  licenceTxt.append( "Overview: \n" );
  licenceTxt.append( "1. Lekan\n" );
  licenceTxt.append( "2. ECW Raster Plugin for GDAL\n" );
  licenceTxt.append( "3. MrSID Raster Plugin for GDAL\n\n\n" );

  licenceTxt.append( "1. Lekan\n" );
  QFile licenceFileLekan( "../LICENSE_LEKAN.txt" );
  QTextStream streamLekan( &licenceFileLekan );
  licenceFileLekan.open( QIODevice::ReadOnly );
  licenceTxt.append( streamLekan.readAll() );

  licenceTxt.append( "\n\n\n\n****************************\n\n\n\n" );

  licenceTxt.append( "2. ECW Raster Plugin for GDAL\n" );
  QFile licenceFileECW( "../ECWLicense.txt" );
  QTextStream streamECW( &licenceFileECW );
  licenceFileECW.open( QIODevice::ReadOnly );
  licenceTxt.append( streamECW.readAll() );

  licenceTxt.append( "\n\n\n\n****************************\n\n\n\n" );

  licenceTxt.append( "3. MrSID Raster Plugin for GDAL\n" );
  QFile licenceFileMrSID( "../MRSIDLicense.txt" );
  QTextStream streamMrSID( &licenceFileMrSID );
  licenceFileMrSID.open( QIODevice::ReadOnly );
  licenceTxt.append( streamMrSID.readAll() );

  about->setLicenceText( licenceTxt );

  about->exec();
}


void ReosMainWindow::closeEvent( QCloseEvent *event )
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "Windows/MainWindow/geometry" ), saveGeometry() );
  settings.setValue( QStringLiteral( "Windows/MainWindow/state" ), saveState() );
  QMainWindow::closeEvent( event );
}

void ReosMainWindow::keyPressEvent( QKeyEvent *event )
{
  if ( event->matches( QKeySequence::Undo ) )
  {
    mUndoStack->undo();
    event->accept();
  }

  if ( event->matches( ( QKeySequence::Redo ) ) )
  {
    mUndoStack->redo();
    event->accept();
  }

  QWidget::keyPressEvent( event );
}

QString ReosMainWindow::projectFileFilter() const
{
  return QString();
}

void ReosMainWindow::newUndoCommand( QUndoCommand *command )
{
  mUndoStack->push( command );
}
