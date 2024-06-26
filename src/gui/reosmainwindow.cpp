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
#include <QDesktopServices>
#include <QStyle>

#include <qgsmapcanvas.h>

#include "reossettings.h"
#include "reosmodule.h"
#include "reosmessagebox.h"
#include "reoslanguageselectionwidget.h"
#include "reosaboutwidget.h"
#include "reosversionmessagebox.h"
#include "reosremoteinformation.h"

#include "reosgisengine.h"
#include "reosplotwidget.h"
#include "reosstyleregistery.h"
#include "gmsh/reosgmshgenerator.h"


ReosMainWindow::ReosMainWindow( ReosModule *rootModule, QWidget *parent )
  : mRootModule( rootModule )
  , mGuiRootModule( new ReosModule( QStringLiteral( "gui-root-module" ), rootModule ) )
  , QMainWindow( parent )
  , mGroupActionFile( new QActionGroup( this ) )
  , mGroupActionEdit( new QActionGroup( this ) )
  , mGroupActionOption( new QActionGroup( this ) )
  , mGroupActionInterrogation( new QActionGroup( this ) )
  , mActionNewProject( new QAction( QIcon( QStringLiteral( ":/images/mActionNew.svg" ) ), tr( "New Project" ), this ) )
  , mActionOpenFile( new QAction( QIcon( QStringLiteral( ":/images/open.svg" ) ), tr( "Open file" ), this ) )
  , mMenuRecentProjects( new QMenu( tr( "Open Recent" ), this ) )
  , mActionSaveFile( new QAction( QIcon( QStringLiteral( ":/images/save.svg" ) ), tr( "Save" ), this ) )
  , mActionSaveFileAs( new QAction( QIcon( QStringLiteral( ":/images/saveAs.svg" ) ), tr( "Save as ..." ), this ) )
  , mActionLanguageSelection( new QAction( tr( "Select language" ), this ) )
  , mActionAbout( new QAction( tr( "About ..." ), this ) )
  , mActionNewVersionAvailable( new QAction( tr( "Check new version" ), this ) )
  , mActionDocumentation( new QAction( tr( "Documentation" ), this ) )
  , mActionHowToSupport( new QAction( tr( "How to help?" ), this ) )
{
  setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  setDockNestingEnabled( true );

  QWidget *centralWidget = new QWidget( this );
  setCentralWidget( centralWidget );
  QGridLayout *centralLayout = new QGridLayout;
  centralWidget->setLayout( centralLayout );
  centralLayout->setContentsMargins( 0, 0, 0, 0 );

  connect( rootModule, &ReosModule::dirtied, this, [this]
  {
    mProjectIsDirty = true;
  } );
}

ReosMainWindow::~ReosMainWindow()
{
  delete mGuiRootModule;
}

void ReosMainWindow::init()
{
  //****************************************************************
  messageBox = new ReosMessageBox( this );
  mDockMessageBox = new QDockWidget( tr( "Message" ) );
  mDockMessageBox->setWidget( messageBox );
  mDockMessageBox->setObjectName( QStringLiteral( "Dock message" ) );
  addDockWidget( Qt::BottomDockWidgetArea, mDockMessageBox );
  //****************************************************************

  ReosRemoteInformation *remoteInformation = new ReosRemoteInformation( this );

  connect( remoteInformation, &ReosRemoteInformation::informationready, this, &ReosMainWindow::onRemoteInformation );
  remoteInformation->requestInformation();

  mGroupActionFile->addAction( mActionNewProject );
  mGroupActionFile->addAction( mActionOpenFile );
  mGroupActionFile->addAction( mActionSaveFile );
  mGroupActionFile->addAction( mActionSaveFileAs );

  mToolBarFile = addToolBar( tr( "File" ) );
  mToolBarFile->addActions( mGroupActionFile->actions() );
  mToolBarFile->setObjectName( QStringLiteral( "toolBarFile" ) );
  mMenuFile = menuBar()->addMenu( tr( "File" ) );
  mMenuFile->addAction( mActionNewProject );
  mMenuFile->addAction( mActionOpenFile );
  mMenuFile->addMenu( mMenuRecentProjects );
  mMenuFile->addAction( mActionSaveFile );
  mMenuFile->addAction( mActionSaveFileAs );

  const QList<QMenu *> &sm = specificMenus();
  for ( QMenu *menu : sm )
  {
    menuBar()->addMenu( menu );
    QToolBar *tb = addToolBar( menu->title() );
    tb->setObjectName( menu->objectName() );
    tb->addActions( menu->actions() );
  }

  mGroupActionOption->addAction( mActionLanguageSelection );
  mMenuOption = menuBar()->addMenu( tr( "Options" ) );
  mMenuOption->addActions( mGroupActionOption->actions() );

  mGroupActionInterrogation->addAction( mActionAbout );
  mGroupActionInterrogation->addAction( mActionNewVersionAvailable );
  mMenuInterrogation = menuBar()->addMenu( tr( "?" ) );
  mMenuInterrogation->addActions( mGroupActionInterrogation->actions() );

  connect( mActionNewProject, &QAction::triggered, this, &ReosMainWindow::newProject );
  connect( mActionOpenFile, &QAction::triggered, this, &ReosMainWindow::openFile );
  connect( mActionSaveFile, &QAction::triggered, this, &ReosMainWindow::save );
  connect( mActionSaveFileAs, &QAction::triggered, this, &ReosMainWindow::saveAs );

  connect( mActionLanguageSelection, &QAction::triggered, this, &ReosMainWindow::languageSelection );
  connect( mActionAbout, &QAction::triggered, this, &ReosMainWindow::about );
  connect( mActionNewVersionAvailable, &QAction::triggered, this, &ReosMainWindow::newVersionAvailable );
  connect( mActionDocumentation, &QAction::triggered, this, [this] { QDesktopServices::openUrl( mDocumentationUrl );} );
  connect( mActionHowToSupport, &QAction::triggered, this, [this] { QDesktopServices::openUrl( mHowToSupportUrl );} );

  connect( mRootModule, &ReosModule::emitMessage, messageBox, &ReosMessageBox::receiveMessage );
}

void ReosMainWindow::onRemoteInformation( const QVariantMap &information )
{
  if ( information.contains( QStringLiteral( "documentationUrl" ) ) )
  {
    mDocumentationUrl = information.value( QStringLiteral( "documentationUrl" ) ).toString();
    mGroupActionInterrogation->addAction( mActionDocumentation );
    mMenuInterrogation->addAction( mActionDocumentation );
  }

  if ( information.contains( QStringLiteral( "howToSupportUrl" ) ) )
  {
    mHowToSupportUrl = information.value( QStringLiteral( "howToSupportUrl" ) ).toString();
    mGroupActionInterrogation->addAction( mActionHowToSupport );
    mMenuInterrogation->addAction( mActionHowToSupport );
  }
}

void ReosMainWindow::addActionsFile( const QList<QAction *> &actions )
{
  for ( QAction *action : actions )
    mGroupActionFile->addAction( action );
}

void ReosMainWindow::addActionEdit( const QList<QAction *> &actions )
{
  for ( QAction *action : actions )
    mGroupActionEdit->addAction( action );
}

void ReosMainWindow::addActionOption( const QList<QAction *> &actions )
{
  for ( QAction *action : actions )
    mGroupActionOption->addAction( action );
}

void ReosMainWindow::addActionInterrogation( const QList<QAction *> &actions )
{
  for ( QAction *action : actions )
    mGroupActionInterrogation->addAction( action );
}

bool ReosMainWindow::openFile()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/Project" ) ).toString();

  QString fileName = QFileDialog::getOpenFileName( this, tr( "Open a project" ), path, projectFileFilter() );

  if ( fileName.isEmpty() )
    return false;

  return openFileWithPath( fileName );
}

bool ReosMainWindow::openFileWithPath( const QString &filePath )
{
  ReosSettings settings;
  mCurrentProjectFileInfo = QFileInfo( filePath );
  mRootModule->setProjectFileName( filePath );
  settings.setValue( QStringLiteral( "Path/Project" ), mCurrentProjectFileInfo.path() );

  bool result = openProject();

  if ( !result )
  {
    mCurrentProjectFileInfo = QFileInfo();
    mRootModule->setProjectFileName( QString() );
  }

  mProjectIsDirty = false;

  return result;
}

bool ReosMainWindow::save()
{
  if ( !mCurrentProjectFileInfo.exists() )
    return saveAs();

  bool saved = saveProject();
  if ( saved )
    mProjectIsDirty = false;
  return saved;
}

bool ReosMainWindow::saveAs()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/Project" ) ).toString();

  QString filePath = QFileDialog::getSaveFileName( this, tr( "Save project as" ), path, projectFileFilter() );

  if ( filePath.isEmpty() )
    return false;

  mCurrentProjectFileInfo = QFileInfo( filePath );
  if ( mCurrentProjectFileInfo.suffix() != projectFileSuffix() )
  {
    filePath.append( '.' + projectFileSuffix() );
    mCurrentProjectFileInfo = QFileInfo( filePath );
  }
  mRootModule->setProjectFileName( filePath );
  settings.setValue( QStringLiteral( "Path/Project" ), mCurrentProjectFileInfo.path() );

  bool saved = saveProject();
  if ( saved )
    mProjectIsDirty = false;
  return saved;
}


void ReosMainWindow::newProject()
{
  if ( mProjectIsDirty )
  {
    int returnButton = QMessageBox::warning( this, tr( "New project" ), tr( "Save current project?" ), QMessageBox::Save | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Cancel );

    if ( returnButton == QMessageBox::Cancel )
      return;

    if ( returnButton == QMessageBox::Save )
      save();
  }

  mCurrentProjectFileInfo = QFileInfo();
  mRootModule->setProjectFileName( QString() );
  clearProject();

  mProjectIsDirty = false;
}

void ReosMainWindow::languageSelection()
{
  QLocale localeLanguage;
  QLocale localeGlobal;

  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "Locale-language" ) ) )
    localeLanguage = settings.value( QStringLiteral( "Locale-language" ) ).toLocale();
  else
    localeLanguage = QLocale::system();

  if ( settings.contains( QStringLiteral( "Locale-global" ) ) )
    localeGlobal = settings.value( QStringLiteral( "Locale-global" ) ).toLocale();
  else
    localeGlobal = QLocale::system();

  ReosLanguageSelectionWidget dial( localeLanguage, localeGlobal );

  if ( dial.exec() )
  {
    settings.setValue( QStringLiteral( "Locale-language" ), dial.language() );
    settings.setValue( QStringLiteral( "Locale-global" ), dial.global() );
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

ReosModule *ReosMainWindow::guiRootModule() const
{
  return mGuiRootModule;
}

QString ReosMainWindow::currentProjectFilePath() const
{
  return mCurrentProjectFileInfo.filePath();
}

QString ReosMainWindow::currentProjectFileName() const
{
  return mCurrentProjectFileInfo.fileName();
}

QString ReosMainWindow::currentProjectPath() const
{
  return mCurrentProjectFileInfo.path();
}

void ReosMainWindow::setRecentProjects( const QStringList &recentProjects )
{
  mMenuRecentProjects->clear();
  for ( const QString &filePath : recentProjects )
  {
    QFileInfo fileInfo( filePath );
    QString text = fileInfo.completeBaseName() + QStringLiteral( " (%1)" ).arg( QDir::toNativeSeparators( fileInfo.filePath() ) );
    QAction *action = mMenuRecentProjects->addAction( text, this, [this, filePath]
    {
      openFileWithPath( filePath );
    } );

    action->setEnabled( fileInfo.exists() );
  }
}

QString ReosMainWindow::currentProjectBaseName() const
{
  return mCurrentProjectFileInfo.baseName();
}


void ReosMainWindow::about()
{
  ReosAboutWidget *about = new ReosAboutWidget( this );

  about->setBan( QPixmap( ":/images/bannerLekan.svg" ) );
  about->setVersion( version().softwareNameWithVersion() );
  about->setWebAddress( webSite );
  about->addLibrary( "Qt", qVersion(), "www.qt.io" );
  about->addLibrary( ReosGisEngine::gisEngineName(), ReosGisEngine::gisEngineVersion(),  ReosGisEngine::gisEngineLink() );
  about->addLibrary( ReosPlotWidget::plotEngineName(),  ReosPlotWidget::plotEngineVersion(),  ReosPlotWidget::plotEngineLink() );
  about->addLibrary( QStringLiteral( "Gmsh" ), ReosGmshGenerator::version(), QStringLiteral( "gmsh.info/" ) );

  QString licenceTxt;
  QDir dir( QCoreApplication::applicationDirPath() );
  dir.cdUp();
  QFile licenceFileLekan( dir.filePath( QStringLiteral( "LICENSE_LEKAN.txt" ) ) );
  QTextStream streamLekan( &licenceFileLekan );
  licenceFileLekan.open( QIODevice::ReadOnly );
  licenceTxt.append( streamLekan.readAll() );

  about->setLicenceText( licenceTxt );

  about->setDevelopersTextFile( QStringLiteral( ":/developers" ) );
  about->setTranslatorsTextFile( QStringLiteral( ":/translators" ) );

  if ( mFinancialContribution.isEmpty() )
  {
    QFile devFiles( QStringLiteral( ":/financial" ) );
    if ( devFiles.open( QIODevice::ReadOnly ) )
    {
      QTextStream txtStream( &devFiles );
      about->setFinancialContributorText( txtStream.readAll() );
    }
  }
  else
    about->setFinancialContributorText( mFinancialContribution );

  about->exec();
}


void ReosMainWindow::closeEvent( QCloseEvent *event )
{
  if ( mProjectIsDirty )
  {
    if ( QMessageBox::question( this, tr( "Closing current project" ), tr( "The current project has been modified, do you want to save it?" ) )
         == QMessageBox::Yes )
      save();
  }
  else
    checkExtraProjectToSave();

  ReosSettings settings;
  settings.setValue( QStringLiteral( "Windows/MainWindow/geometry" ), saveGeometry() );
  settings.setValue( QStringLiteral( "Windows/MainWindow/state" ), saveState() );

  clearProject();

  QMainWindow::closeEvent( event );
}

QString ReosMainWindow::projectFileFilter() const
{
  return QString();
}

QString ReosMainWindow::projectFileSuffix() const
{
  return QString();
}


ReosRecentProjectModel::ReosRecentProjectModel( QObject *parent ): QAbstractListModel( parent )
{}

int ReosRecentProjectModel::rowCount( const QModelIndex & ) const
{
  return mPathes.count();
}

QVariant ReosRecentProjectModel::data( const QModelIndex &index, int role ) const
{
  if ( !index.isValid() )
    return QVariant();

  if ( index.row() < 0 || index.row() >= mPathes.count() )
    return QVariant();

  QFileInfo fileInfo( mPathes.at( index.row() ) );

  switch ( role )
  {
    case Qt::DisplayRole:
      return fileInfo.completeBaseName() + QStringLiteral( " (%1)" ).arg( QDir::toNativeSeparators( fileInfo.filePath() ) );
      break;
    case Qt::ForegroundRole:
      if ( fileInfo.exists() )
        return QColor( Qt::black );
      else
        return QColor( Qt::gray );
      break;
    default:
      break;
  }

  return QVariant();
}

void ReosRecentProjectModel::setPathes( const QStringList &newPathes )
{
  beginResetModel();
  mPathes = newPathes;
  endResetModel();
}

const QString ReosRecentProjectModel::path( const QModelIndex &index )
{
  if ( index.row() < 0 || index.row() >= mPathes.count() )
    return QString();

  return mPathes.at( index.row() );
}
