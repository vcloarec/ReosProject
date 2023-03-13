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
#include "reoswatershedmodule.h"
#include "reoswatershedwidget.h"
#include "reosrainfallmanager.h"
#include "reosrainfallmodel.h"
#include "reosrainfallregistery.h"
#include "reosrunoffmanager.h"
#include "reosrunoffmodel.h"
#include "reoshydraulicnetwork.h"
#include "reoshydraulicnetworkwidget.h"
#include "reosstructure2dtoolbar.h"
#include "reosoverridecursor.h"
#include "reoscoremodule.h"

#define PROJECT_FILE_MAGIC_NUMBER 19092014

LekanMainWindow::LekanMainWindow( ReosCoreModule *core, QWidget *parent )
  : ReosMainWindow( core, parent )
  , mGisEngine( core->gisEngine() )
  , mMap( new ReosMap( mGisEngine, this ) )
  , mActionRainfallManager( new QAction( QIcon( QStringLiteral( ":/images/rainfall.svg" ) ), tr( "Rainfall manager" ), this ) )
  , mActionRunoffManager( new QAction( QIcon( QStringLiteral( ":/images/runoff.svg" ) ), tr( "Runoff manager" ), this ) )
{
  int verMaj = QString( MAJ_VER_LEKAN ).toInt();
  int verMin = QString( MIN_VER_LEKAN ).toInt();
#ifdef LEKAN_EXP
  bool ok = true;
  QString ps = QString( PAT_VER_LEKAN );
  int verPatch = QString( PAT_VER_LEKAN ).toInt( &ok, 16 );
#else
  int verPatch = QString( PAT_VER_LEKAN ).toInt();
#endif
  ReosVersion version( "Lekan", verMaj, verMin, verPatch );

  ReosVersion::setCurrentApplicationVersion( version );
  ReosGuiContext guiContext( this );
  guiContext.setMap( mMap );

  setWindowIcon( QIcon( QStringLiteral( ":/images/lekan.svg" ) ) );

  ReosPlotItemFactories::instantiate( guiRootModule() );
  ReosFormWidgetFactories::instantiate( guiRootModule() );

  mRainFallManagerWidget = new ReosRainfallManager( mMap, ReosRainfallRegistery::instance()->rainfallModel(), this );
  mActionRainfallManager->setCheckable( true );
  mRainFallManagerWidget->setAction( mActionRainfallManager );
  mRainFallManagerWidget->loadDataFile();
  connect( ReosRainfallRegistery::instance()->rainfallModel(), &ReosRainfallModel::changed, this, [this] {mIsRainfallDirty = true;} );
  connect( ReosRainfallRegistery::instance()->rainfallModel(), &ReosRainfallModel::saved, this, [this] {mIsRainfallDirty = false;} );

  mRunoffManagerWidget = new ReosRunoffManager( ReosRunoffModelRegistery::instance()->model(), this );
  mActionRunoffManager->setCheckable( true );
  mRunoffManagerWidget->setAction( mActionRunoffManager );
  mRunoffManagerWidget->loadDataFile();
  connect( ReosRunoffModelRegistery::instance()->model(), &ReosRunoffModelModel::modelChanged, this, [this] {mIsRunoffDirty = true;} );

  statusBar()->addPermanentWidget( new ReosMapCursorPosition( mMap, this ) );
  centralWidget()->layout()->addWidget( mMap->mapCanvas() );

  mMap->temporalControllerDockWidget()->setObjectName( "temporalDock" );

  mGisDock = new QDockWidget( tr( "GIS Layers" ) );
  mGisDock->setObjectName( QStringLiteral( "gisDock" ) );
  mGisDock->setWidget( new ReosGisLayersWidget( mGisEngine, mMap, this ) );

  mWatershedModule = core->watershedModule();
  mHydraulicNetwork = core->hydraulicNetwork();

  mDockHydraulicNetwork = new ReosHydraulicNetworkDockWidget( mHydraulicNetwork, mWatershedModule, guiContext );
  mDockHydraulicNetwork->setObjectName( QStringLiteral( "hydraulicDock" ) );

  mHydraulicNetworkWidget = mDockHydraulicNetwork->hydraulicNetworkWidget();
  connect( mHydraulicNetworkWidget, &ReosHydraulicNetworkWidget::mapTimeStepChanged, this, &LekanMainWindow::onMapTimeStepChanged );
  mMap->addSelectToolTarget( ReosHydraulicNetworkElement::staticType() );
  addToolBar( mHydraulicNetworkWidget->structure2dToolBar() );

  mDockWatershed = new  ReosWatershedDockWidget( guiContext, mWatershedModule, mHydraulicNetwork );
  mDockWatershed->setObjectName( QStringLiteral( "watershedDock" ) );

  mWatershedWidget = mDockWatershed->watershedWidget();
  connect( mWatershedWidget, &ReosWatershedWidget::mapTimeStepChanged, this, &LekanMainWindow::onMapTimeStepChanged );
  mMap->addSelectToolTarget( mWatershedWidget->descriptionKeyWatershed() );

  addDockWidget( Qt::LeftDockWidgetArea, mGisDock );
  addDockWidget( Qt::RightDockWidgetArea, mDockWatershed );
  addDockWidget( Qt::RightDockWidgetArea, mDockHydraulicNetwork );
  addDockWidget( Qt::TopDockWidgetArea, mMap->temporalControllerDockWidget() );

  mMap->setDefaultMapTool();

  init();

  ReosSettings settings;
  restoreGeometry( settings.value( QStringLiteral( "Windows/MainWindow/geometry" ) ).toByteArray() );
  restoreState( settings.value( QStringLiteral( "Windows/MainWindow/state" ) ).toByteArray() );

  newProject();
}

bool LekanMainWindow::openProject()
{
  ReosOverrideCursor overrideCursor;
  QString filePath = currentProjectFilePath();
  QString path = currentProjectPath();
  QString baseName = currentProjectBaseName();

  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  clearProject();

  ReosVersion version;
  ReosEncodeContext encodeContext;
  encodeContext.setBaseDir( QDir( path ) );

  QDataStream stream( &file );

  //*** read header
  qint32 magicNumber;
  QByteArray bytesVersion;
  stream >> magicNumber;

  if ( magicNumber == PROJECT_FILE_MAGIC_NUMBER )
  {
    // since Lekan 2.2
    qint32 serialisationVersion;
    stream >> serialisationVersion;
    stream >> bytesVersion;
    QDataStream::Version v = static_cast<QDataStream::Version>( serialisationVersion );
    ReosEncodedElement::setSerialisationVersion( v );
    version = ReosVersion( bytesVersion, v );
  }
  else
  {
    //old version don't have header
    ReosEncodedElement::setSerialisationVersion( QDataStream::Qt_5_12 ); /// TODO : check the Qt version of Lekan 2.0 / 2.1
    stream.device()->reset();
  }

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

  mMap->initialize();

  mWatershedModule->decode( lekanProject.getEncodedData( QStringLiteral( "watershed-module" ) ), encodeContext );
  mHydraulicNetwork->decode( lekanProject.getEncodedData( QStringLiteral( "hydaulic-network" ) ), path, baseName );

  storeProjectPath( filePath );

  return true;
}

void LekanMainWindow::onMapTimeStepChanged()
{
  ReosDuration ts = mWatershedWidget->mapTimeStep();

  ReosDuration hnwTs = mHydraulicNetworkWidget->mapTimeStep();
  if ( hnwTs != ReosDuration() &&
       ( ts == ReosDuration() || hnwTs < ts ) )
    ts = hnwTs;

  mMap->setTimeStep( ts );
}

bool LekanMainWindow::saveProject()
{
  QString filePath = currentProjectFilePath();
  QString path = currentProjectPath();
  QString baseName = currentProjectBaseName();

  mRainFallManagerWidget->saveRainfallFile();
  mRunoffManagerWidget->save();

  ReosEncodeContext context;
  context.setBaseDir( QDir( currentProjectPath() ) );
  ReosEncodedElement lekanProject( QStringLiteral( "Lekan-project" ) );
  ReosEncodedElement encodedGisEngine = mGisEngine->encode( path, baseName );
  lekanProject.addEncodedData( QStringLiteral( "GIS-engine" ), encodedGisEngine );
  lekanProject.addEncodedData( QStringLiteral( "watershed-module" ), mWatershedModule->encode( context ) );
  lekanProject.addEncodedData( QStringLiteral( "hydaulic-network" ), mHydraulicNetwork->encode( path, baseName ) );

  QFileInfo fileInfo( filePath );
  if ( fileInfo.suffix().isEmpty() )
    filePath.append( QStringLiteral( ".lkn" ) );

  QFile file( filePath );
  if ( !file.open( QIODevice::WriteOnly ) )
    return false;

  QDataStream stream( &file );

  //**** header
  qint32 magicNumber = PROJECT_FILE_MAGIC_NUMBER;
  qint32 serialisationVersion = stream.version();

  QByteArray versionBytes = ReosVersion::currentApplicationVersion().bytesVersion();

  Q_ASSERT( versionBytes.size() == 21 );

  stream << magicNumber;
  stream << serialisationVersion;
  stream << versionBytes;
  //*****

  stream << lekanProject.bytes();

  storeProjectPath( filePath );

  const QFileInfoList filesToRemove = rootModule()->uselessFiles( true );
  for ( const QFileInfo &fi : filesToRemove )
  {
    if ( !fi.exists() )
      continue;
    if ( fi.isDir() )
    {
      QDir dir( fi.filePath() );
      dir.removeRecursively();
    }
    else if ( fi.isFile() )
    {
      QFile fileToRemove( fi.path() );
      fileToRemove.remove();
    }
  }

  return true;
}

void LekanMainWindow::clearProject()
{
  mDockHydraulicNetwork->closePropertieWidget();

  if ( mGisEngine )
    mGisEngine->clearProject();

  if ( mMap )
    mMap->initialize();

  if ( mWatershedModule )
    mWatershedModule->reset();

  if ( mHydraulicNetwork )
    mHydraulicNetwork->reset();

}

void LekanMainWindow::checkExtraProjectToSave()
{
  if ( mIsRainfallDirty )
  {
    if ( QMessageBox::question( this, tr( "Rainfall Data Changed" ), tr( "Rainfall data have changed, do you want to save?" ) ) == QMessageBox::Yes )
      mRainFallManagerWidget->saveRainfallFile();
  }

  if ( mIsRunoffDirty )
  {
    if ( QMessageBox::question( this, tr( "Runoff Data Changed" ), tr( "Runoff data have changed, do you want to save?" ) ) == QMessageBox::Yes )
      mRunoffManagerWidget->save();
  }
}

QString LekanMainWindow::projectFileFilter() const
{
  return QStringLiteral( "Lekan file (*.%1)" ).arg( projectFileSuffix() );
}

QString LekanMainWindow::projectFileSuffix() const
{
  return QStringLiteral( "lkn" );
}

QFileInfo LekanMainWindow::gisFileInfo() const
{
  QString gisFileName = currentProjectBaseName();
  gisFileName.prepend( QStringLiteral( "lkn_" ) );
  gisFileName.append( QStringLiteral( ".qgz" ) );
  QDir dir( currentProjectPath() );
  return QFileInfo( dir, gisFileName );
}

void LekanMainWindow::storeProjectPath( const QString &path )
{
  ReosSettings settings;
  QStringList pathes;
  if ( settings.contains( QStringLiteral( "/recent-lekan-project" ) ) )
    pathes = settings.value( QStringLiteral( "/recent-lekan-project" ) ).value<QStringList>();

  int presentIndex = pathes.indexOf( path );
  if ( presentIndex >= 0 )
    pathes.removeOne( path );

  pathes.prepend( path );
  if ( pathes.count() > 20 )
    pathes.takeLast();
  settings.setValue( QStringLiteral( "/recent-lekan-project" ), pathes );

  setRecentProjects( pathes );
}

QStringList LekanMainWindow::recentProjectPathes() const
{
  ReosSettings settings;
  if ( settings.contains( QStringLiteral( "/recent-lekan-project" ) ) )
    return settings.value( QStringLiteral( "/recent-lekan-project" ) ).value<QStringList>();

  return QStringList();
}

QList<QMenu *> LekanMainWindow::specificMenus()
{
  QList<QMenu *> menusList;

  QMenu *hydrologyMenu = new QMenu( tr( "Hydrology" ), this );
  hydrologyMenu->addAction( mActionRainfallManager );
  hydrologyMenu->addAction( mActionRunoffManager );
  hydrologyMenu->addAction( mWatershedWidget->meteorologicalModelAction() );
  hydrologyMenu->addAction( mDockWatershed->actionToggle() );
  hydrologyMenu->setObjectName( QStringLiteral( "Hydrology" ) );

  QMenu *mapMenu = new QMenu( tr( "Map" ), this );
  mapMenu->addActions( mMap->mapToolActions() );
  mapMenu->addAction( mWatershedWidget->displayGriddedPrecipitationOnMap() );
  mapMenu->setObjectName( QStringLiteral( "Map" ) );

  menusList << hydrologyMenu << mapMenu;

  return menusList;
}


