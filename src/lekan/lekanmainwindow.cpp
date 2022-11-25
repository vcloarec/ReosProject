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

#include "reosstyleregistery.h"
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
#include "reosrainfallmodel.h"
#include "reosrainfallregistery.h"
#include "reosrunoffmanager.h"
#include "reosrunoffmodel.h"
#include "reoshydraulicnetwork.h"
#include "reoshydraulicnetworkwidget.h"

#define PROJECT_FILE_MAGIC_NUMBER 19092014


LekanMainWindow::LekanMainWindow( QWidget *parent ) :
  ReosMainWindow( parent ),
  mGisEngine( new ReosGisEngine( rootModule() ) ),
  mMap( new ReosMap( mGisEngine, this ) )
{
  ReosVersion::setCurrentApplicationVersion( lekanVersion );
  ReosGuiContext guiContext( this );
  guiContext.setMap( mMap );

  init();
  setWindowIcon( QIcon( QStringLiteral( ":/images/lekan.svg" ) ) );

  ReosStyleRegistery::instantiate( rootModule() );

  ReosRainfallRegistery::instantiate( rootModule() );
  ReosRunoffModelRegistery::instantiate( rootModule() );

  ReosPlotItemFactories::instantiate( rootModule() );
  ReosFormWidgetFactories::instantiate( rootModule() );

  mRainFallManagerWidget = new ReosRainfallManager( mMap, ReosRainfallRegistery::instance()->rainfallModel(), this );
  mActionRainfallManager->setCheckable( true );
  mRainFallManagerWidget->setAction( mActionRainfallManager );
  mRainFallManagerWidget->loadDataFile();
  connect( ReosRainfallRegistery::instance()->rainfallModel(), &ReosRainfallModel::changed, this, [this] {mIsRainfallDirty = true;} );

  mRunoffManagerWidget = new ReosRunoffManager( ReosRunoffModelRegistery::instance()->model(), this );
  mActionRunoffManager->setCheckable( true );
  mRunoffManagerWidget->setAction( mActionRunoffManager );
  mRunoffManagerWidget->loadDataFile();
  connect( ReosRunoffModelRegistery::instance()->model(), &ReosRunoffModelModel::modelChanged, this, [this] {mIsRunoffDirty = true;} );

  statusBar()->addPermanentWidget( new ReosMapCursorPosition( mMap, this ) );
  centralWidget()->layout()->addWidget( mMap->mapCanvas() );

  addDockWidget( Qt::TopDockWidgetArea, mMap->temporalControllerDockWidget() );
  mMap->temporalControllerDockWidget()->setObjectName( "temporalDock" );

  mGisDock = new QDockWidget( tr( "GIS Layers" ) );
  mGisDock->setObjectName( QStringLiteral( "gisDock" ) );
  mGisDock->setWidget( new ReosGisLayersWidget( mGisEngine, mMap, this ) );
  addDockWidget( Qt::LeftDockWidgetArea, mGisDock );

  mWatershedModule = new ReosWatershedModule( rootModule(), mGisEngine );

  mHydraulicNetwork = new ReosHydraulicNetwork( rootModule(), mGisEngine, mWatershedModule );

  QToolBar *networkToolBar = new QToolBar( this );
  addToolBar( networkToolBar );
  guiContext.addMainToolBar( QStringLiteral( "hydraulic-network" ), networkToolBar );
  mDockHydraulicNetwork = new ReosHydraulicNetworkDockWidget( mHydraulicNetwork, mWatershedModule, guiContext );
  mDockHydraulicNetwork->setObjectName( QStringLiteral( "hydraulicDock" ) );
  addDockWidget( Qt::RightDockWidgetArea, mDockHydraulicNetwork );
  mHydraulicNetworkWidget = mDockHydraulicNetwork->hydraulicNetworkWidget();
  connect( mHydraulicNetworkWidget, &ReosHydraulicNetworkWidget::timeWindowChanged, this, &LekanMainWindow::onTimeWindowChanged );
  connect( mHydraulicNetworkWidget, &ReosHydraulicNetworkWidget::mapTimeStepChanged, this, &LekanMainWindow::onMapTimeStepChanged );

  mDockWatershed = new  ReosWatershedDockWidget( guiContext, mWatershedModule, mHydraulicNetwork );
  mDockWatershed->setObjectName( QStringLiteral( "watershedDock" ) );
  addDockWidget( Qt::RightDockWidgetArea, mDockWatershed );
  mWatershedWidget = mDockWatershed->watershedWidget();
  connect( mWatershedWidget, &ReosWatershedWidget::timeWindowChanged, this, &LekanMainWindow::onTimeWindowChanged );
  connect( mWatershedWidget, &ReosWatershedWidget::mapTimeStepChanged, this, &LekanMainWindow::onMapTimeStepChanged );

  mMap->setDefaultMapTool();

  ReosSettings settings;
  restoreGeometry( settings.value( QStringLiteral( "Windows/MainWindow/geometry" ) ).toByteArray() );
  restoreState( settings.value( QStringLiteral( "Windows/MainWindow/state" ) ).toByteArray() );

  newProject();
}

bool LekanMainWindow::openProject()
{
  QString filePath = currentProjectFilePath();
  QString path = currentProjectPath();
  QString baseName = currentProjectBaseName();

  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly ) )
    return false;

  clearProject();

  ReosVersion version;

  QDataStream stream( &file );

  //*** read header
  qint32 magicNumber;
  qint32 serialisationVersion;
  QByteArray bytesVersion;
  stream >> magicNumber;

  if ( magicNumber == PROJECT_FILE_MAGIC_NUMBER )
  {
    // since Lekan 2.2
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

  mWatershedModule->decode( lekanProject.getEncodedData( QStringLiteral( "watershed-module" ) ) );
  mHydraulicNetwork->decode( lekanProject.getEncodedData( QStringLiteral( "hydaulic-network" ) ), path, baseName );

  return true;
}

void LekanMainWindow::onTimeWindowChanged()
{
  ReosTimeWindow tw = mWatershedWidget->timeWindow();
  tw = tw.unite( mHydraulicNetworkWidget->timeWindow() );
  mMap->setTemporalRange( tw.start(), tw.end() );
}

void LekanMainWindow::onMapTimeStepChanged()
{
  ReosDuration ts = mWatershedWidget->mapTimeStep();

  ReosDuration hnwTs = mHydraulicNetworkWidget->mapTimeStep();
  if ( hnwTs != ReosDuration() && hnwTs < ts )
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

  ReosEncodedElement lekanProject( QStringLiteral( "Lekan-project" ) );
  ReosEncodedElement encodedGisEngine = mGisEngine->encode( path, baseName );
  lekanProject.addEncodedData( QStringLiteral( "GIS-engine" ), encodedGisEngine );
  lekanProject.addEncodedData( QStringLiteral( "watershed-module" ), mWatershedModule->encode() );
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

  QByteArray versionBytes = lekanVersion.bytesVersion();

  Q_ASSERT( versionBytes.size() == 21 );

  stream << magicNumber;
  stream << serialisationVersion;
  stream << versionBytes;
  //*****

  stream << lekanProject.bytes();


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

QList<QMenu *> LekanMainWindow::specificMenus()
{
  QList<QMenu *> menusList;

  QMenu *hydrologyMenu = new QMenu( tr( "Hydrology" ), this );
  mActionRainfallManager = hydrologyMenu->addAction( QIcon( QStringLiteral( ":/images/rainfall.svg" ) ), tr( "Rainfall manager" ) );
  mActionRunoffManager = hydrologyMenu->addAction( QIcon( QStringLiteral( ":/images/runoff.svg" ) ), tr( "Runoff manager" ) );
  hydrologyMenu->setObjectName( QStringLiteral( "Hydrology" ) );

  QMenu *mapMenu = new QMenu( tr( "Map" ), this );
  mapMenu->addActions( mMap->mapToolActions() );
  mapMenu->setObjectName( QStringLiteral( "Map" ) );

  menusList << hydrologyMenu << mapMenu;

  return menusList;
}


