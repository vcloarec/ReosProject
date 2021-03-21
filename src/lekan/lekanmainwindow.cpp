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
  setWindowIcon( QPixmap( QStringLiteral( ":/images/lekan.svg" ) ) );

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
  ReosWatershedWidget *watersehdWidget = new  ReosWatershedWidget( mMap, mWatershedModule, mDockWatershed );
  mDockWatershed->setWidget( watersehdWidget );
  addDockWidget( Qt::RightDockWidgetArea, mDockWatershed );

  mMap->setDefaultMapTool();

  addDockWidget( Qt::TopDockWidgetArea, mMap->temporalControllerDockWidget() );
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

  mRainFallManagerWidget->saveRainfallFile();
  mRunoffManagerWidget->save();

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

void LekanMainWindow::clearProject()
{
  if ( mGisEngine )
    mGisEngine->clearProject();

  if ( mWatershedModule )
    mWatershedModule->clearWatersheds();
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

  QMenu *mapMenu = new QMenu( tr( "Map" ), this );
  mapMenu->addActions( mMap->mapToolActions() );

  menusList << hydrologyMenu << mapMenu;

  return menusList;
}


