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

#include <QDesktopWidget>

#include <3d/qgs3dmapcanvasdockwidget.h>
#include <qgs3dmapsettings.h>
#include <qgsflatterraingenerator.h>

MainWindow::MainWindow( QWidget *parent ) :
  QMainWindow( parent ),
  ui( new Ui::MainWindow ),
  actionTinEditor( new QAction( QPixmap( "://toolbar/MeshTinEditor.png" ), tr( "Edit TIN" ), this ) )
{
  ui->setupUi( this );

  setDockNestingEnabled( true );


  ReosMap *map = new ReosMap( this );
  centralWidget()->setLayout( new QVBoxLayout );
  centralWidget()->layout()->addWidget( map->getMapCanvas() );
  statusBar()->addPermanentWidget( map->getCursorPosition() );


  ReosGisManager *gisManager = new ReosGisManager( map );
  QDockWidget *dockSIG = new QDockWidget( tr( "GIS panel" ) );
  dockSIG->setWidget( gisManager->getWidget() );
  statusBar()->addPermanentWidget( gisManager->createCRSDisplay( this ) );
  dockSIG->setObjectName( QStringLiteral( "Dock GIS" ) );
  addDockWidget( Qt::LeftDockWidgetArea, dockSIG );

  ReosMessageBox *messageBox = new ReosMessageBox;
  QDockWidget *dockMessage = new QDockWidget( tr( "Message box" ) );
  dockMessage->setWidget( messageBox );
  addDockWidget( Qt::LeftDockWidgetArea, dockMessage );

  QgsProviderRegistry::instance()->registerProvider( new HdTinEditorProviderMetaData() );

  QMenu *fileMenu = new QMenu( tr( "Files" ) );
  menuBar()->addMenu( fileMenu );

  ui->mainToolBar->addAction( actionTinEditor );
  actionTinEditor->setCheckable( true );
  tinEditor = new ReosTinEditorUi( gisManager, this );
  QMenu *tinMenu = new QMenu( tr( "Triangulated Irregular Network DEM" ) );
  tinMenu->addActions( tinEditor->getActions() );
  menuBar()->addMenu( tinMenu );
  connect( actionTinEditor, &QAction::triggered, this, &MainWindow::showTinEditor );
  connect( tinEditor, &ReosModule::widgetVisibility, actionTinEditor, &QAction::setChecked );
  connect( tinEditor, &ReosModule::activeUndoStack, this, &MainWindow::activeUndoStack );

  mUndoGroup = new QUndoGroup( this );
  ui->mainToolBar->addAction( mUndoGroup->createUndoAction( this ) );
  ui->mainToolBar->addAction( mUndoGroup->createRedoAction( this ) );

  //Active Tin editor
  tinEditor->showWidget();
  actionTinEditor->setChecked( true );

  Qgs3DMapCanvasDockWidget *dock3D = new Qgs3DMapCanvasDockWidget( this );
  addDockWidget( Qt::TopDockWidgetArea, dock3D );
  dock3D->setAllowedAreas( Qt::AllDockWidgetAreas );
  dock3D->setFloating( false );
  dock3D->setMainCanvas( map->getMapCanvas() );

  Qgs3DMapSettings *map3dSettings = new Qgs3DMapSettings;
  map3dSettings->setCrs( map->getMapCanvas()->mapSettings().destinationCrs() );
  map3dSettings->setOrigin( QgsVector3D( map->getMapCanvas()->fullExtent().center().x(), map->getMapCanvas()->fullExtent().center().y(), 0 ) );
  map3dSettings->setSelectionColor( map->getMapCanvas()->selectionColor() );
  map3dSettings->setBackgroundColor( map->getMapCanvas()->canvasColor() );
  map3dSettings->setLayers( map->getMapCanvas()->layers() );

  map3dSettings->setTransformContext( QgsProject::instance()->transformContext() );
  map3dSettings->setPathResolver( QgsProject::instance()->pathResolver() );
  map3dSettings->setMapThemeCollection( QgsProject::instance()->mapThemeCollection() );
  connect( QgsProject::instance(), &QgsProject::transformContextChanged, map, [map3dSettings]
  {
    map3dSettings->setTransformContext( QgsProject::instance()->transformContext() );
  } );

  QgsFlatTerrainGenerator *flatTerrain = new QgsFlatTerrainGenerator;
  flatTerrain->setCrs( map->getMapCanvas()->mapSettings().destinationCrs() );
  flatTerrain->setExtent( map->getMapCanvas()->fullExtent() );
  map3dSettings->setTerrainGenerator( flatTerrain );

  QgsPointLightSettings defaultPointLight;
  defaultPointLight.setPosition( QgsVector3D( 0, 1000, 0 ) );
  defaultPointLight.setConstantAttenuation( 0 );
  map3dSettings->setPointLights( QList<QgsPointLightSettings>() << defaultPointLight );
  map3dSettings->setOutputDpi( QgsApplication::desktop()->logicalDpiX() );

  dock3D->setMapSettings( map3dSettings );
}

MainWindow::~MainWindow()
{
  delete ui;
}
