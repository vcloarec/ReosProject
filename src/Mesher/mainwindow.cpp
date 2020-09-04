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
#include <qgs3drendererregistry.h>
#include <qgsvectorlayer3drenderer.h>
#include <qgsrulebased3drenderer.h>
#include <qgsmeshlayer3drenderer.h>


MainWindow::MainWindow( QWidget *parent ) :
  QMainWindow( parent ),
  ui( new Ui::MainWindow ),
  actionTinEditor( new QAction( QPixmap( "://toolbar/MeshTinEditor.png" ), tr( "Edit TIN" ), this ) ),
  actionMap3D( new QAction( tr( "Open Map 3D" ), this ) )
{
  ui->setupUi( this );

  setDockNestingEnabled( true );

  mMap = new ReosMap( this );
  centralWidget()->setLayout( new QVBoxLayout );
  centralWidget()->layout()->addWidget( mMap->getMapCanvas() );
  statusBar()->addPermanentWidget( mMap->getCursorPosition() );


  mGisManager = new ReosGisManager( mMap );
  QDockWidget *dockSIG = new QDockWidget( tr( "GIS panel" ) );
  dockSIG->setWidget( mGisManager->getWidget() );
  statusBar()->addPermanentWidget( mGisManager->createCRSDisplay( this ) );
  dockSIG->setObjectName( QStringLiteral( "Dock GIS" ) );
  addDockWidget( Qt::LeftDockWidgetArea, dockSIG );

  ReosMessageBox *messageBox = new ReosMessageBox;
  QDockWidget *dockMessage = new QDockWidget( tr( "Message box" ) );
  dockMessage->setWidget( messageBox );
  addDockWidget( Qt::LeftDockWidgetArea, dockMessage );

  QgsProviderRegistry::instance()->registerProvider( new ReosMeshPoviderMetadata() );

  QMenu *fileMenu = new QMenu( tr( "Files" ) );
  menuBar()->addMenu( fileMenu );

  ui->mainToolBar->addAction( actionTinEditor );
  actionTinEditor->setCheckable( true );
  ui->mainToolBar->addAction( actionMap3D );

  tinEditor = new ReosTinEditorUi( mGisManager, this );
  QMenu *tinMenu = new QMenu( tr( "Triangulated Irregular Network DEM" ) );
  tinMenu->addActions( tinEditor->getActions() );
  menuBar()->addMenu( tinMenu );
  connect( actionTinEditor, &QAction::triggered, this, &MainWindow::showTinEditor );
  connect( actionMap3D, &QAction::triggered, this, &MainWindow::openMap3D );
  connect( tinEditor, &ReosModule::widgetVisibility, actionTinEditor, &QAction::setChecked );
  connect( tinEditor, &ReosModule::activeUndoStack, this, &MainWindow::activeUndoStack );

  mUndoGroup = new QUndoGroup( this );
  ui->mainToolBar->addAction( mUndoGroup->createUndoAction( this ) );
  ui->mainToolBar->addAction( mUndoGroup->createRedoAction( this ) );

  //Active Tin editor
  tinEditor->showWidget();
  actionTinEditor->setChecked( true );

  QgsApplication::instance()->renderer3DRegistry()->addRenderer( new QgsVectorLayer3DRendererMetadata );
  QgsApplication::instance()->renderer3DRegistry()->addRenderer( new QgsRuleBased3DRendererMetadata );
  QgsApplication::instance()->renderer3DRegistry()->addRenderer( new QgsMeshLayer3DRendererMetadata );
}

MainWindow::~MainWindow()
{
  delete ui;
}

void MainWindow::openMap3D()
{
  Qgs3DMapCanvasDockWidget *dock3D = new Qgs3DMapCanvasDockWidget( this );
  dock3D->setAllowedAreas( Qt::AllDockWidgetAreas );
  addDockWidget( Qt::RightDockWidgetArea, dock3D );
  dock3D->setFloating( false );
  dock3D->setMainCanvas( mMap->getMapCanvas() );

  Qgs3DMapSettings *map3dSettings = new Qgs3DMapSettings;
  map3dSettings->setCrs( mMap->getMapCanvas()->mapSettings().destinationCrs() );
  map3dSettings->setOrigin( QgsVector3D( mMap->getMapCanvas()->fullExtent().center().x(), mMap->getMapCanvas()->fullExtent().center().y(), 0 ) );
  map3dSettings->setSelectionColor( mMap->getMapCanvas()->selectionColor() );
  map3dSettings->setBackgroundColor( mMap->getMapCanvas()->canvasColor() );
  map3dSettings->setLayers( mMap->getMapCanvas()->layers() );

  map3dSettings->setTransformContext( QgsProject::instance()->transformContext() );
  map3dSettings->setPathResolver( QgsProject::instance()->pathResolver() );
  map3dSettings->setMapThemeCollection( QgsProject::instance()->mapThemeCollection() );
  connect( QgsProject::instance(), &QgsProject::transformContextChanged, mMap, [map3dSettings]
  {
    map3dSettings->setTransformContext( QgsProject::instance()->transformContext() );
  } );

  QgsFlatTerrainGenerator *flatTerrain = new QgsFlatTerrainGenerator;
  flatTerrain->setCrs( mMap->getMapCanvas()->mapSettings().destinationCrs() );
  flatTerrain->setExtent( mMap->getMapCanvas()->fullExtent() );
  map3dSettings->setTerrainGenerator( flatTerrain );

  QgsPointLightSettings defaultPointLight;
  defaultPointLight.setPosition( QgsVector3D( 0, 1000, 0 ) );
  defaultPointLight.setConstantAttenuation( 0 );
  map3dSettings->setPointLights( QList<QgsPointLightSettings>() << defaultPointLight );
  map3dSettings->setOutputDpi( QgsApplication::desktop()->logicalDpiX() );

  dock3D->setMapSettings( map3dSettings );

  connect( mGisManager, &ReosGisManager::projectHasToBeClosed, dock3D, &Qgs3DMapCanvasDockWidget::close );
  connect( mGisManager, &ReosGisManager::projectHasToBeClosed, dock3D, &Qgs3DMapCanvasDockWidget::deleteLater );
}
