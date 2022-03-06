/***************************************************************************
  reos3dview.cpp - Reos3dView

 ---------------------
 begin                : 3.3.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reos3dview.h"
#include "ui_reos3dview.h"

#include <QAction>
#include <QToolBar>
#include <QToolButton>
#include <QMenu>

#include <3d/qgs3dmapcanvas.h>
#include <qgsmeshterraingenerator.h>
#include <qgsmeshlayer.h>
#include <qgs3dmapsettings.h>

#include "reosmesh.h"
#include "reoslightwidget.h"
#include "reosstyleregistery.h"

Reos3dView::Reos3dView( ReosMesh *mesh, QWidget *parent )
  : ReosActionWidget( parent )
  , ui( new Ui::Reos3dView )
  , mActionZoomExtent( new QAction( QPixmap( QStringLiteral( ":/images/zoomFullExtent.svg" ) ), tr( "Zoom to Full Extent" ), this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );
  setWindowTitle( tr( "3D View" ) );

  mCanvas = new Qgs3DMapCanvas( this );
  mCanvas->setMinimumSize( QSize( 200, 200 ) );
  mCanvas->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Expanding );

  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *>( mesh->data() );

  QgsMeshTerrainGenerator *terrainGenerator = new QgsMeshTerrainGenerator();
  terrainGenerator->setLayer( meshLayer );

  Qgs3DMapSettings *settings = new Qgs3DMapSettings();

  settings->setTerrainGenerator( terrainGenerator );
  settings->setCrs( meshLayer->crs() );
  settings->setBackgroundColor( QColor( 119, 181, 254 ) );
  QgsDirectionalLightSettings lightSettings;
  settings->setDirectionalLights( {lightSettings} );

  QgsRectangle extent = meshLayer->extent();
  float dist = static_cast< float >( std::max( extent.width(), extent.height() ) );
  settings->setOrigin( QgsVector3D( extent.center().x(), extent.center().y(), 0 ) );
  mCanvas->setMap( settings );
  mCanvas->setViewFromTop( extent.center(), dist, 0 );
  mCanvas->setOnScreenNavigationVisibility( false );


  ui->m3dViewLayout->addWidget( mCanvas );

  QToolBar *toolBar = new QToolBar( this );
  ui->mToolBarLayout->addWidget( toolBar );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  toolBar->addAction( mActionZoomExtent );
  connect( mActionZoomExtent, &QAction::triggered, this, [this, meshLayer, settings]
  {
    QgsRectangle extent = meshLayer->extent();
    float dist = static_cast< float >( std::max( extent.width(), extent.height() ) );
    settings->setOrigin( QgsVector3D( extent.center().x(), extent.center().y(), 0 ) );
    mCanvas->setViewFromTop( extent.center(), dist, 0 );
  } );

  QToolButton *mLightToolButton = new QToolButton( this );
  toolBar->addWidget( mLightToolButton );
  mLightToolButton->setPopupMode( QToolButton::InstantPopup );
  mLightToolButton->setIcon( QPixmap( QStringLiteral( ":/images/lightSettings.svg" ) ) );
  mLightToolButton->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  QMenu *lightMenu = new QMenu( this );
  mLightToolButton->setMenu( lightMenu );
  ReosLightWidget *lw = new ReosLightWidget( this );
  QWidgetAction *widgetAction = new QWidgetAction( lightMenu );
  widgetAction->setDefaultWidget( lw );
  lightMenu->addAction( widgetAction );

  lw->setDirection( lightSettings.direction().toVector3D() );

  connect( lw, &ReosLightWidget::directionChanged, this, [settings, lw]( const QVector3D & direction )
  {
    QgsDirectionalLightSettings lightSettings;
    lightSettings.setDirection( direction );
    lightSettings.setIntensity( lw->lightIntensity() );
    settings->setDirectionalLights( {lightSettings} );
  } );

  connect( lw, &ReosLightWidget::intensityChanged, this, [settings, lw]( float intensity )
  {
    QgsDirectionalLightSettings lightSettings;
    lightSettings.setIntensity( intensity );
    lightSettings.setDirection( lw->direction() );
    settings->setDirectionalLights( {lightSettings} );
  } );
}

Reos3dView::~Reos3dView()
{
  delete ui;
};
