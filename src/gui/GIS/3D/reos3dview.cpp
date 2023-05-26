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
#include <QTimer>

#include <3d/qgs3dmapcanvas.h>
#include <qgsmeshterraingenerator.h>
#include <qgsmeshlayer.h>
#include <qgs3dmapsettings.h>
#include <qgs3dmapscene.h>
#include <qgstemporalcontroller.h>
#include <qgsmeshlayer3drenderer.h>
#include <qgsflatterraingenerator.h>
#include <qgsdirectionallightsettings.h>

#include "reosmesh.h"
#include "reoslightwidget.h"
#include "reosverticalexaggerationwidget.h"
#include "reosstyleregistery.h"
#include "reos3dterrainsettingswidget.h"
#include "reos3dmapsettings.h"
#include "reosencodedelement.h"
#include "reosguicontext.h"
#include "reosmap.h"
#include "reosduration.h"

Reos3dView::Reos3dView( ReosMesh *meshTerrain, const ReosGuiContext &context )
  : ReosActionWidget( context.parent() )
  , ui( new Ui::Reos3dView )
  , mMeshTerrain( meshTerrain )
  , mActionZoomExtent( new QAction( QIcon( QStringLiteral( ":/images/zoomFullExtent.svg" ) ), tr( "Zoom to Full Extent" ), this ) )
{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );
  setWindowTitle( tr( "3D View" ) );

  mCanvas = new Qgs3DMapCanvas( this );
  mCanvas->setSizePolicy( QSizePolicy::Expanding, QSizePolicy::Expanding );

  QgsMeshLayer *meshLayer = nullptr;
  if ( meshTerrain )
    meshLayer = qobject_cast<QgsMeshLayer *>( meshTerrain->data() );

  Qgs3DMapSettings *settings = new Qgs3DMapSettings();
  if ( meshLayer )
    settings->setCrs( meshLayer->crs() );
  settings->setBackgroundColor( QColor( 119, 181, 254 ) );

  QgsRectangle extent;
  if ( meshLayer )
    extent = meshLayer->extent();
  float dist = static_cast< float >( std::max( extent.width(), extent.height() ) );
  settings->setOrigin( QgsVector3D( extent.center().x(), extent.center().y(), 0 ) );

  settings->setTemporalRange(
  {
    context.map()->currentTime(),
    context.map()->currentTime().addMSecs( context.map()->timeStep().valueMilliSecond() )
  } );

  mCanvas->setMap( settings );
  mCanvas->setViewFromTop( extent.center(), dist, 0 );
  mCanvas->setOnScreenNavigationVisibility( false );

  ui->m3dViewLayout->addWidget( mCanvas );

  QToolBar *toolBar = new QToolBar( this );
  ui->mToolBarLayout->addWidget( toolBar );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  toolBar->addAction( mActionZoomExtent );
  connect( mActionZoomExtent, &QAction::triggered, this, [this, meshLayer, settings]
  {
    QgsRectangle extent;
    if ( meshLayer )
      extent = meshLayer->extent();
    float dist = static_cast< float >( std::max( extent.width(), extent.height() ) );
    settings->setOrigin( QgsVector3D( extent.center().x(), extent.center().y(), 0 ) );
    mCanvas->setViewFromTop( extent.center(), dist, 0 );
  } );

  QToolButton *mLightToolButton = new QToolButton( this );
  toolBar->addWidget( mLightToolButton );
  mLightToolButton->setPopupMode( QToolButton::InstantPopup );
  mLightToolButton->setIcon( QIcon( QStringLiteral( ":/images/lightSettings.svg" ) ) );
  mLightToolButton->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );
  QMenu *lightMenu = new QMenu( this );
  mLightToolButton->setMenu( lightMenu );
  mLightWidget = new ReosLightWidget( this );
  QWidgetAction *widgetAction = new QWidgetAction( lightMenu );
  widgetAction->setDefaultWidget( mLightWidget );
  lightMenu->addAction( widgetAction );
  connect( mLightWidget, &ReosLightWidget::lightChanged, this, &Reos3dView::onLightChange );

  QToolButton *mExaggerationButton = new QToolButton( this );
  mExaggerationButton->setPopupMode( QToolButton::InstantPopup );
  mExaggerationButton->setIcon( QIcon( QStringLiteral( ":/images/verticalExaggeration.svg" ) ) );
  toolBar->addWidget( mExaggerationButton );
  QMenu *exaggerationMenu = new QMenu( this );
  mExaggerationButton->setMenu( exaggerationMenu );
  mExagerationWidget = new ReosVerticalExaggerationWidget( this );
  QWidgetAction *widgetExaggerationAction = new QWidgetAction( exaggerationMenu );
  widgetExaggerationAction->setDefaultWidget( mExagerationWidget );
  exaggerationMenu->addAction( widgetExaggerationAction );
  connect( mExagerationWidget, &ReosVerticalExaggerationWidget::valueChanged, this, &Reos3dView::onExaggerationChange );

  QToolButton *mTerrainSettingsButton = new QToolButton( this );
  mTerrainSettingsButton->setIcon( QIcon( QStringLiteral( ":/images/terrain3DSettings.svg" ) ) );
  mTerrainSettingsButton->setPopupMode( QToolButton::InstantPopup );
  toolBar->addWidget( mTerrainSettingsButton );
  QMenu *terrainSettingsMenu = new QMenu( this );
  mTerrainSettingsButton->setMenu( terrainSettingsMenu );

  mTerrainSettingsWidget = new Reos3DTerrainSettingsWidget( this );
  QWidgetAction *widgetWireframeAction = new QWidgetAction( terrainSettingsMenu );
  widgetWireframeAction->setDefaultWidget( mTerrainSettingsWidget );
  terrainSettingsMenu->addAction( widgetWireframeAction );
  connect( mTerrainSettingsWidget, &Reos3DTerrainSettingsWidget::terrainSettingsChanged, this, &Reos3dView::onTerrainSettingsChanged );

  mCanvas->setMinimumSize( QSize( 200, 200 ) );

  const QgsTemporalController *temporalController = qobject_cast<const QgsTemporalController *> ( context.map()->temporalController() );
  if ( temporalController )
    mCanvas->setTemporalController( const_cast< QgsTemporalController *>( temporalController ) );

  connect( mMeshTerrain, &ReosMesh::terrainSymbologyChanged, this, &Reos3dView::onTerrainSettingsChanged );
}

void Reos3dView::setMapSettings( const Reos3DMapSettings &map3DSettings, bool updateView )
{
  mLightWidget->setDirection( map3DSettings.lightDirection() );
  mLightWidget->setLightIntensity( map3DSettings.lightIntensity() );
  onLightChange();

  mExagerationWidget->blockSignals( true );
  mExagerationWidget->setExageration( map3DSettings.verticalExaggeration() );
  mExagerationWidget->blockSignals( false );

  if ( updateView )
    onExaggerationChange( map3DSettings.verticalExaggeration() );
}

Reos3DMapSettings Reos3dView::map3DSettings() const
{
  Reos3DMapSettings settings;

  settings.setLightDirection( mLightWidget->direction() );
  settings.setLightIntensity( mLightWidget->lightIntensity() );
  settings.setVerticalExaggeration( mExagerationWidget->exageration() );

  return settings;
}

void Reos3dView::setTerrainSettings( const Reos3DTerrainSettings &settings, bool updateView )
{
  mTerrainSettingsWidget->setTerrainSettings( settings );
  if ( updateView )
    onTerrainSettingsChanged();
}

Reos3DTerrainSettings Reos3dView::terrainSettings() const
{
  return mTerrainSettingsWidget->settings();
}

Reos3dView::~Reos3dView()
{
  delete ui;
  // in QGIS, update of terrain is done be recreating a new terrain when mesh change
  // this creation is defered by a QTimer::singleShot, to happen once back in the main loop
  // The problem is that some closing widget lead to change the terrain settings, so the creation
  // of new terrain happens when the 3D view is already deleted and leads to crash the application
  // The workaround is to defered also the destruction of the 3D map,
  //  so the destruction will happen after terrain creation
  // before launching the defered deletation, we remove reference to the mesh layer

  mCanvas->map()->setLayers( QList<QgsMapLayer *>() );
  mCanvas->map()->setTerrainGenerator( new QgsFlatTerrainGenerator );
  mCanvas->setParent( nullptr );
  QTimer::singleShot( 0, mCanvas, &QObject::deleteLater );

}

void Reos3dView::addMesh( ReosMesh *mesh )
{
  mMeshes.append( mesh );
  QList<QgsMapLayer *> layers = mCanvas->map()->layers();

  layers.append( qobject_cast<QgsMapLayer *>( mesh->data() ) );

  mCanvas->map()->setLayers( layers );
}

void Reos3dView::onExaggerationChange( double value )
{
  if ( !mMeshTerrain )
    return;

  for ( ReosMesh *mesh : std::as_const( mMeshes ) )
    if ( mesh )
    {
      mesh->setVerticaleSCale( value );
      mesh->update3DRenderer();
    }

  //! For now in QGIS, exaggeration is a terrain settings, so call the method relative to the terrain
  onTerrainSettingsChanged();
  emit mapSettingsChanged();
}

void Reos3dView::onLightChange()
{
  std::unique_ptr<QgsDirectionalLightSettings> lightSettings = std::make_unique<QgsDirectionalLightSettings>();
  lightSettings->setDirection( mLightWidget->direction() );
  lightSettings->setIntensity( mLightWidget->lightIntensity() );
  mCanvas->map()->setLightSources( {lightSettings.release()} );

  emit mapSettingsChanged();
}


void Reos3dView::onTerrainSettingsChanged()
{
  std::unique_ptr<QgsMesh3DSymbol> terrainSymbol = std::make_unique<QgsMesh3DSymbol>();

  terrainSymbol->setVerticalScale( mExagerationWidget->exageration() );
  Reos3DTerrainSettings terrainSettings = mTerrainSettingsWidget->settings();

  switch ( terrainSettings.renderingType() )
  {
    case Reos3DTerrainSettings::UniqueColor:
      terrainSymbol->setRenderingStyle( QgsMesh3DSymbol::SingleColor );
      break;
    case Reos3DTerrainSettings::ColorRamp:
      terrainSymbol->setRenderingStyle( QgsMesh3DSymbol::ColorRamp );
      break;
  }
  terrainSymbol->setSingleMeshColor( terrainSettings.uniqueColor() );
  terrainSymbol->setWireframeEnabled( terrainSettings.isWireframeEnabled() );
  terrainSymbol->setWireframeLineWidth( terrainSettings.wireframeWidth() );
  terrainSymbol->setWireframeLineColor( terrainSettings.wireframeColor() );
  terrainSymbol->setSmoothedTriangles( terrainSettings.isSmoothed() );

  if ( !mMeshTerrain )
    return;
  ReosEncodedElement terrainSymbology = mMeshTerrain->datasetScalarGroupSymbology( mMeshTerrain->verticesElevationDatasetId() );
  if ( terrainSymbology.description() != QStringLiteral( "dataset-symbology" ) )
    return;
  QString docString;
  terrainSymbology.getData( QStringLiteral( "symbology" ), docString );
  QDomDocument doc( QStringLiteral( "dataset-symbology" ) );
  if ( doc.setContent( docString ) )
  {
    QDomElement domElem = doc.firstChildElement( QStringLiteral( "scalar-settings" ) );
    const QDomElement elemShader = domElem.firstChildElement( QStringLiteral( "colorrampshader" ) );
    QgsReadWriteContext context;
    QgsColorRampShader colorRampShader;
    colorRampShader.readXml( elemShader, context );
    terrainSymbol->setColorRampShader( colorRampShader );
  };

  QgsMeshLayer *meshLayer = qobject_cast<QgsMeshLayer *>( mMeshTerrain->data() );
  std::unique_ptr<QgsMeshTerrainGenerator> newTerrain = std::make_unique<QgsMeshTerrainGenerator>();
  newTerrain->setSymbol( terrainSymbol.release() );
  newTerrain->setLayer( meshLayer );
  mCanvas->map()->setTerrainGenerator( newTerrain.release() );

  emit terrainSettingsChanged();
}
