/***************************************************************************
  reosedithydraulicstructure2dwidget.cpp - ReosEditHydraulicStructure2DWidget

 ---------------------
 begin                : 10.1.2022
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
#include "reosedithydraulicstructure2dwidget.h"
#include "ui_reosedithydraulicstructure2dwidget.h"

#include <QToolBar>
#include <QPushButton>
#include <QMessageBox>

#include "reosmeshgeneratorgui.h"
#include "reosmaptooleditgeometrystructure.h"
#include "reoshydraulicstructure2d.h"
#include "reosstyleregistery.h"
#include "reosgmshresolutioncontrollerwidget.h"
#include "reoseditpolylinestructurewidget.h"
#include "reossettings.h"
#include "reosprocesscontroler.h"
#include "reosmeshscalarrenderingwidget.h"
#include "reosmeshtopographywidget.h"
#include "reoseditmeshelementwidget.h"
#include "reosroughnesswidget.h"
#include "reoshydraulic2dsimulationwidget.h"


ReosEditHydraulicStructure2DWidget::ReosEditHydraulicStructure2DWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &context )
  : ReosStackedPageWidget( context.parent() )
  , ui( new Ui::ReosEditStructure2DWidget )
  , mMap( context.map() )
  , mStructure2D( structure2D )
  , mMapStructureItem( context.map(), structure2D->geometryStructure() )
{
  ui->setupUi( this );

  QToolBar *propertiesToolBar = new QToolBar( this );
  propertiesToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mPropertiesToolBarLayout->addWidget( propertiesToolBar );
  QPushButton *backButton = new QPushButton( propertiesToolBar );
  backButton->setIcon( QPixmap( QStringLiteral( ":/images/back.svg" ) ) );
  backButton->setToolTip( tr( "Back to Previous Page" ) );
  propertiesToolBar->addWidget( backButton );
  propertiesToolBar->addActions( context.actions() );
  propertiesToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  backButton->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  //mesh generation setup
  QList<QAction *> meshGenerationToolBarActions;
  QAction *actionGenerateMesh = new QAction( QPixmap( QStringLiteral( ":/images/generateMesh.svg" ) ), tr( "Generate Mesh" ), this );
  connect( actionGenerateMesh, &QAction::triggered, this, &ReosEditHydraulicStructure2DWidget::generateMesh );
  meshGenerationToolBarActions.append( actionGenerateMesh );
  meshGenerationToolBarActions.append( new ReosParameterWidgetAction( structure2D->meshGenerator()->autoUpdateParameter(), this ) );

  ReosEditPolylineStructureWidget *structureWidget = new ReosEditPolylineStructureWidget( structure2D->geometryStructure(), ReosGuiContext( context, this ) );
  structureWidget->addToolBarActions( meshGenerationToolBarActions );
  structureWidget->setSettingsWidget( ReosFormWidgetFactories::instance()->createDataFormWidget( structure2D->meshGenerator(), ReosGuiContext( context, structureWidget ) ) );
  structureWidget->setInformationWidget( new ReosStructureInformationWidget( structure2D, structureWidget ) );
  ui->pageMeshStructure->layout()->addWidget( structureWidget );
  connect( structureWidget, &ReosEditPolylineStructureWidget::boundaryConditionSelectionChanged, this, [this] {mMapStructureItem.updatePosition();} );

  ReosGmshResolutionControllerWidget *resolutionWidget = new ReosGmshResolutionControllerWidget( structure2D, ReosGuiContext( context, this ) );
  resolutionWidget->addToolBarActions( meshGenerationToolBarActions );
  ui->pageMeshResolution->layout()->addWidget( resolutionWidget );

  ReosMeshTopographyStackedWidget *topographyWidget =
    new ReosMeshTopographyStackedWidget( structure2D->mesh(), structure2D->topographyCollecion(), structure2D->terrainMeshDatasetId(), ReosGuiContext( context, this ) );
  ui->pageTopography->layout()->addWidget( topographyWidget );

  mEditElementWidget = new ReosEditMeshElementWidget( structure2D->mesh(), ReosGuiContext( context, this ) );
  ui->pageEditElements->layout()->addWidget( mEditElementWidget );

  ReosRoughnessWidget *roughnessWidget = new ReosRoughnessWidget( structure2D, ReosGuiContext( context, this ) );
  ui->pageRoughness->layout()->addWidget( roughnessWidget );

  ReosHydraulic2DSimulationWidget *mSimulationWidget = new ReosHydraulic2DSimulationWidget( structure2D, this );
  ui->pageSimulation->layout()->addWidget( mSimulationWidget );

  mInitialMapStructureItem = context.mapItems( ReosHydraulicStructure2D::staticType() );

  connect( structure2D->geometryStructure(), &ReosDataObject::dataChanged, this, [this]
  {
    mMapStructureItem.updatePosition();
    mInitialMapStructureItem->updatePosition();
  } );

  connect( backButton, &QPushButton::clicked, this, &ReosStackedPageWidget::backToPreviousPage );
  connect( ui->mOptionListWidget, &QListWidget::currentRowChanged, this, &ReosEditHydraulicStructure2DWidget::onMeshOptionListChanged );
  connect( structure2D, &ReosDataObject::dataChanged, this, [this] {mMap->refreshCanvas();} );

  ReosSettings settings;
  ui->mOptionListWidget->setCurrentRow( settings.value( QStringLiteral( "/hydraulic-structure/edit-widget/current-row" ) ).toInt() );

  mIsWireFrameActiveBefore = mStructure2D->mesh()->isWireFrameActive();
}

ReosEditHydraulicStructure2DWidget::~ReosEditHydraulicStructure2DWidget()
{
  delete ui;
}

void ReosEditHydraulicStructure2DWidget::onMeshOptionListChanged( int row )
{
  mStructure2D->deactivateMeshScalar();

  ui->mStackedWidget->setCurrentIndex( row );
  switch ( row )
  {
    case 0:
      mMapStructureItem.setLineWidth( 5 );
      break;
    case 1:
      mMapStructureItem.setLineWidth( 2 );
      break;
    case 2:
      mMapStructureItem.setLineWidth( 2 );
      mStructure2D->activateMeshTerrain();
      break;
    case 3:
      mMapStructureItem.setLineWidth( 3 );
      if ( mEditElementWidget->topographyDisplayed() )
        mStructure2D->activateMeshTerrain();
      break;
    case 4:
      mMapStructureItem.setLineWidth( 2 );
      break;
    default:
      break;
  }

  ReosSettings settings;
  settings.setValue( QStringLiteral( "/hydraulic-structure/edit-widget/current-row" ), row );
}

void ReosEditHydraulicStructure2DWidget::generateMesh()
{

  if ( mStructure2D->hasResults() )
  {
    if ( QMessageBox::warning( this, tr( "Generate Mesh" ),
                               tr( "If you generate a new mesh, existing results will not be compatible\n"
                                   "anymore and will be removed.\n"
                                   "\n"
                                   "Do you want to continue?" ),
                               QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
    {
      return;
    }
  }

  QApplication::setOverrideCursor( Qt::WaitCursor );
  mStructure2D->removeAllResults();
  QApplication::restoreOverrideCursor();

  std::unique_ptr<ReosMeshGeneratorProcess> generatorProcess( mStructure2D->getGenerateMeshProcess() );

  ReosProcessControler *controler = new ReosProcessControler( generatorProcess.get(), this );
  controler->exec();
  controler->deleteLater();
}

void ReosEditHydraulicStructure2DWidget::showEvent( QShowEvent *e )
{
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( false );

  mStructure2D->mesh()->activateWireFrame( true );

  ReosStackedPageWidget::showEvent( e );
}

void ReosEditHydraulicStructure2DWidget::hideEvent( QHideEvent *e )
{
  if ( mInitialMapStructureItem )
    mInitialMapStructureItem->setVisible( true );

  mStructure2D->mesh()->activateWireFrame( mIsWireFrameActiveBefore );

  if ( mStructure2D->mesh()->isFrameModified() && mStructure2D->hasResults() )
  {
    if ( QMessageBox::warning( this, tr( "Model Structure Modified" ),
                               tr( "As the frame of the mesh has been modified, if you keep these changes,\n"
                                   " existing results will not be compatible anymore and will be removed.\n"
                                   "\n"
                                   "Do you want to keep the mesh modification and remove results?" ),
                               QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::Yes )
    {
      QApplication::setOverrideCursor( Qt::WaitCursor );
      mStructure2D->removeAllResults();
      mStructure2D->mesh()->stopFrameEditing( true );
      QApplication::restoreOverrideCursor();
    }
    else
    {
      mStructure2D->mesh()->stopFrameEditing( false );
    }
  }
  else
  {
    mStructure2D->mesh()->stopFrameEditing( false );
  }

  emit hidden();
  ReosStackedPageWidget::hideEvent( e );
}


ReosStructureInformationWidget::ReosStructureInformationWidget( ReosHydraulicStructure2D *structure, QWidget *parent )
  : QWidget( parent )
  , mHydraulicStructure( structure )
{
  QGridLayout *lay = new QGridLayout( this );
  setLayout( lay );

  lay->addWidget( new QLabel( tr( "Vertices count" ), this ), 0, 0 );
  mLabelVerticesCount = new QLabel( QString( '-' ), this );
  lay->addWidget( mLabelVerticesCount, 0, 1 );
  lay->addWidget( new QLabel( tr( "Faces count" ), this ), 1, 0 );
  mLabelFacesCount = new QLabel( QString( '-' ), this );
  lay->addWidget( mLabelFacesCount, 1, 1 );

  connect( mHydraulicStructure, &ReosDataObject::dataChanged, this, &ReosStructureInformationWidget::update );

  update();
}

void ReosStructureInformationWidget::update()
{
  if ( mHydraulicStructure.isNull() )
    return;

  mLabelVerticesCount->setText( QLocale().toString( mHydraulicStructure->mesh()->vertexCount() ) );
  mLabelFacesCount->setText( QLocale().toString( mHydraulicStructure->mesh()->faceCount() ) );
}
