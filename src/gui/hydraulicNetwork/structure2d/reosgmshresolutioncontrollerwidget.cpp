/***************************************************************************
  reosgmshresolutioncontroller.cpp - ReosGmshResolutionController

 ---------------------
 begin                : 31.1.2022
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
#include "reosgmshresolutioncontrollerwidget.h"
#include "ui_reosgmshresolutioncontroller.h"

#include "reosguicontext.h"
#include "reosmap.h"
#include "reosmaptooleditgeometrystructure.h"
#include "reoshydraulicstructure2d.h"
#include "reosstyleregistery.h"

ReosGmshResolutionControllerWidget::ReosGmshResolutionControllerWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &guiContext )
  :  QWidget( guiContext.parent() )
  ,  ui( new Ui::ReosGmshResolutionControllerWidget )
  , mMap( guiContext.map() )
  , mController( static_cast<ReosGmshResolutionController *>( structure2D->meshResolutionController() ) )
  , mActionEditResolutionPolygons( new QAction( QPixmap( QStringLiteral( ":/images/editStructureLines.svg" ) ), tr( "Edit Resolution Polygons" ), this ) )
  , mMapStructureItem( mMap, mController->resolutionPolygons() )
{
  ui->setupUi( this );

  QToolBar *toolBar = new QToolBar( this );
  ui->mToolBarWidget->layout()->addWidget( toolBar );

  ui->mDefaultSizeParameterWidget->setDouble( mController->defaultSize() );
  mMapToolEditResolutionPolygon = new ReosMapToolEditPolygonStructure( mController->resolutionPolygons(), this, guiContext.map() );
  mMapToolEditResolutionPolygon->setAction( mActionEditResolutionPolygons );
  mActionEditResolutionPolygons->setCheckable( true );

  toolBar->addAction( mActionEditResolutionPolygons );
  toolBar->addActions( mMapToolEditResolutionPolygon->mainActions()->actions() );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  connect( mController->resolutionPolygons(), &ReosDataObject::dataChanged, this, [this]
  {
    mMapStructureItem.updatePosition();
  } );

  mMapStructureItem.setVisible( isVisible() );
}

ReosGmshResolutionControllerWidget::~ReosGmshResolutionControllerWidget()
{
  delete ui;
}

void ReosGmshResolutionControllerWidget::hideEvent( QHideEvent * )
{
  mMap->removeSnappableStructure( mController->resolutionPolygons() );
  mMapStructureItem.setVisible( false );
}

void ReosGmshResolutionControllerWidget::showEvent( QShowEvent * )
{
  mMap->addSnappableStructure( mController->resolutionPolygons() );
  mMapStructureItem.setVisible( true );
}
