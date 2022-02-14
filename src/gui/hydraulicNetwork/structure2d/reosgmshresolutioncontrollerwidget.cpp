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

#include <QUuid>

#include "reosguicontext.h"
#include "reosmap.h"
#include "reosmaptooleditgeometrystructure.h"
#include "reoshydraulicstructure2d.h"
#include "reosstyleregistery.h"
#include "reosformwidget.h"

ReosGmshResolutionControllerWidget::ReosGmshResolutionControllerWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &guiContext )
  :  QWidget( guiContext.parent() )
  ,  ui( new Ui::ReosGmshResolutionControllerWidget )
  , mMap( guiContext.map() )
  , mController( static_cast<ReosMeshResolutionController *>( structure2D->meshResolutionController() ) )
  , mActionEditResolutionPolygons( new QAction( QPixmap( QStringLiteral( ":/images/editStructureLines.svg" ) ), tr( "Edit Resolution Polygons" ), this ) )
  , mMapStructureItem( mMap, mController->resolutionPolygons() )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  ui->mToolBarWidget->layout()->addWidget( mToolBar );

  ui->mDefaultSizeParameterWidget->setDouble( mController->defaultSize() );
  mMapToolEditResolutionPolygon = new ReosMapToolEditPolygonStructure( mController->resolutionPolygons(), this, guiContext.map() );
  mMapToolEditResolutionPolygon->setAction( mActionEditResolutionPolygons );
  mActionEditResolutionPolygons->setCheckable( true );

  mToolBar->addAction( mActionEditResolutionPolygons );
  mToolBar->addActions( mMapToolEditResolutionPolygon->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  connect( mController->resolutionPolygons(), &ReosDataObject::dataChanged, this, [this]
  {
    mMapStructureItem.updatePosition();
  } );

  ReosGeometryStructureClassModelList *model = new ReosGeometryStructureClassModelList( mController->resolutionPolygons(), this );
  ui->mPolygonClassView->setModel( model );

  connect( ui->mToolButtonAddClass, &QToolButton::clicked, this, &ReosGmshResolutionControllerWidget::addClass );
  connect( ui->mPolygonClassView->selectionModel(), &QItemSelectionModel::currentChanged, mMapToolEditResolutionPolygon,
           [this, model]( const QModelIndex & current, const QModelIndex & )
  {
    QString classId;
    if ( current.isValid() )
    {
      classId = model->classId( current.row() );
    }
    mMapToolEditResolutionPolygon->setCurrentClass( classId );
  } );

  mMapStructureItem.setVisible( isVisible() );
}

ReosGmshResolutionControllerWidget::~ReosGmshResolutionControllerWidget()
{
  delete ui;
}

void ReosGmshResolutionControllerWidget::addToolBarActions( const QList<QAction *> actions )
{
  mToolBar->addActions( actions );
}

void ReosGmshResolutionControllerWidget::hideEvent( QHideEvent * )
{
  if ( !mController.isNull() )
    mMap->removeSnappableStructure( mController->resolutionPolygons() );
  mMapStructureItem.setVisible( false );
}

void ReosGmshResolutionControllerWidget::showEvent( QShowEvent * )
{
  if ( !mController.isNull() )
    mMap->addSnappableStructure( mController->resolutionPolygons() );
  mMapStructureItem.setVisible( true );
}

void ReosGmshResolutionControllerWidget::addClass()
{
  ReosParameterDouble paramValue( tr( "Element size:" ), false );
  paramValue.setValue( 10 );
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "New resolution class" ) );
  dial->addText( tr( "Enter a value for this new resolution class" ) );
  dial->addParameter( &paramValue );

  if ( dial->exec() )
  {
    mController->resolutionPolygons()->addClass( QUuid::createUuid().toString(), paramValue.value() );
  }

  dial->deleteLater();
}
