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
#include <QMessageBox>

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
  , mActionEditResolutionPolygons( new QAction( QPixmap( QStringLiteral( ":/images/editStructurePolygon.svg" ) ), tr( "Edit Resolution Polygons" ), this ) )
  , mMapStructureItem( mMap, mController->resolutionPolygons() )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  mToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBarWidget->layout()->addWidget( mToolBar );

  ui->mDefaultSizeParameterWidget->setDouble( mController->defaultSize() );
  mMapToolEditResolutionPolygon = new ReosMapToolEditPolygonStructure( mController->resolutionPolygons(), this, guiContext.map() );
  mMapToolEditResolutionPolygon->addHelperStructure( structure2D->geometryStructure() );
  mMapToolEditResolutionPolygon->setAction( mActionEditResolutionPolygons );
  mActionEditResolutionPolygons->setCheckable( true );

  mToolBar->addAction( mActionEditResolutionPolygons );
  mToolBar->addActions( mMapToolEditResolutionPolygon->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  connect( mController->resolutionPolygons(), &ReosDataObject::dataChanged, this, [this]
  {
    mMapStructureItem.updatePosition();
    mMap->refreshCanvas();
  } );

  mModel = new ReosGeometryStructureClassModelList( mController->resolutionPolygons(), this );
  ui->mPolygonClassView->setModel( mModel );

  connect( ui->mToolButtonAddClass, &QToolButton::clicked, this, &ReosGmshResolutionControllerWidget::addClass );
  connect( ui->mToolButtonRemoveClass, &QToolButton::clicked, this, &ReosGmshResolutionControllerWidget::removeCurrentClass );
  connect( ui->mPolygonClassView->selectionModel(), &QItemSelectionModel::currentChanged, mMapToolEditResolutionPolygon,
           [this]( const QModelIndex & current, const QModelIndex & )
  {
    QString classId;
    if ( current.isValid() )
    {
      classId = mModel->classId( current.row() );
    }
    mMapToolEditResolutionPolygon->setCurrentClass( classId );
    mCurrentClass = classId;
    ui->mToolButtonRemoveClass->setEnabled( !classId.isEmpty() );
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

void ReosGmshResolutionControllerWidget::hideEvent( QHideEvent *e )
{
  if ( !mController.isNull() )
    mMap->removeSnappableStructure( mController->resolutionPolygons() );
  mMapStructureItem.setVisible( false );

  mMapToolEditResolutionPolygon->quitMap();

  QWidget::hideEvent( e );
}

void ReosGmshResolutionControllerWidget::showEvent( QShowEvent *e )
{
  if ( !mController.isNull() )
    mMap->addSnappableStructure( mController->resolutionPolygons() );
  mMapStructureItem.setVisible( true );

  mMapToolEditResolutionPolygon->activate();
  mMapToolEditResolutionPolygon->setCurrentToolInMap();
  QWidget::showEvent( e );
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
    double value = paramValue.value();
    if ( mController->resolutionPolygons()->valueToClass( value ) != QString() )
    {
      QMessageBox::information( this, tr( "Add New Size Class" ), tr( "This size value already exists." ) );
    }
    else
    {
      QString classId = QUuid::createUuid().toString();
      mController->resolutionPolygons()->addClass( classId, paramValue.value() );
      ui->mPolygonClassView->setCurrentIndex( mModel->classToindex( classId ) );
    }
  }

  dial->deleteLater();
}

void ReosGmshResolutionControllerWidget::removeCurrentClass()
{
  if ( mCurrentClass.isEmpty() )
    return;

  mController->resolutionPolygons()->removeClass( mCurrentClass );
  ui->mPolygonClassView->setCurrentIndex( mModel->index( 0, 0, QModelIndex() ) );
}
