/***************************************************************************
  reosroughnesswidget.cpp - ReosRoughnessWidget

 ---------------------
 begin                : 17.3.2022
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
#include "reosroughnesswidget.h"
#include "ui_reosroughnesswidget.h"

#include <QUuid>
#include <QMessageBox>

#include "reosguicontext.h"
#include "reospolygonstructure.h"
#include "reoshydraulicstructure2d.h"
#include "reosroughnesswidget.h"
#include "reosstyleregistery.h"
#include "reosformwidget.h"

ReosRoughnessWidget::ReosRoughnessWidget( ReosHydraulicStructure2D *structure2D, const ReosGuiContext &guiContext )
  : QWidget( guiContext.parent() )
  , ui( new Ui::ReosRoughnessWidget )
  , mMap( guiContext.map() )
  , mStructure( structure2D->roughnessStructure() )
  , mActionEditRoughnessPolygons( new QAction( QPixmap( QStringLiteral( ":/images/editStructurePolygon.svg" ) ), tr( "Edit Roughness Polygons" ), this ) )
  , mMapStructureItem( mMap, mStructure->structure() )
{
  ui->setupUi( this );

  mToolBar = new QToolBar( this );
  mToolBar->layout()->setContentsMargins( 0, 0, 0, 0 );
  ui->mToolBarWidget->layout()->addWidget( mToolBar );

  ui->mDefaultRoughnessWidget->setDouble( mStructure->defaultRoughness() );
  mMapToolEditResolutionPolygon = new ReosMapToolEditPolygonStructure( mStructure->structure(), this, guiContext.map() );
  mMapToolEditResolutionPolygon->addHelperStructure( structure2D->geometryStructure() );
  mMapToolEditResolutionPolygon->setAction( mActionEditRoughnessPolygons );
  mActionEditRoughnessPolygons->setCheckable( true );

  mToolBar->addAction( mActionEditRoughnessPolygons );
  mToolBar->addActions( mMapToolEditResolutionPolygon->mainActions()->actions() );
  mToolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  connect( mStructure->structure(), &ReosDataObject::dataChanged, this, [this]
  {
    mMapStructureItem.updatePosition();
    mMap->refreshCanvas();
  } );

  mModel = new ReosGeometryStructureClassModelList( mStructure->structure(), this );
  ui->mRoughnessListView->setModel( mModel );

  connect( ui->mAddButton, &QToolButton::clicked, this, &ReosRoughnessWidget::addRougness );
  connect( ui->mRemoveButton, &QToolButton::clicked, this, &ReosRoughnessWidget::removeCurrentRoughness );
  connect( ui->mRoughnessListView->selectionModel(), &QItemSelectionModel::currentChanged, mMapToolEditResolutionPolygon,
           [this]( const QModelIndex & current, const QModelIndex & )
  {
    QString classId;
    if ( current.isValid() )
    {
      classId = mModel->classId( current.row() );
    }
    mMapToolEditResolutionPolygon->setCurrentClass( classId );
    mCurrentClass = classId;
    ui->mRemoveButton->setEnabled( !classId.isEmpty() );
  } );

  mMapStructureItem.setVisible( isVisible() );
}

ReosRoughnessWidget::~ReosRoughnessWidget()
{
  delete ui;
}

void ReosRoughnessWidget::hideEvent( QHideEvent *e )
{
  if ( !mStructure.isNull() )
    mMap->removeSnappableStructure( mStructure->structure() );
  mMapStructureItem.setVisible( false );

  mMapToolEditResolutionPolygon->quitMap();

  QWidget::hideEvent( e );
}

void ReosRoughnessWidget::showEvent( QShowEvent *e )
{
  if ( !mStructure.isNull() )
    mMap->addSnappableStructure( mStructure->structure() );
  mMapStructureItem.setVisible( true );

  mMapToolEditResolutionPolygon->activate();
  mMapToolEditResolutionPolygon->setCurrentToolInMap();
  QWidget::showEvent( e );
}

void ReosRoughnessWidget::addRougness()
{
  ReosParameterDouble paramValue( tr( "Manning coefficient:" ), false );
  paramValue.setDisplayPrecision( 3 );
  paramValue.setValue( 0.025 );
  ReosFormDialog *dial = new ReosFormDialog( this );
  dial->setWindowTitle( tr( "New roughness class" ) );
  dial->addText( tr( "Enter a value for this new roughness class" ) );
  dial->addParameter( &paramValue );

  if ( dial->exec() )
  {
    double value = paramValue.value();
    if ( mStructure->structure()->valueToClass( value ) != QString() )
    {
      QMessageBox::information( this, tr( "Add New Roughness" ), tr( "This roughness value already exists." ) );
    }
    else
    {
      QString classId = QUuid::createUuid().toString();
      mStructure->structure()->addClass( classId, paramValue.value() );
      ui->mRoughnessListView->setCurrentIndex( mModel->classToindex( classId ) );
    }

  }

  dial->deleteLater();
}

void ReosRoughnessWidget::removeCurrentRoughness()
{
  if ( mCurrentClass.isEmpty() )
    return;

  mStructure->structure()->removeClass( mCurrentClass );
  ui->mRoughnessListView->setCurrentIndex( mModel->index( 0, 0, QModelIndex() ) );
}
