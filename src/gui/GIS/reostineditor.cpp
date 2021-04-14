/***************************************************************************
  reostineditor.cpp - ReosTinEditor

 ---------------------
 begin                : 13.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reostineditor.h"

#include <QDialog>
#include <QDialogButtonBox>

#include <qgsmapcanvas.h>
#include <qgsprocessingtininputlayerswidget.h>

#include "reosmaptool.h"
#include "reosgisengine.h"

#include "reostineditormapitems_p.h"
#include "reostriangularirregularnetwork.h"
#include "reosprocesscontroler.h"

ReosTinEditor::ReosTinEditor( ReosGisEngine *gisEngine, ReosMap *map, QWidget *parentWidget ):
  ReosModule( gisEngine )
  , mGisEngine( gisEngine )
  , mParentWidget( parentWidget )
  , mActionAddVertex( new QAction( tr( "Add vertex to the current TIN" ), this ) )
  , mActionFromVectorLayer( new QAction( tr( "From vector layer" ), this ) )
  , mItems( map )
{
  ReosMapToolDrawPoint *mapToolAddVertex = new ReosMapToolDrawPoint( map );
  mapToolAddVertex->setAction( mActionAddVertex );
  mActionAddVertex->setCheckable( true );
  connect( mapToolAddVertex, &ReosMapToolDrawPoint::drawn, this, &ReosTinEditor::addVertex );

  connect( mActionFromVectorLayer, &QAction::triggered, this, &ReosTinEditor::fromVectorLayer );

  setCurrentLayer( QString() );
}

QList<QAction *> ReosTinEditor::actions() const
{
  QList<QAction *> ret;
  ret << mActionAddVertex << mActionFromVectorLayer;
  return ret;
}

void ReosTinEditor::setCurrentLayer( const QString &currentLayer )
{
  mCurrentTriangulation = mGisEngine->triangularIrregularNetWork( currentLayer );
  activate( mCurrentTriangulation != nullptr );

  mItems.setTriangulation( mCurrentTriangulation );
}

void ReosTinEditor::addVertex( const QPointF &vertex )
{
  if ( mCurrentTriangulation )
    mCurrentTriangulation->addVertex( {vertex.x(), vertex.y(), 1} );

  mItems.updateItems();
}

void ReosTinEditor::fromVectorLayer()
{
  QDialog *dial = new QDialog( mParentWidget );

  dial->setLayout( new QVBoxLayout );
  QgsProcessingTinInputLayersWidget *vectorLayerWidget = new QgsProcessingTinInputLayersWidget( QgsProject::instance() );
  vectorLayerWidget->setParent( dial );
  dial->layout()->addWidget( vectorLayerWidget );

  QDialogButtonBox *dialButton = new QDialogButtonBox( QDialogButtonBox::Ok | QDialogButtonBox::Cancel, Qt::Horizontal, dial );
  dial->layout()->addWidget( dialButton );
  connect( dialButton, &QDialogButtonBox::accepted, dial, &QDialog::accept );
  connect( dialButton, &QDialogButtonBox::rejected, dial, &QDialog::reject );

  if ( dial->exec() )
  {
    bool autoUpdate = mCurrentTriangulation->autoUpdate();
    mCurrentTriangulation->setAutoUpdate( false );
    ReosAddVectorLayersToTinProcess process( mCurrentTriangulation );
    process.setLayerData( vectorLayerWidget->value() );
    ReosProcessControler *controler = new ReosProcessControler( &process, dial );
    controler->exec();
    mCurrentTriangulation->setAutoUpdate( autoUpdate );
    emit mCurrentTriangulation->updated();
    mItems.updateItems();
  }

  dial->deleteLater();

}

void ReosTinEditor::activate( bool b )
{
  mActionAddVertex->setEnabled( b );
  mActionFromVectorLayer->setEnabled( b );
}

ReosTinEditorMapItems::ReosTinEditorMapItems( ReosMap *map ): ReosMapItem( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  if ( canvas )
  {
    d_ = new ReosTinEditorMapItems_p( canvas ); //the owner ship of d pointer is taken by the scene of the map canvas
    d_->base = this;

    d_->width = 3;
    d_->externalWidth = 6;
    d_->color = Qt::green;
    d_->externalColor = Qt::darkGreen;
  }
}

void ReosTinEditorMapItems::setTriangulation( ReosTriangularIrregularNetwork *triangulation )
{
  if ( !isMapExist() || !d_ )
    return;
  static_cast<ReosTinEditorMapItems_p *>( d_ )->mTriangulation = triangulation;
  d_->updatePosition();
  d_->update();
}

void ReosTinEditorMapItems::updateItems()
{
  d_->updatePosition();
  d_->update();
}
