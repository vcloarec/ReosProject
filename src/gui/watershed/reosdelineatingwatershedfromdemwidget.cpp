/***************************************************************************
                      reosdelineatingwatershedfromdemwidget.cpp
                     --------------------------------------
Date                 : October-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/


#include "reosdelineatingwatershedfromdemwidget.h"
#include "ui_reosdelineatingwatershedfromdemwidget.h"

#include <QBoxLayout>

ReosDelineatingWatershedFromDemWidget::ReosDelineatingWatershedFromDemWidget( ReosWatershedDelineating *watershedDelineatingModule, ReosGisEngine *gisEngine,
    ReosMap *map,
    QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosDelineatingWatershedFromDemWidget ),
  mModule( watershedDelineatingModule ),
  mActionDrawDownstreamLine( new QAction( QPixmap( ":/images/downStreamSelection.png" ), tr( "Draw downstream line" ), this ) ),
  mActionDrawnPredefinedExtent( new QAction( QPixmap( ":/images/extentWatershedSelection.png" ), tr( "Draw predefined extent" ), this ) )
{
  ui->setupUi( this );

  ui->comboBoxDem->setGisEngine( gisEngine );

  QToolBar *toolBar = new QToolBar;
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 0, toolBar );
  toolBar->addAction( mActionDrawDownstreamLine );
  toolBar->addAction( mActionDrawnPredefinedExtent );

  mMapToolDrawDownStreamLine = new ReosMapToolDrawPolyline( map );
  mActionDrawDownstreamLine->setCheckable( true );
  connect( mActionDrawDownstreamLine, &QAction::triggered, [this]() {mMapToolDrawDownStreamLine->setCurrentToolInMap();} );
  mMapToolDrawDownStreamLine->setStrokeWidth( 3 );
  mMapToolDrawDownStreamLine->setColor( Qt::darkGreen );
  mMapToolDrawDownStreamLine->setLineStyle( Qt::DashLine );

  updateToolButton();

  connect( mMapToolDrawDownStreamLine, &ReosMapToolDrawPolyline::polylineDrawn, this, &ReosDelineatingWatershedFromDemWidget::onDownstreamLineDrawn );
  connect( ui->comboBoxDem, &ReosDigitalElevationModelComboBox::currentDigitalElevationChanged, this, &ReosDelineatingWatershedFromDemWidget::onDemComboboxChanged );
}

ReosDelineatingWatershedFromDemWidget::~ReosDelineatingWatershedFromDemWidget()
{
  delete ui;
}

void ReosDelineatingWatershedFromDemWidget::onDownstreamLineDrawn( const QPolygonF &downstreamLine )
{
  mModule->setDownstreamLine( downstreamLine );
  updateToolButton();
}

void ReosDelineatingWatershedFromDemWidget::onDemComboboxChanged()
{
  QString currentDemId = ui->comboBoxDem->currentDemLayerId();
  mModule->setDigitalElevationModelDEM( currentDemId );
  updateToolButton();
}

void ReosDelineatingWatershedFromDemWidget::updateToolButton()
{
  switch ( mModule->currentState() )
  {
    case ReosWatershedDelineating::NoDigitalElevationModel:
      mActionDrawDownstreamLine->setEnabled( false );
      mActionDrawnPredefinedExtent->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForDownstream:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawnPredefinedExtent->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawnPredefinedExtent->setEnabled( true );
      break;
    case ReosWatershedDelineating::WaitingWithBroughtBackExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawnPredefinedExtent->setEnabled( true );
      break;
    case ReosWatershedDelineating::WaitingforProceed:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawnPredefinedExtent->setEnabled( !mModule->hasDirectionData() );
      break;
    case ReosWatershedDelineating::WaitingForValidate:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawnPredefinedExtent->setEnabled( false );
      break;

  }
}
