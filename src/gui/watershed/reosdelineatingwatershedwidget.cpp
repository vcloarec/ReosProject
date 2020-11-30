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


#include "reosdelineatingwatershedwidget.h"
#include "ui_reosdelineatingwatershedwidget.h"
#include "reosprocesscontroler.h"

#include <QBoxLayout>
#include <QMessageBox>

ReosDelineatingWatershedWidget::ReosDelineatingWatershedWidget( ReosWatershedDelineating *watershedDelineatingModule, ReosGisEngine *gisEngine,
    ReosMap *map,
    QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosDelineatingWatershedWidget ),
  mModule( watershedDelineatingModule ),
  mMap( map ),
  mActionDrawDownstreamLine( new QAction( QPixmap( ":/images/downStreamSelection.png" ), tr( "Draw downstream line" ), this ) ),
  mActionDrawPredefinedExtent( new QAction( QPixmap( ":/images/extentWatershedSelection.png" ), tr( "Draw predefined extent" ), this ) ),
  mDownstreamLine( map ),
  mWatershedExtent( map ),
  mTemporaryWatershed( map ),
  mTemporaryStreamLine( map )
{
  ui->setupUi( this );
  ui->comboBoxDem->setGisEngine( gisEngine );

  setWindowFlag( Qt::Dialog );

  mAutomaticToolBar = new QToolBar;
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 1, mAutomaticToolBar );
  mAutomaticToolBar->addAction( mActionDrawDownstreamLine );
  mAutomaticToolBar->addAction( mActionDrawPredefinedExtent );

  mMapToolDrawDownStreamLine = new ReosMapToolDrawPolyline( map );
  mMapToolDrawDownStreamLine->setAction( mActionDrawDownstreamLine );
  mActionDrawDownstreamLine->setCheckable( true );
  connect( mActionDrawDownstreamLine, &QAction::triggered, [this]() {mMapToolDrawDownStreamLine->setCurrentToolInMap();} );
  mMapToolDrawDownStreamLine->setStrokeWidth( 2 );
  mMapToolDrawDownStreamLine->setColor( Qt::darkGreen );
  mMapToolDrawDownStreamLine->setLineStyle( Qt::DashLine );

  mMapToolDrawPredefinedExtent = new ReosMapToolDrawExtent( map );
  mMapToolDrawPredefinedExtent->setAction( mActionDrawPredefinedExtent );
  mActionDrawPredefinedExtent->setCheckable( true );
  connect( mActionDrawPredefinedExtent, &QAction::triggered, [this]() {mMapToolDrawPredefinedExtent->setCurrentToolInMap();} );
  mMapToolDrawPredefinedExtent->setStrokeWidth( 2 );
  mMapToolDrawPredefinedExtent->setColor( Qt::red );
  mMapToolDrawPredefinedExtent->setFillColor( QColor( 150, 0, 0, 30 ) );
  mMapToolDrawPredefinedExtent->setLineStyle( Qt::DashLine );

  mDownstreamLine.setColor( Qt::darkGreen );
  mDownstreamLine.setExternalColor( Qt::white );
  mDownstreamLine.setWidth( 2 );
  mDownstreamLine.setExternalWidth( 4 );

  mWatershedExtent.setColor( Qt::darkRed );
  mWatershedExtent.setExternalColor( Qt::white );
  mWatershedExtent.setWidth( 2 );
  mWatershedExtent.setExternalWidth( 4 );

  mTemporaryWatershed.setColor( QColor( 50, 200, 50 ) );
  mTemporaryWatershed.setExternalColor( Qt::white );
  mTemporaryWatershed.setWidth( 2 );
  mTemporaryWatershed.setExternalWidth( 4 );
  mTemporaryWatershed.setStyle( Qt::DashLine );

  mTemporaryStreamLine.setColor( Qt::blue );
  mTemporaryStreamLine.setExternalColor( Qt::white );
  mTemporaryStreamLine.setWidth( 2 );
  mTemporaryStreamLine.setExternalWidth( 4 );
  mTemporaryStreamLine.setStyle( Qt::DashLine );

  updateTool();


  connect( ui->mRadioButtonAutomatic, &QRadioButton::clicked, this, &ReosDelineatingWatershedWidget::onMethodChange );
  connect( ui->mRadioButtonManual, &QRadioButton::clicked, this, &ReosDelineatingWatershedWidget::onMethodChange );

  connect( mMapToolDrawDownStreamLine, &ReosMapToolDrawPolyline::polylineDrawn, this, &ReosDelineatingWatershedWidget::onDownstreamLineDrawn );
  connect( mMapToolDrawPredefinedExtent, & ReosMapToolDrawExtent::extentDrawn, this, &ReosDelineatingWatershedWidget::onPredefinedExtentDrawn );
  connect( ui->comboBoxDem, &ReosDigitalElevationModelComboBox::currentDigitalElevationChanged, this, &ReosDelineatingWatershedWidget::onDemComboboxChanged );

  connect( ui->mPushButtonDelineate, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onDelineateAsked );
  connect( ui->pushButtonValidate, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onValidateAsked );
}

ReosDelineatingWatershedWidget::~ReosDelineatingWatershedWidget()
{
  delete ui;
}

void ReosDelineatingWatershedWidget::onDownstreamLineDrawn( const QPolygonF &downstreamLine )
{
  if ( mModule->setDownstreamLine( downstreamLine ) )
  {
    mDownstreamLine.resetPolyline( downstreamLine );
    mWatershedExtent.resetPolygon();
  }
  updateTool();
}

void ReosDelineatingWatershedWidget::onPredefinedExtentDrawn( const QRectF &extent )
{
  ReosMapExtent mapExtent( extent );
  mModule->setPreDefinedExtent( mapExtent );
  mWatershedExtent.resetPolygon( mapExtent.toPolygon() );
  updateTool();
}

void ReosDelineatingWatershedWidget::onDemComboboxChanged()
{
  QString currentDemId = ui->comboBoxDem->currentDemLayerId();
  mModule->setDigitalElevationModelDEM( currentDemId );
  updateTool();
}

void ReosDelineatingWatershedWidget::onDelineateAsked()
{
  if ( !mModule->startDelineating() )
    return;
  ReosProcessControler *controler = new ReosProcessControler( mModule->delineatingProcess(), this );
  controler->exec();
  controler->deleteLater();
  updateTool();
  if ( !mModule->isDelineatingFinished() )
    return;

  mTemporaryWatershed.resetPolygon( mModule->lastWatershedDelineated() );
  mTemporaryStreamLine.resetPolyline( mModule->lastStreamLine() );

}

void ReosDelineatingWatershedWidget::onValidateAsked()
{
  try
  {
    mModule->validateWatershed();
  }
  catch ( ReosWatershedException &e )
  {
    QMessageBox::critical( this, tr( "Delineating Watershed" ), tr( "Unable to validate this watershed :%1" ).arg( e.what() ) );
  }

  mTemporaryWatershed.resetPolygon();
  mTemporaryStreamLine.resetPolyline();
  mDownstreamLine.resetPolyline();
  mWatershedExtent.resetPolygon();

  updateTool();
}

void ReosDelineatingWatershedWidget::onMethodChange()
{
  bool isAutomatic = ui->mRadioButtonAutomatic->isChecked() ;
  mAutomaticToolBar->setVisible( isAutomatic );
  ui->mPushButtonDelineate->setVisible( isAutomatic );
  ui->mCuurentDemWidget->setVisible( isAutomatic );
}

void ReosDelineatingWatershedWidget::updateTool()
{
  switch ( mModule->currentState() )
  {
    case ReosWatershedDelineating::NoDigitalElevationModel:
      mActionDrawDownstreamLine->setEnabled( false );
      mActionDrawPredefinedExtent->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForDownstream:
      mActionDrawDownstreamLine->setEnabled( true );
      mMapToolDrawDownStreamLine->setCurrentToolInMap();
      mActionDrawPredefinedExtent->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mMapToolDrawPredefinedExtent->setCurrentToolInMap();
      mActionDrawPredefinedExtent->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingWithBroughtBackExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mMapToolDrawPredefinedExtent->setCurrentToolInMap();
      mActionDrawPredefinedExtent->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingforProceed:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawPredefinedExtent->setEnabled( !mModule->hasDirectionData() );
      ui->mPushButtonDelineate->setEnabled( true );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForValidate:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawPredefinedExtent->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( true );
      break;

  }
}
