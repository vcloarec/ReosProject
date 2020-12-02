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
#include "reosmap.h"
#include "reossettings.h"

#include <QBoxLayout>
#include <QMessageBox>
#include <QCloseEvent>

ReosDelineatingWatershedWidget::ReosDelineatingWatershedWidget( ReosWatershedDelineating *watershedDelineatingModule,
    ReosMap *map,
    QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosDelineatingWatershedWidget ),
  mModule( watershedDelineatingModule ),
  mMap( map ),
  mActionDrawDownstreamLine( new QAction( QPixmap( ":/images/downStreamSelection.png" ), tr( "Draw downstream line" ), this ) ),
  mActionDrawPredefinedExtent( new QAction( QPixmap( ":/images/extentWatershedSelection.png" ), tr( "Draw predefined extent" ), this ) ),
  mActionDrawAddBurningLine( new QAction( QPixmap( ":/images/burningLine.png" ), tr( "Add a burning line" ), this ) ),
  mActionRemoveBurningLine( new QAction( QPixmap( ":/images/mActionRemoveBurningLine.png" ), tr( "Remove a burning line" ), this ) ),
  mDownstreamLine( map ),
  mWatershedExtent( map ),
  mTemporaryWatershed( map ),
  mTemporaryStreamLine( map )
{
  ui->setupUi( this );
  ui->comboBoxDem->setGisEngine( map->engine() );

  setWindowFlag( Qt::Dialog );
  //setWindowFlag( Qt::WindowStaysOnTopHint );

  mAutomaticToolBar = new QToolBar;
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 2, mAutomaticToolBar );
  mAutomaticToolBar->addAction( mActionDrawDownstreamLine );
  mAutomaticToolBar->addAction( mActionDrawPredefinedExtent );
  mAutomaticToolBar->addAction( mActionDrawAddBurningLine );
  mAutomaticToolBar->addAction( mActionRemoveBurningLine );

  mMapToolDrawDownStreamLine = new ReosMapToolDrawPolyline( map );
  mMapToolDrawDownStreamLine->setAction( mActionDrawDownstreamLine );
  mActionDrawDownstreamLine->setCheckable( true );
  mMapToolDrawDownStreamLine->setStrokeWidth( 2 );
  mMapToolDrawDownStreamLine->setColor( Qt::darkGreen );
  mMapToolDrawDownStreamLine->setLineStyle( Qt::DashLine );

  mMapToolDrawPredefinedExtent = new ReosMapToolDrawExtent( map );
  mMapToolDrawPredefinedExtent->setAction( mActionDrawPredefinedExtent );
  mActionDrawPredefinedExtent->setCheckable( true );
  mMapToolDrawPredefinedExtent->setStrokeWidth( 2 );
  mMapToolDrawPredefinedExtent->setColor( Qt::red );
  mMapToolDrawPredefinedExtent->setFillColor( QColor( 150, 0, 0, 30 ) );
  mMapToolDrawPredefinedExtent->setLineStyle( Qt::DashLine );

  mMapToolDrawBurningLine = new ReosMapToolDrawPolyline( map );
  mMapToolDrawBurningLine->setAction( mActionDrawAddBurningLine );
  mActionDrawAddBurningLine->setCheckable( true );
  mMapToolDrawBurningLine->setStrokeWidth( 2 );
  mMapToolDrawBurningLine->setColor( Qt::red );
  mMapToolDrawBurningLine->setLineStyle( Qt::DashLine );

  mMapToolRemoveBurningLine = new ReosMapToolSelectMapItem( map, -1 );
  mMapToolRemoveBurningLine->setAction( mActionRemoveBurningLine );
  mActionRemoveBurningLine->setCheckable( true );
  connect( mMapToolRemoveBurningLine, &ReosMapToolSelectMapItem::found, this, &ReosDelineatingWatershedWidget::onBurningLineRemoved );

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
  connect( mMapToolDrawPredefinedExtent, &ReosMapToolDrawExtent::extentDrawn, this, &ReosDelineatingWatershedWidget::onPredefinedExtentDrawn );
  connect( mMapToolDrawBurningLine, &ReosMapToolDrawPolyline::polylineDrawn, this, &ReosDelineatingWatershedWidget::onBurningLineDrawn );
  connect( ui->comboBoxDem, &ReosDigitalElevationModelComboBox::currentDigitalElevationChanged, this, &ReosDelineatingWatershedWidget::onDemComboboxChanged );

  connect( ui->mPushButtonDelineate, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onDelineateAsked );
  connect( ui->pushButtonValidate, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onValidateAsked );

  connect( this, &QObject::destroyed, this, &ReosDelineatingWatershedWidget::storeGeometry );

  restore();
}

ReosDelineatingWatershedWidget::~ReosDelineatingWatershedWidget()
{
  delete ui;
}

void ReosDelineatingWatershedWidget::closeEvent( QCloseEvent *event )
{
  emit closed();
  storeGeometry();
  event->accept();
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
  mapExtent.setCrs( mMap->mapCrs() );
  mModule->setPreDefinedExtent( mapExtent );
  mWatershedExtent.resetPolygon( mapExtent.toPolygon() );
  updateTool();
}

void ReosDelineatingWatershedWidget::onBurningLineDrawn( const QPolygonF &burningLine )
{
  mBurningLines.emplace_back( std::make_unique<ReosMapPolyline>( mMap, burningLine ) );
  ReosMapPolyline *bl = mBurningLines.back().get();
  bl->setDescription( QStringLiteral( "Burning-line" ) );
  bl->setColor( Qt::red );
  bl->setExternalColor( Qt::white );
  bl->setWidth( 2 );
  bl->setExternalWidth( 4 );

  updateBurningLines();
}

void ReosDelineatingWatershedWidget::onBurningLineRemoved( ReosMapItem *item )
{
  size_t i = 0;
  bool found = false;
  while ( i < mBurningLines.size() && !found )
  {
    found = mBurningLines.at( i ).get() == item;
    if ( !found )
      ++i;
  }

  if ( found )
  {
    mBurningLines.erase( mBurningLines.begin() + i );
    updateBurningLines();
  }
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
    QMessageBox::critical( this, tr( "Delineating Watershed" ), tr( "Unable to validate this watershed: %1" ).arg( e.what() ) );
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
  ui->mCurrentDemWidget->setVisible( isAutomatic );
}

void ReosDelineatingWatershedWidget::storeGeometry()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/Windows/WatershedDelineateWidget/Geometry" ), saveGeometry() );
}

void ReosDelineatingWatershedWidget::restore()
{
  ReosSettings settings;
  restoreGeometry( settings.value( QStringLiteral( "/Windows/WatershedDelineateWidget/Geometry" ) ).toByteArray() );
}

void ReosDelineatingWatershedWidget::updateTool()
{
  switch ( mModule->currentState() )
  {
    case ReosWatershedDelineating::NoDigitalElevationModel:
      mActionDrawDownstreamLine->setEnabled( false );
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForDownstream:
      mActionDrawDownstreamLine->setEnabled( true );
      mMapToolDrawDownStreamLine->setCurrentToolInMap();
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mMapToolDrawPredefinedExtent->setCurrentToolInMap();
      mActionDrawPredefinedExtent->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingWithBroughtBackExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mMapToolDrawPredefinedExtent->setCurrentToolInMap();
      mActionDrawPredefinedExtent->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingforProceed:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawPredefinedExtent->setEnabled( !mModule->hasDirectionData() );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( true );
      ui->pushButtonValidate->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForValidate:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->pushButtonValidate->setEnabled( true );
      break;

  }
}

void ReosDelineatingWatershedWidget::updateBurningLines()
{
  QList<QPolygonF> list;
  for ( size_t i = 0; i < mBurningLines.size(); ++i )
  {
    list.append( mBurningLines.at( i ).get()->mapPolyline() );
  }

  mModule->setBurningLines( list );
}
