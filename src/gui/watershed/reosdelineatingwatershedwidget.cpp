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
#include "reoswatershedmodule.h"
#include "ui_reosdelineatingwatershedwidget.h"
#include "reosprocesscontroler.h"
#include "reosmap.h"
#include "reossettings.h"

#include <QBoxLayout>
#include <QMessageBox>
#include <QCloseEvent>

ReosDelineatingWatershedWidget::ReosDelineatingWatershedWidget( ReosWatershedModule *watershedModule,
    ReosMap *map,
    QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosDelineatingWatershedWidget ),
  mModule( watershedModule ),
  mMap( map ),
  mActionDrawDownstreamLine( new QAction( QPixmap( ":/images/downStreamSelection.png" ), tr( "Draw downstream line" ), this ) ),
  mActionDrawPredefinedExtent( new QAction( QPixmap( ":/images/extentWatershedSelection.png" ), tr( "Draw predefined extent" ), this ) ),
  mActionDrawAddBurningLine( new QAction( QPixmap( ":/images/burningLine.png" ), tr( "Add a burning line" ), this ) ),
  mActionRemoveBurningLine( new QAction( QPixmap( ":/images/mActionRemoveBurningLine.png" ), tr( "Remove a burning line" ), this ) ),
  mDownstreamLine( map ),
  mWatershedExtent( map ),
  mActionDrawWatershed( new QAction( tr( "Draw watershed manually" ), this ) ),
  mTemporaryAutomaticWatershed( map ),
  mTemporaryAutomaticStreamLine( map ),
  mTemporaryManualWatershed( map ),
  mTemporaryManualOutletPoint( map )
{
  ui->setupUi( this );
  ui->comboBoxDem->setGisEngine( map->engine() );

  setWindowFlag( Qt::Dialog );

  mAutomaticToolBar = new QToolBar;
  mManualToolBar = new QToolBar;
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 2, mAutomaticToolBar );
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 2, mManualToolBar );
  mAutomaticToolBar->addAction( mActionDrawDownstreamLine );
  mAutomaticToolBar->addAction( mActionDrawPredefinedExtent );
  mAutomaticToolBar->addAction( mActionDrawAddBurningLine );
  mAutomaticToolBar->addAction( mActionRemoveBurningLine );

  mManualToolBar->addAction( mActionDrawWatershed );

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

  mTemporaryAutomaticWatershed.setColor( QColor( 50, 200, 50 ) );
  mTemporaryAutomaticWatershed.setExternalColor( Qt::white );
  mTemporaryAutomaticWatershed.setWidth( 2 );
  mTemporaryAutomaticWatershed.setExternalWidth( 4 );
  mTemporaryAutomaticWatershed.setStyle( Qt::DashLine );

  mTemporaryAutomaticStreamLine.setColor( Qt::blue );
  mTemporaryAutomaticStreamLine.setExternalColor( Qt::white );
  mTemporaryAutomaticStreamLine.setWidth( 2 );
  mTemporaryAutomaticStreamLine.setExternalWidth( 4 );
  mTemporaryAutomaticStreamLine.setStyle( Qt::DashLine );

  mMapToolDrawWatershed = new ReosMapToolDrawPolygon( map );
  mMapToolDrawWatershed->setAction( mActionDrawWatershed );
  mActionDrawWatershed->setCheckable( true );
  mMapToolDrawWatershed->setStrokeWidth( 2 );
  mMapToolDrawWatershed->setColor( Qt::darkGreen );
  mMapToolDrawWatershed->setLineStyle( Qt::DashLine );
  mMapToolDrawWatershed->setFillColor( QColor( 0, 150, 0, 50 ) );

  mMapToolDrawOutletPoint = new ReosMapToolDrawPoint( map );

  mTemporaryManualWatershed.setColor( QColor( 50, 250, 50 ) );
  mTemporaryManualWatershed.setExternalColor( Qt::white );
  mTemporaryManualWatershed.setWidth( 2 );
  mTemporaryManualWatershed.setExternalWidth( 4 );
  mTemporaryManualWatershed.setStyle( Qt::DashLine );

  mTemporaryManualOutletPoint.setWidth( 8 );
  mTemporaryManualOutletPoint.setExternalWidth( 12 );
  mTemporaryManualOutletPoint.setColor( QColor( 0, 150, 250 ) );
  mTemporaryManualOutletPoint.setExternalColor( Qt::white );

  updateAutomaticTool();

  connect( ui->mRadioButtonAutomatic, &QRadioButton::clicked, this, &ReosDelineatingWatershedWidget::onMethodChange );
  connect( ui->mRadioButtonManual, &QRadioButton::clicked, this, &ReosDelineatingWatershedWidget::onMethodChange );

  connect( mMapToolDrawDownStreamLine, &ReosMapToolDrawPolyline::drawn, this, &ReosDelineatingWatershedWidget::onDownstreamLineDrawn );
  connect( mMapToolDrawPredefinedExtent, &ReosMapToolDrawExtent::extentDrawn, this, &ReosDelineatingWatershedWidget::onPredefinedExtentDrawn );
  connect( mMapToolDrawBurningLine, &ReosMapToolDrawPolyline::drawn, this, &ReosDelineatingWatershedWidget::onBurningLineDrawn );
  connect( ui->comboBoxDem, &ReosDigitalElevationModelComboBox::currentDigitalElevationChanged, this, &ReosDelineatingWatershedWidget::onDemComboboxChanged );

  connect( ui->mPushButtonDelineate, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onDelineateAsked );
  connect( ui->mPushButtonValidateAutomatic, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onAutomaticValidateAsked );

  connect( mMapToolDrawWatershed, &ReosMapToolDrawPolygon::drawn, this, &ReosDelineatingWatershedWidget::onManualWatershedDrawn );
  connect( mMapToolDrawOutletPoint, &ReosMapToolDrawPoint::drawn, this, &ReosDelineatingWatershedWidget::onManualOutletDrawn );
  connect( ui->mPushButtonValidateManual, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onManualValidateAsked );

  connect( this, &QObject::destroyed, this, &ReosDelineatingWatershedWidget::storeGeometry );

  restore();

  onMethodChange();
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
  if ( mModule->delineatingModule()->setDownstreamLine( downstreamLine ) )
  {
    mDownstreamLine.resetPolyline( downstreamLine );
    mWatershedExtent.resetPolygon();
  }
  updateAutomaticTool();
}

void ReosDelineatingWatershedWidget::onPredefinedExtentDrawn( const QRectF &extent )
{
  ReosMapExtent mapExtent( extent );
  mapExtent.setCrs( mMap->mapCrs() );
  mModule->delineatingModule()->setPreDefinedExtent( mapExtent );
  mWatershedExtent.resetPolygon( mapExtent.toPolygon() );
  updateAutomaticTool();
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
  mModule->delineatingModule()->setDigitalElevationModelDEM( currentDemId );
  updateAutomaticTool();
}

void ReosDelineatingWatershedWidget::onDelineateAsked()
{
  if ( !mModule->delineatingModule()->startDelineating() )
    return;
  ReosProcessControler *controler = new ReosProcessControler( mModule->delineatingModule()->delineatingProcess(), this );
  controler->exec();
  controler->deleteLater();
  updateAutomaticTool();

  if ( !mModule->delineatingModule()->isDelineatingFinished() )
    return;

  mTemporaryAutomaticWatershed.resetPolygon( mModule->delineatingModule()->lastWatershedDelineated() );
  mTemporaryAutomaticStreamLine.resetPolyline( mModule->delineatingModule()->lastStreamLine() );

}

void ReosDelineatingWatershedWidget::onAutomaticValidateAsked()
{
  bool needAdjusting = false;
  if ( !mModule->delineatingModule()->validateWatershed( needAdjusting ) )
    return;

  bool adjustIfNeeded = false;
  if ( needAdjusting )
  {
    QMessageBox::StandardButton answer = QMessageBox::warning( this, tr( "Delineating watershed" ), tr( "This watershed intersects existing watershed(s)\n"
                                         "Adjust new watershed?" ), QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes );

    if ( answer == QMessageBox::Cancel )
      return;
    adjustIfNeeded = QMessageBox::Yes == answer;
  }

  mModule->delineatingModule()->storeWatershed( adjustIfNeeded );

  mTemporaryAutomaticWatershed.resetPolygon();
  mTemporaryAutomaticStreamLine.resetPolyline();
  mDownstreamLine.resetPolyline();
  mWatershedExtent.resetPolygon();

  updateAutomaticTool();
}

void ReosDelineatingWatershedWidget::onManualWatershedDrawn( const QPolygonF &polygon )
{
  mTemporaryManualWatershed.resetPolygon( polygon );
  mTemporaryManualOutletPoint.resetPoint();
  mMapToolDrawOutletPoint->setCurrentToolInMap();

  updateManualMapTool();
}

void ReosDelineatingWatershedWidget::onManualOutletDrawn( const QPointF &point )
{
  mTemporaryManualOutletPoint.resetPoint( point );
  updateManualMapTool();
}

void ReosDelineatingWatershedWidget::onManualValidateAsked()
{
  if ( mTemporaryManualOutletPoint.isEmpty() || mTemporaryManualWatershed.mapPolygon().isEmpty() )
    return;
  std::unique_ptr<ReosWatershed> ws( new ReosWatershed( mTemporaryManualWatershed.mapPolygon(), mTemporaryManualOutletPoint.mapPoint() ) );
  bool needAdjusting = mModule->watershedTree()->isWatershedIntersectExisting( ws.get() );
  bool adjustIfNeeded = false;
  mTemporaryManualOutletPoint.resetPoint();
  mTemporaryManualWatershed.resetPolygon();
  if ( needAdjusting )
  {
    QMessageBox::StandardButton answer = QMessageBox::warning( this, tr( "Delineating watershed" ), tr( "This watershed intersects existing watershed(s)\n"
                                         "Adjust new watershed?" ), QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes );

    if ( answer == QMessageBox::Cancel )
      return;
    adjustIfNeeded = QMessageBox::Yes == answer;
  }

  mModule->watershedTree()->addWatershed( ws.release(), adjustIfNeeded );
  updateManualMapTool();
}

void ReosDelineatingWatershedWidget::onMethodChange()
{
  bool isAutomatic = ui->mRadioButtonAutomatic->isChecked();
  showAutomaticDelineating( isAutomatic );
  showManualDelineating( !isAutomatic );
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

void ReosDelineatingWatershedWidget::showAutomaticDelineating( bool shown )
{
  if ( mCurrentAutomaticMapTool && shown )
    mCurrentAutomaticMapTool->setCurrentToolInMap();

  mAutomaticToolBar->setVisible( shown );
  ui->mPushButtonDelineate->setVisible( shown );
  ui->mCurrentDemWidget->setVisible( shown );
  ui->mPushButtonValidateAutomatic->setVisible( shown );

  mDownstreamLine.setVisible( shown );
  mWatershedExtent.setVisible( shown );
  for ( size_t i = 0; i < mBurningLines.size(); ++i )
    mBurningLines.at( i )->setVisible( shown );
  mTemporaryAutomaticWatershed.setVisible( shown );
  mTemporaryAutomaticStreamLine.setVisible( shown );

  updateAutomaticTool();
}

void ReosDelineatingWatershedWidget::showManualDelineating( bool shown )
{
  if ( mCurrentManualMapTool && shown )
    mCurrentManualMapTool->setCurrentToolInMap();

  mManualToolBar->setVisible( shown );
  ui->mPushButtonValidateManual->setVisible( shown );

  updateManualMapTool();
}

void ReosDelineatingWatershedWidget::updateAutomaticTool()
{
  qDebug() << "Current state of delineating module: " << mModule->delineatingModule()->currentState();
  switch ( mModule->delineatingModule()->currentState() )
  {
    case ReosWatershedDelineating::NoDigitalElevationModel:
      mActionDrawDownstreamLine->setEnabled( false );
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->mPushButtonValidateAutomatic->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForDownstream:
      mActionDrawDownstreamLine->setEnabled( true );
      mCurrentAutomaticMapTool = mMapToolDrawDownStreamLine;
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->mPushButtonValidateAutomatic->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mCurrentAutomaticMapTool = mMapToolDrawPredefinedExtent;
      mActionDrawPredefinedExtent->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->mPushButtonValidateAutomatic->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingWithBroughtBackExtent:
      mActionDrawDownstreamLine->setEnabled( true );
      mCurrentAutomaticMapTool = mMapToolDrawPredefinedExtent;
      mActionDrawPredefinedExtent->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->mPushButtonValidateAutomatic->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingforProceed:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawPredefinedExtent->setEnabled( !mModule->delineatingModule()->hasDirectionData() );
      mActionDrawAddBurningLine->setEnabled( true );
      mActionDrawAddBurningLine->setEnabled( true );
      ui->mPushButtonDelineate->setEnabled( true );
      ui->mPushButtonValidateAutomatic->setEnabled( false );
      break;
    case ReosWatershedDelineating::WaitingForValidate:
      mActionDrawDownstreamLine->setEnabled( true );
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->mPushButtonValidateAutomatic->setEnabled( true );
      break;
  }

  if ( mCurrentAutomaticMapTool )
    mCurrentAutomaticMapTool->setCurrentToolInMap();
}

void ReosDelineatingWatershedWidget::updateBurningLines()
{
  QList<QPolygonF> list;
  for ( size_t i = 0; i < mBurningLines.size(); ++i )
  {
    list.append( mBurningLines.at( i ).get()->mapPolyline() );
  }

  mModule->delineatingModule()->setBurningLines( list );
}

void ReosDelineatingWatershedWidget::updateManualMapTool()
{
  if ( mTemporaryManualWatershed.mapPolygon().isEmpty() )
    mCurrentManualMapTool = mMapToolDrawWatershed;
  else if ( mTemporaryManualOutletPoint.isEmpty() )
    mCurrentManualMapTool = mMapToolDrawOutletPoint;

  ui->mPushButtonValidateManual->setEnabled( ! mTemporaryManualWatershed.mapPolygon().isEmpty() && !mTemporaryManualOutletPoint.isEmpty() );

  if ( mCurrentManualMapTool )
    mCurrentManualMapTool->setCurrentToolInMap();
}
