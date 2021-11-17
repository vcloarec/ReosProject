/***************************************************************************
                      reosdelineatingwatershedwidget.cpp
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
#include "reosmaptool.h"
#include "reossettings.h"

#include <QBoxLayout>
#include <QMessageBox>
#include <QCloseEvent>
#include <QFileDialog>

ReosDelineatingWatershedWidget::ReosDelineatingWatershedWidget( ReosWatershedModule *watershedModule,
    ReosMap *map,
    QWidget *parent ) :
  ReosActionWidget( parent ),
  ui( new Ui::ReosDelineatingWatershedWidget ),
  mModule( watershedModule ),
  mMap( map ),
  mActionDrawDownstreamLine( new QAction( QPixmap( QStringLiteral( ":/images/downStreamSelection.png" ) ), tr( "Draw downstream line" ), this ) ),
  mActionDrawPredefinedExtent( new QAction( QPixmap( QStringLiteral( ":/images/extentWatershedSelection.png" ) ), tr( "Draw predefined extent" ), this ) ),
  mActionDrawAddBurningLine( new QAction( QPixmap( QStringLiteral( ":/images/burningLine.png" ) ), tr( "Add a burning line" ), this ) ),
  mActionRemoveBurningLine( new QAction( QPixmap( QStringLiteral( ":/images/mActionRemoveBurningLine.png" ) ), tr( "Remove a burning line" ), this ) ),
  mDownstreamLine( map ),
  mWatershedExtent( map ),
  mActionDrawWatershed( new QAction( QPixmap( QStringLiteral( ":/images/delineateWatershed.svg" ) ), tr( "Draw watershed manually" ), this ) ),
  mActionEditWatershed( new QAction( QPixmap( QStringLiteral( ":/images/editWatershed.svg" ) ), tr( "Edit watershed manually" ), this ) ),
  mActionMoveOutletPoint( new QAction( QPixmap( QStringLiteral( ":/images/moveOutlet.svg" ) ), tr( "Move outlet point" ), this ) ),
  mTemporaryAutomaticWatershed( map ),
  mTemporaryAutomaticStreamLine( map ),
  mTemporaryManualWatershed( map ),
  mTemporaryManualOutletPoint( map )
{
  ui->setupUi( this );
  ui->comboBoxDem->setGisEngine( map->engine() );

  setWindowFlag( Qt::Dialog );

  ReosSettings settings;

  mAutomaticToolBar = new QToolBar;
  mManualToolBar = new QToolBar;
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 3, mAutomaticToolBar );
  qobject_cast<QBoxLayout *>( layout() )->insertWidget( 2, mManualToolBar );
  mAutomaticToolBar->addAction( mActionDrawDownstreamLine );
  mAutomaticToolBar->addAction( mActionDrawPredefinedExtent );
  mAutomaticToolBar->addAction( mActionDrawAddBurningLine );
  mAutomaticToolBar->addAction( mActionRemoveBurningLine );

  mManualToolBar->addAction( mActionDrawWatershed );
  mManualToolBar->addAction( mActionEditWatershed );
  mManualToolBar->addAction( mActionMoveOutletPoint );

  mMapToolDrawDownStreamLine = new ReosMapToolDrawPolyline( map );
  mMapToolDrawDownStreamLine->setAction( mActionDrawDownstreamLine );
  mActionDrawDownstreamLine->setCheckable( true );
  mMapToolDrawDownStreamLine->setStrokeWidth( 2 );
  mMapToolDrawDownStreamLine->setColor( Qt::darkGreen );
  mMapToolDrawDownStreamLine->setLineStyle( Qt::DashLine );
  mMapTools << mMapToolDrawDownStreamLine;

  mMapToolDrawPredefinedExtent = new ReosMapToolDrawExtent( map );
  mMapToolDrawPredefinedExtent->setAction( mActionDrawPredefinedExtent );
  mActionDrawPredefinedExtent->setCheckable( true );
  mMapToolDrawPredefinedExtent->setStrokeWidth( 2 );
  mMapToolDrawPredefinedExtent->setColor( Qt::red );
  mMapToolDrawPredefinedExtent->setFillColor( QColor( 150, 0, 0, 30 ) );
  mMapToolDrawPredefinedExtent->setLineStyle( Qt::DashLine );
  mMapTools << mMapToolDrawPredefinedExtent;

  mMapToolDrawBurningLine = new ReosMapToolDrawPolyline( map );
  mMapToolDrawBurningLine->setAction( mActionDrawAddBurningLine );
  mActionDrawAddBurningLine->setCheckable( true );
  mMapToolDrawBurningLine->setStrokeWidth( 2 );
  mMapToolDrawBurningLine->setColor( Qt::red );
  mMapToolDrawBurningLine->setLineStyle( Qt::DashLine );
  mMapTools << mMapToolDrawBurningLine;

  burningLineFormater.setDescription( QStringLiteral( "watershed:delineating:burningLine" ) );
  burningLineFormater.setColor( Qt::red );
  burningLineFormater.setExternalColor( Qt::white );
  burningLineFormater.setWidth( 2 );
  burningLineFormater.setExternalWidth( 4 );

  mMapToolRemoveBurningLine = new ReosMapToolSelectMapItem( map, burningLineFormater.description() );
  mMapToolRemoveBurningLine->setAction( mActionRemoveBurningLine );
  mActionRemoveBurningLine->setCheckable( true );
  connect( mMapToolRemoveBurningLine, &ReosMapToolSelectMapItem::found, this, &ReosDelineatingWatershedWidget::onBurningLineRemoved );
  mMapTools << mMapToolDrawBurningLine;

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
  mMapTools << mMapToolDrawWatershed;

  mActionEditWatershed->setCheckable( true );
  mActionMoveOutletPoint->setCheckable( true );

  mMapToolDrawOutletPoint = new ReosMapToolDrawPoint( map );
  mMapTools << mMapToolDrawOutletPoint;

  mTemporaryManualWatershed.setColor( QColor( 50, 250, 50 ) );
  mTemporaryManualWatershed.setExternalColor( Qt::white );
  mTemporaryManualWatershed.setWidth( 2 );
  mTemporaryManualWatershed.setExternalWidth( 4 );
  mTemporaryManualWatershed.setStyle( Qt::DashLine );

  mTemporaryManualOutletPoint.setWidth( 8 );
  mTemporaryManualOutletPoint.setExternalWidth( 12 );
  mTemporaryManualOutletPoint.setColor( QColor( 0, 150, 250 ) );
  mTemporaryManualOutletPoint.setExternalColor( Qt::white );

  connect( ui->mRadioButtonAutomatic, &QRadioButton::clicked, this, &ReosDelineatingWatershedWidget::onMethodChange );
  connect( ui->mRadioButtonManual, &QRadioButton::clicked, this, &ReosDelineatingWatershedWidget::onMethodChange );

  connect( mMapToolDrawDownStreamLine, &ReosMapToolDrawPolyline::drawn, this, &ReosDelineatingWatershedWidget::onDownstreamLineDrawn );
  connect( mMapToolDrawPredefinedExtent, &ReosMapToolDrawExtent::extentDrawn, this, &ReosDelineatingWatershedWidget::onPredefinedExtentDrawn );
  connect( mMapToolDrawBurningLine, &ReosMapToolDrawPolyline::drawn, this, &ReosDelineatingWatershedWidget::onBurningLineDrawn );
  connect( ui->comboBoxDem, &ReosDigitalElevationModelComboBox::currentDigitalElevationChanged, this, &ReosDelineatingWatershedWidget::onDemComboboxChanged );
  connect( ui->toolButtonLoadRasterDem, &QToolButton::clicked, this, &ReosDelineatingWatershedWidget::onLoadRasterDem );

  if ( settings.contains( QStringLiteral( "DelineatingWidget/averageElevation" ) ) )
    ui->mCheckBoxAverageElevation->setChecked( settings.value( QStringLiteral( "DelineatingWidget/averageElevation" ) ).toBool() );
  mModule->delineatingModule()->setCalculateAverageElevation( ui->mCheckBoxAverageElevation->isChecked() );
  connect( ui->mCheckBoxAverageElevation, &QCheckBox::toggled, this, [this]
  {
    mModule->delineatingModule()->setCalculateAverageElevation( ui->mCheckBoxAverageElevation->isChecked() );
    ReosSettings settings;
    settings.setValue( QStringLiteral( "DelineatingWidget/averageElevation" ), ui->mCheckBoxAverageElevation->isChecked() );
  } );

  connect( ui->mPushButtonDelineate, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onDelineateAsked );
  connect( ui->mPushButtonValidateAutomatic, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onAutomaticValidateAsked );

  connect( mMapToolDrawWatershed, &ReosMapToolDrawPolygon::drawn, this, &ReosDelineatingWatershedWidget::onManualWatershedDrawn );
  connect( mMapToolDrawOutletPoint, &ReosMapToolDrawPoint::drawn, this, &ReosDelineatingWatershedWidget::onManualOutletDrawn );
  connect( ui->mPushButtonValidateManual, &QPushButton::clicked, this, &ReosDelineatingWatershedWidget::onManualValidateAsked );

  connect( mModule->delineatingModule(), &ReosWatershedDelineating::hasBeenReset, this, &ReosDelineatingWatershedWidget::onModuleReset );

  connect( this, &ReosActionWidget::opened, this, &ReosDelineatingWatershedWidget::onMethodChange );

  mActionRemoveBurningLine->setEnabled( !mBurningLines.isEmpty() );
}

ReosDelineatingWatershedWidget::~ReosDelineatingWatershedWidget()
{
  delete ui;
}

void ReosDelineatingWatershedWidget::setEditingDelineatingMapTool( ReosMapToolEditMapPolygon *mapTool )
{
  mapTool->setAction( mActionEditWatershed );
  mMapTools << mapTool;
}

void ReosDelineatingWatershedWidget::setMoveOutletPointMapTool( ReosMapToolMoveMapItem *mapTool )
{
  mapTool->setAction( mActionMoveOutletPoint );
  mMapTools << mapTool;
}


void ReosDelineatingWatershedWidget::onDownstreamLineDrawn( const QPolygonF &downstreamLine )
{
  if ( mModule->delineatingModule()->setDownstreamLine( downstreamLine ) )
  {
    mDownstreamLine.resetPolyline( downstreamLine );
    if ( mModule->delineatingModule()->currentState() == ReosWatershedDelineating::WaitingforProceed )
      mWatershedExtent.resetPolygon( mModule->delineatingModule()->currentExtent().toPolygon() );
    else
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
  mBurningLines.append( burningLineFormater( ReosMapPolyline( mMap, burningLine ) ) );
  updateBurningLines();
}

void ReosDelineatingWatershedWidget::onBurningLineRemoved( ReosMapItem *item )
{
  if ( !item )
    return;
  int i = 0;
  bool found = false;
  while ( i < mBurningLines.count() && !found )
  {
    found = mBurningLines.at( i ).isItem( item );
    if ( !found )
      ++i;
  }

  if ( found )
  {
    mBurningLines.removeAt( i );
    updateBurningLines();
  }
}

void ReosDelineatingWatershedWidget::onDemComboboxChanged()
{
  QString currentDemId = ui->comboBoxDem->currentDemLayerId();
  mModule->delineatingModule()->setDigitalElevationModelDEM( currentDemId );
  updateAutomaticTool();
}

void ReosDelineatingWatershedWidget::onLoadRasterDem()
{
  ReosSettings settings;
  QString path = settings.value( QStringLiteral( "Path/GisLayer" ) ).toString();

  const QString rasterFileName = QFileDialog::getOpenFileName( this, tr( "Load Raster DEM Layer" ), path, mMap->engine()->rasterLayerFilters() );
  const QFileInfo fileInfo( rasterFileName );
  if ( fileInfo.exists() )
  {
    bool isDEM;
    if ( mMap->engine()->canBeRasterDem( rasterFileName ) )
      isDEM = true;
    else
    {
      if ( QMessageBox::warning( this, tr( "Loading Raster DEM Layer" ),
                                 tr( "This layer is not recognized as a possible DEM, do you want to load it?" ),
                                 QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes ) == QMessageBox::No )
        return;
      isDEM = false;
    }

    const QString layerId = mMap->engine()->addRasterLayer( rasterFileName, fileInfo.fileName(), &isDEM );

    if ( layerId.isEmpty() )
      QMessageBox::warning( this, tr( "Loading Raster Layer" ), tr( "Invalid raster layer, file not loaded." ) );

    if ( isDEM && mMap->engine()->registerLayerAsDigitalElevationModel( layerId ) )
    {
      ui->comboBoxDem->setCurrentDemLayer( layerId );
    }
  }

  settings.setValue( QStringLiteral( "Path/GisLayer" ), fileInfo.path() );
}

void ReosDelineatingWatershedWidget::onDelineateAsked()
{
  mModule->delineatingModule()->prepareDelineating();
  ReosProcessControler *controler = new ReosProcessControler( mModule->delineatingModule()->delineatingProcess(), this );
  controler->exec();
  controler->deleteLater();

  updateAutomaticTool();

  if ( !mModule->delineatingModule()->isDelineatingFinished() )
    return;

  mWatershedExtent.resetPolygon( mModule->delineatingModule()->currentExtent().toPolygon() );
  mTemporaryAutomaticWatershed.resetPolygon( mModule->delineatingModule()->lastWatershedDelineated() );
  mTemporaryAutomaticStreamLine.resetPolyline( mModule->delineatingModule()->lastStreamLine() );
}

void ReosDelineatingWatershedWidget::onAutomaticValidateAsked()
{
  bool needAdjusting = false;
  QApplication::setOverrideCursor( Qt::WaitCursor );
  bool result = mModule->delineatingModule()->validateWatershed( needAdjusting );
  QApplication::restoreOverrideCursor();
  if ( !result )
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

  QApplication::setOverrideCursor( Qt::WaitCursor );
  mModule->delineatingModule()->storeWatershed( adjustIfNeeded );

  mTemporaryAutomaticWatershed.resetPolygon();
  mTemporaryAutomaticStreamLine.resetPolyline();
  mDownstreamLine.resetPolyline();
  mWatershedExtent.resetPolygon();
  QApplication::restoreOverrideCursor();

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
  std::unique_ptr<ReosWatershed> ws( new ReosWatershed( mTemporaryManualWatershed.mapPolygon(),
                                     mTemporaryManualOutletPoint.mapPoint(),
                                     ReosWatershed::Manual ) );
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


void ReosDelineatingWatershedWidget::onModuleReset()
{
  mDownstreamLine.resetPolyline();
  mWatershedExtent.resetPolygon();
  mBurningLines.clear();

  const QList<QPolygonF> burningLines = mModule->delineatingModule()->burninglines();
  for ( const QPolygonF &bl : burningLines )
  {
    mBurningLines.append( burningLineFormater( ReosMapPolyline( mMap, bl ) ) );
    mBurningLines.last().setVisible( ui->mRadioButtonAutomatic->isChecked() && isVisible() );
  }

  mActionRemoveBurningLine->setEnabled( !burningLines.isEmpty() );

  onMethodChange();
  if ( mCurrentAutomaticMapTool )
    mCurrentAutomaticMapTool->quitMap();

  if ( mCurrentManualMapTool )
    mCurrentAutomaticMapTool->quitMap();
}

void ReosDelineatingWatershedWidget::closingWidget()
{
  mDownstreamLine.resetPolyline();
  mWatershedExtent.resetPolygon();
  mTemporaryAutomaticWatershed.resetPolygon();
  mTemporaryAutomaticStreamLine.resetPolyline();
  mTemporaryManualWatershed.resetPolygon();
  mTemporaryManualOutletPoint.resetPoint();
  mModule->delineatingModule()->reset();
}

void ReosDelineatingWatershedWidget::showAutomaticDelineating( bool shown )
{
  shown = shown && isVisible();
  if ( mCurrentAutomaticMapTool && shown )
    mCurrentAutomaticMapTool->setCurrentToolInMap();

  mAutomaticToolBar->setVisible( shown );
  ui->mPushButtonDelineate->setVisible( shown );
  ui->mCurrentDemWidget->setVisible( shown );
  ui->mPushButtonValidateAutomatic->setVisible( shown );
  ui->mCheckBoxAverageElevation->setVisible( shown );

  mDownstreamLine.setVisible( shown );
  mWatershedExtent.setVisible( shown );
  for ( int i = 0; i < mBurningLines.size(); ++i )
    mBurningLines[i].setVisible( shown );
  mTemporaryAutomaticWatershed.setVisible( shown );
  mTemporaryAutomaticStreamLine.setVisible( shown );

  if ( shown )
    updateAutomaticTool();
}

void ReosDelineatingWatershedWidget::showManualDelineating( bool shown )
{
  shown = shown && isVisible();

  if ( mCurrentManualMapTool && shown )
    mCurrentManualMapTool->setCurrentToolInMap();

  mManualToolBar->setVisible( shown );
  ui->mPushButtonValidateManual->setVisible( shown );

  mTemporaryManualOutletPoint.setVisible( shown );
  mTemporaryManualWatershed.setVisible( shown );

  if ( shown )
    updateManualMapTool();
}

void ReosDelineatingWatershedWidget::updateAutomaticTool()
{
  switch ( mModule->delineatingModule()->currentState() )
  {
    case ReosWatershedDelineating::NoDigitalElevationModel:
      mActionDrawDownstreamLine->setEnabled( false );
      mActionDrawPredefinedExtent->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      mActionDrawAddBurningLine->setEnabled( false );
      ui->mPushButtonDelineate->setEnabled( false );
      ui->mPushButtonValidateAutomatic->setEnabled( false );
      mMap->setDefaultMapTool();
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

  if ( mCurrentAutomaticMapTool && isVisible() )
    mCurrentAutomaticMapTool->setCurrentToolInMap();
}

void ReosDelineatingWatershedWidget::updateBurningLines()
{
  QList<QPolygonF> list;
  for ( int i = 0; i < mBurningLines.size(); ++i )
  {
    list.append( mBurningLines.at( i ).mapPolyline() );
  }

  mModule->delineatingModule()->setBurningLines( list );

  mActionRemoveBurningLine->setEnabled( !list.isEmpty() );

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
