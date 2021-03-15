/***************************************************************************
  reoslongitudinalprofilewidget.cpp - ReosLongitudinalProfileWidget

 ---------------------
 begin                : 11.1.2021
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
#include "reoslongitudinalprofilewidget.h"
#include "ui_reoslongitudinalprofilewidget.h"

#include <QLayout>
#include <QAction>
#include <QGraphicsSceneMouseEvent>

#include "reoswatershed.h"
#include "reosplotwidget.h"
#include "reosprocesscontroler.h"
#include "reoseditableprofile.h"
#include "reosdigitalelevationmodel.h"
#include "reosmap.h"
#include "reossettings.h"

ReosLongitudinalProfileWidget::ReosLongitudinalProfileWidget( ReosMap *map,  QWidget *parent ) :
  ReosActionWidget( parent ),
  ui( new Ui::ReosLongitudinalProfileWidget )
  , mMap( map )
  , mCurrentStreamLine( map )
  , mActionDrawStreamLine( new QAction( QPixmap( ":/images/drawStreamLine.svg" ), tr( "Draw Flow Path on Map" ), this ) )
  , mActionEditStreamLine( new QAction( QPixmap( ":/images/editStreamLine.svg" ), tr( "Edit Flow Path on Map" ), this ) )
  , mActionZoomOnDEMProfileExtent( new QAction( QPixmap( ":/images/demProfileExtent.svg" ), tr( "Zoom on DEM Profile Extent" ), this ) )
  , mActionDrawStreamLineFromDownstream( new QAction( QPixmap( ":/images/drawToUpstream.svg" ), tr( "Draw Longest Flow Path From Downstream" ), this ) )
  , mActionDrawStreamLineFromPointToDownstream( new QAction( QPixmap( ":/images/drawToDownstream.svg" ), tr( "Draw Flow Path From a Upstream Point on Map" ), this ) )
  , mActionGroupStreamLineMapTool( new QActionGroup( this ) )

{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );
  ui->mComboBoxDEM->setGisEngine( mMap->engine() );

  ui->mSplitter->setStretchFactor( 0, 3 );
  ui->mSplitter->setStretchFactor( 1, 1 );


//**** set up editable profile
  mDemCurve = new ReosPlotCurve( "Profile on current DEM", QColor( 0, 155, 242 ), 3 );
  mProfile = new ReosEditableProfile();
  ui->mPlotWidget->addPlotItem( mProfile );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::normalMagnifier );
  ui->mProfileTableView->setModel( mProfile->tableModel() );
  ui->mPlotWidget->setLegendAlignement( Qt::AlignRight );
  ui->mPlotWidget->addPlotItem( mDemCurve );

  QToolBar *profileToolBar = new QToolBar;
  profileToolBar->addActions( mProfile->actionsToolBar() );
  ui->mWidgetToolProfile->layout()->addWidget( profileToolBar );

//****** set up stream line tools
  mMapToolDrawStreamLine = new ReosMapToolDrawPolyline( map );
  mMapToolDrawStreamLine->setAction( mActionDrawStreamLine );
  mActionGroupStreamLineMapTool->addAction( mActionDrawStreamLine );
  mActionGroupStreamLineMapTool->addAction( mActionEditStreamLine );
  mActionGroupStreamLineMapTool->addAction( mActionDrawStreamLineFromPointToDownstream );
  mActionGroupStreamLineMapTool->addAction( mActionDrawStreamLineFromDownstream );
  mActionGroupStreamLineMapTool->setExclusive( true );

  QToolBar *streamLineToolBar = new QToolBar;
  mActionDrawStreamLine->setCheckable( true );
  streamLineToolBar->addActions( mActionGroupStreamLineMapTool->actions() );
  streamLineToolBar->addAction( mActionZoomOnDEMProfileExtent );
  ui->mWidgetToolStreamLine->layout()->addWidget( streamLineToolBar );
  connect( mMapToolDrawStreamLine, &ReosMapToolDrawPolyline::drawn, this, &ReosLongitudinalProfileWidget::onStreamLineChanged );
  mMapToolDrawStreamLine->setColor( QColor( 0, 155, 242 ) );
  mMapToolDrawStreamLine->setSecondaryStrokeColor( Qt::white );
  mMapToolDrawStreamLine->setStrokeWidth( 3 );
  mMapToolDrawStreamLine->setLineStyle( Qt::DashLine );
  mCurrentStreamLine.activeMarker( true );
  mMapTools << mMapToolDrawStreamLine;

  ui->mPlotWidget->setTitleAxeX( tr( "Distance (meter)" ) );
  ui->mPlotWidget->setTitleAxeYLeft( tr( "Elevation" ) );

  connect( ui->mPlotWidget, &ReosPlotWidget::cursorMoved, this, &ReosLongitudinalProfileWidget::onProfileCursorMove );

  mMapToolEditStreamLine = new ReosMapToolEditMapPolyline( map );
  mActionEditStreamLine->setCheckable( true );
  mMapToolEditStreamLine->setAction( mActionEditStreamLine );
  mMapToolEditStreamLine->setMapPolyline( &mCurrentStreamLine );
  mMapTools << mMapToolEditStreamLine;
  connect( mMapToolEditStreamLine, &ReosMapToolEditMapPolyline::polylineEdited, this, &ReosLongitudinalProfileWidget::onStreamLineEdited );
  mCurrentStreamLine.setWidth( 3 );
  mCurrentStreamLine.setExternalWidth( 5 );
  mCurrentStreamLine.setColor( QColor( 0, 155, 242 ) );
  mCurrentStreamLine.setExternalColor( Qt::white );
  mCurrentStreamLine.setZValue( 9 );

  mMapToolSelectMapUpstreamPoint = new ReosMapToolDrawPoint( map );
  mMapToolSelectMapUpstreamPoint->setAction( mActionDrawStreamLineFromPointToDownstream );
  mActionDrawStreamLineFromPointToDownstream->setCheckable( true );
  mMapTools << mMapToolSelectMapUpstreamPoint;


  connect( mMapToolSelectMapUpstreamPoint, &ReosMapToolDrawPoint::drawn, this, &ReosLongitudinalProfileWidget::drawStreamLinefromPointToDownstream );
  connect( mActionDrawStreamLineFromDownstream, &QAction::triggered, this, &ReosLongitudinalProfileWidget::drawStreamLinefromPointToUpStream );
  connect( mActionZoomOnDEMProfileExtent, &QAction::triggered, this, &ReosLongitudinalProfileWidget::zoomOnDEMProfileExtent );

  mNeedUpdateDEMProfil = true;
  askForUpdateDEMProfile();

  connect( mProfile, &ReosEditableProfile::profileChanged, this, &ReosLongitudinalProfileWidget::updateProfile );
  connect( ui->mComboBoxDEM, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosLongitudinalProfileWidget::askForUpdateDEMProfile );
  connect( ui->mComboBoxDEM, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosLongitudinalProfileWidget::updateWithDirectionTools );
  connect( this, &ReosActionWidget::opened, this, &ReosLongitudinalProfileWidget::onOpened );
}

ReosLongitudinalProfileWidget::~ReosLongitudinalProfileWidget()
{
  delete ui;
}


void ReosLongitudinalProfileWidget::setCurrentWatershed( ReosWatershed *ws )
{
  if ( ws )
    ui->mLabelWatershedName->setText( ws->name()->value() );

  mCurrentWatershed = ws;

  if ( mCurrentWatershed )
  {
    mProfile->setProfile( mCurrentWatershed->profile() );
    mCurrentStreamLine.resetPolyline( mCurrentWatershed->streamPath() );
  }
  else
  {
    mCurrentStreamLine.resetPolyline();
    mProfile->setProfile( QPolygonF() );
  }

  updateWithDirectionTools();
  mNeedUpdateDEMProfil = true;
  askForUpdateDEMProfile();
}

void ReosLongitudinalProfileWidget::updateProfile()
{
  const QPolygonF &profile = mProfile->profile();
  if ( mCurrentWatershed )
    mCurrentWatershed->setProfile( profile );

  double length = 0;
  double totalDenom = 0;

  for ( int i = 0; i < profile.size() - 1; ++i )
  {
    const QPointF &p1 = profile.at( i );
    const QPointF &p2 = profile.at( i + 1 );
    double dx = fabs( p1.x() - p2.x() );
    double dy = fabs( p1.y() - p2.y() );
    double dl = sqrt( std::pow( dx, 2 ) + pow( dy, 2 ) );
    double sl = dy / dx;
    length += dl;
    totalDenom += dl / sqrt( sl );
  }

  double averageSlope = pow( length / totalDenom, 2 );

  QString txtSlope;

  if ( std::isnan( averageSlope ) || std::isinf( averageSlope ) )
  {
    txtSlope = '-';
  }
  else if ( int( averageSlope * 1000 ) == 0 )
  {
    txtSlope = QString::number( averageSlope * 1000, 'f', 2 );
    txtSlope.append( " " );
    txtSlope.append( QChar( 0x2030 ) );
  }
  else
  {
    txtSlope = QString::number( averageSlope * 100, 'f', 2 );
    txtSlope.append( " %" );
  }

  ui->mLabelAverageSlope->setText( txtSlope );

  QString txtLength;
  if ( std::isnan( length ) || std::isinf( length ) )
  {
    txtLength = '-';
  }
  else if ( length >= 100000 )
  {
    txtLength = QString::number( length / 1000, 'f', 2 );
    txtLength.append( tr( " km" ) );
  }
  else
  {
    txtLength = QString::number( length, 'f', 0 );
    txtLength.append( tr( " m" ) );
  }
  ui->mLabelTotalLength->setText( txtLength );

}

void ReosLongitudinalProfileWidget::onProfileCursorMove( const QPointF &point )
{
  mCurrentStreamLine.setMarkerDistance( point.x() );
}

void ReosLongitudinalProfileWidget::onStreamLineChanged( const QPolygonF &streamLine )
{
  if ( mCurrentWatershed )
    mCurrentWatershed->setStreamPath( streamLine );

  mCurrentStreamLine.resetPolyline( streamLine );
  askForUpdateDEMProfile();
}

void ReosLongitudinalProfileWidget::onStreamLineEdited()
{
  if ( mCurrentWatershed )
    mCurrentWatershed->setStreamPath( mCurrentStreamLine.mapPolyline() );

  askForUpdateDEMProfile();
}


void ReosLongitudinalProfileWidget::updateDEMProfile()
{
  if ( !mNeedUpdateDEMProfil )
    return;

  if ( !isVisible() )
    return;

  if ( !mCurrentWatershed )
    return;

  QPolygonF streamLine = mCurrentWatershed->streamPath();

  QPolygonF profile;
  QString currentDEmId = ui->mComboBoxDEM->currentDemLayerId();
  std::unique_ptr<ReosDigitalElevationModel> dem;

  dem.reset( mMap->engine()->getDigitalElevationModel( currentDEmId ) );
  if ( dem )
  {
    ReosElevationOnPolylineProcess pr( dem.get() );
    pr.setEntryPolyline( streamLine, mMap->engine()->crs() );
    ReosProcessControler *controler = new ReosProcessControler( &pr, this );
    controler->exec();

    profile = pr.resultProfile();//dem->elevationOnPolyline( streamLine, mMap->engine()->crs() );
    if ( profile.count() > 1 )
    {
      mDemCurve->setData( profile );
      if ( mProfile->profile().size() < 2 )
      {
        mProfile->addPoint( profile.first() );
        mProfile->addPoint( profile.last() );
      }
    }
    else
    {
      mDemCurve->setData( QPolygonF() );
    }
  }

  updateProfile();
  mNeedUpdateDEMProfil = false;
}

void ReosLongitudinalProfileWidget::updateWithDirectionTools()
{
  // activate/deactive tools depending of direction data exist
  bool hasDirection = mCurrentWatershed && mCurrentWatershed->hasDirectiondata( ui->mComboBoxDEM->currentDemLayerId() );
  mActionDrawStreamLineFromPointToDownstream->setEnabled( hasDirection );
  mActionDrawStreamLineFromDownstream->setEnabled( hasDirection );
}

void ReosLongitudinalProfileWidget::onOpened()
{
  askForUpdateDEMProfile();
}

void ReosLongitudinalProfileWidget::askForUpdateDEMProfile()
{
  mNeedUpdateDEMProfil = true;
  updateDEMProfile();
}

void ReosLongitudinalProfileWidget::zoomOnDEMProfileExtent()
{
  if ( mDemCurve )
    mDemCurve->zoomOnExtent();

  ui->mPlotWidget->resetZoomBase();

}

void ReosLongitudinalProfileWidget::drawStreamLinefromPointToDownstream( const QPointF &point )
{
  if ( !mCurrentWatershed )
    return;

  if ( !ReosGeometryUtils::pointIsInsidePolygon( point, mCurrentWatershed->delineating() ) )
    return;

  QString demLayerId = ui->mComboBoxDEM->currentDemLayerId();

  if ( mCurrentWatershed->hasDirectiondata( demLayerId ) )
  {
    const ReosRasterExtent &rasterExtent = mCurrentWatershed->directionExtent( demLayerId );
    const ReosRasterCellPos pos = rasterExtent.mapToCellPos( point );
    std::unique_ptr<ReosRasterWatershedTraceDownstream> pr = std::make_unique<ReosRasterWatershedTraceDownstream>(
          mCurrentWatershed->directions( demLayerId ),
          mCurrentWatershed->delineating(),
          rasterExtent,
          pos );

    ReosProcessControler *controler = new ReosProcessControler( pr.get(), this );
    controler->exec();

    if ( pr->isSuccessful() )
      onStreamLineChanged( pr->resultPolyline() );

    controler->deleteLater();
  }

}

void ReosLongitudinalProfileWidget::drawStreamLinefromPointToUpStream()
{
  if ( !mCurrentWatershed )
    return;

  QString demLayerId = ui->mComboBoxDEM->currentDemLayerId();

  if ( mCurrentWatershed->hasDirectiondata( demLayerId ) && !mCurrentWatershed->downstreamLine().empty() )
  {
    const ReosRasterExtent &rasterExtent = mCurrentWatershed->directionExtent( demLayerId );
    ReosRasterLine entryLine;
    for ( const QPointF &pt : mCurrentWatershed->downstreamLine() )
      entryLine.addPoint( rasterExtent.mapToCellPos( pt ) );

    // First search the longest path;
    std::unique_ptr<ReosRasterWatershedFromDirectionAndDownStreamLine> pr_1 =
      std::make_unique<ReosRasterWatershedFromDirectionAndDownStreamLine>(
        mCurrentWatershed->directions( demLayerId ),
        entryLine,
        new ReosRasterTestingCellInPolygon( rasterExtent, mCurrentWatershed->delineating() ) );

    pr_1->setInformation( tr( "Searching for the longest path" ) );
    std::unique_ptr<ReosProcessControler> controler = std::make_unique<ReosProcessControler>( pr_1.get(), this );
    controler->exec();

    if ( pr_1->isSuccessful() )
    {
      // Now need to go downstream to obtain the polyline ...
      ReosRasterCellPos upstreamPos = pr_1->endOfLongerPath();

      pr_1.reset(); //not needd anymore

      std::unique_ptr<ReosRasterWatershedTraceDownstream> pr_2 = std::make_unique<ReosRasterWatershedTraceDownstream>(
            mCurrentWatershed->directions( demLayerId ),
            mCurrentWatershed->delineating(),
            rasterExtent,
            upstreamPos );

      pr_2->setInformation( tr( "trace longer path" ) );

      controler.reset( new ReosProcessControler( pr_2.get(), this ) );
      controler->exec();

      if ( pr_2->isSuccessful() )
        onStreamLineChanged( pr_2->resultPolyline() );

    }
  }
}
