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

#include <QtCharts/QChartView>
#include <QtCharts/QChart>
#include <QtCharts/QLineSeries>
#include <QLayout>
#include <QAction>
#include <QGraphicsSceneMouseEvent>

#include "reoswatershed.h"
#include "reosplotwidget.h"
#include "reoseditableprofile.h"
#include "reosdigitalelevationmodel.h"
#include "reosmap.h"
#include "reossettings.h"

using namespace QtCharts;


ReosLongitudinalProfileWidget::ReosLongitudinalProfileWidget( ReosMap *map,  QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosLongitudinalProfileWidget ),
  mMap( map )
{
  ui->setupUi( this );
  ui->mComboBoxDEM->setGisEngine( mMap->engine() );

  setWindowFlag( Qt::Dialog );

  mProfile = new ReosEditableProfile();
  ui->mPlotWidget->addPlotItem( mProfile );
  ui->mPlotWidget->setMagnifierType( ReosPlotWidget::NormalMagnifier );
  ui->mProfileTableView->setModel( mProfile->tableModel() );
  ui->mPlotWidget->setLegendAlignement( Qt::AlignRight );

  mDemCurve = new ReosPlotCurve( "Profile on current DEM", QColor( 0, 155, 242 ), 3 );
  ui->mPlotWidget->addPlotItem( mDemCurve );

  QToolBar *profileToolBar = new QToolBar;
  profileToolBar->addActions( mProfile->actionsToolBar() );
  ui->mWidgetToolProfile->layout()->addWidget( profileToolBar );

  ui->mPlotWidget->setTitleAxeX( tr( "Distance (map unit)" ) );
  ui->mPlotWidget->setTitleAxeYleft( tr( "Elevation (DEM unit)" ) );

  restore();
  updateDEMProfile();

  connect( mProfile, &ReosPlotItem::itemChanged, this, &ReosLongitudinalProfileWidget::updateProfile );
}

ReosLongitudinalProfileWidget::~ReosLongitudinalProfileWidget()
{
  delete ui;
}

void ReosLongitudinalProfileWidget::setAction( QAction *action )
{
  mAction = action;
  connect( action, &QAction::triggered, [this]
  {
    if ( mAction->isChecked() )
      show();
    else
      close();
  } );
}

void ReosLongitudinalProfileWidget::setCurrentWatershed( ReosWatershed *ws )
{
  if ( ws )
    ui->mLabelWatershedName->setText( ws->name() );

  mCurrentWatershed = ws;

  if ( mCurrentWatershed )
    mProfile->setProfile( mCurrentWatershed->profile() );

  updateDEMProfile();
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

void ReosLongitudinalProfileWidget::storeGeometry()
{
  ReosSettings settings;
  settings.setValue( QStringLiteral( "/Windows/LongitudinalProfileWidget/Geometry" ), saveGeometry() );
}

void ReosLongitudinalProfileWidget::restore()
{
  ReosSettings settings;
  ui->mSplitter->setStretchFactor( 0, 3 );
  ui->mSplitter->setStretchFactor( 1, 1 );

  restoreGeometry( settings.value( QStringLiteral( "/Windows/LongitudinalProfileWidget/Geometry" ) ).toByteArray() );
}

void ReosLongitudinalProfileWidget::closeEvent( QCloseEvent *event )
{
  storeGeometry();
  if ( mAction )
    mAction->setChecked( false );
  setVisible( false );
  mMap->setDefaultMapTool();
  event->accept();
}

void ReosLongitudinalProfileWidget::updateDEMProfile()
{
  if ( !mCurrentWatershed )
    return;

  QPolygonF streamLine = mCurrentWatershed->streamPath();

  QPolygonF profile;
  QString currentDEmId = ui->mComboBoxDEM->currentDemLayerId();
  std::unique_ptr<ReosDigitalElevationModel> dem;

  dem.reset( mMap->engine()->getDigitalElevationModel( currentDEmId ) );
  if ( dem )
  {
    profile = dem->elevationOnPolyline( streamLine, mMap->engine()->crs() );
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

}
