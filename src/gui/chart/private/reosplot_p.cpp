/***************************************************************************
  reosplot_p.cpp - ReosPlot_p

 ---------------------
 begin                : 14.1.2021
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
#include "reosplot_p.h"

#include "qwt_plot_grid.h"
#include "qwt_plot_legenditem.h"
#include "qwt_plot_zoomer.h"
#include "qwt_plot_panner.h"

#include "reosplotwidget.h"


ReosPlot_p::ReosPlot_p( QWidget *parent ): QwtPlot( parent )
{
  mGrid = new QwtPlotGrid();
  mGrid->setMajorPen( Qt::gray );
  mGrid->setMinorPen( Qt::lightGray );
  mGrid->enableYMin( true );
  mGrid->enableXMin( true );
  mGrid->attach( this );

  setCanvasBackground( Qt::white );

  mLegend = new QwtPlotLegendItem();
  QFont fontLegend = mLegend->font();
  fontLegend.setPointSize( 10 );
  mLegend->setFont( fontLegend );
  QBrush brushLegend( QColor( 255, 255, 255, 100 ), Qt::SolidPattern );
  mLegend->setBackgroundBrush( brushLegend );
  mLegend->setBackgroundMode( QwtPlotLegendItem::ItemBackground );
  mLegend->setAlignment( Qt::AlignTop | Qt::AlignLeft );
  mLegend->attach( this );

  mLegend->setVisible( false );

  mZoomerLeft = new QwtPlotZoomer( QwtPlot::xBottom, QwtPlot::yLeft, canvas(), true );
  mZoomerRight = new QwtPlotZoomer( QwtPlot::xTop, QwtPlot::yRight, canvas(), true );
  mZoomerLeft->setTrackerMode( QwtPicker::AlwaysOff );
  mZoomerRight->setTrackerMode( QwtPicker::AlwaysOff );

  QPen rubbberBandPen( Qt::darkGray );
  rubbberBandPen.setWidth( 2 );
  rubbberBandPen.setStyle( Qt::DotLine );
  mZoomerLeft->setRubberBandPen( rubbberBandPen );
  mZoomerRight->setRubberBandPen( QPen( Qt::NoPen ) );

  mPanner = new QwtPlotPanner( canvas() );
  mPanner->setMouseButton( Qt::MidButton );

  mZoomerLeft->setMousePattern( QwtEventPattern::MouseSelect3, Qt::NoButton );
  mZoomerRight->setMousePattern( QwtEventPattern::MouseSelect3, Qt::NoButton );
}

void ReosPlot_p::setLegendVisible( bool b )
{
  mLegend->setVisible( b );
}

void ReosPlot_p::setLegendAlignement( Qt::Alignment align )
{
  mLegend->setAlignment( align );
}

void ReosPlot_p::enableAutoMinimumSize( bool b )
{
  mAutoMinimumSizeIsEnable = b;
}

QSize ReosPlot_p::sizeHint() const
{
  if ( mAutoMinimumSizeIsEnable )
    return QwtPlot::sizeHint();
  else
    return mMinimumSize;
}

QSize ReosPlot_p::minimumSizeHint() const
{
  if ( mAutoMinimumSizeIsEnable )
    return QwtPlot::minimumSizeHint();
  else
    return mMinimumSize;
}

void ReosPlot_p::addItem( ReosPlotItem *item )
{
  item->attach( this );
  mItems.emplace_back( item );
}


void ReosPlot_p::setMinimumPlotSize( QSize minSize )
{
  mMinimumSize = minSize;
}

void ReosPlot_p::setRightAxeEnabled( bool b )
{
  enableAxis( QwtPlot::yRight, b );
}

void ReosPlot_p::setEnableZoomer( bool b )
{
  mZoomerLeft->setEnabled( b );
  mZoomerRight->setEnabled( b );
}

void ReosPlot_p::setNormalMagnifier()
{
  mMagnifier->deleteLater();
  mMagnifier = new QwtPlotMagnifier( canvas() );
}

void ReosPlot_p::setPositiveMagnifier()
{
  mMagnifier->deleteLater();
  mMagnifier = new ReosPositiveMagnifier( canvas() );
}

ReosPositiveMagnifier::ReosPositiveMagnifier( QWidget *canvas ): QwtPlotMagnifier( canvas ), mIsYMinEnabeled( true )
{}

void ReosPositiveMagnifier::setYMinimumEnabled( bool b )
{
  mIsYMinEnabeled = b;
}

void ReosPositiveMagnifier::rescale( double factor )
{
  QwtScaleDiv scaleDivLeft = plot()->axisScaleDiv( QwtPlot::yLeft );
  QwtScaleDiv scaleDivRight = plot()->axisScaleDiv( QwtPlot::yRight );
  QwtScaleDiv scaleDivBottom = plot()->axisScaleDiv( QwtPlot::xBottom );

  double yminLeft = scaleDivLeft.lowerBound();
  double yminRight = scaleDivRight.lowerBound();
  double xmin = scaleDivBottom.lowerBound();
  double xmax = scaleDivBottom.upperBound();

  QwtPlotMagnifier::rescale( factor );
  scaleDivLeft = plot()->axisScaleDiv( QwtPlot::yLeft );
  scaleDivRight = plot()->axisScaleDiv( QwtPlot::yRight );

  if ( mIsYMinEnabeled )
  {
    scaleDivLeft.setLowerBound( yminLeft );
    plot()->setAxisScaleDiv( QwtPlot::yLeft, scaleDivLeft );
    scaleDivRight.setLowerBound( yminRight );
    plot()->setAxisScaleDiv( QwtPlot::yRight, scaleDivRight );
    scaleDivBottom.setLowerBound( xmin );
    scaleDivBottom.setUpperBound( xmax );
    plot()->setAxisScaleDiv( QwtPlot::xBottom, scaleDivBottom );
    plot()->replot();
  }
}
