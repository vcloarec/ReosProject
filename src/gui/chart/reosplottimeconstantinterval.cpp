/***************************************************************************
  reosplottimeconstantinterval.cpp - ReosPlotTimeConstantInterval

 ---------------------
 begin                : 1.2.2021
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
#include "reosplottimeconstantinterval.h"
#include "reostimeserie.h"
#include "reosplot_p.h"

#include <qwt_plot_histogram.h>
#include <qwt_plot_curve.h>

ReosPlotTimeHistogram::ReosPlotTimeHistogram( const QString &name, bool masterItem ):
  ReosPlotItem()
{
  mPlotItem = new ReosPlotHistogramItem_p( name );
  mPlotItem->setItemAttribute( QwtPlotItem::AutoScale, true );
  mMasterItem = masterItem;
}


ReosPlotHistogramItem_p *ReosPlotTimeHistogram::histogram()
{
  return static_cast<ReosPlotHistogramItem_p *>( mPlotItem );
}

void ReosPlotTimeHistogram::setSettings()
{
  if ( mTimeSerie && mTimeSerie->data() )
  {
    QPen pen;
    if ( mBorderWidth > 0 )
      pen.setWidthF( mBorderWidth );
    pen.setColor( mBorderColor );
    QBrush brush;
    brush.setStyle( mBrushStyle );
    QColor brushColor = mBrushColor.isValid() ? mBrushColor : mTimeSerie->data()->currentValueModeColor();
    brush.setColor( brushColor );
    histogram()->setBrush( brush );
    histogram()->setPen( pen );

    if ( histogram()->plot() && mMasterItem )
    {
      histogram()->plot()->setAxisTitle( QwtPlot::yLeft,
                                         mTimeSerie->data()->unitStringCurrentMode() );
    }
  }
}

void ReosPlotTimeHistogram::setTimeSerie( ReosTimeSerieConstantInterval *timeSerie, bool replot )
{
  if ( mTimeSerie && mTimeSerie->data() )
    disconnect( mTimeSerie->data(), &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );

  mTimeSerie = new ReosPlotConstantIntervalTimeIntervalSerie( timeSerie );
  histogram()->setSamples( mTimeSerie );

  if ( timeSerie )
  {
    connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    connect( timeSerie, &ReosDataObject::settingsChanged, this, &ReosPlotTimeHistogram::setSettings );
  }

  setSettings();

  if ( histogram()->plot() && replot )
    histogram()->plot()->replot();
}

void ReosPlotTimeHistogram::setBorderColor( const QColor &color )
{
  mBorderColor = color;
}

void ReosPlotTimeHistogram::setBorderWdidth( double w )
{
  mBorderWidth = w;
}

void ReosPlotTimeHistogram::setBrushColor( const QColor &color )
{
  mBrushColor = color;
}

void ReosPlotTimeHistogram::setBrushStyle( Qt::BrushStyle style )
{
  mBrushStyle = style;
}

ReosPlotTimeCumulativeCurve::ReosPlotTimeCumulativeCurve( const QString &name )
{
  mPlotItem = new QwtPlotCurve( name );
  mPlotItem->setRenderHint( QwtPlotItem::RenderAntialiased, true );
  mPlotItem->setItemAttribute( QwtPlotItem::AutoScale, true );
}

void ReosPlotTimeCumulativeCurve::setTimeSerie( ReosTimeSerieConstantInterval *timeSerie )
{
  if ( mTimeSerie && mTimeSerie->data() )
    disconnect( mTimeSerie->data(), &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );

  mTimeSerie = new ReosPlotConstantIntervalTimePointSerie( timeSerie );
  mTimeSerie->setCumulative( true );
  curve()->setSamples( mTimeSerie );

  if ( timeSerie )
  {
    connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    connect( timeSerie, &ReosDataObject::settingsChanged, this, &ReosPlotTimeCumulativeCurve::setSettings );
  }

  setSettings();
}

void ReosPlotTimeCumulativeCurve::setSettings()
{
  if ( mTimeSerie && mTimeSerie->data() )
  {
    QPen pen;
    pen.setColor( mTimeSerie->data()->valueModeColor( ReosTimeSerieConstantInterval::Cumulative ) );
    pen.setWidth( 3 );
    curve()->setPen( pen );
  }
}

QwtPlotCurve *ReosPlotTimeCumulativeCurve::curve()
{
  return static_cast<QwtPlotCurve *>( mPlotItem );
}

ReosPlotTimeSerieVariableStep::ReosPlotTimeSerieVariableStep( const QString &name )
{
  mPlotItem = new QwtPlotCurve( name );
  mPlotItem->setRenderHint( QwtPlotItem::RenderAntialiased, true );
  mPlotItem->setItemAttribute( QwtPlotItem::AutoScale, true );
}

void ReosPlotTimeSerieVariableStep::setTimeSerie( ReosTimeSerieVariableTimeStep *timeSerie, bool replot )
{
  if ( mTimeSerie && mTimeSerie->data() )
    disconnect( mTimeSerie->data(), &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );

  mTimeSerie = nullptr;
  if ( timeSerie )
    mTimeSerie = new ReosPlotVariableStepTimeSerie( timeSerie );
  curve()->setSamples( mTimeSerie );

  setSettings();

  if ( timeSerie )
    connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );

  if ( curve()->plot() && replot )
    curve()->plot()->replot();
}

void ReosPlotTimeSerieVariableStep::setSettings()
{
  QPen pen;
  pen.setWidthF( 2 );
  if ( mTimeSerie && mTimeSerie->data() )
    pen.setColor( mTimeSerie->data()->color() );
  curve()->setPen( pen );

  if ( mTimeSerie && curve()->plot() && mMasterItem )
  {
    curve()->plot()->setAxisTitle( QwtPlot::yLeft,
                                   mTimeSerie->data()->unitString() );
  }
}

QwtPlotCurve *ReosPlotTimeSerieVariableStep::curve()
{
  return static_cast<QwtPlotCurve *>( mPlotItem );
}
