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

ReosPlotTimeHistogram::ReosPlotTimeHistogram( const QString &name ): ReosPlotItem()
{
  mPlotItem = new QwtPlotHistogram( name );
}


QwtPlotHistogram *ReosPlotTimeHistogram::histogram()
{
  return static_cast<QwtPlotHistogram *>( mPlotItem );
}

void ReosPlotTimeHistogram::setSettings()
{
  if ( mTimeSerie && mTimeSerie->data() )
  {
    QPen pen;
    pen.setColor( Qt::black );
    QBrush brush;
    brush.setStyle( Qt::SolidPattern );
    brush.setColor( mTimeSerie->data()->currentValueModeColor() );
    if ( histogram()->plot() )
      histogram()->plot()->setAxisTitle( QwtPlot::yLeft,
                                         mTimeSerie->data()->valueModeName( mTimeSerie->data()->valueMode() ) );
    histogram()->setBrush( brush );
    histogram()->setPen( pen );
  }
}

void ReosPlotTimeHistogram::setTimeSerie( ReosTimeSerieConstantInterval *timeSerie )
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
}

ReosPlotTimeCumulativeCurve::ReosPlotTimeCumulativeCurve( const QString &name )
{
  mPlotItem = new QwtPlotCurve( name );
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


