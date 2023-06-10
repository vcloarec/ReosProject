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
#include "reostimeseries.h"
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
  if ( mTimeSeries && mTimeSeries->data() )
  {
    QPen pen;
    if ( mBorderWidth > 0 )
      pen.setWidthF( mBorderWidth );
    pen.setColor( mBorderColor );
    QBrush brush;
    brush.setStyle( mBrushStyle );
    QColor brushColor = mBrushColor.isValid() ? mBrushColor : mTimeSeries->data()->currentValueModeColor();
    brush.setColor( brushColor );
    histogram()->setBrush( brush );
    histogram()->setPen( pen );

    if ( histogram()->plot() && mMasterItem )
    {
      histogram()->plot()->setAxisTitle( QwtPlot::yLeft,
                                         mTimeSeries->data()->unitStringCurrentMode() );
    }
  }
}

void ReosPlotTimeHistogram::setTimeSeries( ReosTimeSeriesConstantInterval *timeSeries, bool replot )
{
  if ( mTimeSeries && mTimeSeries->data() )
    disconnect( mTimeSeries->data(), &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );

  mTimeSeries = new ReosPlotConstantIntervalTimeIntervalSerie( timeSeries );
  histogram()->setSamples( mTimeSeries );

  if ( timeSeries )
  {
    connect( timeSeries, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    connect( timeSeries, &ReosDataObject::settingsChanged, this, &ReosPlotTimeHistogram::setSettings );
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

void ReosPlotTimeCumulativeCurve::setTimeSeries( ReosTimeSeriesConstantInterval *timeSerie )
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
    pen.setColor( mTimeSerie->data()->valueModeColor( ReosTimeSeriesConstantInterval::Cumulative ) );
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
  QwtPlotCurve *curve = new QwtPlotCurve( name );;
  mPlotItem = curve;
  mPlotItem->setRenderHint( QwtPlotItem::RenderAntialiased, true );
  mPlotItem->setItemAttribute( QwtPlotItem::AutoScale, true );
  curve->setLegendAttribute( QwtPlotCurve::LegendShowLine );
  curve->setLegendIconSize( QSize( 20, 5 ) );
}

void ReosPlotTimeSerieVariableStep::setTimeSeries( ReosTimeSeriesVariableTimeStep *timeSerie, bool replot, bool applysettings )
{
  if ( mTimeSeries && mTimeSeries->data() )
  {
    disconnect( mTimeSeries->data(), &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    disconnect( mTimeSeries->data(), &ReosDataObject::dataChanged, this, &ReosPlotTimeSerieVariableStep::onNameChanged );
    disconnect( mTimeSeries->data(), &ReosTimeSeriesVariableTimeStep::displayColorChanged, this, &ReosPlotTimeSerieVariableStep::setColor );
  }

  mTimeSeries = nullptr;
  if ( timeSerie )
    mTimeSeries = new ReosPlotVariableStepTimeSerie( timeSerie );
  curve()->setSamples( mTimeSeries );

  if ( applysettings )
    setSettings();

  if ( timeSerie )
  {
    connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    connect( timeSerie, &ReosDataObject::nameChanged, this, &ReosPlotTimeSerieVariableStep::onNameChanged );
    connect( mTimeSeries->data(), &ReosTimeSeriesVariableTimeStep::displayColorChanged, this, &ReosPlotTimeSerieVariableStep::setColor );
  }

  if ( curve()->plot() && replot )
    curve()->plot()->replot();
}

void ReosPlotTimeSerieVariableStep::setSettings()
{
  QPen pen = curve()->pen();
  pen.setWidthF( 2 );
  if ( mTimeSeries && mTimeSeries->data() && mTimeSeries->data()->color().isValid() )
    pen.setColor( mTimeSeries->data()->color() );
  curve()->setPen( pen );

  if ( mTimeSeries && curve()->plot() && mMasterItem )
  {
    curve()->plot()->setAxisTitle( QwtPlot::yLeft,
                                   mTimeSeries->data()->unitString() );
  }

  if ( mTimeSeries && mTimeSeries->data() )
    setName( mTimeSeries->data()->name() );
}

void ReosPlotTimeSerieVariableStep::onNameChanged()
{
  if ( mTimeSeries )
  {
    setName( mTimeSeries->data()->name() );
    if ( curve()->plot() )
      curve()->plot()->replot();
  }
}

QwtPlotCurve *ReosPlotTimeSerieVariableStep::curve() const
{
  return static_cast<QwtPlotCurve *>( mPlotItem );
}

void ReosPlotTimeSerieVariableStep::setColor( const QColor &color )
{
  QPen pen = curve()->pen();
  pen.setColor( color );
  curve()->setPen( pen );
  if ( curve()->plot() )
    curve()->plot()->replot();
}

QColor ReosPlotTimeSerieVariableStep::color() const
{
  return curve()->pen().color();
}

void ReosPlotTimeSerieVariableStep::setStyle( const Qt::PenStyle penStyle )
{
  QPen pen = curve()->pen();
  pen.setStyle( penStyle );
  curve()->setPen( pen );
  if ( penStyle != Qt::SolidLine )
    curve()->setCurveAttribute( QwtPlotCurve::Fitted, true );
  curve()->plot()->replot();
}

void ReosPlotTimeSerieVariableStep::setWidth( double width )
{
  QPen pen = curve()->pen();
  pen.setWidthF( width );
  curve()->setPen( pen );
  curve()->plot()->replot();
}

QPixmap ReosPlotTimeSerieVariableStep::icone( const QSize &size ) const
{
  QPoint p1( std::min( 3, size.width() ), ( std::min( 1, size.height() ) ) );
  QPoint p2( std::max( 3, size.width() - 3 ), ( std::max( 1, size.height() - 1 ) ) );

  QPixmap pixMap( size );
  pixMap.fill( Qt::transparent );
  QPainter painter( &pixMap );
  painter.setRenderHints( QPainter::Antialiasing );
  painter.setPen( curve()->pen() );
  painter.drawLine( p1, p2 );

  return pixMap;
}

QString ReosPlotTimeSerieVariableStep::name() const
{
  if ( mTimeSeries && mTimeSeries->data() )
    return mTimeSeries->data()->name();
  else
    return ReosPlotItem::name();
}
