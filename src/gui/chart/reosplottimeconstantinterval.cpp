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
  {
    disconnect( mTimeSerie->data(), &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    disconnect( mTimeSerie->data(), &ReosDataObject::dataChanged, this, &ReosPlotTimeSerieVariableStep::onNameChanged );
    disconnect( mTimeSerie->data(), &ReosTimeSerieVariableTimeStep::colorChanged, this, &ReosPlotTimeSerieVariableStep::setColor );
  }

  mTimeSerie = nullptr;
  if ( timeSerie )
    mTimeSerie = new ReosPlotVariableStepTimeSerie( timeSerie );
  curve()->setSamples( mTimeSerie );

  setSettings();

  if ( timeSerie )
  {
    connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosPlotItem::itemChanged );
    connect( timeSerie, &ReosDataObject::dataChanged, this, &ReosPlotTimeSerieVariableStep::onNameChanged );
    connect( mTimeSerie->data(), &ReosTimeSerieVariableTimeStep::colorChanged, this, &ReosPlotTimeSerieVariableStep::setColor );
  }

  if ( curve()->plot() && replot )
    curve()->plot()->replot();
}

void ReosPlotTimeSerieVariableStep::setSettings()
{
  QPen pen = curve()->pen();
  pen.setWidthF( 2 );
  if ( mTimeSerie && mTimeSerie->data() && mTimeSerie->data()->color().isValid() )
    pen.setColor( mTimeSerie->data()->color() );
  curve()->setPen( pen );

  if ( mTimeSerie && curve()->plot() && mMasterItem )
  {
    curve()->plot()->setAxisTitle( QwtPlot::yLeft,
                                   mTimeSerie->data()->unitString() );
  }
}

void ReosPlotTimeSerieVariableStep::onNameChanged()
{
  if ( mTimeSerie )
    setName( mTimeSerie->data()->name() );
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
  if ( mTimeSerie && mTimeSerie->data() )
    return mTimeSerie->data()->name();
  else
    return ReosPlotItem::name();
}
