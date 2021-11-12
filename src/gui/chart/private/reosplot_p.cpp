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

#include <QWheelEvent>

#include "qwt_plot_grid.h"
#include "qwt_plot_legenditem.h"
#include "qwt_plot_zoomer.h"
#include "qwt_plot_panner.h"
#include "qwt_scale_engine.h"
#include "qwt_date.h"

#include "reosplotwidget.h"
#include "reostimeserie.h"


ReosPlot_p::ReosPlot_p( QWidget *parent ): QwtPlot( parent )
{
  mGrid = new QwtPlotGrid();
  mGrid->setMajorPen( Qt::lightGray );
  QPen minorPen;
  minorPen.setStyle( Qt::DotLine );
  minorPen.setColor( Qt::lightGray );
  mGrid->setMinorPen( minorPen );
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

  QPen rubbberBandPen( Qt::darkGray );
  rubbberBandPen.setWidth( 2 );
  rubbberBandPen.setStyle( Qt::DotLine );

  mPanner = new QwtPlotPanner( canvas() );
  mPanner->setMouseButton( Qt::MiddleButton );
}

ReosPlot_p::~ReosPlot_p()
{}

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

void ReosPlot_p::setZoomer( QwtPlotZoomer *zoomerLeft, QwtPlotZoomer *zoomerRight )
{
  mZoomerLeft = zoomerLeft;
  mZoomerRight = zoomerRight;
}

void ReosPlot_p::enableAutoScale( bool b )
{
  mAutoScale = b;
}

void ReosPlot_p::autoScale()
{
  setAxisAutoScale( QwtPlot::xBottom, mAutoScale );
  setAxisAutoScale( QwtPlot::yLeft, mAutoScale );
  setAxisAutoScale( QwtPlot::yRight, mAutoScale );
}

void ReosPlot_p::replot()
{
  QwtPlot::replot();
  emit reploted();
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
  if ( mZoomerLeft )
    mZoomerLeft->setEnabled( b );
  if ( mZoomerRight )
    mZoomerRight->setEnabled( b );
}

void ReosPlot_p::resetZoomBase()
{
  if ( mZoomerLeft )
    mZoomerLeft->setZoomBase();
  if ( mZoomerRight )
    mZoomerRight->setZoomBase();
}

void ReosPlot_p::setNormalMagnifier()
{
  if ( mMagnifier )
    mMagnifier->deleteLater();
  mMagnifier = new ReosNormalMagnifier( canvas() );
}

void ReosPlot_p::setPositiveMagnifier()
{
  if ( mMagnifier )
    mMagnifier->deleteLater();
  mMagnifier = new ReosPositiveMagnifier( canvas() );
}

ReosPositiveMagnifier::ReosPositiveMagnifier( QWidget *canvas ): ReosNormalMagnifier( canvas ), mIsYMinEnabeled( true )
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


ReosPlotConstantIntervalTimeIntervalSerie::ReosPlotConstantIntervalTimeIntervalSerie( ReosTimeSerieConstantInterval *timeSerie ):
  QwtSeriesData<QwtIntervalSample>()
  , mTimeSerie( timeSerie )
{}

size_t ReosPlotConstantIntervalTimeIntervalSerie::size() const
{
  if ( mTimeSerie )
    return mTimeSerie->valueCount();
  else
    return 0;
}

QwtIntervalSample ReosPlotConstantIntervalTimeIntervalSerie::sample( size_t i ) const
{
  if ( !mTimeSerie )
    return QwtIntervalSample();

  double y = mTimeSerie->valueAt( i );
  double x1 = QwtDate::toDouble( mTimeSerie->timeAt( i ) );
  double x2 = QwtDate::toDouble( mTimeSerie->timeAt( i ).addMSecs( mTimeSerie->timeStep()->value().valueMilliSecond() ) );

  return QwtIntervalSample( y, x1, x2 );
}

QRectF ReosPlotConstantIntervalTimeIntervalSerie::boundingRect() const
{
  if ( !mTimeSerie )
    return QRectF( 1.0, 1.0, -2.0, -2.0 ); // invalid for qwt
  const QPair<QDateTime, QDateTime> timeExtent = mTimeSerie->timeExtent();
  const  QPair<double, double> valueExtent = mTimeSerie->valueExent( true );
  double x1 = QwtDate::toDouble( timeExtent.first );
  double x2 = QwtDate::toDouble( timeExtent.second );
  return QRectF( x1, valueExtent.first, x2 - x1, valueExtent.second - valueExtent.first );
}

ReosTimeSerieConstantInterval *ReosPlotConstantIntervalTimeIntervalSerie::data() const
{
  if ( mTimeSerie.isNull() )
    return nullptr;
  else
    return mTimeSerie.data();
}

ReosDateScaleDraw_p::ReosDateScaleDraw_p( Qt::TimeSpec timeSpec ):
  QwtDateScaleDraw( timeSpec )
{
  setDateFormat( QwtDate::Millisecond, QStringLiteral( "mm:ss.zzz" ) );
  setDateFormat( QwtDate::Second, QStringLiteral( "hh:mm:ss" ) );
  setDateFormat( QwtDate::Minute, QStringLiteral( "hh:mm" ) );
  setDateFormat( QwtDate::Hour, QStringLiteral( "hh:mm\n dd MMM" ) );
  setDateFormat( QwtDate::Day, QStringLiteral( "hh:mm\nyyyy.MM.dd" ) );
  setDateFormat( QwtDate::Week, QStringLiteral( "yyyy.MM.dd" ) );
  setDateFormat( QwtDate::Month, QStringLiteral( "yyyy MMM" ) );
  setDateFormat( QwtDate::Year, QStringLiteral( "yyyy" ) );

  setLabelAlignment( Qt::AlignHCenter | Qt::AlignBottom );
}

void ReosDateScaleDraw_p::drawLabel( QPainter *painter, double value ) const
{
  QwtText lbl = tickLabel( painter->font(), value );
  if ( lbl.isEmpty() )
    return;
  lbl.setRenderFlags( Qt::AlignCenter );

  QPointF pos = labelPosition( value );
  QSizeF labelSize = lbl.textSize( painter->font() );
  const QTransform transform = labelTransformation( pos, labelSize );

  painter->save();
  painter->setWorldTransform( transform, true );

  lbl.draw( painter, QRect( QPoint( 0, 0 ), labelSize.toSize() ) );

  painter->restore();

}

ReosPlotConstantIntervalTimePointSerie::ReosPlotConstantIntervalTimePointSerie( ReosTimeSerieConstantInterval *timeSerie ):
  QwtSeriesData<QPointF>()
  , mTimeSerie( timeSerie )
{
  mValueMode = timeSerie->valueMode();
}

size_t ReosPlotConstantIntervalTimePointSerie::size() const
{
  if ( mTimeSerie )
    return mTimeSerie->valueCount() + ( mIsCumulative ? 1 : 0 );
  else
    return 0;
}

QPointF ReosPlotConstantIntervalTimePointSerie::sample( size_t i ) const
{
  if ( mIsCumulative )
  {
    double x;
    if ( i == 0 )
      x = QwtDate::toDouble( mTimeSerie->timeAt( 0 ) );
    else
      x = QwtDate::toDouble( mTimeSerie->timeAt( i - 1 ).addMSecs( mTimeSerie->timeStep()->value().valueMilliSecond() ) );

    return QPointF( x, mTimeSerie->valueWithMode( i, mValueMode ) );
  }
  else
    return QPointF( QwtDate::toDouble( mTimeSerie->timeAt( i ) ), mTimeSerie->valueWithMode( i, mValueMode ) );
}

QRectF ReosPlotConstantIntervalTimePointSerie::boundingRect() const
{
  if ( !mTimeSerie )
    return QRectF( 1.0, 1.0, -2.0, -2.0 ); // invalid for qwt
  QPair<QDateTime, QDateTime> timeExtent = mTimeSerie->timeExtent();
  QPair<double, double> valueExtent = mTimeSerie->extentValueWithMode( mValueMode );
  if ( mIsCumulative && mTimeSerie->valueCount() > 0 )
  {
    timeExtent.second = timeExtent.second.addMSecs( mTimeSerie->timeStep()->value().valueMilliSecond() );
    double lastValue = mTimeSerie->valueWithMode( mTimeSerie->valueCount(), mValueMode );
    if ( valueExtent.first > lastValue )
      valueExtent.first = lastValue;
    if ( valueExtent.second < lastValue )
      valueExtent.second = lastValue;
  }
  double x1 = QwtDate::toDouble( timeExtent.first );
  double x2 = QwtDate::toDouble( timeExtent.second );
  return QRectF( x1, valueExtent.first, x2 - x1, valueExtent.second - valueExtent.first );
}

ReosTimeSerieConstantInterval *ReosPlotConstantIntervalTimePointSerie::data() const
{
  if ( mTimeSerie.isNull() )
    return nullptr;
  else
    return mTimeSerie.data();
}

void ReosPlotConstantIntervalTimePointSerie::setCumulative( bool b )
{
  mIsCumulative = b;
  mValueMode = ReosTimeSerieConstantInterval::Cumulative;
}

QwtGraphic ReosPlotHistogramItem_p::legendIcon( int, const QSizeF &size ) const
{
  QwtGraphic icon;
  if ( !size.isEmpty() )
  {
    icon.setDefaultSize( size );
    QRectF r( 0, 0, size.width(), size.height() );
    QPainter painter( &icon );
    painter.setPen( pen() );
    painter.setBrush( brush() );
    painter.drawRect( r );
  }

  return icon;
}

ReosPlotVariableStepTimeSerie::ReosPlotVariableStepTimeSerie( ReosTimeSerieVariableTimeStep *timeSerie ):
  mTimeSerie( timeSerie )
{}

size_t ReosPlotVariableStepTimeSerie::size() const
{
  if ( mTimeSerie )
    return mTimeSerie->valueCount();
  else
    return 0;
}

QPointF ReosPlotVariableStepTimeSerie::sample( size_t i ) const
{
  double x = QwtDate::toDouble( mTimeSerie->timeAt( i ) );
  double y = mTimeSerie->valueAt( i );

  return QPointF( x, y );
}

QRectF ReosPlotVariableStepTimeSerie::boundingRect() const
{
  if ( !mTimeSerie )
    return QRectF( 1.0, 1.0, -2.0, -2.0 ); //invalid for qwt
  QPair<QDateTime, QDateTime> timeExtent = mTimeSerie->timeExtent();
  QPair<double, double> valueExtent = mTimeSerie->valueExent();
  double x1 = QwtDate::toDouble( timeExtent.first );
  double x2 = QwtDate::toDouble( timeExtent.second );
  return QRectF( x1, valueExtent.first, x2 - x1, valueExtent.second - valueExtent.first );
}

ReosTimeSerieVariableTimeStep *ReosPlotVariableStepTimeSerie::data() const
{
  if ( mTimeSerie.isNull() )
    return nullptr;

  return mTimeSerie;
}

ReosNormalMagnifier::ReosNormalMagnifier( QWidget *canvas ): QwtPlotMagnifier( canvas )
{
  setWheelFactor( 1.1 );
}

void ReosNormalMagnifier::widgetWheelEvent( QWheelEvent *wheelEvent )
{
  if ( wheelEvent->buttons() == Qt::NoButton )
    QwtMagnifier::widgetWheelEvent( wheelEvent );
}
