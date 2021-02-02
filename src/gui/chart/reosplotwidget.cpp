/***************************************************************************
  reosplot.cpp - ReosPlot

 ---------------------
 begin                : 13.1.2021
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

#include "reosplotwidget.h"
#include "reosplot_p.h"
#include "reostimeserie.h"
#include "reosplottimeconstantinterval.h"

#include <qwt_plot.h>
#include <qwt_plot_curve.h>
#include <qwt_plot_histogram.h>
#include <qwt_plot_grid.h>
#include <qwt_plot_legenditem.h>
#include <qwt_plot_magnifier.h>
#include <qwt_plot_panner.h>
#include <qwt_picker_machine.h>
#include <qwt_plot_zoomer.h>
#include <qwt_plot_renderer.h>
#include <qwt_date_scale_draw.h>
#include <qwt_date_scale_engine.h>

ReosPlotWidget::ReosPlotWidget( QWidget *parent ): QWidget( parent ),
  mActionExportAsImage( new QAction( QPixmap( ":/images/savePlot.svg" ), tr( "Save as Image" ), this ) ),
  mActionCopyAsImage( new QAction( QPixmap( ":/images/copyPlot.svg" ), tr( "Copy as Image" ), this ) )
{
  setLayout( new QVBoxLayout );
  layout()->setMargin( 0 );
  mPlot = new ReosPlot_p( this );
  setLegendVisible( true );
  mToolBar = new QToolBar;
  mToolBar->addAction( mActionExportAsImage );
  mToolBar->addAction( mActionCopyAsImage );
  layout()->addWidget( mToolBar );

  layout()->addWidget( mPlot );

  setMagnifierType( normalMagnifier );

  connect( mActionExportAsImage, &QAction::triggered, this, &ReosPlotWidget::exportAsImage );
  connect( mActionCopyAsImage, &QAction::triggered, this, &ReosPlotWidget::copyAsImage );

  mPickerTracker = new QwtPlotPicker( mPlot->canvas() );
  mPickerTracker->setStateMachine( new QwtPickerTrackerMachine );
  connect( mPickerTracker, &QwtPlotPicker::moved, this, &ReosPlotWidget::cursorMoved );

}


void ReosPlotWidget::setLegendVisible( bool b )
{
  if ( mPlot )
    mPlot->setLegendVisible( b );
}

void ReosPlotWidget::setMagnifierType( ReosPlotWidget::MagnifierType type )
{
  switch ( type )
  {
    case normalMagnifier:
      mPlot->setNormalMagnifier();
    case positiveMagnifier:
      mPlot->setPositiveMagnifier();
      break;
  }
}

void ReosPlotWidget::setEnableZoomer( bool b )
{

  mPlot->setEnableZoomer( b );
}

void ReosPlotWidget::setLegendAlignement( Qt::Alignment align )
{
  mPlot->setLegendAlignement( align );
}

void ReosPlotWidget::enableAutoMinimumSize( bool b )
{
  mPlot->enableAutoMinimumSize( b );
}

void ReosPlotWidget::setMinimumPlotSize( QSize size )
{
  mPlot->setMinimumPlotSize( size - QSize( mToolBar->size().height(), 0 ) );
}

void ReosPlotWidget::addActions( QList<QAction *> actions )
{
  if ( mToolBar )
    mToolBar->addActions( actions );
}

void ReosPlotWidget::addPlotItem( ReosPlotItem *item )
{
  if ( !item )
    return;
  item->attach( mPlot );
  item->setParent( this );
  connect( item, &ReosPlotItem::itemChanged, this, &ReosPlotWidget::updatePlot );
}

void ReosPlotWidget::addDataObject( ReosDataObject *data )
{
  createItems( data );
}

void ReosPlotWidget::setTitleAxeX( const QString &title )
{
  mPlot->setAxisTitle( QwtPlot::xBottom, title );
}

void ReosPlotWidget::setTitleAxeYleft( const QString &title )
{
  mPlot->setAxisTitle( QwtPlot::yLeft, title );
}

void ReosPlotWidget::setTitleAxeYRight( const QString &title )
{
  mPlot->setAxisTitle( QwtPlot::yRight, title );
}

static void setAxeType( QwtPlot *plot, QwtPlot::Axis axe, ReosPlotWidget::AxeType type )
{
  switch ( type )
  {
    case ReosPlotWidget::normal:
      break;
    case ReosPlotWidget::temporal:
      plot->setAxisScaleDraw( axe, new ReosDateScaleDraw_p( Qt::UTC ) );
      plot->setAxisScaleEngine( axe, new QwtDateScaleEngine( Qt::UTC ) );
      break;
    case ReosPlotWidget::logarithm:
      break;
  }
}

void ReosPlotWidget::setAxeXType( ReosPlotWidget::AxeType type )
{
  setAxeType( mPlot, QwtPlot::xBottom, type );
}

void ReosPlotWidget::updatePlot()
{
  mPlot->setAxisAutoScale( QwtPlot::xBottom );
  mPlot->setAxisAutoScale( QwtPlot::yLeft );
  mPlot->setAxisAutoScale( QwtPlot::yRight );
  mPlot->replot();
}

void ReosPlotWidget::exportAsImage()
{
  // TODO make proper export or try to configure this one to have better export
  QwtPlotRenderer renderer;
  renderer.setDiscardFlag( QwtPlotRenderer::DiscardBackground, true );
  renderer.exportTo( mPlot, "export", QSizeF( 170, 110 ), 300 );
}

void ReosPlotWidget::copyAsImage()
{
  QImage image( 700, 466, QImage::Format_RGB32 );
  image.fill( Qt::white );
  QwtPlotRenderer renderer;
  renderer.setDiscardFlag( QwtPlotRenderer::DiscardBackground, true );
  renderer.renderTo( mPlot, image );

  QClipboard *clip = QApplication::clipboard();
  clip->setImage( image );
}

void ReosPlotWidget::receiveMoveFromPicker( const QPointF &pt )
{
  emit cursorMoved( pt );
}


void ReosPlotWidget::createItems( ReosDataObject *data )
{
  if ( data && data->type() == QStringLiteral( "time-serie-constant-interval" ) )
  {
    ReosTimeSerieConstantInterval *_data = static_cast<ReosTimeSerieConstantInterval *>( data );

    if ( _data->valueMode() != ReosTimeSerieConstantInterval::Cumulative )
    {
      std::unique_ptr<ReosPlotTimeHistogram> histogram = std::make_unique<ReosPlotTimeHistogram>( _data->name() + tr( ", instant value" ) );
      histogram->setTimeSerie( _data );
      addPlotItem( histogram.release() );
    }

    if ( _data->valueMode() == ReosTimeSerieConstantInterval::Cumulative || _data->addCumultive() )
    {
      mPlot->enableAxis( QwtPlot::yRight );
      //mPlot->setAxisAutoScale( QwtPlot::yRight, true );
      std::unique_ptr<ReosPlotTimeCumulativeCurve> cumulCurve = std::make_unique<ReosPlotTimeCumulativeCurve>( _data->name() + tr( ", cumulative value" ) );
      cumulCurve->setTimeSerie( _data );
      cumulCurve->setOnRightAxe();
      addPlotItem( cumulCurve.release() );
    }
  }
}


void ReosPlotItem::attach( ReosPlot_p *plot )
{
  if ( mPlotItem )
  {
    mPlotItem->attach( plot );
    mAttached = true;
  }
}

ReosPlotItem::~ReosPlotItem()
{
  if ( mPlotItem && !mAttached )
  {
    delete mPlotItem;
  }
}

void ReosPlotItem::setOnRightAxe()
{
  if ( mPlotItem )
    mPlotItem->setYAxis( QwtPlot::yRight );
}

void ReosPlotItem::setOnLeftAxe()
{
  if ( mPlotItem )
    mPlotItem->setYAxis( QwtPlot::yLeft );
}

ReosPlotCurve::ReosPlotCurve( const QString &name, const QColor &color, double width ): ReosPlotItem()
{
  mPlotItem = new QwtPlotCurve( name );
  QPen pen;
  pen.setColor( color );
  pen.setWidthF( width );
  curve()->setPen( pen );
}

void ReosPlotCurve::setData( const QPolygonF &data )
{
  curve()->setSamples( data );
  emit itemChanged();
}

void ReosPlotCurve::zoomOnExtent()
{
  if ( curve() && curve()->plot() )
  {
    curve()->plot()->setAxisScale( curve()->xAxis(), curve()->minXValue(), curve()->maxXValue() );
    curve()->plot()->setAxisScale( curve()->yAxis(), curve()->minYValue(), curve()->maxYValue() );
    curve()->plot()->replot();
  }
}

QwtPlotCurve *ReosPlotCurve::curve()
{
  return static_cast<QwtPlotCurve *>( mPlotItem );
}

