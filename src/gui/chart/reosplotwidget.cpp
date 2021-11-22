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

#include <QComboBox>
#include <QMenu>
#include <QToolButton>

#include "reosplotwidget.h"
#include "reosplot_p.h"
#include "reostimeserie.h"
#include "reosplottimeconstantinterval.h"
#include "reosidfcurves.h"
#include "reosplotidfcurve.h"
#include "reosplotitemlist.h"

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
#include <qwt_scale_widget.h>


ReosPlotWidget::ReosPlotWidget( QWidget *parent ): QWidget( parent ),
  mActionExportAsImage( new QAction( QPixmap( ":/images/savePlot.svg" ), tr( "Save as Image" ), this ) ),
  mActionCopyAsImage( new QAction( QPixmap( ":/images/copyPlot.svg" ), tr( "Copy as Image" ), this ) )
{
  QVBoxLayout *mainLayout = new QVBoxLayout ;
  setLayout( mainLayout );
  mainLayout->setContentsMargins( 0, 0, 0, 0 );
  mPlot = new ReosPlot_p( this );

  QFrame *plotCanvasFrame = dynamic_cast<QFrame *>( mPlot->canvas() );
  if ( plotCanvasFrame )
    plotCanvasFrame->setFrameStyle( QFrame::NoFrame );

  mPlot->setStyleSheet( "QFrame { background-color:white; }" );
  mPlot->setContentsMargins( 5, 5, 5, 5 );
  mPlot->setFrameStyle( QFrame::NoFrame );

  setLegendVisible( true );

  QHBoxLayout *toolBarslayout = new QHBoxLayout( this );
  toolBarslayout->setContentsMargins( 0, 0, 0, 0 );
  mainLayout->addItem( toolBarslayout );
  mToolBarRight = new QToolBar( this );
  mToolBarRight->setContentsMargins( 0, 0, 0, 0 );
  mToolBarRight->addAction( mActionExportAsImage );
  mToolBarRight->addAction( mActionCopyAsImage );
  toolBarslayout->addWidget( mToolBarRight );
  toolBarslayout->addItem( new QSpacerItem( 10, 0, QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding ) );
  mToolBarLeft = new QToolBar( this );
  mToolBarLeft->setContentsMargins( 0, 0, 0, 0 );
  toolBarslayout->addWidget( mToolBarLeft );
  mainLayout->addWidget( mPlot );

  setMagnifierType( normalMagnifier );

  connect( mActionExportAsImage, &QAction::triggered, this, &ReosPlotWidget::exportAsImage );
  connect( mActionCopyAsImage, &QAction::triggered, this, &ReosPlotWidget::copyAsImage );

  mPickerTracker = new QwtPlotPicker( mPlot->canvas() );
  mPickerTracker->setStateMachine( new QwtPickerTrackerMachine );
  mPickerTracker->setMousePattern( QwtEventPattern::MouseSelect3, Qt::NoButton );
  connect( mPickerTracker, &QwtPlotPicker::moved, this, &ReosPlotWidget::cursorMoved );


  mZoomerLeft = new QwtPlotZoomer( QwtPlot::xBottom, QwtPlot::yLeft, mPlot->canvas(), true );
  mZoomerLeft->setTrackerMode( QwtPicker::AlwaysOff );
  mZoomerRight = new QwtPlotZoomer( QwtPlot::xTop, QwtPlot::yRight, mPlot->canvas(), true );
  mZoomerRight->setTrackerMode( QwtPicker::AlwaysOff );

  mZoomerLeft->setMousePattern( QwtEventPattern::MouseSelect3, Qt::NoButton );
  mZoomerRight->setMousePattern( QwtEventPattern::MouseSelect3, Qt::NoButton );
  mPlot->setZoomer( mZoomerLeft, mZoomerRight );

  QComboBox *xAxisFormatCombobox = new QComboBox( this );
  mXAxisFormatCombobox = mToolBarLeft->addWidget( xAxisFormatCombobox );
  xAxisFormatCombobox->addItem( tr( "X linear scale" ) );
  xAxisFormatCombobox->addItem( tr( "X logarithmic scale" ) );
  connect( xAxisFormatCombobox, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, [this]( int index )
  {
    if ( index == 0 )
      mPlot->setAxisScaleEngine( QwtPlot::xBottom, new QwtLinearScaleEngine() );

    if ( index == 1 )
      mPlot->setAxisScaleEngine( QwtPlot::xBottom, new QwtLogScaleEngine( 10 ) );

    updatePlot();
  } );
  mXAxisFormatCombobox->setVisible( false );

  mainLayout->setStretch( 1, 1 );
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
      break;
    case positiveMagnifier:
      mPlot->setPositiveMagnifier();
      break;
  }
}

void ReosPlotWidget::setLegendAlignement( Qt::Alignment align )
{
  mPlot->setLegendAlignement( align );
}

void ReosPlotWidget::enableAutoMinimumSize( bool b )
{
  mPlot->enableAutoMinimumSize( b );
}

void ReosPlotWidget::setMinimumPlotSize( const QSize &size )
{
  mPlot->setMinimumPlotSize( size - QSize( mToolBarRight->size().height(), 0 ) );
}

void ReosPlotWidget::addActions( const QList<QAction *> &actions )
{
  if ( mToolBarRight )
    mToolBarRight->addActions( actions );
}

void ReosPlotWidget::addPlotItem( ReosPlotItem *item )
{
  if ( !item )
    return;
  item->attach( mPlot );
  item->setParent( this );
  item->setSettings();
  connect( item, &ReosPlotItem::itemChanged, this, &ReosPlotWidget::updatePlot );

  if ( item->isOnLeftAxe() )
    mZoomerLeft->setZoomBase();

  if ( item->isOnRightAxe() )
    mZoomerRight->setZoomBase();

}

void ReosPlotWidget::addOptionalPlotItem( ReosVariableTimeStepPlotListButton *optionalItemButton )
{
  mToolBarLeft->addWidget( optionalItemButton );
}

void ReosPlotWidget::setTitleAxeX( const QString &title )
{
  mPlot->setAxisTitle( QwtPlot::xBottom, title );
}

void ReosPlotWidget::setTitleAxeYLeft( const QString &title )
{
  mPlot->setAxisTitle( QwtPlot::yLeft, title );
}

void ReosPlotWidget::setTitleAxeYRight( const QString &title )
{
  mPlot->setAxisTitle( QwtPlot::yRight, title );
}

void ReosPlotWidget::enableAxeYright( bool b )
{
  mPlot->enableAxis( QwtPlot::yRight, b );
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

void ReosPlotWidget::setAxeXExtent( double min, double max )
{
  mPlot->setAxisScale( QwtPlot::xBottom, min, max );
  mZoomerLeft->setZoomBase();
  mZoomerRight->setZoomBase();
}

void ReosPlotWidget::setAxeXExtent( const QDateTime &timeMin, const QDateTime &timeMax )
{
  mPlot->setAxisScale( QwtPlot::xBottom, QwtDate::toDouble( timeMin ), QwtDate::toDouble( timeMax ) );
  mZoomerLeft->setZoomBase();
  mZoomerRight->setZoomBase();
}

void ReosPlotWidget::setAxeYLeftExtent( double min, double max )
{
  mPlot->setAxisScale( QwtPlot::yLeft, min, max );
  mZoomerLeft->setZoomBase();
}

void ReosPlotWidget::setAxeYRightExtent( double min, double max )
{
  mPlot->setAxisScale( QwtPlot::yRight, min, max );
  mZoomerRight->setZoomBase();
}

void ReosPlotWidget::setAxeTextSize( int size )
{
  QFont font = mPlot->axisWidget( QwtPlot::xBottom )->font();
  font.setPointSize( size );
  mPlot->axisWidget( QwtPlot::xBottom )->setFont( font );

  font = mPlot->axisWidget( QwtPlot::yLeft )->font();
  font.setPointSize( size );
  mPlot->axisWidget( QwtPlot::yLeft )->setFont( font );

  font = mPlot->axisWidget( QwtPlot::yRight )->font();
  font.setPointSize( size );
  mPlot->axisWidget( QwtPlot::yRight )->setFont( font );
}

void ReosPlotWidget::enableScaleTypeChoice( bool b )
{
  mXAxisFormatCombobox->setVisible( b );
}

QString ReosPlotWidget::plotEngineName()
{
  return QStringLiteral( "Qwt" );
}

QString ReosPlotWidget::plotEngineVersion()
{
  return QWT_VERSION_STR;
}

QString ReosPlotWidget::plotEngineLink()
{
  return QStringLiteral( "qwt.sourceforge.io" );
}

void ReosPlotWidget::resetZoomBase()
{
  mZoomerLeft->setZoomBase();
  mZoomerRight->setZoomBase();
}

void ReosPlotWidget::updatePlot()
{
  mPlot->autoScale();
  mPlot->replot();
  mZoomerLeft->setZoomBase();
  mZoomerRight->setZoomBase();
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

void ReosPlotItem::attach( ReosPlot_p *plot )
{
  if ( mPlotItem )
  {
    mPlotItem->attach( plot );
    mAttached = true;
  }
}

void ReosPlotItem::detach()
{
  if ( mPlotItem && mAttached )
  {
    mPlotItem->detach();
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

bool ReosPlotItem::isOnRightAxe()
{
  if ( mPlotItem )
    return mPlotItem->yAxis() == QwtPlot::yRight;
  else
    return false;
}

void ReosPlotItem::setOnLeftAxe()
{
  if ( mPlotItem )
    mPlotItem->setYAxis( QwtPlot::yLeft );
}

bool ReosPlotItem::isOnLeftAxe()
{
  if ( mPlotItem )
    return mPlotItem->yAxis() == QwtPlot::yLeft;
  else
    return false;
}

void ReosPlotItem::setAsMasterItem( bool b )
{
  mMasterItem = b;
}

void ReosPlotItem::setAutoScale( bool b )
{
  if ( mPlotItem )
    mPlotItem->setItemAttribute( QwtPlotItem::AutoScale, b );
}

QString ReosPlotItem::name() const
{
  if ( mPlotItem )
    return mPlotItem->title().text();
  else
    return QString();
}

void ReosPlotItem::setVisible( bool isVisible, bool replot )
{
  if ( mPlotItem )
  {
    mPlotItem->setVisible( isVisible );
    if ( replot )
      mPlotItem->plot()->replot();
  }
}

bool ReosPlotItem::isVisible() const
{
  if ( mPlotItem )
    return mPlotItem->isVisible();
  else
    return false;
}

void ReosPlotItem::setColor( const QColor & ) {}

void ReosPlotItem::setStyle( Qt::PenStyle ) {}

void ReosPlotItem::setWidth( double ) {}

QPixmap ReosPlotItem::icone( const QSize & ) const
{
  return QPixmap();
}

void ReosPlotItem::fullExtent()
{
  if ( mPlotItem )
    mPlotItem->plot()->replot();
}

void ReosPlotItem::setName( const QString &name )
{
  if ( mPlotItem )
    mPlotItem->setTitle( name );
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


void ReosPlotItemFactories::instantiate( ReosModule *parent )
{
  if ( !sInstance )
    sInstance = new ReosPlotItemFactories( parent );
}

bool ReosPlotItemFactories::isInstantiate()
{
  return sInstance != nullptr;
}

void ReosPlotItemFactories::addFactory( ReosDataPlotItemFactory *fact )
{
  for ( const Factory &currentFact : mFactories )
    if ( currentFact->datatype() == fact->datatype() )
      return;

  mFactories.emplace_back( fact );
}

ReosPlotItemFactories *ReosPlotItemFactories::sInstance = nullptr;

ReosPlotItemFactories *ReosPlotItemFactories::instance()
{
  if ( !sInstance )
    instantiate();
  return sInstance;
}

void ReosPlotItemFactories::buildPlotItemsAndSetup( ReosPlotWidget *plotWidget, ReosDataObject *data, const QString &dataType )
{
  QString type = dataType;
  if ( type.isEmpty() )
    type = data->type();

  for ( const Factory &fact : mFactories )
    if ( type.contains( fact->datatype() ) )
    {
      fact->buildPlotItemsAndSetup( plotWidget, data );
      return;
    }
}

ReosPlotItem *ReosPlotItemFactories::buildPlotItem( ReosPlotWidget *plotWidget, ReosDataObject *data )
{
  for ( const Factory &fact : mFactories )
    if ( data->type().contains( fact->datatype() ) )
    {
      return fact->buildPlotItem( plotWidget, data );;
    }

  return nullptr;
}

ReosPlotItemFactories::ReosPlotItemFactories( ReosModule *parent ): ReosModule( parent ) {}

void ReosDataPlotItemFactory::buildPlotItemsAndSetup( ReosPlotWidget *, ReosDataObject * ) {}

ReosPlotItem *ReosDataPlotItemFactory::buildPlotItem( ReosPlotWidget *, ReosDataObject * ) {return nullptr;}
