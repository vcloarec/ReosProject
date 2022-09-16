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
#include <QSpinBox>
#include <QLabel>
#include <QMoveEvent>

#include "reosplotwidget.h"
#include "reosplot_p.h"
#include "reostimeserie.h"
#include "reosplottimeconstantinterval.h"
#include "reosidfcurves.h"
#include "reosplotidfcurve.h"
#include "reosplotitemlist.h"
#include "reossettings.h"
#include "reosstyleregistery.h"
#include "reosplottimeline.h"

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


class CoordinatesWidget: public QWidget
{
  public:
    CoordinatesWidget( QwtPlot *plot )
      : QWidget( plot->canvas() )
      , mPlot( plot )
    {
      setLayout( new QVBoxLayout );
      layout()->setContentsMargins( 0, 0, 0, 0 );
      layout()->setSpacing( 0 );
      setAttribute( Qt::WA_TransparentForMouseEvents );
      mLabelX = new QLabel( tr( "X:" ), this );
      layout()->addWidget( mLabelX );
      mLabelYLeft = new QLabel( this );
      layout()->addWidget( mLabelYLeft );
      mLabelYRight = new QLabel( this );
      layout()->addWidget( mLabelYRight );

      setStyleSheet( "QLabel{background-color: rgba(255,255,255,127)}" );
    }

    void enableYRight( bool b )
    {
      mIsRightYEnabled = b;
    }

    bool eventFilter( QObject *obj, QEvent *event )
    {
      switch ( event->type() )
      {
        case QEvent::Enter:
          setVisible( isEnabled() );
          break;
        case QEvent::Leave:
          setVisible( false );
          break;
        case QEvent::MouseMove:
          updatePosition( static_cast<QMouseEvent *>( event )->pos() );
          break;
        default:
          break;
      }

      return false;
    }

    void updatePosition( const QPoint &pos )
    {
      move( pos + QPoint( 15, -15 ) );
      if ( mXType == ReosPlotWidget::temporal )
      {
        const QDateTime time = QwtDate::toDateTime( mPlot->canvasMap( QwtPlot::xBottom ).invTransform( pos.x() ) );
        mLabelX->setText( tr( "X: " ) + QLocale().toString( time, QLocale::ShortFormat ) );
      }
      else
      {
        mLabelX->setText( QString::number( mPlot->canvasMap( QwtPlot::xBottom ).invTransform( pos.x() ), 'f', 2 ) );
      }

      QString prefix;
      if ( mIsRightYEnabled )
      {
        prefix = tr( "Y Left: " );
        mLabelYRight->setText( tr( "Y Right: " ) +  QString::number( mPlot->canvasMap( QwtPlot::yRight ).invTransform( pos.y() ), 'f', 2 ) );
      }
      else
      {
        prefix = tr( "Y: " );
      }
      mLabelYLeft->setText( prefix + QString::number( mPlot->canvasMap( QwtPlot::yLeft ).invTransform( pos.y() ), 'f', 2 ) );

      adjustSize();
    }

    void setXType( const ReosPlotWidget::AxeType &xType )
    {
      mXType = xType;
    }

  private:
    QLabel *mLabelX = nullptr;
    QLabel *mLabelYLeft = nullptr;
    QLabel *mLabelYRight = nullptr;
    QwtPlot *mPlot = nullptr;

    ReosPlotWidget::AxeType mXType;
    bool mIsRightYEnabled = false;
};


ReosPlotWidget::ReosPlotWidget( QWidget *parent )
  : QWidget( parent )
  , mActionExportAsImage( new QAction( QPixmap( ":/images/savePlot.svg" ), tr( "Save as Image" ), this ) )
  , mActionCopyAsImage( new QAction( QPixmap( ":/images/copyPlot.svg" ), tr( "Copy as Image" ), this ) )
  , mActionTimeLine( new QAction( QPixmap( ":/images/temporalLine.svg" ), tr( "Time Line" ), this ) )
  , mActionCoordinates( new QAction( QPixmap( ":/images/cursorCoordinates.svg" ), tr( "Display Coordinates on Cursor" ), this ) )
{
  QVBoxLayout *mainLayout = new QVBoxLayout ;
  setLayout( mainLayout );
  mainLayout->setContentsMargins( 0, 0, 0, 0 );
  mPlot = new ReosPlot_p( this );

  mCoordinatesWidget = new CoordinatesWidget( mPlot );
  mCoordinatesWidget->setAttribute( Qt::WA_TransparentForMouseEvents );
  mCoordinatesWidget->setEnabled( false );
  mActionCoordinates->setCheckable( true );
  connect( mActionCoordinates, &QAction::triggered, this, &ReosPlotWidget::enableCursorCoordinates );

  mPlot->canvas()->installEventFilter( mCoordinatesWidget );

  QFrame *plotCanvasFrame = dynamic_cast<QFrame *>( mPlot->canvas() );
  if ( plotCanvasFrame )
    plotCanvasFrame->setFrameStyle( QFrame::NoFrame );

  mPlot->setStyleSheet( "QFrame { background-color:white; }" );
  mPlot->setContentsMargins( 5, 5, 5, 5 );
  mPlot->setFrameStyle( QFrame::NoFrame );

  QHBoxLayout *toolBarslayout = new QHBoxLayout;
  toolBarslayout->setContentsMargins( 0, 0, 0, 0 );
  mainLayout->addItem( toolBarslayout );
  mToolBarLeft = new QToolBar( this );
  mToolBarLeft->setContentsMargins( 3, 3, 0, 0 );
  mToolBarLeft->layout()->setSpacing( 5 );
  toolBarslayout->addWidget( mToolBarLeft );
  toolBarslayout->addItem( new QSpacerItem( 10, 0, QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding ) );
  mToolBarRight = new QToolBar( this );
  mToolBarRight->setContentsMargins( 3, 3, 0, 0 );
  mToolBarRight->layout()->setSpacing( 5 );

  mToolBarLeft->addAction( mActionExportAsImage );
  mToolBarLeft->addAction( mActionCopyAsImage );
  toolBarslayout->addWidget( mToolBarRight );
  mainLayout->addWidget( mPlot );

  mToolBarLeft->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );
  mToolBarRight->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  mLegendController = new ReosPlotLegendController( this );
  mActionLegendController = mToolBarLeft->addWidget( mLegendController );
  connect( mLegendController, &ReosPlotLegendController::legendVisible, this, &ReosPlotWidget::setLegendVisible );
  mLegendController->setChecked( true );

  mToolBarLeft->addAction( mActionTimeLine );
  mActionTimeLine->setCheckable( true );
  mActionTimeLine->setVisible( false );
  connect( mActionTimeLine, &QAction::triggered, this, &ReosPlotWidget::setTimeLineVisible );

  mToolBarLeft->addAction( mActionCoordinates );

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
  mXAxisFormatCombobox = mToolBarRight->addWidget( xAxisFormatCombobox );
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

  setAxesTextSize( 8 );
  setAxesTitleSize( 10 );
}

ReosPlotWidget::~ReosPlotWidget()
{}

void ReosPlotWidget::setSettingsContext( const QString &settingContext )
{
  mSettingsContext = settingContext;

  ReosSettings settings;
  if ( settings.contains( settingsPrefix() + QStringLiteral( "legendAlignment" ) ) )
  {
    Qt::AlignmentFlag align = static_cast<Qt::AlignmentFlag>( settings.value( settingsPrefix() + QStringLiteral( "legendAlignment" ) ).toInt() );
    mLegendController->setCurrentAlignment( align );
    mPlot->setLegendAlignement( align );
  }

  if ( settings.contains( settingsPrefix() + QStringLiteral( "legendColumnCount" ) ) )
  {
    int legenColumnCount = settings.value( settingsPrefix() + QStringLiteral( "legendColumnCount" ) ).toInt();
    mLegendController->setCurrentColumnCount( legenColumnCount );
    mPlot->setLegendColumnCount( legenColumnCount );
  }

  if ( settings.contains( settingsPrefix() + QStringLiteral( "legendVisible" ) ) )
  {
    bool legendVisible = settings.value( settingsPrefix() + QStringLiteral( "legendVisible" ) ).toBool();
    mLegendController->setChecked( legendVisible );
    mPlot->setLegendVisible( legendVisible );
  }

  if ( settings.contains( settingsPrefix() + QStringLiteral( "timeLineVisible" ) ) )
  {
    bool timeLineVisible = settings.value( settingsPrefix() + QStringLiteral( "timeLineVisible" ) ).toBool();
    mActionTimeLine->setChecked( timeLineVisible );
    mTimeLine->setVisible( timeLineVisible );
  }

  if ( settings.contains( settingsPrefix() + QStringLiteral( "cursorCoordinates" ) ) )
  {
    bool cursorCoordinates = settings.value( settingsPrefix() + QStringLiteral( "cursorCoordinates" ) ).toBool();
    mActionCoordinates->setChecked( cursorCoordinates );
    mCoordinatesWidget->setEnabled( cursorCoordinates );
  }
}

void ReosPlotWidget::setLegendEnabled( bool b )
{
  mActionLegendController->setVisible( b );
  mLegendController->setLegendEnabled( b );
}


void ReosPlotWidget::setLegendVisible( bool b )
{
  if ( mPlot )
  {
    mPlot->setLegendVisible( b );
    mPlot->replot();
    ReosSettings settings;
    settings.setValue( settingsPrefix() + QStringLiteral( "legendVisible" ), b );
  }
}

void ReosPlotWidget::setTimeLineVisible( bool b )
{
  ReosSettings settings;
  settings.setValue( settingsPrefix() + QStringLiteral( "timeLineVisible" ), b );
  if ( mTimeLine )
  {
    mActionTimeLine->setChecked( b );
    mTimeLine->setVisible( b );
    mTimeLine->plot()->replot();
  }
}

void ReosPlotWidget::enableCursorCoordinates( bool b )
{
  ReosSettings settings;
  settings.setValue( settingsPrefix() + QStringLiteral( "cursorCoordinates" ), b );
  if ( mCoordinatesWidget )
  {
    mActionCoordinates->setChecked( b );
    mCoordinatesWidget->setEnabled( b );
  }
}

QString ReosPlotWidget::settingsPrefix() const
{
  return mSettingsContext + QStringLiteral( "/plotWidget/" );
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
  ReosSettings settings;
  settings.setValue( settingsPrefix() + QStringLiteral( "legendAlignment" ), static_cast<Qt::Alignment::Int>( align ) );
  if ( mPlot )
    mPlot->replot();
}

void ReosPlotWidget::setLegendColumnCount( int columnCount )
{
  mPlot->setLegendColumnCount( columnCount );
  ReosSettings settings;
  settings.setValue( settingsPrefix() + QStringLiteral( "legendColumnCount" ), columnCount );
  if ( mPlot )
    mPlot->replot();
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
    mZoomerLeft->setZoomBase( true );

  if ( item->isOnRightAxe() )
    mZoomerRight->setZoomBase( true );

}

void ReosPlotWidget::addOptionalPlotItem( ReosVariableTimeStepPlotListButton *optionalItemButton )
{
  mToolBarRight->addWidget( optionalItemButton );
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

void ReosPlotWidget::enableAxeYRight( bool b )
{
  mPlot->enableAxis( QwtPlot::yRight, b );
  mCoordinatesWidget->enableYRight( b );
}

void ReosPlotWidget::enableAutoScale( bool b )
{
  mPlot->enableAutoScale( b );
}

void ReosPlotWidget::enableAutoScaleX( bool b )
{
  mPlot->enableAutoScaleX( b );
}

void ReosPlotWidget::enableAutoScaleY( bool b )
{
  mPlot->enableAutoScaleY( b );
}


class ReoDateScaleEngine_p : public QwtDateScaleEngine
{
  public:
    ReoDateScaleEngine_p( QWidget *container, QwtScaleDraw *scaleDraw, Qt::TimeSpec timespec = Qt::LocalTime )
      : QwtDateScaleEngine( timespec )
      , mScaleDraw( scaleDraw )
      , mContainer( container )
    {}

    void autoScale( int maxNumSteps, double &x1, double &x2, double &stepSize ) const
    {
      // here we check if the label width is not too large leading to label overlay.
      // if yes we reduce the maxNumStep until it is ok
      // it is necessary to insit because Qwt can consider internally greater 'maxNumSteps' to obtain "rounnd" value
      bool ok = false;
      int intervalCount = 0;
      do
      {
        QwtDateScaleEngine::autoScale( maxNumSteps, x1, x2, stepSize );

        if ( stepSize > 0 )
        {
          int labelWidth = mScaleDraw->maxLabelWidth( mContainer->font() );
          intervalCount = ( x2 - x1 ) / stepSize;
          int totalLabelWidth = labelWidth * ( 1 + intervalCount );
          ok = 1.2 * totalLabelWidth < mContainer->width();
          if ( !ok )
            maxNumSteps--;
        }

      }
      while ( !ok && maxNumSteps != 1  && stepSize > 0 );
    }

  private:
    QwtScaleDraw *mScaleDraw = nullptr;
    QWidget *mContainer = nullptr;
};

static void setAxeType( ReosPlot_p *plot, QwtPlot::Axis axe, ReosPlotWidget::AxeType type )
{
  switch ( type )
  {
    case ReosPlotWidget::normal:
      break;
    case ReosPlotWidget::temporal:
    {
      std::unique_ptr<ReosDateScaleDraw_p> scaleDraw( new ReosDateScaleDraw_p( Qt::UTC ) );
      plot->setAxisScaleEngine( axe, new ReoDateScaleEngine_p( plot->axisWidget( axe ), scaleDraw.get(), Qt::UTC ) );
      plot->setAxisScaleDraw( axe, scaleDraw.release() );
      plot->setUpdateAxesWhenResize( true );
    }
    break;
    case ReosPlotWidget::logarithm:
      break;
  }
}

void ReosPlotWidget::setAxeXType( ReosPlotWidget::AxeType type )
{
  mAxeType = type;
  setAxeType( mPlot, QwtPlot::xBottom, type );
  mCoordinatesWidget->setXType( type );
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

void ReosPlotWidget::setAxesTextSize( int size )
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

void ReosPlotWidget::setAxesTitleSize( int size )
{
  for ( int i = 0; i < QwtPlot::axisCnt; ++i )
  {
    QwtText title = mPlot->axisTitle( QwtPlot::xBottom );
    QFont font = title.font();
    font.setPointSize( size );
    title.setFont( font );
    mPlot->setAxisTitle( i, title );
  }
}

void ReosPlotWidget::enableScaleTypeChoice( bool b )
{
  mXAxisFormatCombobox->setVisible( b );
}

void ReosPlotWidget::enableTimeLine( bool b )
{
  if ( b && !mTimeLine && mAxeType == temporal )
  {
    mTimeLine.reset( new ReosPlotTimeLine );
    mTimeLine->attach( mPlot );
    mActionTimeLine->setVisible( true );
    if ( mActionTimeLine->isChecked() )
      mTimeLine->show();
    else
      mTimeLine->hide();
  }

  if ( ( !b && mTimeLine ) &&  mAxeType != temporal )
  {
    mTimeLine->detach();
    mTimeLine.reset();
    mActionTimeLine->setVisible( false );
  }
}

void ReosPlotWidget::setTime( const QDateTime &time )
{
  if ( mTimeLine )
    mTimeLine->setTime( time );
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

void ReosPlotItem::setLegendActive( bool legendActive, bool updateLegend )
{
  if ( mPlotItem )
  {
    mPlotItem->setItemAttribute( QwtPlotItem::Legend, legendActive );
    if ( updateLegend && mPlotItem->plot() )
      mPlotItem->plot()->updateLegend();
  }
}

void ReosPlotItem::setZ( double z )
{
  if ( mPlotItem )
  {
    mPlotItem->setZ( z );
  }
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

class ReosPlotCurve_p: public QwtPlotCurve
{
  public:
    ReosPlotCurve_p( const QString &name ): QwtPlotCurve( name )
    {}

    QRectF dataRect() const override
    {
      const QwtSeriesData<QPointF> *points = data();

      if ( !points )
        return QRectF( 1.0, 1.0, -2.0, -2.0 );

      double xMin = std::numeric_limits<double>::max();
      double yMin = std::numeric_limits<double>::max();
      double xMax = -std::numeric_limits<double>::max();
      double yMax = -std::numeric_limits<double>::max();

      for ( size_t i = 0; i < points->size(); ++i )
      {
        const QPointF &pt = points->sample( i );

        double x = pt.x();
        double y = pt.y();

        if ( std::isnan( x ) || std::isnan( y ) )
          continue;

        if ( x < xMin )
          xMin = x;
        if ( x > xMax )
          xMax = x;
        if ( y < yMin )
          yMin = y;
        if ( pt.y() > yMax )
          yMax = y;
      }

      return QRectF( QPointF( xMin, yMin ), QPointF( xMax, yMax ) );
    }

  protected:
    void drawCurve( QPainter *painter, int style,
                    const QwtScaleMap &xMap, const QwtScaleMap &yMap,
                    const QRectF &canvasRect, int from, int to ) const override
    {
      const QwtSeriesData<QPointF> *points = data();
      if ( !points )
        return;

      int tempFrom = from;
      int tempTo = from;

      auto validPoint = [ points ]( int i )
      {
        const QPointF &pt = points->sample( static_cast<size_t>( i ) );
        return !std::isnan( pt.x() ) && !std::isnan( pt.y() );
      };

      while ( tempFrom <= to && tempTo <= to )
      {
        while ( !validPoint( tempFrom ) && tempFrom <= to )
          tempFrom++;

        tempTo = tempFrom;
        while ( validPoint( tempTo ) && tempTo <= to )
          tempTo++;

        tempTo--;

        if ( validPoint( tempFrom ) && validPoint( tempTo ) )
          QwtPlotCurve::drawCurve( painter, style, xMap, yMap, canvasRect, tempFrom, tempTo );

        tempFrom = tempTo + 1;
      }
    }
};

ReosPlotCurve::ReosPlotCurve( const QString &name, const QColor &color, double width ): ReosPlotItem()
{
  mPlotItem = new ReosPlotCurve_p( name );
  mPlotItem->setRenderHint( QwtPlotItem::RenderAntialiased, true );
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

void ReosPlotCurve::setWidth( double width )
{
  QPen pen = curve()->pen();
  pen.setWidthF( width );
  curve()->setPen( pen );
  curve()->plot()->replot();
}

ReosPlotCurve_p *ReosPlotCurve::curve()
{
  return static_cast<ReosPlotCurve_p *>( mPlotItem );
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

ReosPlotLegendController::ReosPlotLegendController( ReosPlotWidget *plotWidget )
  : QToolButton( plotWidget )
  , mPlotWidget( plotWidget )
{
  setText( tr( "Legend" ) );
  setIcon( QPixmap( QStringLiteral( ":/images/plotLegend.svg" ) ) );
  setCheckable( true );
  setPopupMode( QToolButton::MenuButtonPopup );
  connect( this, &QToolButton::toggled, this, &ReosPlotLegendController::legendVisible );
  QMenu *menu = new QMenu( this );
  setMenu( menu );

  QWidget *legendWidget = new QWidget( this );
  legendWidget->setLayout( new QVBoxLayout );
  legendWidget->layout()->setContentsMargins( 0, 0, 0, 0 );

  QWidget *widgetPlacement = new QWidget( legendWidget );
  legendWidget->layout()->addWidget( widgetPlacement );
  QGridLayout *placementLayout = new QGridLayout( widgetPlacement );
  widgetPlacement->setLayout( placementLayout );
  placementLayout->setContentsMargins( 3, 3, 3, 3 );
  placementLayout->setSpacing( 2 );

  mLegendAlignments << Qt::Alignment( Qt::AlignTop | Qt::AlignLeft )
                    << Qt::Alignment( Qt::AlignTop | Qt::AlignHCenter )
                    << Qt::Alignment( Qt::AlignTop | Qt::AlignRight )
                    << Qt::Alignment( Qt::AlignVCenter | Qt::AlignLeft )
                    << Qt::Alignment( Qt::AlignVCenter | Qt::AlignHCenter )
                    << Qt::Alignment( Qt::AlignVCenter | Qt::AlignRight )
                    << Qt::Alignment( Qt::AlignBottom | Qt::AlignLeft )
                    << Qt::Alignment( Qt::AlignBottom | Qt::AlignHCenter )
                    << Qt::Alignment( Qt::AlignBottom | Qt::AlignRight );
  for ( int i = 0; i < 9; ++i )
  {
    QToolButton *tb = new QToolButton( widgetPlacement );
    tb->setCheckable( true );
    tb->setAutoRaise( true );
    mAlignmentButtons << tb;
  }

  mAlignmentButtons.at( 0 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentTL.svg" ) ) );
  mAlignmentButtons.at( 1 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentTC.svg" ) ) );
  mAlignmentButtons.at( 2 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentTR.svg" ) ) );
  mAlignmentButtons.at( 3 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentCL.svg" ) ) );
  mAlignmentButtons.at( 4 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentCC.svg" ) ) );
  mAlignmentButtons.at( 5 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentCR.svg" ) ) );
  mAlignmentButtons.at( 6 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentBL.svg" ) ) );
  mAlignmentButtons.at( 7 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentBC.svg" ) ) );
  mAlignmentButtons.at( 8 )->setIcon( QPixmap( QStringLiteral( ":/images/alignmentBR.svg" ) ) );

  for ( int i = 0; i < 9; ++i )
  {
    connect( mAlignmentButtons.at( i ), &QToolButton::clicked, this, [this, plotWidget, i]
    {
      if ( !this->mAlignmentButtons.at( i )->isChecked() )
      {
        this->mAlignmentButtons.at( i )->setChecked( true );
        return;
      }

      for ( int ib = 0; ib < this->mAlignmentButtons.count(); ++ib )
        if ( ib != i )
          this->mAlignmentButtons.at( ib )->setChecked( false );

      plotWidget->setLegendAlignement( this->mLegendAlignments.at( i ) );
    } );
  }


  for ( int i = 0; i < 3; ++i )
    for ( int j = 0; j < 3; j++ )
    {
      placementLayout->addWidget( mAlignmentButtons.at( j + i * 3 ), i, j );
    }

  QHBoxLayout *columnSpinLayout = new QHBoxLayout;
  mColumnSpinBox = new QSpinBox( legendWidget );
  mColumnSpinBox->setMinimum( 1 );
  columnSpinLayout->addWidget( mColumnSpinBox );
  legendWidget->layout()->addItem( columnSpinLayout );

  connect( mColumnSpinBox, QOverload<int>::of( &QSpinBox::valueChanged ), this, [this]
  {
    this->mPlotWidget->setLegendColumnCount( this->mColumnSpinBox->value() );
  } );

  QWidgetAction *actionWidget = new QWidgetAction( this );
  actionWidget->setDefaultWidget( legendWidget );

  menu->addAction( actionWidget );
}

void ReosPlotLegendController::setLegendEnabled( bool b )
{
  setEnabled( b );
  emit legendVisible( b && isChecked() );
}


void ReosPlotLegendController::setCurrentAlignment( Qt::Alignment alignment )
{
  Qt::Alignment horAlign = alignment & Qt::AlignHorizontal_Mask;
  Qt::Alignment vertAlign = alignment & Qt::AlignVertical_Mask;

  int currentAlignment = mLegendAlignments.indexOf( horAlign | vertAlign );
  if ( currentAlignment != -1 )
    for ( int i = 0; i < mAlignmentButtons.count(); ++i )
      mAlignmentButtons.at( currentAlignment )->setChecked( i == currentAlignment );
}

void ReosPlotLegendController::setCurrentColumnCount( int columnCount )
{
  mColumnSpinBox->blockSignals( true );
  mColumnSpinBox->setValue( columnCount );
  mColumnSpinBox->blockSignals( false );
}


