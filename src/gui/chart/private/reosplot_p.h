/***************************************************************************
  reosplot_p.h - ReosPlot_p

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
#ifndef REOSPLOT_P_H
#define REOSPLOT_P_H

#include <memory>

#include <QPointer>

#include <qwt_plot.h>
#include <qwt_plot_magnifier.h>
#include <qwt_series_data.h>
#include <qwt_date_scale_draw.h>
#include <qwt_plot_histogram.h>
#include <qwt_scale_widget.h>
#include "reostimeseries.h"

class ReosPlotItem;

class QwtPlotLegendItem;
class QwtPlotGrid;
class QwtPlotMagnifier;
class QwtPlotPanner;
class QwtPlotZoomer;
class QwtPlotPicker;
class QwtPlotItem;

class ReosNormalMagnifier : public QwtPlotMagnifier
{
  public:
    ReosNormalMagnifier( QWidget *canvas );

  protected:
    void widgetWheelEvent( QWheelEvent * ) override;
};


class ReosPositiveMagnifier : public ReosNormalMagnifier
{
  public:
    ReosPositiveMagnifier( QWidget *canvas );
    void setYMinimumEnabled( bool b );

  protected:
    virtual void rescale( double factor ) override;

  private:
    bool mIsYMinEnabeled = true;
};


class ReosDateScaleDraw_p : public QwtDateScaleDraw
{
  public :
    ReosDateScaleDraw_p( Qt::TimeSpec timeSpec = Qt::LocalTime );
    void drawLabel( QPainter *painter, double value ) const override; //Method override because bad align center with qwt
};

class ReosPlot_p: public QwtPlot
{
    Q_OBJECT
  public:
    ReosPlot_p( QWidget *parent = nullptr );
    ~ReosPlot_p();

    void setLegendVisible( bool b );
    void setLegendAlignement( Qt::Alignment align );
    void setLegendColumnCount( int columnCount );
    void enableAutoMinimumSize( bool b );
    void setMinimumPlotSize( QSize minSize );
    void setRightAxeEnabled( bool b );

    void setEnableZoomer( bool b );
    void resetZoomBase();

    void setNormalMagnifier();
    void setPositiveMagnifier();

    QSize sizeHint() const override;
    QSize minimumSizeHint() const override;

    //! Adds an item to the plot, takes ownership
    void addItem( ReosPlotItem *item );

    void setZoomer( QwtPlotZoomer *zoomerLeft, QwtPlotZoomer *zoomerRight );

    void autoScale();

    void setUpdateAxesWhenResize( bool updateAxesWhenResize );

  signals:
    void reploted();

  public slots:
    void replot() override;
    void enableAutoScale( bool b = true );
    void enableAutoScaleY( bool b = true );
    void enableAutoScaleX( bool b = true );

  protected:
    void resizeEvent( QResizeEvent *e ) override;

  private:
    QwtPlotGrid *mGrid = nullptr;
    QwtPlotLegendItem *mLegend = nullptr;
    QSize mMinimumSize = QSize( 300, 225 );
    bool mAutoMinimumSizeIsEnable = false;
    std::vector<std::unique_ptr<ReosPlotItem>> mItems;

    QwtPlotMagnifier *mMagnifier = nullptr;
    QwtPlotPanner *mPanner = nullptr;
    QwtPlotZoomer *mZoomerLeft = nullptr;
    QwtPlotZoomer *mZoomerRight = nullptr;

    QMap<QwtPlot::Axis, bool> mAutoScalePerAxe;
    bool mUpdateAxeXWhenResize = false;
};


class ReosPlotConstantIntervalTimeIntervalSerie: public QwtSeriesData<QwtIntervalSample>
{
  public:
    ReosPlotConstantIntervalTimeIntervalSerie( ReosTimeSeriesConstantInterval *timeSeries );

    size_t size() const override;
    QwtIntervalSample sample( size_t i ) const override;
    QRectF boundingRect() const override;

    ReosTimeSeriesConstantInterval *data() const;

  private:
    QPointer<ReosTimeSeriesConstantInterval> mTimeSeries;
};


class ReosPlotConstantIntervalTimePointSerie: public QwtSeriesData<QPointF>
{
  public:
    ReosPlotConstantIntervalTimePointSerie( ReosTimeSeriesConstantInterval *timeSeries );

    size_t size() const override;
    QPointF sample( size_t i ) const override;
    QRectF boundingRect() const override;

    ReosTimeSeriesConstantInterval *data() const;
    void setCumulative( bool b );

  private:
    QPointer<ReosTimeSeriesConstantInterval> mTimeSerie;
    bool mIsCumulative = false;
    ReosTimeSeriesConstantInterval::ValueMode mValueMode = ReosTimeSeriesConstantInterval::Value ;
};

class ReosPlotVariableStepTimeSerie: public  QwtSeriesData<QPointF>
{
  public:
    ReosPlotVariableStepTimeSerie( ReosTimeSeriesVariableTimeStep *timeSeries );

    size_t size() const override;
    QPointF sample( size_t i ) const override;
    QRectF boundingRect() const override;

    ReosTimeSeriesVariableTimeStep *data() const;

  private:
    QPointer<ReosTimeSeriesVariableTimeStep> mTimeSeries;
};


class ReosPlotHistogramItem_p: public QwtPlotHistogram
{
  public:
    ReosPlotHistogramItem_p( const QString &title ): QwtPlotHistogram( title ) {}
    QwtGraphic legendIcon( int, const QSizeF &size ) const override;
};


#endif // REOSPLOT_P_H
