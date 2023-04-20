/***************************************************************************
  reosplottimeconstantinterval.h - ReosPlotTimeConstantInterval

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
#ifndef REOSPLOTTIMECONSTANTINTERVAL_H
#define REOSPLOTTIMECONSTANTINTERVAL_H

#include "reosplotwidget.h"

class ReosPlotHistogramItem_p;

class ReosPlotConstantIntervalTimeIntervalSerie;
class ReosPlotConstantIntervalTimePointSerie;
class ReosTimeSeriesVariableTimeStep;
class ReosPlotVariableStepTimeSerie;

/**
 *  Plot item that can be used to represent time serie with constant time step with histogram
 */
class REOSGUI_EXPORT ReosPlotTimeHistogram: public ReosPlotItem
{
    Q_OBJECT
  public:
    //! Contructor with the name of the item. If this item is contructed with \a masterItem true, this item will control the title of Y left axe.
    explicit ReosPlotTimeHistogram( const QString &name, bool masterItem = false );
    void setTimeSeries( ReosTimeSeriesConstantInterval *timeSeries, bool replot = true );

    //! Sets the color of the border (default black);
    void setBorderColor( const QColor &color );

    //! Set the border of the each interval bars
    void setBorderWdidth( double w );

    //! Sets the brush color, if not defined the color defined on the time series will be used (default black);
    void setBrushColor( const QColor &color );

    //! Sets the brush style, default: Qt::SolidPattern
    void setBrushStyle( Qt::BrushStyle style );

  private slots:
    void setSettings() override;

  private:
    ReosPlotHistogramItem_p *histogram();
    ReosPlotConstantIntervalTimeIntervalSerie *mTimeSeries = nullptr;

    QColor mBorderColor = Qt::black;
    QColor mBrushColor;
    Qt::BrushStyle mBrushStyle = Qt::SolidPattern;
    double mBorderWidth = -1;
};

//! Plot item that can be used to represent time serie with contant time step with cummulative curve
class REOSGUI_EXPORT ReosPlotTimeCumulativeCurve: public ReosPlotItem
{
    Q_OBJECT
  public:
    ReosPlotTimeCumulativeCurve( const QString &name = QString() );
    void setTimeSeries( ReosTimeSeriesConstantInterval *timeSerie );

  private slots:
    void setSettings() override;

  private:
    QwtPlotCurve *curve();
    ReosPlotConstantIntervalTimePointSerie *mTimeSerie = nullptr;
};


class REOSGUI_EXPORT ReosPlotTimeSerieVariableStep: public ReosPlotItem
{
    Q_OBJECT
  public:
    ReosPlotTimeSerieVariableStep( const QString &name = QString() );
    void setTimeSeries( ReosTimeSeriesVariableTimeStep *timeSerie, bool replot = true, bool applysettings = true );

    QColor color() const override;
    QPixmap icone( const QSize &size ) const override;
    QString name() const override;

  public slots:
    void setColor( const QColor &color ) override;
    void setStyle( Qt::PenStyle penStyle ) override;
    void setWidth( double width ) override;

  private slots:
    void setSettings() override;
    void onNameChanged();

  private:
    QwtPlotCurve *curve() const;
    ReosPlotVariableStepTimeSerie *mTimeSeries = nullptr;
};

#endif // REOSPLOTTIMECONSTANTINTERVAL_H
