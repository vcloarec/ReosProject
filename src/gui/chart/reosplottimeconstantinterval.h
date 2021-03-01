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
class ReosTimeSerieVariableTimeStep;
class ReosPlotVariableStepTimeSerie;

/**
 *  Plot item that can be used to represent time serie with constant time step with histogram
 */
class ReosPlotTimeHistogram: public ReosPlotItem
{
    Q_OBJECT
  public:
    //! Contructor with the name of the item. If this item is contructed with \a masterItem true, this item will control the title of Y left axe.
    ReosPlotTimeHistogram( const QString &name, bool masterItem = false );
    void setTimeSerie( ReosTimeSerieConstantInterval *timeSerie );

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
    ReosPlotConstantIntervalTimeIntervalSerie *mTimeSerie = nullptr;

    QColor mBorderColor = Qt::black;
    QColor mBrushColor;
    Qt::BrushStyle mBrushStyle = Qt::SolidPattern;
    double mBorderWidth = -1;
};

//! Plot item that can be used to represent time serie with contant time step with cummulative curve
class ReosPlotTimeCumulativeCurve: public ReosPlotItem
{
    Q_OBJECT
  public:
    ReosPlotTimeCumulativeCurve( const QString &name = QString() );
    void setTimeSerie( ReosTimeSerieConstantInterval *timeSerie );

  private slots:
    void setSettings();

  private:
    QwtPlotCurve *curve();
    ReosPlotConstantIntervalTimePointSerie *mTimeSerie = nullptr;

};


class ReosPlotTimeSerieVariableStep: public ReosPlotItem
{
    Q_OBJECT
  public:
    ReosPlotTimeSerieVariableStep( const QString &name = QString() );
    void setTimeSerie( ReosTimeSerieVariableTimeStep *timeSerie );

  private slots:
    void setSettings();

  private:
    QwtPlotCurve *curve();
    ReosPlotVariableStepTimeSerie *mTimeSerie = nullptr;
};

#endif // REOSPLOTTIMECONSTANTINTERVAL_H
