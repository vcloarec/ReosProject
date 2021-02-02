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


class ReosPlotConstantIntervalTimeIntervalSerie;
class ReosPlotConstantIntervalTimePointSerie;

//! Plot item that can be used to represent time serie with contant time step with histogram
class ReosPlotTimeHistogram: public ReosPlotItem
{
    Q_OBJECT
  public:
    ReosPlotTimeHistogram( const QString &name = QString() );
    void setTimeSerie( ReosTimeSerieConstantInterval *timeSerie );

  private slots:
    void setColors();

  private:
    QwtPlotHistogram *histogram();
    ReosPlotConstantIntervalTimeIntervalSerie *mTimeSerie = nullptr;
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

#endif // REOSPLOTTIMECONSTANTINTERVAL_H
