/***************************************************************************
  reoschart.h - ReosChart

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
#ifndef REOSCHART_H
#define REOSCHART_H

#include <QtCharts/QLineSeries>
#include <QtCharts/QChart>

using namespace QtCharts;

class ReosXYChartEditableSeries: public QLineSeries
{
    Q_OBJECT
  public:
    ReosXYChartEditableSeries( QObject *parent = nullptr );


  private slots:
    void onPressed( const QPointF &point );
  private:


};

class ReosChart
{
  public:
    ReosChart();
};

#endif // REOSCHART_H
