/***************************************************************************
  reoschartview.h - ReosChartView

 ---------------------
 begin                : 12.1.2021
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
#ifndef REOSCHARTVIEW_H
#define REOSCHARTVIEW_H

#include <QtCharts/QChartView>

#include "reoschart.h"

using namespace QtCharts;

class ReosChartView : public QChartView
{
  public:
    ReosChartView( QWidget *parent = nullptr );

    void setUniqueXYSeries( const QPolygonF &polyline );

  protected:
    void mouseMoveEvent( QMouseEvent *event ) override;
    void enterEvent( QEvent *event ) override;
    void leaveEvent( QEvent *event ) override;

  private:
    QGraphicsLineItem *mHorizontalLine;
    QChartView *mChartView = nullptr;

};


#endif // REOSCHARTVIEW_H
