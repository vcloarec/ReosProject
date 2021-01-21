/***************************************************************************
  reoschartwidget.h - ReosChartWidget

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
#ifndef REOSCHARTWIDGET_H
#define REOSCHARTWIDGET_H

#include <QWidget>

namespace Ui
{
  class ReosChartWidget;
}

namespace QtCharts
{
  class QAbstractSeries;
}

class ReosChartWidget : public QWidget
{
    Q_OBJECT

  public:
    explicit ReosChartWidget( QWidget *parent = nullptr );
    ~ReosChartWidget();

    void setUniqueXYSeries( const QPolygonF &xySerie );

    void addCustomedSeries( QtCharts::QAbstractSeries *series );


  private:
    Ui::ReosChartWidget *ui;

};

#endif // REOSCHARTWIDGET_H
