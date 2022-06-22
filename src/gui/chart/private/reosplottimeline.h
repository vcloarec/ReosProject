/***************************************************************************
  reosplottimeline.h - ReosPlotTimeLine

 ---------------------
 begin                : 21.6.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSPLOTTIMELINE_H
#define REOSPLOTTIMELINE_H

#include <qwt_plot_item.h>
#include <QDateTime>

class ReosPlotTimeLine : public QwtPlotItem
{
  public:
    ReosPlotTimeLine();

    void draw( QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect ) const override;

    void setTime( const QDateTime &time );

  private:
    QDateTime mTime;
};

#endif // REOSPLOTTIMELINE_H
