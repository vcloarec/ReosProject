/***************************************************************************
  reosplotpolygons_p.h - ReosPlotPolygons_p

 ---------------------
 begin                : 17.9.2022
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
#ifndef REOSPLOTPOLYGONS_P_H
#define REOSPLOTPOLYGONS_P_H

#include "qwt_plot_item.h"

class ReosPlotPolygons_p :public QwtPlotItem
{
public:
    ReosPlotPolygons_p();

    void draw(QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect) const;
    void setPolygons(const QList<QPolygonF> &newPolygons);

    void setPen(const QPen &pen);
    void setBrush(const QBrush &brush);

private:
    QList<QPolygonF> mPolygons;
    QPen mPen;
    QBrush mBrush;
};

#endif // REOSPLOTPOLYGONS_P_H
