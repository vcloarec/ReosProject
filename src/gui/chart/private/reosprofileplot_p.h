/***************************************************************************
  reoseditableplot_p.h - ReosEditablePlot_p

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
#ifndef REOSPROFILEPLOT_P_H
#define REOSPROFILEPLOT_P_H

#include <qwt_plot_item.h>

#include "reosplotwidget.h"

class ReosProfilePlot_p: public QwtPlotItem
{
  public:
    ReosProfilePlot_p( const QPolygonF &points );
    void setDiplayingSlope( bool b );

    void draw( QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect ) const override;

  private:
    const QPolygonF &mPoints;
    double mMarkerSize = 8;
    QPen mPenMarker;
    QBrush mBrushMarker;
    QPen mPenLine;
    QPen mPenTxt;
    QBrush mBrushTxtBackground;
    QPen mPenTxtBackground;
    bool mDisplayingSlope = true;

};

#endif // REOSPROFILEPLOT_P_H
