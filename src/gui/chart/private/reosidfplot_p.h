/***************************************************************************
  reosidfplot_p.h - ReosIdfPlot_p

 ---------------------
 begin                : 5.2.2021
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
#ifndef REOSIDFPLOT_P_H
#define REOSIDFPLOT_P_H

#include <qwt_plot_item.h>
#include "reosplotwidget.h"
#include "reosduration.h"


class ReosIntensityDurationCurve;


class ReosIdfPlot_p: public QwtPlotItem
{
  public:
    ReosIdfPlot_p( ReosIntensityDurationCurve *curve );
    void draw( QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect ) const override;

    void fullExtent();
    QRectF extent() const;

    void setColor( const QColor &color );

    QwtGraphic legendIcon( int index, const QSizeF & ) const override;

  private:
    QPointer<ReosIntensityDurationCurve> mCurve;
    ReosDuration::Unit mUnit = ReosDuration::minute;
    QColor mColor;

};

#endif // REOSIDFPLOT_P_H
