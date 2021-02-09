/***************************************************************************
  reosplotidfcurve.h - ReosPlotIdfCurve

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
#ifndef REOSPLOTIDFCURVE_H
#define REOSPLOTIDFCURVE_H

#include "reosplotwidget.h"
#include "reosidfcurves.h"

class ReosIdfPlot_p;

class ReosPlotIdfCurve: public ReosPlotItem
{
    Q_OBJECT
  public:
    ReosPlotIdfCurve( ReosIntensityDurationCurve *mCurve, const QString &name = QString() );

  public slots:
    void fullExtent();

    void setColors( const QColor &color );

  private:
    ReosIdfPlot_p *item();

};

#endif // REOSPLOTIDFCURVE_H
