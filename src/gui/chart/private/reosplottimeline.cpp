/***************************************************************************
  reosplottimeline.cpp - ReosPlotTimeLine

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
#include "reosplottimeline.h"

#include <qwt_date.h>
#include <qwt_plot.h>
#include <qwt_scale_map.h>

#include "reosstyleregistery.h"

ReosPlotTimeLine::ReosPlotTimeLine() : QwtPlotItem()
{
  setZ( 150 );
}

void ReosPlotTimeLine::draw( QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect ) const
{
  painter->save();
  double x = QwtDate::toDouble( mTime );
  QPen pen;
  pen.setColor( ReosStyleRegistery::instance()->blueReos( 150 ) );
  pen.setWidth( 3 );
  painter->setPen( pen );
  double deviceX = xMap.transform( x );
  painter->drawLine( deviceX, canvasRect.top(), deviceX, canvasRect.bottom() );
  painter->restore();
}

void ReosPlotTimeLine::setTime( const QDateTime &time )
{
  mTime = time;
  plot()->replot();
}
