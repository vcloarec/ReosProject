/***************************************************************************
  reoschart.cpp - ReosChart

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
#include "reoschart.h"

#include <QtDebug>

ReosChart::ReosChart()
{

}

ReosXYChartEditableSeries::ReosXYChartEditableSeries( QObject *parent ): QLineSeries( parent )
{
  setPointsVisible( true );
  setPointLabelsVisible( true );
  QPen pen;
  pen.setColor( Qt::red );
  pen.setWidthF( 5 );
  QBrush brush;
  brush.setStyle( Qt::NoBrush );
  setPen( pen );
  setBrush( brush );


  connect( this, &QLineSeries::pressed, this, &ReosXYChartEditableSeries::onPressed );
}

void ReosXYChartEditableSeries::onPressed( const QPointF &point )
{
  qDebug() << point;
}
