/***************************************************************************
  reosplotpolygons_p.cpp - ReosPlotPolygons_p

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
#include "reosplotpolygons_p.h"
#include "qwt_scale_map.h"
#include "qwt_painter.h"

ReosPlotPolygons_p::ReosPlotPolygons_p(): QwtPlotItem()
{}

void ReosPlotPolygons_p::draw( QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect ) const
{
  painter->save();

  bool doAlign = QwtPainter::roundingAlignment( painter );
  for ( const QPolygonF &poly : mPolygons )
  {
    int pointCount = poly.count();
    QPolygonF drawnPoly( pointCount );

    for ( int i = 0; i < pointCount; ++i )
    {
      const QPointF &pt = poly.at( i );
      double x = xMap.transform( pt.x() );
      double y = yMap.transform( pt.y() );

      if ( doAlign )
      {
        x = qRound( x );
        y = qRound( y );
      }

      drawnPoly[i] = QPointF( x, y );
    }

    painter->setPen( mPen );
    painter->setBrush( mBrush );

    QwtPainter::drawPolygon( painter, drawnPoly );
  }

  painter->restore();
}

void ReosPlotPolygons_p::setPolygons( const QList<QPolygonF> &newPolygons )
{
  mPolygons = newPolygons;
}

void ReosPlotPolygons_p::setPen( const QPen &pen )
{
  mPen = pen;
}

void ReosPlotPolygons_p::setBrush( const QBrush &brush )
{
  mBrush = brush;
}
