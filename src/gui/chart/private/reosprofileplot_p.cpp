/***************************************************************************
  reoseditableplot_p.cpp - ReosEditablePlot_p

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
#include "reosprofileplot_p.h"
#include "reosparameter.h"

#include <qwt_scale_map.h>


ReosProfilePlot_p::ReosProfilePlot_p( const QPolygonF &points ):
  QwtPlotItem(),
  mPoints( points )
{
  mPenLine = QPen( QColor( 0, 0, 0, 100 ) );
  mPenLine.setWidth( 3 );
  mPenLine.setStyle( Qt::DashLine );
  mPenMarker = QPen( Qt::black );
  mPenMarker.setWidth( 2 );
  mBrushMarker.setColor( Qt::gray );
  mBrushMarker.setStyle( Qt::SolidPattern );
  mPenTxt.setColor( Qt::red );
  mBrushTxtBackground.setStyle( Qt::SolidPattern );
  mBrushTxtBackground.setColor( QColor( 220, 220, 220, 150 ) );
  mPenTxtBackground.setStyle( Qt::NoPen );
  setZ( 50 ); //ordinary curves have z=20 (see qwt_plo_curve.cpp
}

void ReosProfilePlot_p::draw( QPainter *painter, const QwtScaleMap &xMap, const QwtScaleMap &yMap, const QRectF &canvasRect ) const
{
  Q_UNUSED( canvasRect );
  painter->save();

  if ( mPoints.count() == 0 )
    return;

  for ( int i = 0; i < mPoints.count() - 1; i++ )
  {
    double xc1 = xMap.transform( mPoints.at( i ).x() );
    double yc1 = yMap.transform( mPoints.at( i ).y() );
    double xc2 = xMap.transform( mPoints.at( i + 1 ).x() );
    double yc2 = yMap.transform( mPoints.at( i + 1 ).y() );
    const QLineF line( xc1, yc1, xc2, yc2 );
    painter->setPen( mPenLine );
    painter->drawLine( line );
    painter->setPen( mPenMarker );
    painter->setBrush( mBrushMarker );
    painter->drawEllipse( QPointF( xc1, yc1 ), mMarkerSize / 2, mMarkerSize / 2 );

    if ( mDisplayingSlope )
    {
      QPointF textPosition( ( xc1 + xc2 ) / 2, ( yc1 + yc2 ) / 2 );
      const QPointF delta = mPoints.at( i + 1 ) - mPoints.at( i );

      QString slope;
      if ( delta.x() > 0 )
      {
        double slp = -delta.y() / delta.x();

        if ( int( slp * 1000 ) == 0 )
        {
          slope = ReosParameter::doubleToString( slp * 1000, 2 );
          slope.append( ' ' );
          slope.append( QChar( 0x2030 ) );
        }
        else
        {
          slope =  ReosParameter::doubleToString( slp * 100,  2 );
          slope.append( QStringLiteral( " %" ) );
        }
      }
      else
        slope = "-";


      QFont font = painter->font();
      font.setBold( true );
      painter->setFont( font );

      QFontMetrics fontMetric = painter->fontMetrics();
      QSize size = fontMetric.size( Qt::TextSingleLine, slope );
      if ( ( size.width() < abs( xc2 - xc1 ) ) || ( size.height() < abs( yc2 - yc1 ) ) )
      {
        textPosition = textPosition - QPointF( size.width() / 2, 0 );
        painter->setBrush( mBrushTxtBackground );
        painter->setPen( mPenTxtBackground );
        QRectF rectTxt( textPosition - QPointF( 0, size.height() ), size );
        painter->drawRect( rectTxt );
        painter->setPen( mPenTxt );
        painter->drawText( textPosition, slope );
      }
    }
  }

  double xcf = xMap.transform( mPoints.last().x() );
  double ycf = yMap.transform( mPoints.last().y() );
  painter->setPen( mPenMarker );
  painter->setBrush( mBrushMarker );
  painter->drawEllipse( QPointF( xcf, ycf ), mMarkerSize / 2, mMarkerSize / 2 );
  painter->restore();
}
