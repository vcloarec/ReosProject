/***************************************************************************
  reoschartview.cpp - ReosChartView

 ---------------------
 begin                : 12.1.2021
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
#include "reoschartview.h"

#include <QLineSeries>

ReosChartView::ReosChartView( QWidget *parent ): QChartView( new QChart, parent )
{
  setCursor( Qt::CrossCursor );

  mHorizontalLine = new QGraphicsLineItem;
  scene()->addItem( mHorizontalLine );
}

void ReosChartView::setUniqueXYSeries( const QPolygonF &polyline )
{
  chart()->removeAllSeries();
  std::unique_ptr<QLineSeries> profileSerie = std::make_unique<QLineSeries>();
  profileSerie->append( polyline.toList() );
  chart()->addSeries( profileSerie.release() );
  chart()->createDefaultAxes();

}

void ReosChartView::mouseMoveEvent( QMouseEvent *event )
{
  QPointF pos = event->localPos();

  QPointF scenePos = mapToScene( pos.toPoint() );
  QLineF line( QPointF( 5, scenePos.y() ), QPointF( 20, scenePos.y() ) );
  mHorizontalLine->setLine( line );

  QChartView::mouseMoveEvent( event );
}

void ReosChartView::enterEvent( QEvent *event )
{
  mHorizontalLine->show();
  QChartView::enterEvent( event );
}

void ReosChartView::leaveEvent( QEvent *event )
{
  mHorizontalLine->hide();
  QChartView::enterEvent( event );
}
