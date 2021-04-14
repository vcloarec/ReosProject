/***************************************************************************
  reostineditormapitems_p.cpp - ReosTinEditorMapItems_p

 ---------------------
 begin                : 13.4.2021
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
#include "reostineditormapitems_p.h"
#include <qgsmapcanvas.h>
#include <QPainter>

ReosTinEditorMapItems_p::ReosTinEditorMapItems_p( QgsMapCanvas *canvas ): ReosMapItem_p( canvas )
{

}

void ReosTinEditorMapItems_p::updatePosition()
{
  prepareGeometryChange();
  mBoundingRect = QRectF();
  mVertexView.clear();

  QgsMapToPixel mtp = mMapCanvas->mapSettings().mapToPixel();
  int colCount = mtp.mapWidth() / resolReduction;
  int rowCount = mtp.mapHeight() / resolReduction;
  QRect gridViewExtent( 0, 0, colCount, rowCount );

  mGridView = QVector<QVector<int>>( colCount + 1, QVector<int>( rowCount + 1, 0 ) );

  if ( !mTriangulation.isNull() )
  {
    int vertexCount = mTriangulation->vertexCount();
    mVertexView.reserve( ( colCount + 1 ) * ( rowCount + 1 ) );
    QPointF topLeft( std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
    QPointF bottomRight( -std::numeric_limits<double>::max(), -std::numeric_limits<double>::max() );
    for ( int i = 0; i < vertexCount; ++i )
    {
      QPointF viewPoint =  toCanvasCoordinates( mTriangulation->vertexXY( i ) );

      QPoint gridPos( int( viewPoint.x() / resolReduction ), int( viewPoint.y() / resolReduction ) );

      if ( !gridViewExtent.contains( gridPos ) )
        continue;

      if ( mGridView[gridPos.x()][gridPos.y()] != 0 )
      {
        mGridView[gridPos.x()][gridPos.y()]++;
        continue;
      }

      mGridView[gridPos.x()][gridPos.y()] = 1;

      mVertexView.append( {viewPoint, gridPos } );

      if ( topLeft.x() > viewPoint.x() )
        topLeft.setX( viewPoint.x() );
      if ( topLeft.y() > viewPoint.y() )
        topLeft.setY( viewPoint.y() );

      if ( bottomRight.x() < viewPoint.x() )
        bottomRight.setX( viewPoint.x() );
      if ( bottomRight.y() < viewPoint.y() )
        bottomRight.setY( viewPoint.y() );

    }
    mBoundingRect = QRectF( topLeft, bottomRight );
  }

}

void ReosTinEditorMapItems_p::paint( QPainter *painter )
{
  painter->save();
  QPen pen;
  if ( externalWidth > width )
  {
    pen.setColor( externalColor );
    QBrush brush( Qt::SolidPattern );
    brush.setColor( externalColor );
    painter->setBrush( brush );
    painter->setPen( pen );
    for ( const PointView &viewPoint : std::as_const( mVertexView ) )
    {
      double w = externalWidth / 2;
      w = std::clamp( w * sqrt( mGridView[viewPoint.gridView.x()][viewPoint.gridView.y()] ), w, 3 * w );
      painter->drawEllipse( viewPoint.posView, w, w );
    }
  }

  pen.setColor( color );
  QBrush brush( Qt::SolidPattern );
  brush.setColor( color );
  painter->setPen( pen );
  painter->setBrush( brush );
  for ( const PointView &viewPoint : std::as_const( mVertexView ) )
  {
    double w = width / 2;
    w = std::clamp( w * sqrt( mGridView[viewPoint.gridView.x()][viewPoint.gridView.y()] ), w, 3 * w );
    painter->drawEllipse( viewPoint.posView, w, w );
  }

  painter->restore();
}
