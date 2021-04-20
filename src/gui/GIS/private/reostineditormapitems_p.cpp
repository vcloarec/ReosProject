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
  QElapsedTimer timer;
  timer.start();
  prepareGeometryChange();
  mBoundingRect = QRectF();
  mVertexView.clear();

  QgsMapToPixel mtp = mMapCanvas->mapSettings().mapToPixel();
  int colCount = mtp.mapWidth() / mResolReduction;
  int rowCount = mtp.mapHeight() / mResolReduction;
  QRect gridViewExtent( 0, 0, colCount, rowCount );

  mGridView = QVector<QVector<QPair<int, int>>>( colCount + 1, QVector<QPair<int, int>>( rowCount + 1, QPair<int, int>( -1, 0 ) ) );
  mHighlightVertex = -1;

  if ( !mTriangulation.isNull() )
  {
    int vertexCount = mTriangulation->vertexCount();
    mVertexView.reserve( ( colCount + 1 ) * ( rowCount + 1 ) );
    QPointF topLeft( std::numeric_limits<double>::max(), std::numeric_limits<double>::max() );
    QPointF bottomRight( -std::numeric_limits<double>::max(), -std::numeric_limits<double>::max() );
    for ( int i = 0; i < vertexCount; ++i )
    {
      QPointF viewPoint =  toCanvasCoordinates( mTriangulation->vertexXY( i ) );

      QPoint gridPos( int( viewPoint.x() / mResolReduction ), int( viewPoint.y() / mResolReduction ) );

      if ( !gridViewExtent.contains( gridPos ) )
        continue;

      if ( mGridView[gridPos.x()][gridPos.y()].second != 0 )
      {
        mGridView[gridPos.x()][gridPos.y()].second++;
        continue;
      }

      mGridView[gridPos.x()][gridPos.y()].second = 1;
      mGridView[gridPos.x()][gridPos.y()].first = mVertexView.count();

      mVertexView.append( {viewPoint, gridPos, i } );

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

void ReosTinEditorMapItems_p::highlightVertex( const QPointF &mapPoint,  const QSizeF searchSize )
{
  mHighlightVertex = searchPointView( mapPoint, searchSize );
  update();
}

int ReosTinEditorMapItems_p::vertexIndexAt( const QPointF &mapPoint, const QSizeF searchSize )
{
  int viewIndex = searchPointView( mapPoint, searchSize );
  if ( viewIndex >= 0 && viewIndex < mVertexView.count() )
    return mVertexView.at( viewIndex ).triangulationIndex;
  else
    return -1;
}

void ReosTinEditorMapItems_p::paint( QPainter *painter )
{
  QElapsedTimer timer;
  timer.start();
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
      w = std::clamp( w * sqrt( mGridView[viewPoint.gridView.x()][viewPoint.gridView.y()].second ), w, 3 * w );
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
    w = std::clamp( w * sqrt( mGridView[viewPoint.gridView.x()][viewPoint.gridView.y()].second ), w, 3 * w );
    painter->drawEllipse( viewPoint.posView, w, w );
  }


  if ( mHighlightVertex >= 0 )
  {
    //highlight vertex
    brush.setColor( Qt::red );
    painter->setPen( pen );
    painter->setBrush( brush );

    const PointView &viewPoint = mVertexView.at( mHighlightVertex );
    double w = externalWidth;
    w = std::clamp( w * sqrt( mGridView[viewPoint.gridView.x()][viewPoint.gridView.y()].second ), w, 3 * w );
    painter->drawEllipse( viewPoint.posView, w, w );
  }

  painter->restore();

  int elapsedTime = timer.elapsed();
  if ( elapsedTime > 150 )
  {
    mResolReduction = mResolReduction * 2;
    updatePosition();
  }
  else if ( elapsedTime < 30 && mResolReduction > mMinResolReduction )
  {
    mResolReduction = std::max( mMinResolReduction, mResolReduction / 2 );
    updatePosition();
  }
}

int ReosTinEditorMapItems_p::searchPointView( const QPointF &mapPoint, const QSizeF &searchSize ) const
{
  QPointF viewPoint = toCanvasCoordinates( mapPoint );

  int colPos = viewPoint.x() / mResolReduction;
  int rowPos = viewPoint.y() / mResolReduction;

  if ( colPos < 0 || rowPos < 0 )
    return -1;

  if ( colPos < mGridView.count() && rowPos < mGridView[colPos].count() )
  {
    // First try the center
    if ( mGridView[colPos][rowPos].second == 1 ) //only when there is only one vertex in the box
      return  mGridView[colPos][rowPos].first;

    QSizeF correctedSearchSize = ( searchSize / mResolReduction );
    int dx = ( correctedSearchSize.width() / 2 ) + 0.5;
    int dy = ( correctedSearchSize.height() / 2 ) + 0.5;
    if ( dx > 0 && dy > 0 )
      for ( int xi = colPos - dx; xi <= colPos + dx; ++xi )
      {
        for ( int yi = rowPos - dy; yi <= rowPos + dy; ++yi )
          if ( xi >= 0 && xi < mGridView.count() && yi >= 0 && yi < mGridView[xi].count() )
          {
            if ( mGridView[xi][yi].second == 1 ) //only when there is only one vertex in the box
            {
              return mGridView[xi][yi].first;
            }
          }
      }

  }

  return -1;
}

void ReosTinEditorMapToolSelectVertex_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  QPointF mapPoint = e->mapPoint().toQPointF();

  if ( mEditorMapItems )
    mEditorMapItems->highlightVertex( mapPoint, QSizeF( 5, 5 ) );
}

void ReosTinEditorMapToolSelectVertex_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( mEditorMapItems )
    emit selectedVertexIndex( mEditorMapItems->vertexIndexAt( e->mapPoint().toQPointF(), QSizeF( 5, 5 ) ) );
}
