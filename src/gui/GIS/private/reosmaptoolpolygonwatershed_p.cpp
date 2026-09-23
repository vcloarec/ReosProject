/***************************************************************************
  reosmaptoolpolygonwatershed_p.cpp - ReosMapToolEditPolygonWatershed_p

 ---------------------
 begin                : 1.9.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmaptoolpolygonwatershed_p.h"

#include <qgsvertexmarker.h>
#include <qgsguiutils.h>

#include "reospolygonwatershed.h"
#include "reosstyleregistery.h"

ReosMapToolEditPolygonWatershed_p::ReosMapToolEditPolygonWatershed_p( QgsMapCanvas *mapCanvas )
  : ReosMapTool_p( mapCanvas )
{
  enableSnapping( true );
  mRubberBand = new QgsRubberBand( mCanvas, Qgis::GeometryType::Line );
  mRubberBand->setWidth( 1 );
  mRubberBand->setLineStyle( Qt::DashLine );
  mRubberBand->setStrokeColor( ReosStyleRegistery::instance()->greenReos() );
  mRubberBand->setSecondaryStrokeColor( Qt::white );
  mRubberBand->setZValue( 50 );

  mMarkerVertex = new QgsVertexMarker( mapCanvas );
  mMarkerVertex->setVisible( false );
  mMarkerVertex->setColor( ReosStyleRegistery::instance()->greenReos() );
  mMarkerVertex->setIconSize( QgsGuiUtils::scaleIconSize( 12 ) );
  mMarkerVertex->setPenWidth( QgsGuiUtils::scaleIconSize( 4 ) );
  mMarkerVertex->setIconType( QgsVertexMarker::ICON_CIRCLE );
  mMarkerVertex->setZValue( 55 );
}

void ReosMapToolEditPolygonWatershed_p::setPolygonWatershed( ReosPolygonWatershed *polygonWatershed )
{
  mPolygonWatershed = polygonWatershed;
}

void ReosMapToolEditPolygonWatershed_p::setCurrentWatershed( const QString &watershedId )
{
  mCurrentWatershedId = watershedId;
}

void ReosMapToolEditPolygonWatershed_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( mCurrentWatershedId.isEmpty() or !mPolygonWatershed )
    return;

  QgsPointXY mapPoint = e->snapPoint();

  switch ( mCurrentState )
  {
    case None:
    {
      int prevIndex = -1;
      int index = -1;
      int nextIndex = -1;

      double mapTolerance = tolerance();
      QgsPointXY position( mPolygonWatershed->closestVertex( mCurrentWatershedId, mapPoint.toQPointF(), mapCrs(), mapTolerance, prevIndex, index, nextIndex ) );

      if ( position.distance( mapPoint ) < mapTolerance )
      {
        mMarkerVertex->setCenter( position );
        mMarkerVertex->show();
      }
      else
      {
        mMarkerVertex->hide();
      }
    }
    break;
    case DraggingVertex:
    {
      QgsPointXY vertex1( mPolygonWatershed->vertexPosition( mCurrentWatershedId, mPrevVertexIndex, mapCrs() ) );
      QgsPointXY vertex3( mPolygonWatershed->vertexPosition( mCurrentWatershedId, mNextVertexIndex, mapCrs() ) );
      mRubberBand->setToGeometry( QgsGeometry::fromPolylineXY( { vertex1, mapPoint, vertex3 } ) );

      if ( mPolygonWatershed->canVertexBeMoved( mCurrentWatershedId, mCurrentSelectedVertexIndex, mapPoint.toQPointF(), mapCrs() ) )
        mRubberBand->setColor( ReosStyleRegistery::instance()->greenReos() );
      else
        mRubberBand->setColor( ReosStyleRegistery::instance()->invalidColor() );
    }
    break;
    case InsertingVertex:
      break;
  }
}


void ReosMapToolEditPolygonWatershed_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( mCurrentWatershedId.isEmpty() or !mPolygonWatershed )
    return;

  QgsPointXY mapPoint = e->snapPoint();

  if ( e->button() == Qt::RightButton )
  {
    reset();
    return;
  }

  switch ( mCurrentState )
  {
    case None:
    {
      if ( e->button() == Qt::LeftButton )
      {
        int prevIndex = -1;
        int index = -1;
        int nextIndex = -1;

        double mapTolerance = tolerance();
        QgsPointXY position( mPolygonWatershed->closestVertex( mCurrentWatershedId, mapPoint.toQPointF(), mapCrs(), mapTolerance, prevIndex, index, nextIndex ) );

        if ( position.distance( mapPoint ) < mapTolerance )
        {
          mCurrentSelectedVertexIndex = index;
          mPrevVertexIndex = prevIndex;
          mNextVertexIndex = nextIndex;
          mCurrentState = DraggingVertex;
        }
        else
        {
          mMarkerVertex->hide();
        }
      }
      break;
      case DraggingVertex:
        if ( e->button() == Qt::LeftButton )
        {
          if ( mPolygonWatershed->canVertexBeMoved( mCurrentWatershedId, mCurrentSelectedVertexIndex, mapPoint.toQPointF(), mapCrs() ) )
          {
            mPolygonWatershed->moveVertex( mCurrentWatershedId, mCurrentSelectedVertexIndex, mapPoint.toQPointF(), mapCrs() );
            reset();
          }
        }
        break;
      case InsertingVertex:
        break;
    }
  }
}

void ReosMapToolEditPolygonWatershed_p::keyPressEvent( QKeyEvent *e )
{
  switch ( e->key() )
  {
    case Qt::Key_Escape:
      reset();
      break;
    default:
      break;
  }
}

void ReosMapToolEditPolygonWatershed_p::reset()
{
  mCurrentState = None;
  mCurrentSelectedVertexIndex = -1;
  mPrevVertexIndex = -1;
  mNextVertexIndex = -1;
  mMarkerVertex->hide();
  mRubberBand->reset( Qgis::GeometryType::Line );
}

ReosMapToolSelectPolygonWatershed_p::ReosMapToolSelectPolygonWatershed_p( QgsMapCanvas *mapCanvas )
  : ReosMapTool_p( mapCanvas )
{}

void ReosMapToolSelectPolygonWatershed_p::setPolygonWatershed( ReosPolygonWatershed *polygonWatershed )
{
  mPolygonWatershed = polygonWatershed;
}

void ReosMapToolSelectPolygonWatershed_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  const QPointF position = e->mapPoint().toQPointF();
  QString watershedId = mPolygonWatershed->watershedUnderPosition( position, mapCrs() );
  if ( watershedId.isEmpty() )
    return;

  emit watershedFound( watershedId, position );
}
