/***************************************************************************
  reosmaptooleditgeometrustructure_p.cpp - ReosMapToolEditGeometruStructure_p

 ---------------------
 begin                : 12.1.2022
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
#include "reosmaptooleditgeometrystructure_p.h"
#include "reospolylinesstructure.h"

#include "qgsvertexmarker.h"

ReosMapToolEditPolylineStructure_p::ReosMapToolEditPolylineStructure_p( QgsMapCanvas *map ):
  ReosMapTool_p( map )
{
  enableSnapping( true );

  mVertexMarker = new QgsVertexMarker( map );
  mVertexMarker->setVisible( false );
  mVertexMarker->setColor( QColor( 250, 175, 100 ) );
  mVertexMarker->setIconSize( 13 );
  mVertexMarker->setPenWidth( 4 );
  mVertexMarker->setIconType( QgsVertexMarker::ICON_CIRCLE );

  mMapCrs = mapCrs();
}

void ReosMapToolEditPolylineStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  mMapCrs = mapCrs(); //update the crs if changed
}

void ReosMapToolEditPolylineStructure_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      if ( mStructure )
      {
        QgsPointXY mapPoint = e->mapPoint();
        mCurrentVertex = mStructure->searchForVertex( searchZone( mapPoint ) );
        if ( mCurrentVertex )
        {
          mSnappingIndicator->setMatch( QgsPointLocator::Match() );
          mVertexMarker->setCenter( mStructure->vertexPosition( mCurrentVertex, mMapCrs ) );
          mVertexMarker->setVisible( true );
          return;
        }
        else
          mVertexMarker->setVisible( false );
      }
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      mVertexMarker->setCenter( e->mapPoint() );
      break;
  }


  ReosMapTool_p::canvasMoveEvent( e );
}

void ReosMapToolEditPolylineStructure_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
//      if ( mCurrentVertex )
//      {
//        if ( e->button() == Qt::LeftButton )
//        {
//          mCurrentState = DraggingVertex;
//          qDebug() << "start drag vertex";
//        }
//      }
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      break;
  }
}

void ReosMapToolEditPolylineStructure_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      if ( mCurrentVertex )
      {
        if ( e->button() == Qt::LeftButton )
        {
          mCurrentState = DraggingVertex;
        }
      }
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      if ( mCurrentVertex )
      {
        if ( e->button() == Qt::LeftButton )
        {
          mCurrentState = None;
          mVertexMarker->setVisible( false );
          mStructure->moveVertex( mCurrentVertex, ReosSpatialPosition( e->mapPoint().toQPointF(), mMapCrs ) );
          canvas()->refresh();
        }
      }
      break;
  }
}

ReosMapExtent ReosMapToolEditPolylineStructure_p::searchZone( const QgsPointXY &point ) const
{
  const QgsSnappingConfig &snapConfig = QgsProject::instance()->snappingConfig();
  double tolerance = QgsTolerance::toleranceInProjectUnits( snapConfig.tolerance(),
                     nullptr, canvas()->mapSettings(), snapConfig.units() );

  ReosMapExtent zone( point.x() - tolerance, point.y() - tolerance, point.x() + tolerance, point.y() + tolerance );
  zone.setCrs( mMapCrs );

  return zone;
}
