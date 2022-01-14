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

  mTolerance = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );

  mMapCrs = mapCrs();

}

void ReosMapToolEditPolylineStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
}

void ReosMapToolEditPolylineStructure_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( mStructure )
  {
    QgsPointXY mapPoint = e->mapPoint();

    mCurrentVertex = mStructure->searchForVertex( searchZone( mapPoint ) );

    if ( mCurrentVertex )
    {
      mSnappingIndicator->setMatch( QgsPointLocator::Match() );
      mVertexMarker->setCenter( mStructure->vertexPosition( mCurrentVertex, mapCrs() ) );
      mVertexMarker->setVisible( true );
      return;
    }
    else
    {
      mVertexMarker->setVisible( false );
      ReosMapTool_p::canvasMoveEvent( e );
    }
  }
}

void ReosMapToolEditPolylineStructure_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{

}

ReosMapExtent ReosMapToolEditPolylineStructure_p::searchZone( const QgsPointXY &point ) const
{
  ReosMapExtent zone( point.x() - mTolerance, point.y() - mTolerance, point.x() + mTolerance, point.y() + mTolerance );
  zone.setCrs( mMapCrs );

  return zone;
}
