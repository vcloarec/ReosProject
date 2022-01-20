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

#include <QMenu>

#include "qgsvertexmarker.h"
#include "qgsguiutils.h"

#include "reospolylinesstructure.h"
#include "reosstyleregistery.h"


ReosMapToolEditPolylineStructure_p::ReosMapToolEditPolylineStructure_p( QgsMapCanvas *map )
  :  ReosMapTool_p( map )
  , mActionInsertVertex( new QAction( tr( "Insert Vertex" ), this ) )
  , mActionRemoveVertex( new QAction( tr( "Remove Vertex" ), this ) )
{
  enableSnapping( true );

  mVertexMarker = new QgsVertexMarker( map );
  mVertexMarker->setVisible( false );
  mVertexMarker->setColor( ReosStyleRegistery::instance()->blueReos() );
  mVertexMarker->setIconSize( QgsGuiUtils::scaleIconSize( 12 ) );
  mVertexMarker->setPenWidth( QgsGuiUtils::scaleIconSize( 4 ) );
  mVertexMarker->setIconType( QgsVertexMarker::ICON_CIRCLE );
  mVertexMarker->setZValue( 55 );

  mMovingLineRubberBand = new QgsRubberBand( mCanvas, QgsWkbTypes::LineGeometry );
  mMovingLineRubberBand->setWidth( 1 );

  mMovingLineRubberBand->setLineStyle( Qt::DashLine );
  mMovingLineRubberBand->setStrokeColor( ReosStyleRegistery::instance()->blueReos() );
  mMovingLineRubberBand->setSecondaryStrokeColor( Qt::white );
  mMovingLineRubberBand->setZValue( 50 );

  mMovingVertexRubberBand = new QgsRubberBand( mCanvas, QgsWkbTypes::PointGeometry );
  mMovingVertexRubberBand->setIcon( QgsRubberBand::ICON_CIRCLE );
  mMovingVertexRubberBand->setWidth( QgsGuiUtils::scaleIconSize( 2 ) );
  mMovingVertexRubberBand->setColor( ReosStyleRegistery::instance()->blueReos() );
  mMovingVertexRubberBand->setSecondaryStrokeColor( Qt::white );
  mMovingVertexRubberBand->setIconSize( QgsGuiUtils::scaleIconSize( 5 ) );
  mMovingVertexRubberBand->setZValue( 51 );
  mMovingVertexRubberBand->setVisible( true );

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
      }

      mVertexMarker->setVisible( false );
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
    {
      QgsPointXY mapPoint = e->snapPoint();
      mVertexMarker->setCenter( mapPoint );
      updateMovingVertexRubberBand( mapPoint );
    }
    break;
  }

  ReosMapTool_p::canvasMoveEvent( e );
}

void ReosMapToolEditPolylineStructure_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      break;
  }

  ReosMapTool_p::canvasPressEvent( e );
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
          mNeighborPosition = mStructure->neighborsPositions( mCurrentVertex, mMapCrs );
          const QgsPointXY center( mStructure->vertexPosition( mCurrentVertex, mMapCrs ) );
          updateMovingVertexRubberBand( center );
          mVertexMarker->setVisible( false );
        }
      }
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      if ( mCurrentVertex )
      {
        if ( e->button() == Qt::LeftButton )
        {
          if ( mStructure->vertexCanBeMoved( mCurrentVertex, ReosSpatialPosition( e->snapPoint().toQPointF(), mMapCrs ) ) )
            mStructure->moveVertex( mCurrentVertex, ReosSpatialPosition( e->snapPoint().toQPointF(), mMapCrs ) );

          stopDraggingVertex();
          canvas()->refresh();
        }
        else if ( e->button() == Qt::RightButton )
        {
          stopDraggingVertex();
        }
      }
      break;
  }

  ReosMapTool_p::canvasReleaseEvent( e );
}

void ReosMapToolEditPolylineStructure_p::keyPressEvent( QKeyEvent *e )
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      ReosMapTool_p::keyPressEvent( e );
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      stopDraggingVertex();
      break;

  }
}

void ReosMapToolEditPolylineStructure_p::insertVertex( const QPointF &mapPoint, qint64 lineId )
{
  mStructure->insertVertex( mapPoint, lineId );
}

void ReosMapToolEditPolylineStructure_p::removeVertex( ReosGeometryStructureVertex *vertex )
{
  mStructure->removeVertex( vertex );
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

void ReosMapToolEditPolylineStructure_p::updateMovingVertexRubberBand( const QgsPointXY &movingPosition )
{
  mMovingLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mMovingVertexRubberBand->reset( QgsWkbTypes::PointGeometry );

  if ( mStructure->vertexCanBeMoved( mCurrentVertex, ReosSpatialPosition( movingPosition.toQPointF(), mMapCrs ) ) )
  {
    mMovingLineRubberBand->setColor( ReosStyleRegistery::instance()->blueReos() );
  }
  else
  {
    mMovingLineRubberBand->setColor( ReosStyleRegistery::instance()->invalidColor() );
  }

  for ( const QPointF &np : std::as_const( mNeighborPosition ) )
  {
    const QgsGeometry geom( new QgsLineString( {movingPosition, QgsPointXY( np )} ) );
    mMovingLineRubberBand->addGeometry( geom, QgsCoordinateReferenceSystem(), false );
    mMovingVertexRubberBand->addPoint( np, false );
  }

  mMovingVertexRubberBand->addPoint( movingPosition );
  mMovingLineRubberBand->updatePosition();
}

void ReosMapToolEditPolylineStructure_p::stopDraggingVertex()
{
  mMovingLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mMovingVertexRubberBand->reset( QgsWkbTypes::PointGeometry );
  mNeighborPosition.clear();
  mCurrentVertex = nullptr;
  mCurrentState = None;
}

ReosEditGeometryStructureMenuPopulator::ReosEditGeometryStructureMenuPopulator( ReosMapToolEditPolylineStructure_p *toolMap )
  : mToolMap( toolMap )
{}

void ReosEditGeometryStructureMenuPopulator::populate( QMenu *menu, QgsMapMouseEvent *e )
{
  if ( !mToolMap )
    return;

  QPointF nonSnapPos = e->mapPoint().toQPointF();
  ReosMapExtent searchZone = mToolMap->searchZone( e->snapPoint() );
  QPointF snapPos = e->mapPoint().toQPointF();


  ReosGeometryStructureVertex *vertex = mToolMap->mStructure->searchForVertex( searchZone );
  if ( vertex )
    populateVertexAction( vertex, menu );

  qint64 id = 0;
  if ( !vertex && mToolMap->mStructure->searchForLine( mToolMap->searchZone( nonSnapPos ), id ) )
    populateLineAction( id, snapPos, menu );

}

void ReosEditGeometryStructureMenuPopulator::populateVertexAction( ReosGeometryStructureVertex *vertex, QMenu *menu )
{
  menu->clear();
  menu->addAction( mToolMap->mActionRemoveVertex );
  QObject::connect( mToolMap->mActionRemoveVertex, &QAction::triggered, menu, [this, vertex]
  {
    mToolMap->removeVertex( vertex );
  } );
}

void ReosEditGeometryStructureMenuPopulator::populateLineAction( QgsFeatureId id, const QPointF &point, QMenu *menu )
{
  menu->clear();
  menu->addAction( mToolMap->mActionInsertVertex );
  QObject::connect( mToolMap->mActionInsertVertex, &QAction::triggered, menu, [this, point, id]
  {
    mToolMap->insertVertex( point, id );
  } );
}
