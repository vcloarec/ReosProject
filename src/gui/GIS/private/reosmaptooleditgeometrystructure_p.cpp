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
  , mMainActions( new QActionGroup( this ) )
  , mActionAddLines( new QAction( tr( "Add lines" ), this ) )
  , mActionMoveVertex( new QAction( tr( "Move vertex" ), this ) )
  , mActionInsertVertex( new QAction( tr( "Insert Vertex" ), this ) )
  , mActionRemoveVertex( new QAction( tr( "Remove Vertex" ), this ) )

{
  enableSnapping( true );

  mActionAddLines->setCheckable( true );
  mActionMoveVertex->setCheckable( true );
  mMainActions->addAction( mActionAddLines );
  mMainActions->addAction( mActionMoveVertex );
  mMainActions->setExclusive( true );

  connect( mActionAddLines, &QAction::triggered, this, &ReosMapToolEditPolylineStructure_p::resetTool );
  connect( mActionMoveVertex, &QAction::triggered, this, &ReosMapToolEditPolylineStructure_p::resetTool );

  mVertexMarker = new QgsVertexMarker( map );
  mVertexMarker->setVisible( false );
  mVertexMarker->setColor( ReosStyleRegistery::instance()->blueReos() );
  mVertexMarker->setIconSize( QgsGuiUtils::scaleIconSize( 12 ) );
  mVertexMarker->setPenWidth( QgsGuiUtils::scaleIconSize( 4 ) );
  mVertexMarker->setIconType( QgsVertexMarker::ICON_CIRCLE );
  mVertexMarker->setZValue( 55 );

  mLineRubberBand = new QgsRubberBand( mCanvas, QgsWkbTypes::LineGeometry );
  mLineRubberBand->setWidth( 1 );

  mLineRubberBand->setLineStyle( Qt::DashLine );
  mLineRubberBand->setStrokeColor( ReosStyleRegistery::instance()->blueReos() );
  mLineRubberBand->setSecondaryStrokeColor( Qt::white );
  mLineRubberBand->setZValue( 50 );

  mVertexRubberBand = new QgsRubberBand( mCanvas, QgsWkbTypes::PointGeometry );
  mVertexRubberBand->setIcon( QgsRubberBand::ICON_CIRCLE );
  mVertexRubberBand->setWidth( QgsGuiUtils::scaleIconSize( 2 ) );
  mVertexRubberBand->setColor( ReosStyleRegistery::instance()->blueReos() );
  mVertexRubberBand->setSecondaryStrokeColor( Qt::white );
  mVertexRubberBand->setIconSize( QgsGuiUtils::scaleIconSize( 5 ) );
  mVertexRubberBand->setZValue( 51 );
  mVertexRubberBand->setVisible( true );

  mMapCrs = mapCrs();
}

void ReosMapToolEditPolylineStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  mMapCrs = mapCrs(); //update the crs if changed
}

QgsMapTool::Flags ReosMapToolEditPolylineStructure_p::flags() const
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      return Flags();
      break;
    case ReosMapToolEditPolylineStructure_p::AddingLines:
      return Flags();
      break;
    case ReosMapToolEditPolylineStructure_p::None:
      if ( mActionAddLines->isChecked() && hasFeatureOnMap( mCurrentPosition ) )
        return Flags();
      return ShowContextMenu;
      break;
  }

  return Flags();
}

void ReosMapToolEditPolylineStructure_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  mCurrentPosition = e->mapPoint().toQPointF();

  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
    case ReosMapToolEditPolylineStructure_p::AddingLines:

      if ( mStructure )
      {
        QgsPointXY mapPoint = e->mapPoint();
        mCurrentVertex = mStructure->searchForVertex( searchZone( mapPoint ) );
        if ( mCurrentVertex )
        {
          mSnappingIndicator->setMatch( QgsPointLocator::Match() );
          const QPointF &position = mStructure->vertexPosition( mCurrentVertex, mMapCrs );
          mVertexMarker->setCenter( position );
          mVertexMarker->setVisible( true );
          if ( mCurrentState == AddingLines )
            moveAddingLineRubberBand( position );
          return;
        }
        else if ( mCurrentState == AddingLines )
        {
          QgsPointXY mapPoint = e->snapPoint();
          moveAddingLineRubberBand( mapPoint );
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
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
    case ReosMapToolEditPolylineStructure_p::AddingLines:
      break;
  }

  ReosMapTool_p::canvasPressEvent( e );
}

void ReosMapToolEditPolylineStructure_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  const QPointF &snapPoint = e->snapPoint().toQPointF();


  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      if ( e->button() == Qt::LeftButton )
      {
        if ( mCurrentVertex )
        {
          if ( mActionMoveVertex->isChecked() )
          {
            mCurrentState = DraggingVertex;
            mNeighborPosition = mStructure->neighborsPositions( mCurrentVertex, mMapCrs );
            const QgsPointXY center( mStructure->vertexPosition( mCurrentVertex, mMapCrs ) );
            updateMovingVertexRubberBand( center );
          }
          else if ( mActionAddLines->isChecked() )
          {
            mCurrentState = AddingLines;
          }
          mVertexMarker->setVisible( false );

        }
        else
        {
          if ( mActionAddLines->isChecked() )
          {
            mCurrentState = AddingLines;
            mAddingPolyline.append( snapPoint );
            mLineRubberBand->addPoint( snapPoint );
            mVertexRubberBand->addPoint( snapPoint );
          }
        }
      }
      else if ( e->button() == Qt::RightButton && mActionAddLines->isChecked() )
      {
        QgsGeometry geom = selectFeatureOnMap( e );
        if ( !geom.isNull() )
          mStructure->addPolylines( geom.asQPolygonF(), tolerance(), mMapCrs );
      }

      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      if ( mCurrentVertex )
      {
        if ( e->button() == Qt::LeftButton )
        {
          if ( mStructure->vertexCanBeMoved( mCurrentVertex, ReosSpatialPosition( snapPoint, mMapCrs ) ) )
            mStructure->moveVertex( mCurrentVertex, ReosSpatialPosition( snapPoint, mMapCrs ) );

          stopDraggingVertex();
          canvas()->refresh();
        }
        else if ( e->button() == Qt::RightButton )
        {
          stopDraggingVertex();
        }
      }
      break;
    case ReosMapToolEditPolylineStructure_p::AddingLines:
      if ( e->button() == Qt::LeftButton )
      {
        QPointF position;

        if ( mCurrentVertex )
          position =  mStructure->vertexPosition( mCurrentVertex, mMapCrs );
        else
          position = snapPoint;

        mLineRubberBand->addPoint( position );
        mVertexRubberBand->addPoint( position );
        mAddingPolyline.append( position );
      }
      else if ( e->button() == Qt::RightButton )
      {
        if ( !mAddingPolyline.isEmpty() )
          mStructure->addPolylines( mAddingPolyline, tolerance(), mMapCrs );

        mAddingPolyline.clear();
        mLineRubberBand->reset( QgsWkbTypes::LineGeometry );
        mVertexRubberBand->reset( QgsWkbTypes::PointGeometry );
        canvas()->refresh();
        mCurrentState = None;
      }
      break;
  }

  ReosMapTool_p::canvasReleaseEvent( e );
}

void ReosMapToolEditPolylineStructure_p::keyPressEvent( QKeyEvent *e )
{
  resetTool();
}

void ReosMapToolEditPolylineStructure_p::insertVertex( const QPointF &mapPoint, qint64 lineId )
{
  mStructure->insertVertex( mapPoint, lineId );
}

void ReosMapToolEditPolylineStructure_p::removeVertex( ReosGeometryStructureVertex *vertex )
{
  mStructure->removeVertex( vertex );
}

void ReosMapToolEditPolylineStructure_p::resetTool()
{
  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::AddingLines:
      stopAddingLines();
      break;
    case ReosMapToolEditPolylineStructure_p::None:
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
      stopDraggingVertex();
      break;
  }
}

double ReosMapToolEditPolylineStructure_p::tolerance() const
{
  const QgsSnappingConfig &snapConfig = QgsProject::instance()->snappingConfig();
  return QgsTolerance::toleranceInProjectUnits( snapConfig.tolerance(),
         nullptr, canvas()->mapSettings(), snapConfig.units() );
}

ReosMapExtent ReosMapToolEditPolylineStructure_p::searchZone( const QgsPointXY &point ) const
{
  double tol = tolerance();
  ReosMapExtent zone( point.x() - tol, point.y() - tol, point.x() + tol, point.y() + tol );
  zone.setCrs( mMapCrs );

  return zone;
}

void ReosMapToolEditPolylineStructure_p::updateMovingVertexRubberBand( const QgsPointXY &movingPosition )
{
  mLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mVertexRubberBand->reset( QgsWkbTypes::PointGeometry );

  if ( mStructure->vertexCanBeMoved( mCurrentVertex, ReosSpatialPosition( movingPosition.toQPointF(), mMapCrs ) ) )
  {
    mLineRubberBand->setColor( ReosStyleRegistery::instance()->blueReos() );
  }
  else
  {
    mLineRubberBand->setColor( ReosStyleRegistery::instance()->invalidColor() );
  }

  for ( const QPointF &np : std::as_const( mNeighborPosition ) )
  {
    const QgsGeometry geom( new QgsLineString( {movingPosition, QgsPointXY( np )} ) );
    mLineRubberBand->addGeometry( geom, QgsCoordinateReferenceSystem(), false );
    mVertexRubberBand->addPoint( np, false );
  }

  mVertexRubberBand->addPoint( movingPosition );
  mLineRubberBand->updatePosition();
}

void ReosMapToolEditPolylineStructure_p::moveAddingLineRubberBand( const QgsPointXY &movingPosition )
{
  mLineRubberBand->movePoint( movingPosition );
}

void ReosMapToolEditPolylineStructure_p::stopDraggingVertex()
{
  mLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mVertexRubberBand->reset( QgsWkbTypes::PointGeometry );
  mNeighborPosition.clear();
  mCurrentVertex = nullptr;
  mCurrentState = None;
}

void ReosMapToolEditPolylineStructure_p::stopAddingLines()
{
  mLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mVertexRubberBand->reset( QgsWkbTypes::PointGeometry );
  mNeighborPosition.clear();
  mCurrentVertex = nullptr;
  mCurrentState = None;
}

QActionGroup *ReosMapToolEditPolylineStructure_p::mainActions() const
{
  return mMainActions;
}

ReosEditGeometryStructureMenuPopulator::ReosEditGeometryStructureMenuPopulator( ReosMapToolEditPolylineStructure_p *toolMap )
  : mToolMap( toolMap )
{}

bool ReosEditGeometryStructureMenuPopulator::populate( QMenu *menu, QgsMapMouseEvent *e )
{
  if ( !mToolMap )
    return false;

  menu->clear();

  QPointF nonSnapPos = e->mapPoint().toQPointF();
  ReosMapExtent searchZone = mToolMap->searchZone( e->snapPoint() );
  QPointF snapPos = e->mapPoint().toQPointF();


  ReosGeometryStructureVertex *vertex = mToolMap->mStructure->searchForVertex( searchZone );
  if ( vertex )
    populateVertexAction( vertex, menu );

  qint64 id = 0;
  if ( !vertex && mToolMap->mStructure->searchForLine( mToolMap->searchZone( nonSnapPos ), id ) )
    populateLineAction( id, snapPos, menu );


  switch ( mToolMap->mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      menu->addActions( mToolMap->mainActions()->actions() );
      break;
    case ReosMapToolEditPolylineStructure_p::DraggingVertex:
    case ReosMapToolEditPolylineStructure_p::AddingLines:
      break;
  }

  return true;

}

void ReosEditGeometryStructureMenuPopulator::populateVertexAction( ReosGeometryStructureVertex *vertex, QMenu *menu )
{
  menu->addAction( mToolMap->mActionRemoveVertex );
  QObject::connect( mToolMap->mActionRemoveVertex, &QAction::triggered, menu, [this, vertex]
  {
    mToolMap->removeVertex( vertex );
  } );
}

void ReosEditGeometryStructureMenuPopulator::populateLineAction( QgsFeatureId id, const QPointF &point, QMenu *menu )
{
  menu->addAction( mToolMap->mActionInsertVertex );
  QObject::connect( mToolMap->mActionInsertVertex, &QAction::triggered, menu, [this, point, id]
  {
    mToolMap->insertVertex( point, id );
  } );
}
