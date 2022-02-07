/***************************************************************************
  reosmaptooleditpolylinestructure_p.cpp - ReosMapToolEditPolylineStructure_p

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
#include "reosmaptooleditpolylinestructure_p.h"

#include <QMenu>

#include "qgsvertexmarker.h"
#include "qgsguiutils.h"

#include "reospolylinesstructure.h"
#include "reosstyleregistery.h"


ReosMapToolEditPolylineStructure_p::ReosMapToolEditPolylineStructure_p( QgsMapCanvas *map )
  :  ReosMapTool_p( map )
  , mMainActions( new QActionGroup( this ) )
  , mActionAddLines( new QAction( QPixmap( QStringLiteral( ":/images/addStructureLines.svg" ) ), tr( "Add Lines" ), this ) )
  , mActionMoveVertex( new QAction( QPixmap( QStringLiteral( ":/images/moveVertesStructure.svg" ) ), tr( "Move Vertex" ), this ) )
  , mActionInsertVertex( new QAction( tr( "Insert Vertex" ), this ) )
  , mActionRemoveVertex( new QAction( tr( "Remove Vertex" ), this ) )
  , mActionRemoveLine( new QAction( tr( "Remove Line" ), this ) )

{
  enableSnapping( true );

  mActionAddLines->setCheckable( true );
  mActionMoveVertex->setCheckable( true );
  mMainActions->addAction( mActionAddLines );
  mMainActions->addAction( mActionMoveVertex );
  mMainActions->setExclusive( true );
  mActionAddLines->setChecked( true );
  mMainActions->setEnabled( false );

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

  mHoveredLineBand = new QgsRubberBand( mCanvas, QgsWkbTypes::LineGeometry );
  mHoveredLineBand->setColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mHoveredLineBand->setWidth( 7 );

  mMapCrs = mapCrs();
}

void ReosMapToolEditPolylineStructure_p::setStructure( ReosPolylinesStructure *structure )
{
  mStructure = structure;
  mMapCrs = mapCrs(); //update the crs if changed

  mActionUndo = structure->undoStack()->createUndoAction( this );
  mActionUndo->setIcon( QPixmap( QStringLiteral( ":/images/undoOrange.svg" ) ) );
  mActionRedo = structure->undoStack()->createRedoAction( this );
  mActionRedo->setIcon( QPixmap( QStringLiteral( ":/images/redoOrange.svg" ) ) );

  mMainActions->addAction( mActionUndo );
  mMainActions->addAction( mActionRedo );
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

void ReosMapToolEditPolylineStructure_p::activate()
{
  mMainActions->setEnabled( true );
  mCurrentState = None;
  ReosMapTool_p::activate();
}

void ReosMapToolEditPolylineStructure_p::deactivate()
{
  mMainActions->setEnabled( false );
  ReosMapTool_p::deactivate();
}

void ReosMapToolEditPolylineStructure_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  mCurrentPosition = e->mapPoint().toQPointF();

  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
    case ReosMapToolEditPolylineStructure_p::AddingLines:

      mHoveredLineBand->reset( QgsWkbTypes::LineGeometry );

      if ( mStructure )
      {
        const QgsPointXY mapPoint = e->mapPoint();
        const ReosMapExtent sr = searchZone( mapPoint );
        mCurrentVertex = mStructure->searchForVertex( sr );
        if ( mCurrentVertex )
        {
          mSnappingIndicator->setMatch( QgsPointLocator::Match() );
          const QPointF &position = mStructure->vertexPosition( mCurrentVertex, mMapCrs );
          if ( mCurrentState == AddingLines )
            moveAddingLineRubberBand( position );
          mVertexMarker->setCenter( position );
          mVertexMarker->setVisible( true );
          return;
        }
        else
        {
          qint64 lineId;
          if ( mStructure->searchForLine( sr, lineId ) )
          {
            const QLineF line = mStructure->line( lineId, mMapCrs );
            if ( !line.isNull() )
            {
              mHoveredLineBand->addPoint( line.p1() );
              mHoveredLineBand->addPoint( line.p2() );
            }
          }
        }

        mVertexMarker->setVisible( false );

        if ( mCurrentState == AddingLines )
        {
          QgsPointXY mapPoint = e->snapPoint();
          moveAddingLineRubberBand( mapPoint );
        }
      }
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
  const QPointF &snapPoint = e->snapPoint().toQPointF();

  switch ( mCurrentState )
  {
    case ReosMapToolEditPolylineStructure_p::None:
      if ( e->button() == Qt::LeftButton )
      {
        if ( mActionMoveVertex->isChecked() && mCurrentVertex )
        {
          const QPointF &vertexPosition = mStructure->vertexPosition( mCurrentVertex, mMapCrs );
          mCurrentState = DraggingVertex;
          mNeighborPosition = mStructure->neighborsPositions( mCurrentVertex, mMapCrs );
          const QgsPointXY center( vertexPosition );
          updateMovingVertexRubberBand( center );
        }

        if ( mActionAddLines->isChecked() )
        {
          mCurrentState = AddingLines;
          addVertexForNewLines( snapPoint );
        }
      }
      else if ( e->button() == Qt::RightButton && mActionAddLines->isChecked() )
      {
        QgsGeometry geom = selectFeatureOnMap( e );
        const QPolygonF poly = geom.asQPolygonF();
        QList<double> tolerances;
        tolerances.reserve( poly.count() );
        for ( int i = 0; i < poly.count(); ++i )
          tolerances.append( -1 );

        if ( !geom.isNull() )
          mStructure->addPolylines( geom.asQPolygonF(), tolerances, mMapCrs );
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
        addVertexForNewLines( snapPoint );
      }
      else if ( e->button() == Qt::RightButton )
      {
        if ( mAddingPolyline.count() > 1 )
          mStructure->addPolylines( mAddingPolyline, mAddingLineTolerance, mMapCrs );

        stopAddingLines();
      }
      break;
  }

  ReosMapTool_p::canvasReleaseEvent( e );
}

void ReosMapToolEditPolylineStructure_p::canvasReleaseEvent( QgsMapMouseEvent *e )
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

void ReosMapToolEditPolylineStructure_p::keyPressEvent( QKeyEvent *e )
{
  if ( e->key() == Qt::Key_Escape )
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

void ReosMapToolEditPolylineStructure_p::removeLine( qint64 lineId )
{
  mStructure->removeLine( lineId );
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
  ReosMapExtent sr = searchZone( movingPosition );

  QgsPointXY pt = movingPosition;
  for ( const QPointF &addingPoint : std::as_const( mAddingPolyline ) )
    if ( sr.contains( addingPoint ) )
    {
      pt = QgsPointXY( addingPoint );
      mVertexMarker->setCenter( pt );
      mVertexMarker->setVisible( true );
      break;
    }

  mLineRubberBand->movePoint( pt );
}

void ReosMapToolEditPolylineStructure_p::stopDraggingVertex()
{
  mLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mVertexRubberBand->reset( QgsWkbTypes::PointGeometry );
  mNeighborPosition.clear();
  mCurrentVertex = nullptr;
  mCurrentState = None;
}

void ReosMapToolEditPolylineStructure_p::addVertexForNewLines( const QPointF &point )
{
  QPointF vertexPosition;
  qint64 lineId;

  ReosMapExtent sr = searchZone( point );

  if ( mCurrentVertex )
    vertexPosition = mStructure->vertexPosition( mCurrentVertex, mMapCrs );
  else if ( mStructure->searchForLine( sr, lineId ) )
    vertexPosition = mStructure->projectedPoint( point, lineId, mMapCrs );
  else
  {
    vertexPosition = point;
    for ( const QPointF &addingPoint : std::as_const( mAddingPolyline ) )
      if ( sr.contains( addingPoint ) )
      {
        vertexPosition = addingPoint;
        break;
      }
  }

  if ( !mAddingPolyline.isEmpty() )
  {
    const QPointF prevPt = mAddingPolyline.last();
    QList<QPointF> intersections = mStructure->intersectionPoints( QLineF( prevPt, vertexPosition ), mMapCrs, mLineRubberBand->asGeometry().asQPolygonF() );
    for ( const QPointF &pt : std::as_const( intersections ) )
    {
      mAddingPolyline.append( pt );
      mAddingLineTolerance.append( -1 );
      mLineRubberBand->movePoint( pt );
      mLineRubberBand->addPoint( pt );
      mVertexRubberBand->addPoint( pt );
    }
  }

  mAddingPolyline.append( vertexPosition );
  mAddingLineTolerance.append( tolerance() );
  mLineRubberBand->movePoint( vertexPosition );
  mLineRubberBand->addPoint( vertexPosition );
  mVertexRubberBand->addPoint( vertexPosition );
}

void ReosMapToolEditPolylineStructure_p::stopAddingLines()
{
  mLineRubberBand->reset( QgsWkbTypes::LineGeometry );
  mVertexRubberBand->reset( QgsWkbTypes::PointGeometry );
  mNeighborPosition.clear();
  mAddingPolyline.clear();
  mAddingLineTolerance.clear();
  mCurrentVertex = nullptr;
  mCurrentState = None;
}

QActionGroup *ReosMapToolEditPolylineStructure_p::mainActions() const
{
  return mMainActions;
}

ReosEditPolylineStructureMenuPopulator::ReosEditPolylineStructureMenuPopulator( ReosMapToolEditPolylineStructure_p *toolMap )
  : mToolMap( toolMap )
{}

bool ReosEditPolylineStructureMenuPopulator::populate( QMenu *menu, QgsMapMouseEvent *e )
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

void ReosEditPolylineStructureMenuPopulator::populateVertexAction( ReosGeometryStructureVertex *vertex, QMenu *menu )
{
  if ( !mToolMap->mStructure->isOnBoundary( vertex ) || mToolMap->mStructure->boundary().count() > 3 )
  {
    menu->addAction( mToolMap->mActionRemoveVertex );
    mToolMap->mActionRemoveVertex->setEnabled( mToolMap->mStructure->vertexCanBeRemoved( vertex ) );
    QObject::connect( mToolMap->mActionRemoveVertex, &QAction::triggered, menu, [this, vertex]
    {
      mToolMap->removeVertex( vertex );
    } );
  }
}

void ReosEditPolylineStructureMenuPopulator::populateLineAction( QgsFeatureId id, const QPointF &point, QMenu *menu )
{
  menu->addAction( mToolMap->mActionInsertVertex );

  QObject::connect( mToolMap->mActionInsertVertex, &QAction::triggered, menu, [this, point, id]
  {
    mToolMap->insertVertex( point, id );
  } );

  menu->addAction( mToolMap->mActionRemoveLine );
  mToolMap->mActionRemoveLine->setEnabled( mToolMap->mStructure->lineCanBeRemoved( id ) );
  QObject::connect( mToolMap->mActionRemoveLine, &QAction::triggered, menu, [this, id]
  {
    mToolMap->removeLine( id );
  } );
}
