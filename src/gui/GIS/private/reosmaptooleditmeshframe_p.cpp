/***************************************************************************
  reosmaptooleditmeshframe_p.cpp - ReosMapToolEditMeshFrame_p

 ---------------------
 begin                : 9.3.2022
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
#include "reosmaptooleditmeshframe_p.h"

#include <QMessageBox>
#include <QMenu>

#include <qgsmeshlayer.h>
#include <qgsmesheditor.h>
#include <qgsguiutils.h>
#include <qgsvertexmarker.h>
#include <qgsmeshlayerutils.h>
#include <qgspolygon.h>
#include <qgsgeometryengine.h>

#include "reosmesh.h"
#include "reosstyleregistery.h"
#include "reoszvaluemodificationwidget.h"

ReosMapToolEditMeshFrame_p::ReosMapToolEditMeshFrame_p( ReosMesh *mesh, QgsMapCanvas *canvas )
  : ReosMapTool_p( canvas )
  , mReosMesh( mesh )
  , mMeshLayer( qobject_cast<QgsMeshLayer*>( mesh->data() ) )
  , mMainActions( new QActionGroup( this ) )
{
  mActionEditMesh = new QAction( QIcon( QStringLiteral( ":/images/editMeshElement.svg" ) ), tr( "Edit Element" ), this );
  mActionEditMesh->setCheckable( true );
  mActionEditMesh->setChecked( true );
  mMainActions->addAction( mActionEditMesh );
  mActionSelectElementByPolygon = new QAction( QIcon( QStringLiteral( ":/images/selectMeshElement.svg" ) ),  tr( "Select element by polygon" ), this );
  mActionSelectElementByPolygon->setCheckable( true );
  mMainActions->addAction( mActionSelectElementByPolygon );

  mActionRemoveVertices = new QAction( QIcon( QStringLiteral( ":/images/removeMeshVertex.svg" ) ), tr( "Remove vertices" ), this );
  mMainActions->addAction( mActionRemoveVertices );
  connect( mActionRemoveVertices, &QAction::triggered, this, &ReosMapToolEditMeshFrame_p::removeSelectedVerticesFromMesh );
  mActionRemoveVertices->setEnabled( false );

  mActionChangeZValue = new QAction( QIcon( QStringLiteral( ":/images/changeZValue.svg" ) ), tr( "Change Z value" ), this );
  mMainActions->addAction( mActionChangeZValue );
  connect( mActionChangeZValue, &QAction::triggered, this, &ReosMapToolEditMeshFrame_p::changeZValue );
  mActionChangeZValue->setEnabled( false );

  mActionUndo = mMeshLayer->undoStack()->createUndoAction( this );
  mActionUndo->setIcon( QIcon( QStringLiteral( ":/images/undoBlue.svg" ) ) );
  mActionRedo = mMeshLayer->undoStack()->createRedoAction( this );
  mActionRedo->setIcon( QIcon( QStringLiteral( ":/images/redoBlue.svg" ) ) );
  mMainActions->addAction( mActionUndo );
  mMainActions->addAction( mActionRedo );
  mMainActions->setExclusive( true );

  connect( mMeshLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosMapToolEditMeshFrame_p::onEdit );
  connect( mActionEditMesh, &QAction::triggered, this, &ReosMapToolEditMeshFrame_p::onModeChange );
  connect( mActionSelectElementByPolygon, &QAction::triggered, this, &ReosMapToolEditMeshFrame_p::onModeChange );

  mFaceRubberBand = new QgsRubberBand( mCanvas, QgsWkbTypes::PolygonGeometry );
  mFaceRubberBand->setZValue( 5 );
  mFaceRubberBand->setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mFaceRubberBand->setStrokeColor( ReosStyleRegistery::instance()->blueReos( 200 ) );
  mFaceRubberBand->setWidth( 1 );

  mVertexBand = new QgsRubberBand( mCanvas );
  mVertexBand->setIcon( QgsRubberBand::ICON_CIRCLE );
  mVertexBand->setColor( ReosStyleRegistery::instance()->orangeReos() );
  mVertexBand->setWidth( QgsGuiUtils::scaleIconSize( 2 ) );
  mVertexBand->setBrushStyle( Qt::NoBrush );
  mVertexBand->setIconSize( QgsGuiUtils::scaleIconSize( 15 ) );
  mVertexBand->setVisible( false );
  mVertexBand->setZValue( 5 );

  mFaceVerticesBand = new QgsRubberBand( mCanvas );
  mFaceVerticesBand->setIcon( QgsRubberBand::ICON_CIRCLE );
  mFaceVerticesBand->setColor( ReosStyleRegistery::instance()->blueReos() );
  mFaceVerticesBand->setWidth( QgsGuiUtils::scaleIconSize( 2 ) );
  mFaceVerticesBand->setBrushStyle( Qt::NoBrush );
  mFaceVerticesBand->setIconSize( QgsGuiUtils::scaleIconSize( 8 ) );
  mFaceVerticesBand->setVisible( false );
  mFaceVerticesBand->setZValue( 5 );

  mSelectFaceMarker = new QgsVertexMarker( mCanvas );
  mSelectFaceMarker->setIconType( QgsVertexMarker::ICON_BOX );
  mSelectFaceMarker->setIconSize( QgsGuiUtils::scaleIconSize( 10 ) );
  mSelectFaceMarker->setColor( Qt::gray );
  mSelectFaceMarker->setFillColor( Qt::gray );
  mSelectFaceMarker->setVisible( false );
  mSelectFaceMarker->setPenWidth( 3 );
  mSelectFaceMarker->setZValue( 10 );

  mEdgeBand = new QgsRubberBand( mCanvas );
  mEdgeBand->setColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mEdgeBand->setWidth( QgsGuiUtils::scaleIconSize( 10 ) );

  mFlipEdgeMarker = new QgsVertexMarker( mCanvas );
  mFlipEdgeMarker->setIconType( QgsVertexMarker::ICON_CIRCLE );
  mFlipEdgeMarker->setIconSize( QgsGuiUtils::scaleIconSize( 12 ) );
  mFlipEdgeMarker->setColor( Qt::gray );
  mFlipEdgeMarker->setVisible( false );
  mFlipEdgeMarker->setPenWidth( 3 );
  mFlipEdgeMarker->setZValue( 10 );

  mSelectEdgeMarker = new QgsVertexMarker( mCanvas );
  mSelectEdgeMarker->setIconType( QgsVertexMarker::ICON_BOX );
  mSelectEdgeMarker->setIconSize( QgsGuiUtils::scaleIconSize( 10 ) );
  mSelectEdgeMarker->setColor( Qt::gray );
  mSelectEdgeMarker->setFillColor( Qt::gray );
  mSelectEdgeMarker->setVisible( false );
  mSelectEdgeMarker->setPenWidth( 3 );
  mSelectEdgeMarker->setZValue( 10 );

  mSelectionBand = new QgsRubberBand( mCanvas, QgsWkbTypes::PolygonGeometry );
  mSelectionBand->setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mSelectionBand->setStrokeColor( ReosStyleRegistery::instance()->blueReos( 200 ) );
  mSelectionBand->setZValue( 10 );

  mSelectedFacesRubberband = new QgsRubberBand( mCanvas, QgsWkbTypes::PolygonGeometry );
  mSelectedFacesRubberband->setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  mSelectedFacesRubberband->setStrokeColor( ReosStyleRegistery::instance()->blueReos( 200 ) );
  mSelectedFacesRubberband->setZValue( 1 );

  mMovingEdgesRubberband = new QgsRubberBand( mCanvas, QgsWkbTypes::LineGeometry );
  mMovingFacesRubberband = new QgsRubberBand( mCanvas, QgsWkbTypes::PolygonGeometry );

}

ReosMapToolEditMeshFrame_p::~ReosMapToolEditMeshFrame_p()
{
  delete mSelectFaceMarker;
  delete mFlipEdgeMarker;
  delete mSelectEdgeMarker;
  clearSelection();
  mFaceRubberBand->deleteLater();
  mEdgeBand->deleteLater();
  mFaceVerticesBand->deleteLater();
  mSelectionBand->deleteLater();
  mVertexBand->deleteLater();
  mSelectedFacesRubberband->deleteLater();
  mMovingEdgesRubberband->deleteLater();
  mMovingFacesRubberband->deleteLater();
}

QActionGroup *ReosMapToolEditMeshFrame_p::mainActions() const
{
  return mMainActions;
}

void ReosMapToolEditMeshFrame_p::activate()
{
  if ( !mMeshLayer->isEditable() )
    startMeshEditing();
  else
    mMeshEditor = mMeshLayer->meshEditor();

  mMainActions->setEnabled( true );

  if ( mMeshEditor )
    ReosMapTool_p::activate();
}

void ReosMapToolEditMeshFrame_p::deactivate()
{
  mMeshEditor = nullptr;
  clearSelection();
  clearCanvasHelpers();
  clearEdgeHelpers();

  mMainActions->setEnabled( false );

  ReosMapTool_p::deactivate();
}

QgsMapTool::Flags ReosMapToolEditMeshFrame_p::flags() const
{
  switch ( mCurrentState )
  {
    case Digitizing:
      return ShowContextMenu;
      break;
    case ReosMapToolEditMeshFrame_p::Selecting:
    case ReosMapToolEditMeshFrame_p::MovingSelection:
      return Flags();
      break;
    case ReosMapToolEditMeshFrame_p::SelectingByPolygon:
      if ( mSelectionBand->numberOfVertices() > 2 ||
           hasFeatureOnMap( ( mCurrentPosition.toQPointF() ) ) )
        return Flags();
      else
        return ShowContextMenu;
      break;
  }

  return Flags();
}

bool ReosMapToolEditMeshFrame_p::populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event )
{
  Q_UNUSED( event );

  menu->clear();

  QList<QAction *> newActions;
  switch ( mCurrentState )
  {
    case Digitizing:
    case SelectingByPolygon:
    {

      if ( !mSelectedVertices.isEmpty() )
      {
        newActions << mActionChangeZValue << mActionRemoveVertices;
      }
    }
    break;

    case Selecting:
    case MovingSelection:
      return false;
  }

  if ( !newActions.isEmpty() )
  {
    for ( QAction *act : std::as_const( newActions ) )
      menu->addAction( act );

    menu->addSeparator();
  }

  menu->addAction( mActionEditMesh );
  menu->addAction( mActionSelectElementByPolygon );
  return true;
}

void ReosMapToolEditMeshFrame_p::canvasMoveEvent( QgsMapMouseEvent *e )
{
  if ( !mMeshEditor )
    return;

  const QgsPointXY &mapPoint = e->mapPoint();

  mCurrentPosition = mapPoint;

  if ( mLeftButtonPressed && mCurrentState == Digitizing )
    mCurrentState = Selecting;

  switch ( mCurrentState )
  {
    case ReosMapToolEditMeshFrame_p::Digitizing:
      highLight( mapPoint );
      break;
    case Selecting:
    {
      const QRect &rect = QRect( e->pos(), mStartSelectionPos );
      mSelectionBand->setToCanvasRectangle( rect );
    }
    break;
    case MovingSelection:
    {
      moveSelection( mapPoint );
    }
    break;
    case SelectingByPolygon:
      mSelectionBand->movePoint( mapPoint );
      break;
  }

  ReosMapTool_p::canvasMoveEvent( e );
}

void ReosMapToolEditMeshFrame_p::canvasPressEvent( QgsMapMouseEvent *e )
{
  if ( !mMeshEditor )
    return;

  QgsPointXY mapPoint = e->mapPoint();

  if ( e->button() == Qt::LeftButton )
    mLeftButtonPressed = true;

  switch ( mCurrentState )
  {
    case Digitizing:
      if ( e->button() == Qt::LeftButton )
        mStartSelectionPos = e->pos();
      break;
    case Selecting:
    case MovingSelection:
      if ( e->button() == Qt::LeftButton )
        mSelectionBand->reset( QgsWkbTypes::PolygonGeometry );
      break;
    case SelectingByPolygon:
      if ( e->button() == Qt::LeftButton )
      {
        mSelectionBand->addPoint( mapPoint );
        mSelectionBand->movePoint( mCurrentPosition );
      }
      else if ( e->button() == Qt::RightButton && mSelectionBand->numberOfVertices() < 2 )
      {
        QgsGeometry geom = selectFeatureOnMap( e );
        selectByGeometry( geom, e->modifiers() );
      }
      break;
  }

}

void ReosMapToolEditMeshFrame_p::canvasReleaseEvent( QgsMapMouseEvent *e )
{
  if ( !mMeshEditor )
    return;

  double tolerance = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );

  if ( e->button() == Qt::LeftButton )
    mLeftButtonPressed = false;

  QgsPointXY mapPoint = e->mapPoint();

  switch ( mCurrentState )
  {
    case Digitizing:
      if ( e->button() == Qt::LeftButton )
      {
        if ( mDoubleClicks &&
             ( mCurrentFaceIndex != -1 || mCurrentEdge != Edge{-1, -1} ) ) //double clicks --> add a vertex onlky iw we are on a edge or face
        {
          addVertex( mapPoint, e->mapPointMatch() );
        }
        else if ( isSelectionGrapped( mapPoint )  && //click on a selected vertex, an edge or face box
                  !( e->modifiers() &Qt::ControlModifier ) ) // without control modifier that is used to remove from the selection
        {
          mCurrentState = MovingSelection;
          mStartMovingPoint = mapPoint;
        }
        else if ( mFlipEdgeMarker->isVisible() &&
                  e->mapPoint().distance( mFlipEdgeMarker->center() ) < tolerance &&
                  mCurrentEdge.first != -1 && mCurrentEdge.second != -1 )  // flip edge
        {
          clearSelection();
          QVector<int> edgeVert = edgeVertices( mCurrentEdge );
          mMeshEditor->flipEdge( edgeVert.at( 0 ), edgeVert.at( 1 ) );
          mCurrentEdge = {-1, -1};
          highLight( mapPoint );
        }
        else
          select( mapPoint, e->modifiers(), tolerance );
      }
      break;
    case Selecting:
    {
      QgsGeometry selectionGeom = mSelectionBand->asGeometry();
      selectByGeometry( selectionGeom, e->modifiers() );
      mSelectionBand->reset( QgsWkbTypes::PolygonGeometry );
      mCurrentState = Digitizing;
    }
    break;
    case MovingSelection:
      if ( mIsMovingAllowed )
      {
        const QList<int> verticesIndexes = mSelectedVertices.keys();
        QList<QgsPointXY> newPosition;
        newPosition.reserve( verticesIndexes.count() );

        const QgsMeshVertex &mapPointInNativeCoordinate =
          mMeshLayer->triangularMesh()->triangularToNativeCoordinates( QgsMeshVertex( mapPoint.x(), mapPoint.y() ) );
        const QgsMeshVertex &startingPointInNativeCoordinate =
          mMeshLayer->triangularMesh()->triangularToNativeCoordinates( QgsMeshVertex( mStartMovingPoint.x(), mStartMovingPoint.y() ) );
        const QgsVector &translationInLayerCoordinate = mapPointInNativeCoordinate - startingPointInNativeCoordinate;

        const QgsMesh &mesh = *mMeshLayer->nativeMesh();
        mKeepSelectionOnEdit = true;
        if ( verticesIndexes.count() != 1 )
        {
          for ( int i = 0; i < verticesIndexes.count(); ++i )
            newPosition.append( QgsPointXY( mesh.vertex( verticesIndexes.at( i ) ) ) + translationInLayerCoordinate );
          mMeshEditor->changeXYValues( verticesIndexes, newPosition );
        }
        else
        {
          //only one vertex, change also the Z value if snap on a 3D vector layer
          if ( e->mapPointMatch().isValid() &&
               QgsWkbTypes::hasZ( e->mapPointMatch().layer()->wkbType() ) )
          {
            const QgsMeshVertex mapPointInMapCoordinate =
              QgsMeshVertex( mapPoint.x(), mapPoint.y(), e->mapPointMatch().interpolatedPoint( mCanvas->mapSettings().destinationCrs() ).z() );

            const QgsMeshVertex &mapPointInNativeCoordinate =
              mMeshLayer->triangularMesh()->triangularToNativeCoordinates( mapPointInMapCoordinate ) ;
            mMeshEditor->changeCoordinates( verticesIndexes,
                                            QList<QgsPoint>()
                                            << mapPointInNativeCoordinate ) ;
          }
          else
            mMeshEditor->changeXYValues( verticesIndexes, QList<QgsPointXY>()
                                         << QgsPointXY( mesh.vertex( verticesIndexes.at( 0 ) ) ) + translationInLayerCoordinate );
        }
      }
      updateSelectecVerticesMarker();
      prepareSelection();
      clearCanvasHelpers();
      mMovingEdgesRubberband->reset();
      mMovingFacesRubberband->reset();
      mCurrentState = Digitizing;
      break;

    case SelectingByPolygon:
      if ( e->button() == Qt::RightButton )
      {
        if ( mSelectionBand->numberOfVertices() > 1 )
          selectByGeometry( mSelectionBand->asGeometry(), e->modifiers() );
        mSelectionBand->reset( QgsWkbTypes::PolygonGeometry );
      }
  }

  mDoubleClicks = false;
}

void ReosMapToolEditMeshFrame_p::highLight( const QgsPointXY &mapPoint )
{
  highlightCurrentHoveredFace( mapPoint );
  highlightCloseEdge( mapPoint );
  highlightCloseVertex( mapPoint );
}

void ReosMapToolEditMeshFrame_p::searchFace( const QgsPointXY &mapPoint )
{
  if ( !mMeshLayer.isNull() && mMeshLayer->triangularMesh() )
    mCurrentFaceIndex = mMeshLayer->triangularMesh()->nativeFaceIndexForPoint( mapPoint );
}

void ReosMapToolEditMeshFrame_p::highlightCurrentHoveredFace( const QgsPointXY &mapPoint )
{
  searchFace( mapPoint );

  if ( mSelectFaceMarker->isVisible() )
  {
    double tol = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );
    if ( mapPoint.distance( mSelectFaceMarker->center() ) < tol )
    {
      mSelectFaceMarker->setColor( ReosStyleRegistery::instance()->orangeReos() );
      mSelectFaceMarker->setFillColor( ReosStyleRegistery::instance()->orangeReos() );
    }
    else
    {
      mSelectFaceMarker->setColor( Qt::gray );
      mSelectFaceMarker->setFillColor( Qt::gray );
    }
  }

  QgsPointSequence faceGeometry = nativeFaceGeometry( mCurrentFaceIndex );
  mFaceRubberBand->reset( QgsWkbTypes::PolygonGeometry );
  mFaceVerticesBand->reset( QgsWkbTypes::PointGeometry );
  for ( const QgsPoint &pt : faceGeometry )
  {
    mFaceRubberBand->addPoint( pt );
    mFaceVerticesBand->addPoint( pt );
  }

  if ( mCurrentFaceIndex != -1 && faceCanBeInteractive( mCurrentFaceIndex ) )
  {
    mSelectFaceMarker->setCenter( mMeshLayer->triangularMesh()->faceCentroids().at( mCurrentFaceIndex ) );
    mSelectFaceMarker->setVisible( true );
  }
  else
    mSelectFaceMarker->setVisible( false );
}

void ReosMapToolEditMeshFrame_p::searchEdge( const QgsPointXY &mapPoint )
{
  mCurrentEdge = {-1, -1};
  double tolerance = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );

  QList<int> candidateFaceIndexes;

  if ( mCurrentFaceIndex != -1 )
  {
    candidateFaceIndexes.append( mCurrentFaceIndex );
  }
  else
  {
    const QgsRectangle searchRect( mapPoint.x() - tolerance, mapPoint.y() - tolerance, mapPoint.x() + tolerance, mapPoint.y() + tolerance );
    candidateFaceIndexes = mMeshLayer->triangularMesh()->nativeFaceIndexForRectangle( searchRect );
  }

  double minimumDistance = std::numeric_limits<double>::max();
  for ( const int faceIndex : std::as_const( candidateFaceIndexes ) )
  {
    const QgsMeshFace &face = nativeFace( faceIndex );
    int faceSize = face.count();
    for ( int i = 0; i < faceSize; ++i )
    {
      int iv1 = face.at( i );
      int iv2 = face.at( ( i + 1 ) % faceSize );

      QgsPointXY pt1 = mapVertexXY( iv1 );
      QgsPointXY pt2 = mapVertexXY( iv2 );

      QgsPointXY pointOneEdge;
      double distance = sqrt( mapPoint.sqrDistToSegment( pt1.x(), pt1.y(), pt2.x(), pt2.y(), pointOneEdge ) );
      if ( distance < tolerance && distance < minimumDistance && edgeCanBeInteractive( iv1, iv2 ) )
      {
        mCurrentEdge = {faceIndex, iv2};
        minimumDistance = distance;
      }
    }
  }
}

bool ReosMapToolEditMeshFrame_p::edgeCanBeInteractive( int vertexIndex1, int vertexIndex2 ) const
{
  // If the edge is less than 90px width, the interactive marker will not be displayed to avoid too close marker and
  // avoid the user to click on a marker if he doesn't want
  double mapUnitPerPixel = mCanvas->mapSettings().mapUnitsPerPixel();
  return mapVertexXY( vertexIndex1 ).distance( mapVertexXY( vertexIndex2 ) ) / mapUnitPerPixel > 90;
}

bool ReosMapToolEditMeshFrame_p::faceCanBeInteractive( int faceIndex ) const
{
  // If both side of the face boundng box is less than 60px width, the interactive marker will not be displayed to avoid too close marker and
  // avoid the user to click on a marker if he doesn't want
  double mapUnitPerPixel = mCanvas->mapSettings().mapUnitsPerPixel();
  QgsGeometry faceGeom( new QgsLineString( nativeFaceGeometry( faceIndex ) ) );
  QgsRectangle bbox = faceGeom.boundingBox();

  return bbox.width() / mapUnitPerPixel > 60 || bbox.height() / mapUnitPerPixel > 60;
}

int ReosMapToolEditMeshFrame_p::closeVertex( const QgsPointXY &mapPoint ) const
{
  if ( !mMeshEditor )
    return -1;

  double tolerance = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );

  if ( mCurrentEdge.first != -1 && mCurrentEdge.second  != -1 )
  {
    const QVector<int> &edge = edgeVertices( mCurrentEdge );

    for ( const int vertexIndex : edge )
    {
      const QgsPointXY &meshVertex = mapVertexXY( vertexIndex );
      if ( meshVertex.distance( mapPoint ) < tolerance )
        return vertexIndex;
    }
  }

  if ( mCurrentFaceIndex >= 0 )
  {
    const QgsMeshFace &face = mMeshLayer->nativeMesh()->face( mCurrentFaceIndex );
    for ( const int vertexIndex : face )
    {
      const QgsPointXY &meshVertex = mapVertexXY( vertexIndex );
      if ( meshVertex.distance( mapPoint ) < tolerance )
        return vertexIndex;
    }

    //nothing found int the face --> return -1;
    return -1;
  }

  return -1;
}

void ReosMapToolEditMeshFrame_p::highlightCloseEdge( const QgsPointXY &mapPoint )
{
  if ( mMeshLayer.isNull() )
    return;

  double tolerance = QgsTolerance::vertexSearchRadius( mCanvas->mapSettings() );

  if ( mFlipEdgeMarker->isVisible() )
  {
    if ( mapPoint.distance( mFlipEdgeMarker->center() ) < tolerance )
      mFlipEdgeMarker->setColor( ReosStyleRegistery::instance()->orangeReos() );
    else
      mFlipEdgeMarker->setColor( Qt::gray );
  }

  if ( mSelectEdgeMarker->isVisible() )
  {
    if ( mapPoint.distance( mSelectEdgeMarker->center() ) < tolerance )
    {
      mSelectEdgeMarker->setColor( ReosStyleRegistery::instance()->orangeReos() );
      mSelectEdgeMarker->setFillColor( ReosStyleRegistery::instance()->orangeReos() );
    }
    else
    {
      mSelectEdgeMarker->setColor( Qt::gray );
      mSelectEdgeMarker->setFillColor( Qt::gray );
    }
  }

  searchEdge( mapPoint );

  mEdgeBand->reset();
  mFlipEdgeMarker->setVisible( false );
  mSelectEdgeMarker->setVisible( false );
  if ( mCurrentEdge.first != -1 && mCurrentEdge.second != -1 &&  mCurrentState == Digitizing )
  {
    const QVector<QgsPointXY> &edgeGeom = edgeGeometry( mCurrentEdge );
    mEdgeBand->addPoint( edgeGeom.at( 0 ) );
    mEdgeBand->addPoint( edgeGeom.at( 1 ) );

    if ( mCurrentFaceIndex == -1 )
    {
      mFaceVerticesBand->reset( QgsWkbTypes::PointGeometry );
      mFaceVerticesBand->addPoint( edgeGeom.at( 0 ) );
      mFaceVerticesBand->addPoint( edgeGeom.at( 1 ) );
    }

    const QVector<int> edgeVert = edgeVertices( mCurrentEdge );
    QgsPointXY basePoint;
    QgsVector intervalOfMarkers;
    if ( std::fabs( edgeGeom.at( 0 ).x() - edgeGeom.at( 1 ).x() ) > std::fabs( edgeGeom.at( 0 ).y() - edgeGeom.at( 1 ).y() ) )
    {
      // edge are more horizontal than vertical, take the vertex on the left side
      if ( edgeGeom.at( 0 ).x() < edgeGeom.at( 1 ).x() )
      {
        basePoint = edgeGeom.at( 0 );
        intervalOfMarkers = ( edgeGeom.at( 1 ) - edgeGeom.at( 0 ) ) / 4;
      }
      else
      {
        basePoint = edgeGeom.at( 1 );
        intervalOfMarkers = ( edgeGeom.at( 0 ) - edgeGeom.at( 1 ) ) / 4;
      }
    }
    else
    {
      // edge are more vertical than horizontal, take the vertex on the bottom
      if ( edgeGeom.at( 0 ).y() < edgeGeom.at( 1 ).y() )
      {
        basePoint = edgeGeom.at( 0 );
        intervalOfMarkers = ( edgeGeom.at( 1 ) - edgeGeom.at( 0 ) ) / 4;
      }
      else
      {
        basePoint = edgeGeom.at( 1 );
        intervalOfMarkers = ( edgeGeom.at( 0 ) - edgeGeom.at( 1 ) ) / 4;
      }
    }

    mSelectEdgeMarker->setVisible( true );
    mSelectEdgeMarker->setCenter( basePoint + intervalOfMarkers * 2 );

    if ( mMeshEditor->edgeCanBeFlipped( edgeVert.at( 0 ), edgeVert.at( 1 ) ) )
    {
      mFlipEdgeMarker->setVisible( true );
      mFlipEdgeMarker->setCenter( basePoint + intervalOfMarkers );
    }
    else
      mFlipEdgeMarker->setVisible( false );

  }
}

void ReosMapToolEditMeshFrame_p::highlightCloseVertex( const QgsPointXY &mapPoint )
{
  if ( !mMeshEditor )
    return;

  if ( mVertexBand->isVisible() && mVertexBand->numberOfVertices() > 0 )
  {
    double tol = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );
    if ( mVertexBand->getPoint( 0 )->distance( mapPoint ) > tol )
    {
      mVertexBand->reset( QgsWkbTypes::PointGeometry );
      mVertexBand->setVisible( false );
      mCurrentVertexIndex = -1;
    }
  }
  else
  {
    int closeVert = closeVertex( mapPoint );
    mCurrentVertexIndex = -1;
    mVertexBand->reset( QgsWkbTypes::PointGeometry );

    if ( closeVert >= 0 &&
         !( mReosMesh->vertexIsOnBoundary( closeVert )  || mReosMesh->vertexIsOnHoleBorder( closeVert ) ) )
    {
      mCurrentVertexIndex = closeVert;
      mVertexBand->addPoint( mapVertexXY( closeVert ) );
    }
  }

}

QgsPointSequence ReosMapToolEditMeshFrame_p::nativeFaceGeometry( int faceIndex ) const
{
  QgsPointSequence faceGeometry;
  const QgsMeshFace &face = mMeshLayer->nativeMesh()->face( faceIndex );

  for ( const int index : face )
    faceGeometry.append( mapVertex( index ) );

  return faceGeometry;
}

const QgsMeshVertex ReosMapToolEditMeshFrame_p::mapVertex( int index ) const
{
  if ( mMeshLayer.isNull() || ! mMeshLayer->triangularMesh() )
    return QgsMeshVertex();

  return mMeshLayer->triangularMesh()->vertices().at( index );
}

const QgsPointXY ReosMapToolEditMeshFrame_p::mapVertexXY( int index ) const
{
  const QgsMeshVertex &v = mapVertex( index );
  return QgsPointXY( v.x(), v.y() );
}

QVector<QgsPointXY> ReosMapToolEditMeshFrame_p::edgeGeometry( const Edge &edge ) const
{
  const QVector<int> &vertexIndexes = edgeVertices( edge );
  return {mapVertexXY( vertexIndexes.at( 0 ) ), mapVertexXY( vertexIndexes.at( 1 ) )};
}

const QgsMeshFace ReosMapToolEditMeshFrame_p::nativeFace( int index ) const
{
  if ( mMeshLayer.isNull() || ! mMeshLayer->nativeMesh() )
    return QgsMeshFace();

  return mMeshLayer->nativeMesh()->face( index );
}

QVector<int> ReosMapToolEditMeshFrame_p::edgeVertices( const ReosMapToolEditMeshFrame_p::Edge &edge ) const
{
  const QgsMeshFace &face = nativeFace( edge.first );
  int faceSize = face.count();
  int posInface = ( face.indexOf( edge.second ) + faceSize - 1 ) % faceSize;

  return {face.at( posInface ), edge.second};
}

void ReosMapToolEditMeshFrame_p::startMeshEditing()
{
  QgsCoordinateTransform transform( mMeshLayer->crs(), mCanvas->mapSettings().destinationCrs(), QgsProject::instance() );

  if ( !mMeshLayer->startFrameEditing( transform ) )
  {
    QMessageBox::warning( mCanvas, tr( "Start Editing Elements" ), "Unable to start editing mesh elements." );
    return;
  }

  mMeshEditor = mMeshLayer->meshEditor();
}

void ReosMapToolEditMeshFrame_p::removeSelectedVerticesFromMesh()
{
  mMeshEditor->removeVerticesFillHoles( mSelectedVertices.keys() );
}

void ReosMapToolEditMeshFrame_p::changeZValue()
{
  ReosZValueModificationWidget *w = new ReosZValueModificationWidget( mCanvas );

  if ( w->exec() )
  {
    ReosZValueModificationWidget::ModificationType type = w->modificationType();
    double value = w->value();

    QList<double> newValues;
    QList<int> vertIndex = mSelectedVertices.keys();
    newValues.reserve( vertIndex.count() );
    switch ( type )
    {
      case ReosZValueModificationWidget::NewValue:
        for ( int i = 0; i < vertIndex.count(); ++i )
          newValues.append( value );
        break;
      case ReosZValueModificationWidget::Offset:
        for ( int i = 0; i < vertIndex.count(); ++i )
        {
          double oldVal = mMeshLayer->nativeMesh()->vertex( vertIndex.at( i ) ).z();
          newValues.append( oldVal + value );
        }
        break;
    }

    mKeepSelectionOnEdit = true;
    mMeshEditor->changeZValues( vertIndex, newValues );
  }

  w->deleteLater();
}

void ReosMapToolEditMeshFrame_p::canvasDoubleClickEvent( QgsMapMouseEvent *e )
{
  Q_UNUSED( e )
  //canvasReleseaseEvent() will be called just after the last click, so just flag the double clicks
  mDoubleClicks = true;

}

void ReosMapToolEditMeshFrame_p::keyPressEvent( QKeyEvent *e )
{
  if ( e->matches( QKeySequence::Undo ) )
    mMeshLayer->undoStack()->undo();

  if ( e->matches( QKeySequence::Redo ) )
    mMeshLayer->undoStack()->redo();

  switch ( mCurrentState )
  {
    case Digitizing:
    {
      if ( e->key() == Qt::Key_Escape )
      {
        clearSelection();
      }
    }
    break;

    case MovingSelection:
      if ( e->key() == Qt::Key_Escape )
      {
        mCurrentState = Digitizing;
        mMovingEdgesRubberband->reset( QgsWkbTypes::LineGeometry );
        mMovingFacesRubberband->reset( QgsWkbTypes::PolygonGeometry );
      }
      break;
    case Selecting:
      if ( e->key() == Qt::Key_Escape )
      {
        clearSelection();
      }
      break;
    case SelectingByPolygon:
      if ( e->key() == Qt::Key_Backspace )
      {
        mSelectionBand->removeLastPoint();
        if ( mSelectionBand->numberOfVertices() == 1 )
          mSelectionBand->reset( QgsWkbTypes::PolygonGeometry );
        mSelectionBand->movePoint( mCurrentPosition );
      }
      else if ( e->key() == Qt::Key_Escape )
        mSelectionBand->reset( QgsWkbTypes::PolygonGeometry );
      break;
  }

  ReosMapTool_p::keyPressEvent( e );
}

void ReosMapToolEditMeshFrame_p::addVertex( const QgsPointXY &mapPoint, const QgsPointLocator::Match &mapPointMatch )
{
  QgsTemporaryCursorOverride waitCursor( Qt::WaitCursor );
  double zValue;

  if ( mapPointMatch.isValid() )
  {
    QgsPoint layerPoint = mapPointMatch.interpolatedPoint( mCanvas->mapSettings().destinationCrs() );
    zValue = layerPoint.z();
  }
  else if ( mCurrentFaceIndex != -1 ) //we are on a face -->interpolate the z value
  {
    const QgsTriangularMesh &triangularMesh = *mMeshLayer->triangularMesh();
    int triangleFaceIndex = triangularMesh.faceIndexForPoint_v2( mapPoint );
    const QgsMeshFace &triangleFace = triangularMesh.triangles().at( triangleFaceIndex );
    const QgsMeshVertex &v1 = triangularMesh.vertices().at( triangleFace.at( 0 ) );
    const QgsMeshVertex &v2 = triangularMesh.vertices().at( triangleFace.at( 1 ) );
    const QgsMeshVertex &v3 = triangularMesh.vertices().at( triangleFace.at( 2 ) );
    zValue = QgsMeshLayerUtils::interpolateFromVerticesData( v1, v2, v3, v1.z(), v2.z(), v3.z(), mapPoint );
  }
  else if ( mCurrentEdge.first != -1 && mCurrentEdge.second  != -1 )
  {
    const QVector<int> &edge = edgeVertices( mCurrentEdge );
    const QgsMeshVertex v1 = mMeshLayer->triangularMesh()->vertices().at( edge.at( 0 ) );
    const QgsMeshVertex v2 = mMeshLayer->triangularMesh()->vertices().at( edge.at( 1 ) );

    double totDist = v1.distance( v2 );
    zValue = v1.z() + ( mapPoint.distance( QgsPointXY( v1 ) / totDist ) * ( v2.z() - v1.z() ) );
  }
  else
    zValue = currentZValue();

  QVector<QgsMeshVertex> points( 1, QgsMeshVertex( mapPoint.x(), mapPoint.y(), zValue ) );
  if ( mMeshEditor )
  {
    double tolerance = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );
    mMeshEditor->addVertices( points, tolerance );
  }
}

bool ReosMapToolEditMeshFrame_p::isSelectionGrapped( QgsPointXY &grappedPoint ) const
{
  if ( mCurrentVertexIndex != -1 && mSelectedVertices.contains( mCurrentVertexIndex ) )
  {
    grappedPoint = mapVertexXY( mCurrentVertexIndex );
    return true;
  }

  double tolerance = QgsTolerance::vertexSearchRadius( canvas()->mapSettings() );

  if ( mCurrentEdge.first != -1 && mCurrentEdge.second != -1  &&
       mSelectEdgeMarker->isVisible() &&
       grappedPoint.distance( mSelectEdgeMarker->center() ) < tolerance )
  {
    const QVector<int> vertices = edgeVertices( mCurrentEdge );
    if ( mSelectedVertices.contains( vertices.at( 0 ) ) && mSelectedVertices.contains( vertices.at( 1 ) ) )
    {
      const QgsPointXY &point1 = mapVertexXY( vertices.at( 0 ) );
      const QgsPointXY &point2 = mapVertexXY( vertices.at( 1 ) );
      grappedPoint =  QgsPointXY( point1.x() + point2.x(), point1.y() + point2.y() ) / 2;
      return true;
    }
  }

  if ( ( mSelectFaceMarker->isVisible() &&
         grappedPoint.distance( mSelectFaceMarker->center() ) < tolerance
         && mCurrentFaceIndex >= 0
         && mSelectedFaces.contains( mCurrentFaceIndex ) ) )
  {
    grappedPoint = mMeshLayer->triangularMesh()->faceCentroids().at( mCurrentFaceIndex );
    return true;
  }

  return false;
}

void ReosMapToolEditMeshFrame_p::clearSelection()
{
  mSelectedVertices.clear();
  mSelectedFaces.clear();
  mSelectedFacesRubberband->reset( QgsWkbTypes::PolygonGeometry );
  qDeleteAll( mSelectedVerticesMarker );
  mSelectedVerticesMarker.clear();
  prepareSelection();
}

void ReosMapToolEditMeshFrame_p::prepareSelection()
{
  if ( !mSelectedVertices.isEmpty() )
  {
    double vertexZValue = 0;
    for ( int i : mSelectedVertices.keys() )
      vertexZValue += mapVertex( i ).z();
    vertexZValue /= mSelectedVertices.count();
  }

  mConcernedFaceBySelection.clear();

  double xMin = std::numeric_limits<double>::max();
  double xMax = -std::numeric_limits<double>::max();
  double yMin = std::numeric_limits<double>::max();
  double yMax = -std::numeric_limits<double>::max();

  // search for moving edges and mesh fixed edges
  for ( QMap<int, SelectedVertexData>::iterator it = mSelectedVertices.begin(); it != mSelectedVertices.end(); ++it )
  {
    SelectedVertexData &vertexData = it.value();
    int vertexIndex = it.key();

    QgsPointXY vert = mapVertex( vertexIndex );
    if ( vert.x() < xMin )
      xMin = vert.x();
    if ( vert.x() > xMax )
      xMax = vert.x();
    if ( vert.y() < yMin )
      yMin = vert.y();
    if ( vert.y() > yMax )
      yMax = vert.y();

    vertexData.borderEdges.clear();
    vertexData.meshFixedEdges.clear();
    QgsMeshVertexCirculator circulator = mMeshEditor->vertexCirculator( vertexIndex );

    if ( !circulator.isValid() )
      continue;

    circulator.goBoundaryClockwise();
    int firstface = circulator.currentFaceIndex();
    do
    {
      int oppositeVertex = circulator.oppositeVertexClockwise();
      if ( mSelectedVertices.contains( oppositeVertex ) )
        vertexData.borderEdges.append( {circulator.currentFaceIndex(), oppositeVertex} );
      else
        vertexData.meshFixedEdges.append( {circulator.currentFaceIndex(), oppositeVertex} );

      mConcernedFaceBySelection.insert( circulator.currentFaceIndex() );
    }
    while ( circulator.turnCounterClockwise() != firstface && circulator.currentFaceIndex() != -1 );

    if ( circulator.currentFaceIndex() == -1 )
    {
      circulator.turnClockwise();
      int oppositeVertex = circulator.oppositeVertexCounterClockwise();
      if ( mSelectedVertices.contains( oppositeVertex ) )
        vertexData.borderEdges.append( {-1, oppositeVertex} );
      else
        vertexData.meshFixedEdges.append( {-1, oppositeVertex} );
    }
  }

  mSelectedMapExtent = QgsRectangle( xMin, yMin, xMax, yMax );

  // remove faces that have at least one vertex not selected
  mSelectedFaces = mConcernedFaceBySelection;
  for ( const int faceIndex : std::as_const( mConcernedFaceBySelection ) )
  {
    const QgsMeshFace &face = nativeFace( faceIndex );
    for ( const int vi : std::as_const( face ) )
      if ( !mSelectedVertices.contains( vi ) )
      {
        mSelectedFaces.remove( faceIndex );
        continue;
      }
  }

  bool removingVerticesAllowed = true;
  // here, we search for border edges that have associate face in the selection and remove it
  for ( QMap<int, SelectedVertexData>::iterator it = mSelectedVertices.begin(); it != mSelectedVertices.end(); ++it )
  {
    int vertexIndex = it.key();
    if ( removingVerticesAllowed && ( mReosMesh->vertexIsOnBoundary( vertexIndex ) || mReosMesh->vertexIsOnHoleBorder( vertexIndex ) ) )
      removingVerticesAllowed = false;

    SelectedVertexData &vertexData = it.value();
    int i = 0;
    while ( i < vertexData.borderEdges.count() )
    {
      int associateFace = vertexData.borderEdges.at( i ).first;
      if ( associateFace == -1 || mSelectedFaces.contains( associateFace ) )
        vertexData.borderEdges.removeAt( i );
      else
        i++;
    }
  }

  if ( !mSelectedFaces.isEmpty() )
  {
    const QList<int> faceList = qgis::setToList( mSelectedFaces );
    QgsGeometry faceGeometrie( new QgsPolygon( new QgsLineString( nativeFaceGeometry( faceList.at( 0 ) ) ) ) );
    if ( mSelectedFaces.count() == 1 )
    {
      mSelectedFacesRubberband->setToGeometry( faceGeometrie );
    }
    else
    {
      std::unique_ptr<QgsGeometryEngine> geomEngine( QgsGeometry::createGeometryEngine( faceGeometrie.constGet() ) );
      geomEngine->prepareGeometry();

      QVector<QgsGeometry> otherFaces( mSelectedFaces.count() );
      for ( int i = 0; i < faceList.count(); ++i )
        otherFaces[i] = QgsGeometry( new QgsPolygon( new QgsLineString( nativeFaceGeometry( faceList.at( i ) ) ) ) );
      QString error;
      const QgsGeometry allFaces( geomEngine->combine( otherFaces, &error ) );
      mSelectedFacesRubberband->setToGeometry( allFaces );
    }
  }
  else
    mSelectedFacesRubberband->reset( QgsWkbTypes::PolygonGeometry );

  if ( mSelectedVertices.count() == 1 )
  {
    mActionChangeZValue->setText( tr( "Change Z value of selected vertex" ) );
    mActionChangeZValue->setEnabled( true );
    if ( removingVerticesAllowed )
    {
      mActionRemoveVertices->setText( tr( "Remove selected vertex" ) );
      mActionRemoveVertices->setEnabled( true );
    }
    else
    {
      mActionRemoveVertices->setText( tr( "Selected vertex can't be removed", nullptr, mSelectedVertices.count() ) );
      mActionRemoveVertices->setEnabled( false );
    }

  }
  else if ( mSelectedVertices.count() > 1 )
  {
    mActionChangeZValue->setText( tr( "Change Z value of %n selected vertices", nullptr, mSelectedVertices.count() ) );
    mActionChangeZValue->setEnabled( true );
    if ( removingVerticesAllowed )
    {
      mActionRemoveVertices->setText( tr( "Remove %n selected vertices", nullptr, mSelectedVertices.count() ) );
      mActionRemoveVertices->setEnabled( true );
    }
    else
    {
      mActionRemoveVertices->setText( tr( "Some selected vertices can't be removed", nullptr, mSelectedVertices.count() ) );
      mActionRemoveVertices->setEnabled( false );
    }
  }
  else
  {
    mActionRemoveVertices->setText( tr( "None vertices selected" ) );
    mActionRemoveVertices->setEnabled( false );
    mActionChangeZValue->setEnabled( false );
  }
}

void ReosMapToolEditMeshFrame_p::select( const QgsPointXY &mapPoint, Qt::KeyboardModifiers modifiers, double tolerance )
{
  Qgis::SelectBehavior behavior;
  if ( modifiers & Qt::ShiftModifier )
    behavior = Qgis::SelectBehavior::AddToSelection;
  else if ( modifiers & Qt::ControlModifier )
    behavior = Qgis::SelectBehavior::RemoveFromSelection;
  else
    behavior = Qgis::SelectBehavior::SetSelection;

  QgsPointXY currentPoint = mapPoint;

  if ( mSelectFaceMarker->isVisible() &&
       mapPoint.distance( mSelectFaceMarker->center() ) < tolerance
       && mCurrentFaceIndex >= 0 )
  {
    setSelectedVertices( nativeFace( mCurrentFaceIndex ).toList(), behavior );
    currentPoint = mMeshLayer->triangularMesh()->faceCentroids().at( mCurrentFaceIndex );
  }
  else if ( mCurrentVertexIndex != -1 )
  {
    setSelectedVertices( QList<int>() << mCurrentVertexIndex, behavior );
    currentPoint = mMeshLayer->triangularMesh()->vertices().at( mCurrentVertexIndex );
  }
  else if ( mSelectEdgeMarker->isVisible() &&
            mapPoint.distance( mSelectEdgeMarker->center() ) < tolerance &&
            mCurrentEdge.first != -1 && mCurrentEdge.second != -1 )
  {
    QVector<int> edgeVert = edgeVertices( mCurrentEdge );
    setSelectedVertices( edgeVert.toList(), behavior );
    const QgsMeshVertex v1 = mMeshLayer->triangularMesh()->vertices().at( edgeVert.at( 0 ) );
    const QgsMeshVertex v2 = mMeshLayer->triangularMesh()->vertices().at( edgeVert.at( 1 ) );
    currentPoint = QgsPointXY( ( v1.x() + v2.x() ) / 2, ( v1.y() + v2.y() ) / 2 );
  }
  else
    setSelectedVertices( QList<int>(),  behavior );
}

void ReosMapToolEditMeshFrame_p::setSelectedVertices( const QList<int> newSelectedVertices, Qgis::SelectBehavior behavior )
{
  if ( mSelectedVertices.isEmpty() )
  {
    mUserZValue = currentZValue();
  }

  bool removeVertices = false;

  switch ( behavior )
  {
    case Qgis::SelectBehavior::SetSelection:
      clearSelection();
      break;
    case Qgis::SelectBehavior::AddToSelection:
      break;
    case Qgis::SelectBehavior::RemoveFromSelection:
      removeVertices = true;
      break;
    case Qgis::SelectBehavior::IntersectSelection:
      return;
      break;
  }

  for ( const int vertexIndex : newSelectedVertices )
  {
    bool contained = mSelectedVertices.contains( vertexIndex );
    if ( contained &&  removeVertices )
      removeFromSelection( vertexIndex );
    else if ( ! removeVertices && !contained )
      addNewSelectedVertex( vertexIndex );
  }

  prepareSelection();
}

void ReosMapToolEditMeshFrame_p::removeFromSelection( int vertexIndex )
{
  mSelectedVertices.remove( vertexIndex );
  delete mSelectedVerticesMarker.value( vertexIndex );
  mSelectedVerticesMarker.remove( vertexIndex );
}

void ReosMapToolEditMeshFrame_p::addNewSelectedVertex( int vertexIndex )
{
  mSelectedVertices.insert( vertexIndex, SelectedVertexData() );
  QgsVertexMarker *marker = new QgsVertexMarker( canvas() );
  marker->setIconType( QgsVertexMarker::ICON_CIRCLE );
  marker->setIconSize( QgsGuiUtils::scaleIconSize( 6 ) );
  marker->setPenWidth( QgsGuiUtils::scaleIconSize( 2 ) );
  marker->setColor( ReosStyleRegistery::instance()->blueReos( 200 ) );
  marker->setFillColor( ReosStyleRegistery::instance()->blueReos() );
  marker->setCenter( mapVertexXY( vertexIndex ) );
  marker->setZValue( 2 );
  mSelectedVerticesMarker[vertexIndex] = marker;
}

void ReosMapToolEditMeshFrame_p::selectByGeometry( const QgsGeometry &geometry, Qt::KeyboardModifiers modifiers )
{
  if ( mMeshLayer.isNull() || !mMeshLayer->triangularMesh() || mMeshEditor.isNull() )
    return;

  Qgis::SelectBehavior behavior;
  if ( modifiers & Qt::ShiftModifier )
    behavior = Qgis::SelectBehavior::AddToSelection;
  else if ( modifiers & Qt::ControlModifier )
    behavior = Qgis::SelectBehavior::RemoveFromSelection;
  else
    behavior = Qgis::SelectBehavior::SetSelection;

  if ( modifiers & Qt::AltModifier )
    selectContainedByGeometry( geometry, behavior );
  else
    selectTouchedByGeometry( geometry, behavior );
}

void ReosMapToolEditMeshFrame_p::selectContainedByGeometry( const QgsGeometry &geometry, Qgis::SelectBehavior behavior )
{
  QSet<int> selectedVertices;
  const QList<int> nativeFaceIndexes = mMeshLayer->triangularMesh()->nativeFaceIndexForRectangle( geometry.boundingBox() );

  std::unique_ptr<QgsGeometryEngine> engine( QgsGeometry::createGeometryEngine( geometry.constGet() ) );
  engine->prepareGeometry();
  for ( const int faceIndex : nativeFaceIndexes )
  {
    const QgsMeshFace &face = nativeFace( faceIndex );
    for ( const int vertexIndex : face )
    {
      const QgsMeshVertex &vertex = mapVertex( vertexIndex );
      if ( engine->contains( &vertex ) )
        selectedVertices.insert( vertexIndex );
    }
  }

  setSelectedVertices( selectedVertices.values(), behavior );
}

void ReosMapToolEditMeshFrame_p::selectTouchedByGeometry( const QgsGeometry &geometry, Qgis::SelectBehavior behavior )
{
  QSet<int> selectedVertices;
  const QList<int> nativeFaceIndexes = mMeshLayer->triangularMesh()->nativeFaceIndexForRectangle( geometry.boundingBox() );

  std::unique_ptr<QgsGeometryEngine> engine( QgsGeometry::createGeometryEngine( geometry.constGet() ) );
  engine->prepareGeometry();

  for ( const int faceIndex : nativeFaceIndexes )
  {
    const QgsMeshFace &face = nativeFace( faceIndex );
    std::unique_ptr<QgsPolygon> faceGeom( new QgsPolygon( new QgsLineString( nativeFaceGeometry( faceIndex ) ) ) );
    if ( engine->intersects( faceGeom.get() ) )
    {
      QSet<int> faceToAdd = qgis::listToSet( face.toList() );
      selectedVertices.unite( faceToAdd );
    }
  }

  setSelectedVertices( selectedVertices.values(), behavior );
}

void ReosMapToolEditMeshFrame_p::moveSelection( const QgsPointXY &destinationPoint )
{
  const QgsVector &translation = destinationPoint - mStartMovingPoint;
  mMovingEdgesRubberband->reset( QgsWkbTypes::LineGeometry );
  mMovingFacesRubberband->reset( QgsWkbTypes::PolygonGeometry );
  QgsGeometry movingFacesGeometry = mSelectedFacesRubberband->asGeometry();
  movingFacesGeometry.translate( translation.x(), translation.y() );
  mMovingFacesRubberband->setToGeometry( movingFacesGeometry );

  QSet<int> borderMovingFace;

  mIsMovingAllowed = true;

  for ( QMap<int, SelectedVertexData>::const_iterator it = mSelectedVertices.constBegin(); it != mSelectedVertices.constEnd(); ++it )
  {
    int vertexIndex = it.key();
    if ( mIsMovingAllowed && ( mReosMesh->vertexIsOnBoundary( vertexIndex ) || mReosMesh->vertexIsOnHoleBorder( vertexIndex ) ) )
      mIsMovingAllowed = false;
    const QgsPointXY &point1 = mapVertexXY( vertexIndex ) + translation;
    const SelectedVertexData &vertexData = it.value();
    for ( int i = 0; i < vertexData.meshFixedEdges.count(); ++i )
    {
      const QgsPointXY point2 = mapVertexXY( vertexData.meshFixedEdges.at( i ).second );
      QgsGeometry edge( new QgsLineString( {point1, point2} ) );
      mMovingEdgesRubberband->addGeometry( edge );
      int associateFace = vertexData.meshFixedEdges.at( i ).first;
      if ( associateFace != -1 )
        borderMovingFace.insert( associateFace );
    }

    for ( int i = 0; i < vertexData.borderEdges.count(); ++i )
    {
      const QgsPointXY point2 = mapVertexXY( vertexData.borderEdges.at( i ).second ) + translation;
      const QgsGeometry edge( new QgsLineString( {point1, point2} ) );
      mMovingEdgesRubberband->addGeometry( edge );
    }
  }

  const QgsMeshVertex &mapPointInNativeCoordinate =
    mMeshLayer->triangularMesh()->triangularToNativeCoordinates( QgsMeshVertex( destinationPoint.x(), destinationPoint.y() ) );
  const QgsMeshVertex &startingPointInNativeCoordinate =
    mMeshLayer->triangularMesh()->triangularToNativeCoordinates( QgsMeshVertex( mStartMovingPoint.x(), mStartMovingPoint.y() ) );
  const QgsVector &translationInLayerCoordinate = mapPointInNativeCoordinate - startingPointInNativeCoordinate;

  auto transformFunction = [translationInLayerCoordinate, this ]( int vi )-> const QgsMeshVertex
  {
    if ( mSelectedVertices.contains( vi ) )
      return mMeshLayer->nativeMesh()->vertex( vi ) + translationInLayerCoordinate;
    else
      return mMeshLayer->nativeMesh()->vertex( vi );
  };

  // we test only the faces that are deformed on the border, moving and not deformed faces are tested later
  if ( mIsMovingAllowed )
    mIsMovingAllowed = mMeshEditor->canBeTransformed( qgis::setToList( borderMovingFace ), transformFunction );

  if ( mIsMovingAllowed )
  {
    //to finish test if the polygons formed by the moving faces contains something else
    const QList<int> &faceIndexesIntersect = mMeshLayer->triangularMesh()->nativeFaceIndexForRectangle( movingFacesGeometry.boundingBox() );
    for ( const int faceIndex : faceIndexesIntersect )
    {
      if ( mConcernedFaceBySelection.contains( faceIndex ) )
        continue;
      const QgsGeometry otherFaceGeom( new QgsPolygon( new QgsLineString( nativeFaceGeometry( faceIndex ) ) ) );
      mIsMovingAllowed &= !movingFacesGeometry.intersects( otherFaceGeom );
      if ( !mIsMovingAllowed )
        break;
    }
  }

  setMovingRubberBandValidity( mIsMovingAllowed );
}

void ReosMapToolEditMeshFrame_p::setMovingRubberBandValidity( bool valid )
{
  if ( valid )
  {
    mMovingFacesRubberband->setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
    mMovingFacesRubberband->setStrokeColor( ReosStyleRegistery::instance()->blueReos() );
    mMovingEdgesRubberband->setColor( ReosStyleRegistery::instance()->blueReos() );
  }
  else
  {
    mMovingFacesRubberband->setFillColor( ReosStyleRegistery::instance()->invalidColor( 100 ) );
    mMovingFacesRubberband->setStrokeColor( ReosStyleRegistery::instance()->invalidColor() );
    mMovingEdgesRubberband->setColor( ReosStyleRegistery::instance()->invalidColor() );
  }
}

void ReosMapToolEditMeshFrame_p::updateSelectecVerticesMarker()
{
  qDeleteAll( mSelectedVerticesMarker );
  mSelectedVerticesMarker.clear();
  QColor color = ReosStyleRegistery::instance()->blueReos( 200 );
  for ( const int vertexIndex : mSelectedVertices.keys() )
  {
    QgsVertexMarker *marker = new QgsVertexMarker( canvas() );
    marker->setIconType( QgsVertexMarker::ICON_CIRCLE );
    marker->setIconSize( QgsGuiUtils::scaleIconSize( 6 ) );
    marker->setPenWidth( QgsGuiUtils::scaleIconSize( 2 ) );
    marker->setColor( color );
    marker->setFillColor( color );
    marker->setCenter( mapVertexXY( vertexIndex ) );
    marker->setZValue( 2 );
    mSelectedVerticesMarker[vertexIndex] = marker;
  }
}

void ReosMapToolEditMeshFrame_p::clearCanvasHelpers()
{
  mCurrentFaceIndex = -1;
  mCurrentVertexIndex = -1;
  mFaceRubberBand->reset();
  mFaceVerticesBand->reset();
  mVertexBand->reset();
  mSelectFaceMarker->setVisible( false );
  clearEdgeHelpers();
  updateSelectecVerticesMarker();
  prepareSelection();
}

void ReosMapToolEditMeshFrame_p::onModeChange()
{
  mSelectionBand->reset( QgsWkbTypes::PolygonGeometry );

  if ( mActionEditMesh->isChecked() )
  {
    mCurrentState = Digitizing;
  }
  else if ( mActionSelectElementByPolygon->isChecked() )
  {
    clearCanvasHelpers();
    mCurrentState = SelectingByPolygon;
  }
}

void ReosMapToolEditMeshFrame_p::onEdit()
{
  if ( !mKeepSelectionOnEdit )
    clearSelection();
  mKeepSelectionOnEdit = false;
  clearCanvasHelpers();
}

void ReosMapToolEditMeshFrame_p::clearEdgeHelpers()
{
  mCurrentEdge = {-1, -1};
  mEdgeBand->reset();
  mSelectEdgeMarker->setVisible( false );
  mFlipEdgeMarker->setVisible( false );
}
