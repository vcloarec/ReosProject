/***************************************************************************
  reosmapttooleditmeshframe_p.h - ReosMaptToolEditMeshFrame_p

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
#ifndef REOSMAPTOOLEDITMESHFRAME_P_H
#define REOSMAPTOOLEDITMESHFRAME_P_H

#include <qgsmeshdataprovider.h>

#include "reosmaptool_p.h"

class ReosMesh;
class QgsMeshEditor;

class ReosMapToolEditMeshFrame_p : public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolEditMeshFrame_p( ReosMesh *mesh, QgsMapCanvas *canvas );
    ~ReosMapToolEditMeshFrame_p();

    QActionGroup *mainActions() const;

    void activate() override;
    void deactivate() override;

    Flags flags() const override;

    bool populateContextMenuWithEvent( QMenu *menu, QgsMapMouseEvent *event ) override;

  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    void canvasDoubleClickEvent( QgsMapMouseEvent *e ) override;
    void keyPressEvent( QKeyEvent *e ) override;

  private slots:
    void clearCanvasHelpers();
    void onModeChange();
    void onEdit();
    void removeSelectedVerticesFromMesh();
    void changeZValue();

  private:
    enum State
    {
      Digitizing,
      Selecting,
      MovingSelection,
      SelectingByPolygon
    };
    typedef QPair<int, int> Edge; //first face index, second the vertex index corresponding to the end extremity (ccw)

    State mCurrentState = Digitizing;
    bool mKeepSelectionOnEdit = false;

    QPointer<ReosMesh> mReosMesh;
    QPointer<QgsMeshLayer> mMeshLayer;
    QPointer < QgsMeshEditor> mMeshEditor;
    QActionGroup *mMainActions = nullptr;
    QAction *mActionEditMesh = nullptr;
    QAction *mActionSelectElementByPolygon = nullptr;
    QAction *mActionUndo = nullptr;
    QAction *mActionRedo = nullptr;

    QAction *mActionRemoveVertices = nullptr;
    QAction *mActionChangeZValue = nullptr;

    int mCurrentFaceIndex = -1;
    int mCurrentVertexIndex = -1;
    Edge mCurrentEdge = {-1, -1};
    double mUserZValue = 0;

    void highLight( const QgsPointXY &mapPoint );
    void searchFace( const QgsPointXY &mapPoint );
    void highlightCurrentHoveredFace( const QgsPointXY &mapPoint );
    void highlightCloseEdge( const QgsPointXY &mapPoint );
    void highlightCloseVertex( const QgsPointXY &mapPoint );
    void searchEdge( const QgsPointXY &mapPoint );
    bool edgeCanBeInteractive( int vertexIndex1, int vertexIndex2 ) const;
    bool faceCanBeInteractive( int faceIndex ) const;

    int closeVertex( const QgsPointXY &mapPoint ) const;

    QgsPointSequence nativeFaceGeometry( int faceIndex ) const;
    const QgsMeshVertex mapVertex( int index ) const;
    const QgsPointXY mapVertexXY( int index ) const;
    QVector<QgsPointXY> edgeGeometry( const Edge &edge ) const;
    const QgsMeshFace nativeFace( int index ) const;
    QVector<int> edgeVertices( const Edge &edge ) const;

    QgsRubberBand *mFaceRubberBand = nullptr;
    QgsRubberBand *mFaceVerticesBand = nullptr;
    QgsRubberBand *mEdgeBand = nullptr;
    QgsRubberBand *mSelectionBand = nullptr;
    QgsRubberBand *mSelectedFacesRubberband = nullptr;
    QgsRubberBand *mVertexBand = nullptr;
    QgsRubberBand *mMovingEdgesRubberband = nullptr;
    QgsRubberBand *mMovingFacesRubberband = nullptr;
    bool mIsMovingAllowed = false;

    QgsVertexMarker *mSelectFaceMarker = nullptr;
    QgsVertexMarker *mFlipEdgeMarker = nullptr;
    QgsVertexMarker *mSelectEdgeMarker = nullptr;
    QMap< int, QgsVertexMarker * > mSelectedVerticesMarker;

    QPoint mStartSelectionPos;
    QgsPointXY mStartMovingPoint;
    QgsPointXY mCurrentPosition;
    bool mLeftButtonPressed = false;
    bool mDoubleClicks = false;

    double currentZValue() {return 0;}
    void addVertex( const QgsPointXY &mapPoint, const QgsPointLocator::Match &mapPointMatch );

    struct SelectedVertexData
    {
      //Here edges are the indexes of the face where the following vertices (ccw) is the other extremity of the edge
      QList<Edge> meshFixedEdges; // that have one extremity not on the selection
      QList<Edge> borderEdges; // that are on the border of the selection
    };

    QMap<int, SelectedVertexData> mSelectedVertices;
    QSet<int> mSelectedFaces;
    QSet<int> mConcernedFaceBySelection;
    QgsRectangle mSelectedMapExtent;

    bool isSelectionGrapped( QgsPointXY &grappedPoint ) const;
    void clearSelection();
    void prepareSelection();
    void select( const QgsPointXY &mapPoint, Qt::KeyboardModifiers modifiers, double tolerance );
    void setSelectedVertices( const QList<int> newSelectedVertices, Qgis::SelectBehavior behavior );
    void removeFromSelection( int vertexIndex );
    void addNewSelectedVertex( int vertexIndex );
    void selectByGeometry( const QgsGeometry &geometry, Qt::KeyboardModifiers modifiers );
    void selectContainedByGeometry( const QgsGeometry &geometry, Qgis::SelectBehavior behavior );
    void selectTouchedByGeometry( const QgsGeometry &geometry, Qgis::SelectBehavior behavior );
    void moveSelection( const QgsPointXY &destinationPoint );
    void setMovingRubberBandValidity( bool valid );

    void updateSelectecVerticesMarker();
    void clearEdgeHelpers();

    void startMeshEditing();

};

#endif // REOSMAPTOOLEDITMESHFRAME_P_H
