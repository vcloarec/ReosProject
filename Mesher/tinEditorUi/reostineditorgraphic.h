/***************************************************************************
                      reostineditorgraphic.h
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDTINEDITORGRAPHIC_H
#define HDTINEDITORGRAPHIC_H

#include "reosvertexzspecifiereditorwidget.h"
#include "hdtineditoruidialog.h"

#include "reosmapmeshitem.h"
#include "../../GIS/reosmaptool.h"
#include "../../Reos/reosmodule.h"


class QgsMultiLineString;

class ReosGisManager;
class ReosTinEditorUi;
class ReosTinUndoCommandNewVertex;
class ReosTinUndoCommandNewSegmentWithNewSecondVertex;
class ReosTinUndoCommandNewSegmentWithExistingSecondVertex;
class ReosTinUndoCommandRemoveVertex;
class ReosTinUndoCommandRemoveHardLine;
class ReosTinUndoCommandFlipFaces;


struct ReosMeshItemVertexAndNeighbours
{
  ReosMeshItemVertex *vertex = nullptr;
  QList<ReosMeshItemVertexAndNeighbours> neighbours;
};

struct PointAndNeighbours
{
  PointAndNeighbours() {}
  PointAndNeighbours( const ReosMeshItemVertexAndNeighbours &graphicsItems )
  {
    double x = graphicsItems.vertex->realWorldVertex()->x();
    double y = graphicsItems.vertex->realWorldVertex()->y();
    point = QPointF( x, y );
    for ( auto nv : graphicsItems.neighbours )
    {
      neighbours.append( PointAndNeighbours( nv ) );
    }
  }

  QPointF point;
  QList<PointAndNeighbours> neighbours;
};

class ReosTinMapToolHardLineSegement: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosTinMapToolHardLineSegement( ReosMap *map, ReosTinEditorUi *uiEditor );


    void suspend() override;
    void unsuspend() override;

    void vertexHasToBeRemoved( ReosMeshItemVertex *vert );

  public slots:
    void reset() override;

  protected:
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;

  private:
    ReosMeshItemVertex *firstVertex = nullptr;
    QPointF firstPoint;
    ReosTinEditorUi *mUiEditor;
    QgsRubberBand *rubberBand;
    bool showWhenMoving = false;
};

class ReosTinMapToolFlipFaces: public ReosMapTool
{
    Q_OBJECT
  public:
    ReosTinMapToolFlipFaces( ReosMap *map, ReosTinEditorUi *uiEditor ): ReosMapTool( map ),
      mUiEditor( uiEditor )
    {}
    virtual ~ReosTinMapToolFlipFaces() override {}

  protected:
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;

  private:
    ReosTinEditorUi *mUiEditor;
    FacePointer firstFace = nullptr;
    ReosMeshItemFace *currentFaceItem = nullptr;
    ReosMeshItemFace *firstFaceItem = nullptr;
    QList<QPointF> selectedFace( QPointF mapPoint );

    const QColor firstFaceColor = QColor( 150, 150, 0, 150 );
    const QColor currentFaceColor = QColor( 0, 150, 0, 150 );
    const QColor wrongFaceColor = QColor( 150, 0, 0, 150 );

    bool faceCoupleIsValid( FacePointer f1, FacePointer f2 );

    // ReosMapTool interface
  public slots:
    void reset() override;
};

class UIMeshEditorTesting;

class ReosTinEditorUi : public ReosModule
{
    Q_OBJECT
  public:
    explicit ReosTinEditorUi( ReosGisManager *gismanager, QObject *parent = nullptr );

    //! Returns the graphic domain that represents the vertives and faces
    ReosMapMeshEditorItemDomain *domain() const {return mDomain;}

    //! Sets the mesh layer
    void setMeshLayer( QgsMeshLayer *meshLayer );

    //! Returns the item representing a vertex localized at map point
    ReosMeshItemVertex *mapVertex( const QPointF &mapPoint ) const;

    //! Returns the vertex localized at map point
    VertexPointer realWorldVertex( const QPointF &mapPoint ) const;

    //! Returns the three map points of the face located at the map point, void list if no face
    QList<QPointF> mapFace( const QPointF &mapPoint ) const;
    //! Returns the face located at the map point
    FacePointer realWorldFace( const QPointF &mapPoint ) const;

    //! Returns true is the two faces are flippable
    bool isFlipable( FacePointer f1, FacePointer f2 ) const;

    //! Do and Undo commands
    void doCommand( ReosTinUndoCommandNewVertex *command );
    void undoCommand( ReosTinUndoCommandNewVertex *command );
    void doCommand( ReosTinUndoCommandNewSegmentWithNewSecondVertex *command );
    void undoCommand( ReosTinUndoCommandNewSegmentWithNewSecondVertex *command );
    void doCommand( ReosTinUndoCommandNewSegmentWithExistingSecondVertex *command );
    void undoCommand( ReosTinUndoCommandNewSegmentWithExistingSecondVertex * );
    void doCommand( ReosTinUndoCommandRemoveVertex *command );
    void undoCommand( ReosTinUndoCommandRemoveVertex * );
    void doCommand( ReosTinUndoCommandRemoveHardLine *command );
    void undoCommand( ReosTinUndoCommandRemoveHardLine * ) {}
    void doCommand( ReosTinUndoCommandFlipFaces *command );
    void undoCommand( ReosTinUndoCommandFlipFaces *command );

    void newTin( const QString &fileName, const QString &name, const QgsCoordinateReferenceSystem &crs );

  public slots:
    void startNewVertex();
    void newVertex( const QPointF &mapPoint );
    void stopNewVertex();

    void startRemoveVertex();
    void removeVertexFromRect( const QRectF &selectionZone );

    void startNewHardLineSegment();
    void newSegment( ReosMeshItemVertex *firstVertx, const QPointF &secondMapPoint );
    void newSegment( ReosMeshItemVertex *firstVertex, ReosMeshItemVertex *secondVertex );
    void stopNewHardLineSegment();

    void startRemoveSegment();
    void removeSegmentFromRect( const QRectF &selectionZone );

    void startZSpecifierEditor();
    void selectVertexForZSpecifierEditor( const QRectF &zone );

    void startFlipFaces();
    void flipFaces( FacePointer f1, FacePointer f2 );

    void populateDomain();

    bool saveTin();

    bool openTin();
    bool openTinWithFileName( QString fileName );

    void meshHasBeenChanged();

  private slots :

    void currentLayerChanged( QgsMapLayer *layer );
    void layerHasToBeUpdated( QgsMapLayer *layer );
    void layerHasToBeRemoved( QgsMapLayer *layer );
    void mapCrsChanged( const QgsCoordinateReferenceSystem &crs );
    void stopZSpecifierEditor();

    void vectorLayerToTin();

  private: //method

    QWidget *getWidget() const override {return uiDialog;}


    void updateMesh();
    void enableEditAction( bool enable );
    void newTinLayer();

    void widgetClosed();

    void startVertexEntry();
    void stopVertexEntry();

    //! Returns the mesh coordinate in map coordinates, ok is true if the transform doesn't fail
    QPointF mapCoordinatesToMesh( const QPointF &meshCoordinate, bool &ok ) const;
    //! Returns the map coordinate in mesh coordinates, ok is true if the transform doesn't fail
    QPointF meshCoordinatesFromMap( const QPointF &mapCoordinate, bool &ok ) const;
    //! Return the transformed coordinate, ok is true if the transform doesn't fail
    QPointF transformCoordinates( const QPointF &coordinate, bool &ok, const QgsCoordinateTransform &transform ) const;

    //! Adds a vertex item ossociated with the vertex to the map, the map location is retrieve using transform (can fail)
    ReosMeshItemVertex *addMapVertex( VertexPointer vert );
    //! Adds a vertex item ossociated with the vertex to the map at mapPoint
    ReosMeshItemVertex *addMapVertex( const QPointF &mapPoint, VertexPointer vert );

    //! Remove the vertex
    void removeVertex( VertexPointer vert );

    //! add a vertex to the TIN with a simple Z specifier (with z value)
    VertexPointer addRealWorldVertexFromMap( const QPointF &mapPoint, double z );

    //! add a vertex to the TIN with a the Z specifier specified in the widget
    VertexPointer addRealWorldVertexFromMap( const QPointF &mapPoint );

    //! add a hardline segment between the two vertices, returns the neighbours structure of the news vertices (used for undo)
    void addSegment( VertexPointer v1, VertexPointer v2, QList<PointAndNeighbours> &oldNeigboursStructure );

    //! remove the hardline segment between the two vertices
    void removeHardLine( VertexPointer v1, VertexPointer v2 );

    //! Adds element to the TIN from a vector layer (points or lines)
    void addVectorLayer( QgsVectorLayer *vectorLayer );
    //! Adds vertex to the TIN from a point vector layer
    void addPointVectorLayer( QgsVectorLayer *vectorLayer, const QgsCoordinateTransformContext &transformContext );
    //! Adds multipoint to the TIN using the transform and returns the count of point effectivly added
    int addMultiPointGeometry( QgsMultiPoint *multipoint, const QgsCoordinateTransform &transform = QgsCoordinateTransform() );
    //! Adds point to the TIN using the transform and returns the count of point effectivly added
    bool addPointGeometry( QgsPoint *point, const QgsCoordinateTransform &transform = QgsCoordinateTransform() );

    void addLineVectorLayer( QgsVectorLayer *vectorLayer, const QgsCoordinateTransformContext &transformContext );
    int addMultiCurveGeometry( QgsMultiLineString *multiLine, const QgsCoordinateTransform &transform = QgsCoordinateTransform() );
    int addCurveGeometry( QgsLineString *line, const QgsCoordinateTransform &transform = QgsCoordinateTransform() );


    ReosMeshItemVertexAndNeighbours saveStructure( ReosMeshItemVertex *vert ) const;

    void restoreStructure( VertexPointer, const PointAndNeighbours ) const;

    void updateGraphics( VertexPointer realWorldVertex );

    void updateMeshLayer();

    bool writeToFile( QString fileName );

    void newCommand( QUndoCommand *command ) override;

    void vertexHasToBeRemoved( VertexPointer vert );


  private: //attributes
    ReosMapMeshEditorItemDomain *mDomain;
    ReosGisManager *mGisManager;
    ReosMap *mMap;
    HdTinEditorUiDialog *uiDialog;
    ReosVertexZSpecifierWidget *mZSpecifierWidget;
    ReosVertexZSpecifierEditorWidget *mZSpecifierEditor;
    QgsCoordinateTransform mTransform;

    QgsMeshLayer *mMeshLayer = nullptr;
    //TINEditor *mTIN=nullptr;
    ReosTin *mTIN = nullptr;

    QHash<ReosTin *, QUndoStack *> mUndoStacks;

    QAction *actionNewTinLayer;
    QAction *actionOpenTinLayer;
    QAction *actionSaveTinLayer;

    QList<QAction *> actionEditList;
    QAction *actionNewVertex;
    ReosMapToolClickPoint *mapToolNewVertex;
    QAction *actionRemoveVertex;
    ReosMapToolSelection *mapToolRemoveVertex;
    QAction *actionNewHardLineSegment;
    ReosTinMapToolHardLineSegement *mapToolHardLineSegment;
    QAction *actionRemoveSegment;
    ReosMapToolSelection *mapToolRemoveSegment;

    QAction *actionZSpecifierEditor;
    ReosMapToolSelection *mapToolZSpecifierEditor;

    QAction *actionFlipFaces;
    ReosTinMapToolFlipFaces *mapToolFlipFaces;

    QAction *actionTriangulateTIN;

    QAction *actionVectorLayerToTin;

    friend class UIMeshEditorTesting;
};



class ReosTinUndoCommandNewVertex: public QUndoCommand
{
  public:
    void undo() override
    {
      mEditor->undoCommand( this );
    }
    void redo() override
    {
      mEditor->doCommand( this );
    }

  private:
    ReosTinUndoCommandNewVertex( ReosTinEditorUi *editor, QPointF mapPoint ): mEditor( editor ), mMapPoint( mapPoint )
    {
      setText( QObject::tr( "New Vertex" ) );
    }

    ReosTinEditorUi *mEditor;
    QPointF mMapPoint;
    friend class ReosTinEditorUi;
};


class ReosTinUndoCommandNewSegmentWithNewSecondVertex: public QUndoCommand
{

  public:
    void undo() override
    {
      mEditor->undoCommand( this );
    }
    void redo() override
    {
      mEditor->doCommand( this );
    }

  private:
    ReosTinUndoCommandNewSegmentWithNewSecondVertex( ReosTinEditorUi *editor,
        QPointF mapPointFirstVertex,
        QPointF mapPointSecondVertex ):
      mEditor( editor ), mMapPointFirst( mapPointFirstVertex ), mMapPointSecond( mapPointSecondVertex )
    {

      setText( QObject::tr( "Nouveaux point et segment" ) );
    }

    ReosTinEditorUi *mEditor;
    QPointF mMapPointFirst;
    QPointF mMapPointSecond;
    QList<PointAndNeighbours> mVerticesPositionAndStructureMemory; //store the extremity's position and the position of new intersection vertices but also the old structure of their neighbours

    friend class ReosTinEditorUi;
};

class ReosTinUndoCommandNewSegmentWithExistingSecondVertex: public QUndoCommand
{

  public:
    void undo() override
    {
      mEditor->undoCommand( this );
    }
    void redo() override
    {
      mEditor->doCommand( this );
    }

  private:
    ReosTinUndoCommandNewSegmentWithExistingSecondVertex( ReosTinEditorUi *editor,
        QPointF mapPointFirstVertex,
        QPointF mapPointSecondVertex ):
      mEditor( editor ), mMapPointFirst( mapPointFirstVertex ), mMapPointSecond( mapPointSecondVertex )
    {
      setText( QObject::tr( "Nouveau segment" ) );
    }

    ReosTinEditorUi *mEditor;
    QPointF mMapPointFirst;
    QPointF mMapPointSecond;
    QList<PointAndNeighbours> mVerticesPositionAndStructureMemory;

    friend class ReosTinEditorUi;
};

class ReosTinUndoCommandRemoveVertex : public QUndoCommand
{
  public:
    void undo() override
    {
      mEditor->undoCommand( this );
    }
    void redo() override
    {
      mEditor->doCommand( this );
    }

  private:
    ReosTinUndoCommandRemoveVertex( ReosTinEditorUi *editor,
                                    const QPointF &mapPoint ):
      mEditor( editor ), mMapPoint( mapPoint ) {}

    ReosTinEditorUi *mEditor;
    QPointF mMapPoint;

    friend class ReosTinEditorUi;
};

class ReosTinUndoCommandRemoveHardLine: public QUndoCommand
{
  public:
    void undo() override
    {
      mEditor->undoCommand( this );
    }
    void redo() override
    {
      mEditor->doCommand( this );
    }

  private:
    ReosTinUndoCommandRemoveHardLine( ReosTinEditorUi *editor, const QPointF &mapPoint1, const QPointF &mapPoint2 ):
      mEditor( editor ), mMapPointForVertex1( mapPoint1 ), mMapPointForVertex2( mapPoint2 )
    {}

    ReosTinEditorUi *mEditor;
    QPointF mMapPointForVertex1;
    QPointF mMapPointForVertex2;

    friend class ReosTinEditorUi;

};


class ReosTinUndoCommandFlipFaces: public QUndoCommand
{
  public:
    void undo() override
    {
      mEditor->undoCommand( this );
    }
    void redo() override
    {
      mEditor->doCommand( this );
    }

  private:
    ReosTinUndoCommandFlipFaces( FacePointer face1, FacePointer face2, ReosTinEditorUi *editor ): mEditor( editor )
    {
      setFacesCentroid( face1, face2 );
    }
    ReosTinEditorUi *mEditor;
    QPointF mFaceCentroidToFlip_1;
    QPointF mFaceCentroidToFlip_2;

    void setFacesCentroid( FacePointer face1, FacePointer face2 )
    {
      auto xy1 = face1->faceCentroid();
      auto xy2 = face2->faceCentroid();

      mFaceCentroidToFlip_1 = QPointF( xy1[0], xy1[1] );
      mFaceCentroidToFlip_2 = QPointF( xy2[0], xy2[1] );
    }


    friend class ReosTinEditorUi;

};

#endif // HDTINEDITORGRAPHIC_H
