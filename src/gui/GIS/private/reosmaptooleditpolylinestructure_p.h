/***************************************************************************
  reosmaptooleditpolylinestructure_p.h - ReosMapToolEditPolylineStructure_p

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
#ifndef REOSMAPTOOLEDITPOLYLINESTRUCTURE_P_H
#define REOSMAPTOOLEDITPOLYLINESTRUCTURE_P_H

#include "reosmaptool_p.h"

class ReosGeometryStructureVertex;
class ReosMapToolEditPolylineStructure_p;

class ReosEditPolylineStructureMenuPopulator: public ReosMenuPopulator
{
  public:
    ReosEditPolylineStructureMenuPopulator( ReosMapToolEditPolylineStructure_p *toolMap );

    bool populate( QMenu *menu, QgsMapMouseEvent *e = nullptr ) override;

  private:
    ReosPolylinesStructure *mStructure = nullptr;
    ReosMapToolEditPolylineStructure_p *mToolMap = nullptr;

  private:
    void populateVertexAction( ReosGeometryStructureVertex *vertex, QMenu *menu );
    void populateLineAction( QgsFeatureId id, const QPointF &point, QMenu *menu );
    void populateHoleAction( int holeIndex, QMenu *menu );


};

class ReosMapToolEditPolylineStructure_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosMapToolEditPolylineStructure_p( QgsMapCanvas *map );

    void setStructure( ReosPolylinesStructure *structure );

    Flags flags() const override;
    void activate() override;
    void deactivate() override;

    QActionGroup *mainActions() const;

  protected:
    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasPressEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;
    void keyPressEvent( QKeyEvent *e ) override;

  private slots:
    void insertVertex( const QPointF &mapPoint, qint64 lineId );
    void removeVertex( ReosGeometryStructureVertex *vertex );
    void removeLine( qint64 lineId );
    void resetTool();

  private:
    enum State
    {
      None,
      DraggingVertex,
      AddingLines,
      DraggingHolePoint
    };
    State mCurrentState = None;
    QPointF mCurrentPosition;

    ReosPolylinesStructure *mStructure = nullptr;
    QgsVertexMarker *mVertexMarker = nullptr;

    QString mMapCrs;
    ReosGeometryStructureVertex *mCurrentVertex = nullptr;
    QList<QPointF> mNeighborPosition;
    int mMovingHolePointIndex = -1;

    double tolerance() const;
    ReosMapExtent searchZone( const QgsPointXY &point ) const;

    QgsRubberBand *mLineRubberBand = nullptr;
    QgsRubberBand *mVertexRubberBand = nullptr;
    QgsRubberBand *mHoveredLineBand = nullptr;
    QgsRubberBand *mPolygonHoleBand = nullptr;
    void updateMovingVertexRubberBand( const QgsPointXY &movingPosition );
    void moveAddingLineRubberBand( const QgsPointXY &movingPosition );
    void stopDraggingVertex();
    void addVertexForNewLines( const QPointF &point );
    void stopAddingLines();

    QPolygonF mAddingPolyline;
    QList<double> mAddingLineTolerance;

    QActionGroup *mMainActions = nullptr;
    QAction *mActionAddLines = nullptr;
    QAction *mActionAddHole = nullptr;
    QAction *mActionMoveVertex = nullptr;

    QAction *mActionInsertVertex = nullptr;
    QAction *mActionRemoveVertex = nullptr;
    QAction *mActionRemoveLine = nullptr;
    QAction *mActionRemoveHole = nullptr;

    QAction *mActionUndo = nullptr;
    QAction *mActionRedo = nullptr;

    friend class ReosEditPolylineStructureMenuPopulator;
};

#endif // REOSMAPTOOLEDITPOLYLINESTRUCTURE_P_H
