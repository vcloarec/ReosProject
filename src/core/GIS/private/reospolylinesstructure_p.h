/***************************************************************************
  reospolylinesstructure_p.h - ReosPolylinesStructure_p

 ---------------------
 begin                : 10.1.2022
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
#ifndef REOSPOLYLINESSTRUCTURE_P_H
#define REOSPOLYLINESSTRUCTURE_P_H

#include <qgsvectorlayer.h>
#include <qgscoordinatetransform.h>

#include "reospolylinesstructure.h"

class ReosPolylineStructureVectorLayerUndoCommandRemoveLine;
class ReosPolylineStructureVectorLayerUndoCommandAddLine;


class ReosStructureVertexHandler_p : public ReosGeometryStructureVertex
{
  public:

    ReosStructureVertexHandler_p( QgsVectorLayer *source, QgsFeatureId fid, int pos );

    QPointF position( const QgsCoordinateTransform &transform = QgsCoordinateTransform() );

    void attachLine( QgsFeatureId fid, int pos );
    void detachLine( QgsFeatureId fid );
    QgsFeatureIds attachedLines() const;
    bool hasLineAttached() const;
    bool oneOtherLine( QgsFeatureId id, QgsFeatureId *otherLine );

    void move( const QgsPointXY &newPosition );

  private:

    struct PositionInFeature
    {
      PositionInFeature() = default;

      //! Contructs a generic position, that is, this instance is equal to all position related to line with \a id
      PositionInFeature( QgsFeatureId id ): fid( id ) {}

      //! Contructs a position on line with \a id at position \a p on the line
      PositionInFeature( QgsFeatureId id, int p ): fid( id ), pos( p ) {}

      bool operator==( const PositionInFeature &other ) const
      {
        if ( other.pos == -1 )
        {
          return fid == other.fid;
        }
        return fid == other.fid && pos == other.pos;
      }

      QgsFeatureId fid;
      int pos = -1;
    };

    QList<PositionInFeature> mLinkedFeatures;

    QgsVectorLayer *mSource = nullptr;
};

class ReosPolylineStructureVectorLayer: public ReosPolylinesStructure
{
    Q_OBJECT
  public:

    ReosPolylineStructureVectorLayer( const QString &wktCrs );
    ReosPolylineStructureVectorLayer( const QPolygonF &boundary, const QString &wktCrs );

    void addPolylines( const QPolygonF &polyline, const QString &sourceCrs = QString( ), const QString &id = QString() ) override;
    QPolygonF polyline( const QString &destinationCrs = QString(), const QString &id = QString() ) const override;

    QPolygonF boundary( const QString &destinationCrs ) const override;

    QgsVectorLayer *data() override {return mVectorLayer.get();}

    void removeAll() override;
    void translate( const QPointF &translation, const QString &crs, const QString &id = QString() ) override {};

    bool vertexCanBeMoved( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition ) const override;
    void moveVertex( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition ) override;
    void insertVertex( const ReosSpatialPosition &point, qint64 lineId ) override;;
    void removeVertex( int index, const QString &id = QString() )override {}
    ReosMapExtent extent( const QString &destinationCrs ) const override;
    ReosGeometryStructureVertex *searchForVertex( const ReosMapExtent &zone ) const override;
    bool searchForLine( const ReosMapExtent &zone, qint64 &id ) const override;
    QPointF vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const override;
    QList<QPointF> neighborsPositions( ReosGeometryStructureVertex *vertex, const QString &crs ) const override;

    QUndoStack *undoStack() const;

    void setTolerance( double tolerance, const QString &wktCrs = QString() );

  private:
    typedef QgsFeatureId SegmentId;
    typedef ReosStructureVertexHandler_p *VertexP;
    typedef std::shared_ptr<ReosStructureVertexHandler_p> VertexH;
    typedef std::array<VertexH, 2> Segment;

    ReosPolylineStructureVectorLayer() = default;

    // Data member defining the structure ********
    std::unique_ptr<QgsVectorLayer> mVectorLayer;
    QMap<QgsFeatureId, Segment> mSegments;
    QList<VertexP> mBoundariesVertex;
    // *******************************************
    double mTolerance = 0.01;

    VertexH createVertex( QgsFeatureId id, int positionInFeature );
    const QgsCoordinateTransform toLayerTransform( const QString &crs ) const;
    const QgsCoordinateTransform toDestinationTransform( const QString &destinationCrs ) const;

    QgsFeatureIterator closeLines( const ReosMapExtent &zone, QgsRectangle &rect ) const;
    QList<ReosStructureVertexHandler_p *> neighorsVertices( ReosGeometryStructureVertex *vertex, QgsFeatureIds &fids ) const;

    QgsPointXY toLayerCoordinates( const QPointF &position, const QgsCoordinateTransform &transform ) const;
    QgsPointXY toLayerCoordinates( const ReosSpatialPosition &position ) const;
    Segment idToSegment( SegmentId id ) const;
    VertexH idToVertex( SegmentId id, int pos );
    bool idToLinkedSegment( ReosPolylineStructureVectorLayer::SegmentId id, int pos, ReosPolylineStructureVectorLayer::SegmentId *linkedSeg );

    VertexP oppositeVertex( VertexP other, SegmentId sid ) const;

    friend class ReosPolylineStructureVectorLayerUndoCommandRemoveLine;
    friend class ReosPolylineStructureVectorLayerUndoCommandAddLine;
};



class ReosPolylineStructureVectorLayerUndoCommandRemoveLine : public QUndoCommand
{
  public:
    ReosPolylineStructureVectorLayerUndoCommandRemoveLine( QgsFeatureId id,  ReosPolylineStructureVectorLayer *structure );
    void redo() override;
    void undo() override;

  private:
    QgsFeatureId mId;
    ReosPolylineStructureVectorLayer *mStructure = nullptr;
    ReosPolylineStructureVectorLayer::Segment mSeg;

};

class ReosPolylineStructureVectorLayerUndoCommandAddLine : public QUndoCommand
{
  public:
    ReosPolylineStructureVectorLayerUndoCommandAddLine(
      QgsFeatureId idLineToAdd,
      QgsFeatureId idExisting0,
      QgsFeatureId idExisting1,
      bool onBoundary,
      ReosPolylineStructureVectorLayer *structure );

    void redo() override;
    void undo() override;

  private:
    QgsFeatureId mIdToAdd;
    QgsFeatureId mId0;
    QgsFeatureId mId1;
    bool mOnBoundary = false;
    ReosPolylineStructureVectorLayer *mStructure = nullptr;
};


#endif // REOSPOLYLINESSTRUCTURE_P_H
