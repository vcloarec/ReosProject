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


class ReosStructureVertexHandler_p : public ReosGeometryStructureVertex
{
  public:

    ReosStructureVertexHandler_p( QgsVectorLayer *source, QgsFeatureId fid, int pos );

    QPointF position( const QgsCoordinateTransform &transform = QgsCoordinateTransform() );

    void linkFeature( QgsFeatureId fid, int pos );

    void move( const QgsPointXY &newPosition );

    QgsFeatureIds linkedFeatures() const;

  private:

    struct PositionInFeature
    {
      PositionInFeature() = default;

      bool operator==( const PositionInFeature &other ) const
      {
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
    void insertVertex( int index, const ReosSpatialPosition &point, const QString &id = QString() )override {};
    void removeVertex( int index, const QString &id = QString() )override {}
    ReosMapExtent extent( const QString &destinationCrs ) const override;
    ReosGeometryStructureVertex *searchForVertex( const ReosMapExtent &zone ) const override;
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

    // Data member defining the structure
    std::unique_ptr<QgsVectorLayer> mVectorLayer;
    QMap<QgsFeatureId, Segment> mSegments;
    QList<VertexH> mBoundariesVertex;
    double mTolerance = 0.01;

    VertexH createVertex( QgsFeatureId id, int positionInFeature );
    const QgsCoordinateTransform toLayerTransform( const QString &crs ) const;
    const QgsCoordinateTransform toDestinationTransform( const QString &destinationCrs ) const;

    QList<ReosStructureVertexHandler_p *> neighorsVertices( ReosGeometryStructureVertex *vertex, QgsFeatureIds &fids ) const;

    QgsPointXY toLayerCoordinates( const QPointF &position, const QgsCoordinateTransform &transform ) const;
    QgsPointXY toLayerCoordinates( const ReosSpatialPosition &position ) const;
    Segment idToSegment( QgsFeatureId id ) const;

    VertexP oppositeVertex( VertexP other, SegmentId sid ) const;
};


#endif // REOSPOLYLINESSTRUCTURE_P_H
