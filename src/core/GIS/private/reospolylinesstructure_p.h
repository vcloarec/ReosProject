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
#include "reospolylinesstructure.h"


class ReosStructureVertexHandler_p : public ReosGeometryStructureVertex
{
  public:

    ReosStructureVertexHandler_p( QgsFeatureId fid, int pos );

    QPointF position( QgsVectorLayer *source, const QgsCoordinateTransform &transform );

    void linkFeature( QgsFeatureId fid, int pos );

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

};

class ReosPolylineStructureVectorLayer: public ReosPolylinesStructure
{
  public:

    ReosPolylineStructureVectorLayer( const QString &wktCrs );

    ReosPolylineStructureVectorLayer *clone() override;

    void addPolylines( const QPolygonF &polyline, const QString &sourceCrs = QString( ), const QString &id = QString() ) override;
    QPolygonF polyline( const QString &destinationCrs = QString(), const QString &id = QString() ) const override;

    virtual void setBoundary( const QPolygonF &polyline, const QString &sourceCrs = QString() ) override;
    virtual QPolygonF boundary( const QString &destinationCrs ) const override;

    QgsVectorLayer *data() override {return mVectorLayer.get();}

    virtual void removeAll() override;
    virtual void translate( const QPointF &translation, const QString &crs, const QString &id = QString() ) override {};
    virtual void moveVertex( int index, const ReosSpatialPosition &newPosition, const QString &id = QString() )override {};
    virtual void insertVertex( int index, const ReosSpatialPosition &point, const QString &id = QString() )override {};
    virtual void removeVertex( int index, const QString &id = QString() )override {}

    ReosMapExtent extent( const QString &destinationCrs ) const override;

    ReosGeometryStructureVertex *searchForVertex( const ReosMapExtent &zone ) const override;

    QPointF vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const override;

  private:
    ReosPolylineStructureVectorLayer() = default;
    std::unique_ptr<QgsVectorLayer> mVectorLayer;
    QHash<QString, QgsFeatureId> mReosToQgisId;
    QHash<QgsFeatureId, QString> mQgisToReos;

    bool hasPolyline( const QString &id );
    void removePolyline( const QString &id );

    QgsGeometry toGeometry( const QPolygonF &polyline, const QString &sourceCrs ) const;
    QPolygonF toPolygonF( const QgsGeometry &geom, const QString &destinationCrs ) const;

    typedef std::array<std::shared_ptr<ReosStructureVertexHandler_p>, 2> Segment;

    QMap<QgsFeatureId, Segment> mSegments;
    QList<std::shared_ptr<ReosStructureVertexHandler_p>> mBoundariesVertex;
    QgsFeatureId mBoundaryFeature;

};


#endif // REOSPOLYLINESSTRUCTURE_P_H
