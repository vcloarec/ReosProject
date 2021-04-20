/***************************************************************************
  reostriangularirregularnetworkqgsdualedge.h - ReosTriangularIrregularNetworkQgsDualEdge

 ---------------------
 begin                : 12.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSTRIANGULARIRREGULARNETWORKQGSDUALEDGE_P_H
#define REOSTRIANGULARIRREGULARNETWORKQGSDUALEDGE_P_H

#include "reostriangularirregularnetwork.h"
#include "qgsmeshdataprovider.h"

class QgsDualEdgeTriangulation;


class ReosTriangularIrregularNetworkQgsDualEdge_p : public ReosTriangularIrregularNetwork
{
  public:
    ReosTriangularIrregularNetworkQgsDualEdge_p( QObject *parent = nullptr );
    ~ReosTriangularIrregularNetworkQgsDualEdge_p();

    double minimumElevation() const override;
    double maximumElevation() const override;
    ReosMapExtent extent() const override;

    QgsRectangle qgsExtent() const;

    bool addVertex( const Vertex &vert ) override;
    bool removeVertex( int vertexIndex ) override;
    void addConstraintLine( const QVector<Vertex> &vertices ) override;
    int vertexCount() const override;
    QPointF vertexXY( int index ) const override;
    int triangleCount() const override;

    //! Returns the QGIS mesh
    QgsMesh triangulatedMesh() const;

  private:
    std::unique_ptr<QgsDualEdgeTriangulation> mTriangulation;
    mutable bool mDirty = true;
    mutable QgsMesh mMesh;
    mutable double mMinimumElevation = std::numeric_limits<double>::quiet_NaN();
    mutable double mMaximumElevation = std::numeric_limits<double>::quiet_NaN();
    mutable QgsRectangle mExtent;

    void updateMesh() const;

};

#endif // REOSTRIANGULARIRREGULARNETWORKQGSDUALEDGE_P_H
