/***************************************************************************
  reostriangularirregularnetworkqgsdualedge.cpp - ReosTriangularIrregularNetworkQgsDualEdge

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
#include "reostriangularirregularnetworkqgsdualedge_p.h"

#include <qgsdualedgetriangulation.h>

ReosTriangularIrregularNetworkQgsDualEdge_p::ReosTriangularIrregularNetworkQgsDualEdge_p( QObject *parent ): ReosTriangularIrregularNetwork( parent )
{
  mTriangulation = std::make_unique<QgsDualEdgeTriangulation>();
}

double ReosTriangularIrregularNetworkQgsDualEdge_p::minimumElevation() const
{
  updateMesh();
  return mMinimumElevation;
}

double ReosTriangularIrregularNetworkQgsDualEdge_p::maximumElevation() const
{
  updateMesh();
  return mMaximumElevation;
}

ReosMapExtent ReosTriangularIrregularNetworkQgsDualEdge_p::extent() const
{
  return ReosMapExtent( mExtent.toRectF() );
}

QgsRectangle ReosTriangularIrregularNetworkQgsDualEdge_p::qgsExtent() const
{
  updateMesh();
  return mExtent;
}

ReosTriangularIrregularNetworkQgsDualEdge_p::~ReosTriangularIrregularNetworkQgsDualEdge_p() = default;

bool ReosTriangularIrregularNetworkQgsDualEdge_p::addVertex( const Vertex &vert )
{
  QgsPoint point( vert.x, vert.y, vert.z );
  int pointCountBefore = mTriangulation->pointsCount();
  bool pointAdded = mTriangulation->addPoint( point ) >= 0 && pointCountBefore < mTriangulation->pointsCount();
  if ( pointAdded )
    mDirty = true;

  if ( autoUpdate() )
  {
    updateMesh();
    emit updated();
  }

  return pointAdded;
}

bool ReosTriangularIrregularNetworkQgsDualEdge_p::removeVertex( int vertexIndex )
{
  bool pointRemoved = mTriangulation->removePoint( vertexIndex );

  if ( pointRemoved )
    mDirty = true;

  if ( autoUpdate() )
  {
    updateMesh();
    emit updated();
  }

  return pointRemoved;
}

void ReosTriangularIrregularNetworkQgsDualEdge_p::addConstraintLine( const QVector<ReosTriangularIrregularNetwork::Vertex> &vertices )
{
  QVector<QgsPoint> points( vertices.count() );
  for ( int i = 0; i < vertices.count(); ++i )
    points[i] = QgsPoint( vertices.at( i ).x, vertices.at( i ).y, vertices.at( i ).z );

  mTriangulation->addLine( points, QgsInterpolator::SourceBreakLines );
}

int ReosTriangularIrregularNetworkQgsDualEdge_p::vertexCount() const
{
  return ( mTriangulation->pointsCount() );
}

QPointF ReosTriangularIrregularNetworkQgsDualEdge_p::vertexXY( int index ) const
{
  if ( mTriangulation->point( index ) )
    return ( mTriangulation->point( index )->toQPointF() );

  return QPointF();
}

int ReosTriangularIrregularNetworkQgsDualEdge_p::triangleCount() const
{
  updateMesh();
  return mMesh.faceCount();
}

QgsMesh ReosTriangularIrregularNetworkQgsDualEdge_p::triangulatedMesh() const
{
  updateMesh();
  return mMesh;
}

QgsMesh ReosTriangularIrregularNetworkQgsDualEdge_p::updatedTriangulatedMesh( QgsRectangle &updatedExtent )
{
  if ( !mTriangulation )
    return QgsMesh();

  return mTriangulation->editedTriangulationToMesh( updatedExtent );

}

void ReosTriangularIrregularNetworkQgsDualEdge_p::updateMesh() const
{
  if ( !mTriangulation )
    return;
  if ( mDirty && mTriangulation->pointsCount() > 0 )
  {
    mMesh = mTriangulation->triangulationToMesh();
    mMinimumElevation = std::numeric_limits<double>::max();
    mMaximumElevation = -std::numeric_limits<double>::max();
    for ( const QgsMeshVertex &vertex : std::as_const( mMesh.vertices ) )
    {
      if ( mMinimumElevation > vertex.z() )
        mMinimumElevation = vertex.z();
      if ( mMaximumElevation < vertex.z() )
        mMaximumElevation = vertex.z();
    }
    mExtent = QgsRectangle( mTriangulation->xMin(), mTriangulation->yMin(), mTriangulation->xMax(), mTriangulation->yMax() );
    mDirty = false;
  }
}
