/***************************************************************************
  reospolylinesstructure_p.cpp - ReosPolylinesStructure_p

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
#include "reospolylinesstructure_p.h"

#include <QUuid>
#include <QDebug>

#include <qgsproject.h>
#include <qgscoordinatetransform.h>
#include <qgslinestring.h>
#include <qgsgeometryutils.h>

#include "reosmapextent.h"
#include "reosexception.h"


#define BOUNDARY_ID QStringLiteral("boundary")

ReosPolylineStructureVectorLayer::ReosPolylineStructureVectorLayer( const QString &wktCrs )
  : mVectorLayer( new QgsVectorLayer( QStringLiteral( "Linestring?crs=" )
                                      + wktCrs
                                      + QStringLiteral( "&index=yes" )
                                      , QStringLiteral( "internalLayer" ),
                                      QStringLiteral( "memory" ) ) )
{
  mVectorLayer->startEditing();
  mVectorLayer->extent();

  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
}

void ReosPolylineStructureVectorLayer::setTolerance( double tolerance, const QString &wktCrs )
{
  QgsCoordinateReferenceSystem crs;
  crs.createFromWkt( wktCrs );
  QgsUnitTypes::DistanceUnit unitSource = crs.mapUnits();
  QgsUnitTypes::DistanceUnit structureUnit = mVectorLayer->crs().mapUnits();
  mTolerance = QgsUnitTypes::fromUnitToUnitFactor( unitSource, structureUnit ) * tolerance;
}


ReosPolylineStructureVectorLayer::ReosPolylineStructureVectorLayer( const QPolygonF &boundary, const QString &wktCrs )
  : ReosPolylineStructureVectorLayer( wktCrs )
{
  if ( boundary.count() < 3 )
    return;

  mVectorLayer->undoStack()->blockSignals( true );

  QgsPointXY point0( boundary.at( 0 ) );
  QgsPointXY point1( boundary.at( 1 ) );
  QgsGeometry geomSegment( new QgsLineString( {point0, point1} ) );
  QgsFeature feat;
  feat.setGeometry( geomSegment );
  mVectorLayer->addFeature( feat );
  VertexH vert0 = createVertex( feat.id(), 0 );
  VertexH firstVertex = vert0;

  VertexH vert1 = createVertex( feat.id(), 1 );
  QgsPointXY firstPoint( point0 );
  mBoundariesVertex.append( vert0 );
  mBoundariesVertex.append( vert1 );
  mSegments.insert( feat.id(), {vert0, vert1} );

  for ( int i = 2; i < boundary.count() ; ++i )
  {
    vert0 = vert1;
    point0 = point1;
    point1 = QgsPointXY( boundary.at( i ) );
    QgsGeometry geomSegment( new QgsLineString( {point0, point1} ) );
    QgsFeature feat;
    feat.setGeometry( geomSegment );
    mVectorLayer->addFeature( feat );
    vert0->linkFeature( feat.id(), 0 );
    vert1 = createVertex( feat.id(), 1 );
    mBoundariesVertex.append( vert1 );
    mSegments.insert( feat.id(), {vert0, vert1} );
  }
  feat = QgsFeature();
  geomSegment = QgsGeometry( new QgsLineString( {point1, firstPoint} ) );
  feat.setGeometry( geomSegment );
  mVectorLayer->addFeature( feat );
  vert1->linkFeature( feat.id(), 0 );
  firstVertex->linkFeature( feat.id(), 1 );
  mSegments.insert( feat.id(), {vert1, firstVertex} );

  mVectorLayer->undoStack()->clear();

  mVectorLayer->undoStack()->blockSignals( false );
}


ReosPolylineStructureVectorLayer::VertexH ReosPolylineStructureVectorLayer::createVertex( QgsFeatureId id, int positionInFeature )
{
  return std::make_shared<ReosStructureVertexHandler_p>( mVectorLayer.get(), id, positionInFeature );
}

const QgsCoordinateTransform ReosPolylineStructureVectorLayer::toLayerTransform( const QString &crs ) const
{
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.fromWkt( crs );

  return QgsCoordinateTransform( qgsCrs, mVectorLayer->crs(), QgsProject::instance() );
}

const QgsCoordinateTransform ReosPolylineStructureVectorLayer::toDestinationTransform( const QString &destinationCrs ) const
{
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.fromWkt( destinationCrs );

  return QgsCoordinateTransform( mVectorLayer->crs(), qgsCrs, QgsProject::instance() );
}

QList<ReosStructureVertexHandler_p *> ReosPolylineStructureVectorLayer::neighorsVertices( ReosGeometryStructureVertex *vertex, QgsFeatureIds &fids ) const
{
  QList<VertexP> neighbors;

  VertexP vert = static_cast<ReosStructureVertexHandler_p *>( vertex );
  fids = vert->linkedFeatures();

  for ( const QgsFeatureId &fid : std::as_const( fids ) )
  {
    neighbors.append( oppositeVertex( vert, fid ) );
  }

  return neighbors;
}

QgsPointXY ReosPolylineStructureVectorLayer::toLayerCoordinates( const QPointF &position, const QgsCoordinateTransform &transform ) const
{
  try
  {
    return transform.transform( position );
  }
  catch ( ... )
  {
    return position;
  }
}

QgsPointXY ReosPolylineStructureVectorLayer::toLayerCoordinates( const ReosSpatialPosition &position ) const
{
  QgsCoordinateReferenceSystem crs;
  crs.fromWkt( position.crs() );

  QgsCoordinateTransform transform( crs, mVectorLayer->crs(), QgsProject::instance() );

  return toLayerCoordinates( position.position(), toLayerTransform( position.crs() ) );
}

ReosPolylineStructureVectorLayer::Segment ReosPolylineStructureVectorLayer::idToSegment( QgsFeatureId id ) const
{
  auto it = mSegments.find( id );
  if ( it == mSegments.end() )
    return Segment();

  return it.value();
}

ReosPolylineStructureVectorLayer::VertexP ReosPolylineStructureVectorLayer::oppositeVertex( ReosPolylineStructureVectorLayer::VertexP other,
    ReosPolylineStructureVectorLayer::SegmentId sid ) const
{
  const Segment &seg = idToSegment( sid );
  if ( seg.at( 0 ).get() == other )
    return seg.at( 1 ).get();
  else if ( seg.at( 1 ).get() == other )
    return seg.at( 0 ).get();
  else
    throw ReosException( QStringLiteral( "topologic error" ) );
}

void ReosPolylineStructureVectorLayer::addPolylines( const QPolygonF &polyline, const QString &sourceCrs, const QString &id )
{

}

QPolygonF ReosPolylineStructureVectorLayer::polyline( const QString &destinationCrs, const QString &id ) const
{

}

QPolygonF ReosPolylineStructureVectorLayer::boundary( const QString &destinationCrs ) const
{
  QPolygonF ret( mBoundariesVertex.size() );
  const QgsCoordinateTransform transform( toLayerTransform( destinationCrs ) );
  for ( int i = 0; i < mBoundariesVertex.count(); ++i )
  {
    const VertexH &vert = mBoundariesVertex.at( i );
    ret[i] = vert->position( transform );
  }

  return ret;
}

void ReosPolylineStructureVectorLayer::removeAll()
{
  if ( !mVectorLayer )
    return;

  QgsFeatureIterator it = mVectorLayer->getFeatures();
  QgsFeature feat;
  while ( it.nextFeature( feat ) )
    mVectorLayer->deleteFeature( feat.id() );

  mBoundariesVertex.clear();
}

bool ReosPolylineStructureVectorLayer::vertexCanBeMoved( ReosGeometryStructureVertex *geometryVertex, const ReosSpatialPosition &newPosition ) const
{
  VertexP vertex = static_cast<VertexP>( geometryVertex );
  QgsFeatureIds ids;
  const QList<ReosStructureVertexHandler_p *> neighbors = neighorsVertices( vertex, ids );
  QgsPointXY newPosInLayer = toLayerCoordinates( newPosition );
  double x = newPosInLayer.x();
  double y = newPosInLayer.y();

  ReosMapExtent searchExtent( x - mTolerance, y - mTolerance, x + mTolerance, y + mTolerance );
  VertexP closeVertex = static_cast<VertexP >( searchForVertex( searchExtent ) );

  if ( closeVertex )
  {
    const QgsPointXY closeVertexPosition = closeVertex->position();
    if ( closeVertexPosition.distance( newPosInLayer ) < mTolerance )
      if ( closeVertex->linkedFeatures().count() > 1 || vertex->linkedFeatures().count() > 1 )
        return false;
  }

  QgsFeatureIterator fit = mVectorLayer->getFeatures( ids );

  QgsRectangle concernedExtent;
  concernedExtent.include( newPosInLayer );

  QVector<QgsPointXY> neighborPosition;
  for ( ReosStructureVertexHandler_p *vert : neighbors )
  {
    const QgsPointXY pt =  vert->position();
    concernedExtent.include( pt );
    neighborPosition.append( pt );
  }

  QgsFeatureIterator concernedFeatIt = mVectorLayer->getFeatures( concernedExtent );

  QgsFeature feat;
  while ( concernedFeatIt.nextFeature( feat ) )
  {
    if ( ids.contains( feat.id() ) )
      continue;

    const Segment &concernedSeg = idToSegment( feat.id() );
    VertexH vert1 = concernedSeg.at( 0 );
    VertexH vert2 = concernedSeg.at( 1 );
    QgsPointXY p1 = vert1->position();
    QgsPointXY p2 = vert2->position();

    for ( int i = 0; i < neighbors.count(); ++i )
    {
      QgsPoint pointIntersect;
      bool isIntersect;
      if ( QgsGeometryUtils::segmentIntersection( QgsPoint( p1 ), QgsPoint( p2 ),
           QgsPoint( newPosInLayer ), QgsPoint( neighborPosition.at( i ) ),
           pointIntersect, isIntersect ) )
        return false;
    }

  }

  return true;
}

void ReosPolylineStructureVectorLayer::moveVertex( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition )
{
  QgsPointXY newLayerPosition = toLayerCoordinates( newPosition );

  VertexP vert = static_cast<ReosStructureVertexHandler_p *>( vertex );
  vert->move( newLayerPosition );

}

ReosMapExtent ReosPolylineStructureVectorLayer::extent( const QString &destinationCrs ) const
{
  QgsRectangle internalExtent = mVectorLayer->extent();
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.createFromString( destinationCrs );

  QgsCoordinateTransform transform( mVectorLayer->crs(), qgsCrs, QgsProject::instance() );

  if ( transform.isValid() )
  {
    try
    {
      QgsRectangle destExtent;
      destExtent = transform.transform( internalExtent );
      ReosMapExtent ret( destExtent.toRectF() );
      ret.setCrs( destinationCrs );
      return ret;
    }
    catch ( ... )
    {  }
  }

  ReosMapExtent ret( internalExtent.toRectF() );
  return ret;
}

ReosGeometryStructureVertex *ReosPolylineStructureVectorLayer::searchForVertex( const ReosMapExtent &zone ) const
{
  QgsCoordinateReferenceSystem destinationCrs;
  destinationCrs.createFromWkt( zone.crs() );

  QgsCoordinateTransform transform( destinationCrs, mVectorLayer->crs(), QgsProject::instance() );
  QgsRectangle rect( zone.toRectF() );

  try
  {
    transform.transform( rect );
  }
  catch ( ... )
  {
    rect = QgsRectangle( zone.toRectF() );
  }

  QgsFeatureRequest request;

  request.setFilterRect( rect );

  QgsFeatureIterator it = mVectorLayer->getFeatures( request );

  QgsPointXY center = rect.center();


  double minDist = std::numeric_limits<double>::max();

  QgsFeatureId selectedFid = 0;

  bool foundOne = false;
  int vertexIndex = -1;

  // search for the closest of the center point.
  QgsFeature feat;
  while ( it.nextFeature( feat ) )
  {
    foundOne |= true;
    QgsGeometry geom = feat.geometry();

    int closestIndex;
    int previousIndex;
    int nextIndex;
    double distance;
    geom.closestVertex( center, closestIndex, previousIndex, nextIndex, distance );

    QPointF pos = geom.vertexAt( closestIndex ).toQPointF();
    if ( rect.contains( pos ) && distance < minDist )
    {
      selectedFid = feat.id();
      vertexIndex = closestIndex;
    }
  }

  if ( vertexIndex >= 0 && vertexIndex < 2 )
  {
    auto itSeg = mSegments.find( selectedFid );
    const Segment &seg = itSeg.value();
    return seg[vertexIndex].get();
  }

  return nullptr;
}

QPointF ReosPolylineStructureVectorLayer::vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const
{
  QgsCoordinateReferenceSystem destinationCrs;
  destinationCrs.createFromWkt( crs );
  QgsCoordinateTransform transform( mVectorLayer->crs(), destinationCrs, QgsProject::instance() );

  return static_cast<ReosStructureVertexHandler_p *>( vertex )->position( transform );
}

QList<QPointF> ReosPolylineStructureVectorLayer::neighborsPositions( ReosGeometryStructureVertex *vertex, const QString &crs ) const
{
  QgsFeatureIds ids;
  QList<ReosStructureVertexHandler_p *> neighbors = neighorsVertices( static_cast<ReosStructureVertexHandler_p *>( vertex ), ids );

  QList<QPointF> ret;
  const QgsCoordinateTransform &transform = toDestinationTransform( crs );

  for ( ReosStructureVertexHandler_p *vert : std::as_const( neighbors ) )
    ret.append( vert->position( transform ) );

  return ret;
}


QUndoStack *ReosPolylineStructureVectorLayer::undoStack() const
{
  return mVectorLayer->undoStack();
}


ReosStructureVertexHandler_p::ReosStructureVertexHandler_p( QgsVectorLayer *source, QgsFeatureId fid, int pos ):
  mSource( source )
{
  mLinkedFeatures.append( PositionInFeature( {fid, pos} ) );
}


QPointF ReosStructureVertexHandler_p::position( const QgsCoordinateTransform &transform )
{
  if ( !mSource || mLinkedFeatures.isEmpty() )
    return QPointF();

  const PositionInFeature &pf = mLinkedFeatures.at( 0 );

  const QgsPoint &pt = mSource->getGeometry( pf.fid ).vertexAt( pf.pos );

  if ( transform.isValid() )
  {
    try
    {
      return transform.transform( pt ).toQPointF();
    }
    catch ( ... )
    { }
  }

  return pt.toQPointF();
}

void ReosStructureVertexHandler_p::linkFeature( QgsFeatureId fid, int pos )
{
  PositionInFeature position( {fid, pos} );
  if ( ! mLinkedFeatures.contains( position ) )
    mLinkedFeatures.append( {fid, pos} );
}

void ReosStructureVertexHandler_p::move( const QgsPointXY &newPosition )
{
  if ( !mSource )
    return;
  mSource->beginEditCommand( QObject::tr( "Move vertex in structure" ) );
  for ( const PositionInFeature &pos : std::as_const( mLinkedFeatures ) )
    mSource->moveVertex( newPosition.x(), newPosition.y(), pos.fid, pos.pos );
  mSource->endEditCommand();
}

QgsFeatureIds ReosStructureVertexHandler_p::linkedFeatures() const
{
  QgsFeatureIds ret;
  for ( const PositionInFeature &pif : mLinkedFeatures )
    ret.insert( pif.fid );

  return ret;
}
