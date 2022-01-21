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
  VertexS vert0 = createVertex( feat.id(), 0 );
  VertexS firstVertex = vert0;

  VertexS vert1 = createVertex( feat.id(), 1 );
  QgsPointXY firstPoint( point0 );
  mBoundariesVertex.append( vert0.get() );
  mBoundariesVertex.append( vert1.get() );
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
    vert0->attachLine( feat.id(), 0 );
    vert1 = createVertex( feat.id(), 1 );
    mBoundariesVertex.append( vert1.get() );
    mSegments.insert( feat.id(), {vert0, vert1} );
  }
  feat = QgsFeature();
  geomSegment = QgsGeometry( new QgsLineString( {point1, firstPoint} ) );
  feat.setGeometry( geomSegment );
  mVectorLayer->addFeature( feat );
  vert1->attachLine( feat.id(), 0 );
  firstVertex->attachLine( feat.id(), 1 );
  mSegments.insert( feat.id(), {vert1, firstVertex} );

  mVectorLayer->undoStack()->clear();

  mVectorLayer->undoStack()->blockSignals( false );
}


VertexS ReosPolylineStructureVectorLayer::createVertex( QgsFeatureId id, int positionInFeature )
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

QList<ReosStructureVertexHandler_p *> ReosPolylineStructureVectorLayer::neighorsVertices( ReosGeometryStructureVertex *vertex, QList<SegmentId> &fids ) const
{
  QList<VertexP> neighbors;

  VertexP vert = static_cast<ReosStructureVertexHandler_p *>( vertex );
  fids = vert->attachedLines().toList();

  for ( const QgsFeatureId &fid : std::as_const( fids ) )
  {
    neighbors.append( oppositeVertexPointer( vert, fid ) );
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

Segment ReosPolylineStructureVectorLayer::idToSegment( QgsFeatureId id ) const
{
  auto it = mSegments.find( id );
  if ( it == mSegments.end() )
    return Segment();

  return it.value();
}

VertexS ReosPolylineStructureVectorLayer::idToVertex( QgsFeatureId id, int pos )
{
  const Segment &seg = idToSegment( id );
  return seg.at( pos );
}

bool ReosPolylineStructureVectorLayer::idToOneLinkedSegment( SegmentId id, int pos, SegmentId *linkedSeg )
{
  VertexP vert = idToVertex( id, pos ).get();
  return vert->oneOtherLine( id, linkedSeg );
}



VertexP ReosPolylineStructureVectorLayer::oppositeVertexPointer( VertexP other, SegmentId sid ) const
{
  return oppositeVertex( other, sid ).get();
}

VertexS ReosPolylineStructureVectorLayer::oppositeVertex( VertexP other, SegmentId sid ) const
{
  const Segment &seg = idToSegment( sid );
  if ( seg.at( 0 ).get() == other )
    return seg.at( 1 );
  else if ( seg.at( 1 ).get() == other )
    return seg.at( 0 );
  else
    throw ReosException( QStringLiteral( "topologic error" ) );
}

QPair<SegmentId, SegmentId> ReosPolylineStructureVectorLayer::boundarieLines( VertexP vertex )
{
  SegmentId segId0;
  SegmentId segId1;
  QList<SegmentId> segs;
  QList<VertexP> vertices = neighorsVertices( vertex, segs );

  for ( int i = 0; i < segs.count(); ++i )
  {
    if ( mBoundariesVertex.contains( vertices.at( i ) ) )
    {
      if ( vertices.at( i )->posInLine( segs.at( i ) ) == 0 )
        segId0 = segs.at( i );
      else
        segId1 = segs.at( i );
    }
  }
  return QPair<SegmentId, SegmentId>( segId0, segId1 );
}

void ReosPolylineStructureVectorLayer::purge()
{
  auto it = mBoundariesVertex.begin();
  while ( it != mBoundariesVertex.end() )
  {
    if ( ( *it )->attachedLines().isEmpty() )
      it = mBoundariesVertex.erase( it );
    else
      ++it;
  }
}

void ReosPolylineStructureVectorLayer::addPolylines( const QPolygonF &polyline, const QString &sourceCrs )
{
  QgsCoordinateTransform transform = toLayerTransform( sourceCrs );

  mVectorLayer->beginEditCommand( "Add lines" );

  for ( int i = 0; i < polyline.count() - 1; ++i )
  {
    const QPointF &pt0 = polyline.at( i );
    const QPointF &pt1 = polyline.at( i + 1 );

    const QgsRectangle sr0Source( pt0.x() - mTolerance, pt0.y() - mTolerance, pt0.x() + mTolerance, pt0.y() + mTolerance );
    QgsRectangle sr0Layer;
    QgsFeatureIterator fit0 = closeLines( sr0Source, sr0Layer, transform );
    VertexS vert0 = searchForVertexPrivate( fit0, sr0Layer );

    const QgsRectangle sr1Source( pt1.x() - mTolerance, pt1.y() - mTolerance, pt1.x() + mTolerance, pt1.y() + mTolerance );
    QgsRectangle sr1Layer;
    QgsFeatureIterator fit1 = closeLines( sr1Source, sr1Layer, transform );
    VertexS vert1 = searchForVertexPrivate( fit1, sr1Layer );

    QgsFeature feature;
    feature.setGeometry( QgsGeometry( new QgsLineString( {pt0, pt1} ) ) );
    mVectorLayer->addFeature( feature );

    mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayerUndoCommandAddLine(
                                       feature.id(),
                                       vert0,
                                       vert1,
                                       false,
                                       this ) );
  }

  mVectorLayer->endEditCommand();
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
    const VertexP &vert = mBoundariesVertex.at( i );
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
  QList<SegmentId> ids;
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
      if ( closeVertex->attachedLines().count() > 1 || vertex->attachedLines().count() > 1 )
        return false;
  }

  QgsFeatureIterator fit = mVectorLayer->getFeatures( ids.toSet() );

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
    VertexS vert1 = concernedSeg.at( 0 );
    VertexS vert2 = concernedSeg.at( 1 );
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

void ReosPolylineStructureVectorLayer::insertVertex( const ReosSpatialPosition &point, qint64 lineId )
{
  QgsPointXY pointI = toLayerCoordinates( point );

  QgsFeature oldFeature = mVectorLayer->getFeature( lineId );
  // get the projected point on the line
  QgsPointXY projPoint;
  int vi;
  oldFeature.geometry().closestSegmentWithContext( pointI, projPoint, vi );
  pointI = projPoint;

  Segment seg = idToSegment( lineId );

  QgsPointXY point0 = oldFeature.geometry().vertexAt( 0 );
  QgsPointXY point1 = oldFeature.geometry().vertexAt( 1 );

  mVectorLayer->beginEditCommand( "Insert vertex" );
  mVectorLayer->undoStack()->push( //remove the old line
    new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( lineId, true, this ) );
  mVectorLayer->deleteFeature( lineId );

  QgsFeature feat0;
  feat0.setGeometry( QgsGeometry( new QgsLineString( {point0, pointI} ) ) );
  QgsFeature feat1;
  feat1.setGeometry( QgsGeometry( new QgsLineString( {pointI, point1} ) ) );
  mVectorLayer->addFeature( feat0 );
  mVectorLayer->addFeature( feat1 );
  mVectorLayer->undoStack()->push( //add first line
    new ReosPolylineStructureVectorLayerUndoCommandAddLine(
      feat0.id(),
      seg.at( 0 ),
      nullptr,
      true,
      this ) );

  VertexS createdVertex =  oppositeVertex( seg.at( 0 ).get(), feat0.id() );

  mVectorLayer->undoStack()->push( // add second line
    new ReosPolylineStructureVectorLayerUndoCommandAddLine(
      feat1.id(),
      createdVertex,
      seg.at( 1 ),
      true,
      this ) );

  mVectorLayer->endEditCommand();
}

void ReosPolylineStructureVectorLayer::removeVertex( ReosGeometryStructureVertex *vertex )
{
  VertexP vert = static_cast<VertexP>( vertex );

  int posInBoundary = mBoundariesVertex.indexOf( vert );
  bool onBoundary = posInBoundary != -1;

  const QSet<SegmentId> attachedLinesIds = vert->attachedLines();

  VertexS boundaryVert0;
  VertexS boundaryvert1;

  mVectorLayer->beginEditCommand( "Remove vertex" );

  for ( SegmentId id : attachedLinesIds )
  {
    VertexS oppVertex = oppositeVertex( vert, id );
    if ( onBoundary && mBoundariesVertex.contains( oppVertex.get() ) )
    {
      int pos = oppVertex->posInLine( id );
      if ( pos == -1 )
        throw ReosException( QStringLiteral( "topologic error" ) );
      if ( pos == 0 )
        boundaryVert0 = oppVertex;
      else
        boundaryvert1 = oppVertex;
    }

    mVectorLayer->deleteFeature( id );
    mVectorLayer->undoStack()->push( //remove the old line
      new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( id, true, this ) );
  }

  if ( onBoundary )
  {
    QgsPointXY pt0 = boundaryVert0->position();
    QgsPointXY pt1 = boundaryvert1->position();
    QgsFeature feat;
    feat.setGeometry( QgsGeometry( new QgsLineString( {pt0, pt1} ) ) );
    mVectorLayer->addFeature( feat );
    mVectorLayer->undoStack()->push(
      new ReosPolylineStructureVectorLayerUndoCommandAddLine(
        feat.id(),
        boundaryVert0,
        boundaryvert1,
        true,
        this ) );
  }

  mVectorLayer->endEditCommand();
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

QgsFeatureIterator ReosPolylineStructureVectorLayer::closeLines( const ReosMapExtent &zone, QgsRectangle &rect ) const
{
  QgsCoordinateTransform transform = toLayerTransform( zone.crs() );

  return closeLines( zone.toRectF(), rect, transform );
}

QgsFeatureIterator ReosPolylineStructureVectorLayer::closeLines( const QgsRectangle &rectSource, QgsRectangle &rectLayer, const QgsCoordinateTransform &transform ) const
{

  if ( transform.isValid() )
  {
    try
    {
      rectLayer = transform.transform( rectSource );
    }
    catch ( ... )
    {
      rectLayer = QgsRectangle( rectSource );
    }
  }
  else
  {
    rectLayer = QgsRectangle( rectSource );
  }

  QgsFeatureRequest request;

  request.setFilterRect( rectLayer );

  return mVectorLayer->getFeatures( request );
}


VertexS ReosPolylineStructureVectorLayer::searchForVertexPrivate( QgsFeatureIterator &it, const QgsRectangle &rect ) const
{
  QgsPointXY center = rect.center();

  QgsFeatureId selectedFid = 0;
  bool foundOne = false;
  int vertexIndex = -1;

  // search for the closest of the center point.
  QgsFeature feat;
  double minDist = std::numeric_limits<double>::max();
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
    return seg[vertexIndex];
  }

  return nullptr;
}


ReosGeometryStructureVertex *ReosPolylineStructureVectorLayer::searchForVertex( const ReosMapExtent &zone ) const
{
  QgsRectangle rect;
  QgsFeatureIterator it = closeLines( zone, rect );

  return searchForVertexPrivate( it, rect ).get();

}

bool ReosPolylineStructureVectorLayer::searchForLine( const ReosMapExtent &zone, qint64 &id ) const
{
  QgsRectangle rect;
  QgsFeatureIterator it = closeLines( zone, rect );
  QgsPointXY center = rect.center();

  QgsFeature feat;
  double minDist = std::numeric_limits<double>::max();
  bool found = false;
  while ( it.nextFeature( feat ) )
  {
    QgsGeometry geom = feat.geometry();
    double dist = geom.distance( QgsGeometry( new QgsPoint( center ) ) );
    if ( dist < rect.width() / 2 && dist < minDist )
    {
      id = feat.id();
      found = true;
    }
  }

  return found;
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
  QList<QgsFeatureId> ids;
  QList<ReosStructureVertexHandler_p *> neighbors = neighorsVertices( static_cast<ReosStructureVertexHandler_p *>( vertex ), ids );

  QList<QPointF> ret;
  const QgsCoordinateTransform &transform = toDestinationTransform( crs );

  for ( ReosStructureVertexHandler_p *vert : std::as_const( neighbors ) )
    ret.append( vert->position( transform ) );

  return ret;
}

ReosPolylinesStructure::Data ReosPolylineStructureVectorLayer::structuredLinesData( const QString &destinationCrs ) const
{
  const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );
  Data data;
  QMap<VertexP, int> mVertexPointerToIndex;

  data.boundaryPointCount = mBoundariesVertex.count();

  data.vertices.resize( data.boundaryPointCount );

  for ( int i = 0; i < mBoundariesVertex.count(); ++i )
  {
    VertexP vert = mBoundariesVertex.at( i );
    const QPointF position = mBoundariesVertex.at( i )->position( transform );
    mVertexPointerToIndex.insert( vert, i );
    data.vertices[i] = position;
  }

  for ( const Segment &seg : std::as_const( mSegments ) )
  {
    std::array<VertexP, 2> vert = {seg.at( 0 ).get(), seg.at( 1 ).get()};
    std::array<int, 2> ind;

    for ( int i = 0; i < 2; ++i )
    {
      auto it = mVertexPointerToIndex.find( vert[i] );
      if ( it == mVertexPointerToIndex.end() )
      {
        ind[i] = data.vertices.count();
        mVertexPointerToIndex.insert( vert[i],   ind[i] );
        data.vertices.append( vert[i]->position( transform ) );
      }
      else
      {
        ind[i] = it.value();
      }
    }

    if ( ind[0] >= data.boundaryPointCount || ind[1] >= data.boundaryPointCount )
      data.internalLines.append( {ind[0], ind[1]} );
  }

  return data;
}

QUndoStack *ReosPolylineStructureVectorLayer::undoStack() const
{
  return mVectorLayer->undoStack();
}


ReosStructureVertexHandler_p::ReosStructureVertexHandler_p( QgsVectorLayer *source, QgsFeatureId fid, int pos ):
  mSource( source )
{
  mLinkedSegments.append( PositionInFeature( {fid, pos} ) );
}


QPointF ReosStructureVertexHandler_p::position( const QgsCoordinateTransform &transform )
{
  if ( !mSource || mLinkedSegments.isEmpty() )
    return QPointF();

  const PositionInFeature &pf = mLinkedSegments.at( 0 );

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

SegmentId ReosStructureVertexHandler_p::firstLinkedLine( int posInLine ) const
{
  for ( const PositionInFeature &position : mLinkedSegments )
    if ( position.pos == posInLine )
      return position.fid;

  return InvalidSegment;
}

void ReosStructureVertexHandler_p::attachLine( SegmentId fid, int pos )
{
  PositionInFeature position( {fid, pos} );
  if ( ! mLinkedSegments.contains( position ) )
    mLinkedSegments.append( position );
}

void ReosStructureVertexHandler_p::detachLine( SegmentId fid )
{
  mLinkedSegments.removeOne( PositionInFeature( fid ) );
}

void ReosStructureVertexHandler_p::move( const QgsPointXY &newPosition )
{
  if ( !mSource )
    return;
  mSource->beginEditCommand( QObject::tr( "Move vertex in structure" ) );
  for ( const PositionInFeature &pos : std::as_const( mLinkedSegments ) )
    mSource->moveVertex( newPosition.x(), newPosition.y(), pos.fid, pos.pos );
  mSource->endEditCommand();
}

QSet<SegmentId> ReosStructureVertexHandler_p::attachedLines() const
{
  QgsFeatureIds ret;
  for ( const PositionInFeature &pif : mLinkedSegments )
    ret.insert( pif.fid );

  return ret;
}

bool ReosStructureVertexHandler_p::hasLineAttached() const
{
  return !mLinkedSegments.empty();
}

bool ReosStructureVertexHandler_p::oneOtherLine( QgsFeatureId id, QgsFeatureId *otherLine ) const
{

  for ( const PositionInFeature &posFeat : std::as_const( mLinkedSegments ) )
    if ( posFeat.fid != id )
    {
      *otherLine  = posFeat.fid;
      return true;
    }

  return false;
}

ReosPolylineStructureVectorLayerUndoCommandRemoveLine::ReosPolylineStructureVectorLayerUndoCommandRemoveLine(
  QgsFeatureId id,
  bool onBoundary,
  ReosPolylineStructureVectorLayer *structure )
  : mId( id ), mStructure( structure )
{
  if ( onBoundary )
  {
    mBoundaryPos = mStructure->mBoundariesVertex.indexOf( mStructure->idToSegment( id ).at( 0 ).get() );
  }
}

void ReosPolylineStructureVectorLayerUndoCommandRemoveLine::redo()
{
  mSeg = mStructure->mSegments.take( mId );
  mSeg.at( 0 )->detachLine( mId );
  mSeg.at( 1 )->detachLine( mId );
  mStructure->purge();
}

void ReosPolylineStructureVectorLayerUndoCommandRemoveLine::undo()
{
  mSeg.at( 0 )->attachLine( mId, 0 );
  mSeg.at( 1 )->attachLine( mId, 1 );
  mStructure->mSegments.insert( mId, mSeg );
  if ( mBoundaryPos >= 0 )
  {
    if ( !mStructure->mBoundariesVertex.contains( mSeg.at( 0 ).get() ) )
      mStructure->mBoundariesVertex.insert( mBoundaryPos, mSeg.at( 0 ).get() );

    if ( !mStructure->mBoundariesVertex.contains( mSeg.at( 1 ).get() ) )
      mStructure->mBoundariesVertex.insert( mBoundaryPos + 1, mSeg.at( 1 ).get() );
  }

}

ReosPolylineStructureVectorLayerUndoCommandAddLine::ReosPolylineStructureVectorLayerUndoCommandAddLine(
  QgsFeatureId idLineToAdd, VertexS vert0, VertexS vert1, bool onBoundary, ReosPolylineStructureVectorLayer *structure )
  : mIdToAdd( idLineToAdd ), mVert0( vert0 ), mVert1( vert1 ), mOnBoundary( onBoundary )
  , mStructure( structure )
{
  if ( vert0 )
  {
    for ( int i = 0; i < 2; ++i )
    {
      SegmentId sid = vert0->firstLinkedLine( i );
      if ( sid != InvalidSegment )
      {
        mExistingLine0 = sid;
        mPosInExistingLine0 = i;
        break;
      }
    }
  }

  if ( vert1 )
  {
    for ( int i = 0; i < 2; ++i )
    {
      SegmentId sid = vert1->firstLinkedLine( i );
      if ( sid != InvalidSegment )
      {
        mExistingLine1 = sid;
        mPosInExistingLine1 = i;
        break;
      }
    }
  }


}

void ReosPolylineStructureVectorLayerUndoCommandAddLine::redo()
{
  VertexS vert0 = mVert0.lock();
  VertexS vert1 = mVert1.lock();

  if ( !vert0 )
  {
    if ( mExistingLine0 != InvalidSegment )
      vert0 = mStructure->idToVertex( mExistingLine0, mPosInExistingLine0 ); //the shared ptr to the vertex has expired, try with the existing id;

    if ( !vert0 ) //nothing exist,we need to create it
    {
      vert0 = mStructure->createVertex( mIdToAdd, 0 );
      if ( mOnBoundary && vert1 )
      {
        int pos = mStructure->mBoundariesVertex.indexOf( vert1.get() );
        mStructure->mBoundariesVertex.insert( pos, vert0.get() );
      }
    }
  }

//  VertexH vert0 = mStructure->idToVertex( mId0, 1 );
//  VertexH vert1 = mStructure->idToVertex( mId1, 0 );
//  if ( !vert0 )
//  {
//    vert0 = mStructure->createVertex( mIdToAdd, 0 );
//    if ( mOnBoundary && vert1 )
//    {
//      int pos = mStructure->mBoundariesVertex.indexOf( vert1.get() );
//      mStructure->mBoundariesVertex.insert( pos, vert0.get() );
//    }
//  }

//  if ( !vert1 )
//  {
//    vert1 = mStructure->createVertex( mIdToAdd, 1 );
//    if ( mOnBoundary && vert0 )
//    {
//      int pos = mStructure->mBoundariesVertex.indexOf( vert0.get() ) + 1;
//      mStructure->mBoundariesVertex.insert( pos, vert1.get() );
//    }
//  }

  if ( !vert1 )
  {
    if ( mExistingLine1 != InvalidSegment )
      vert1 = mStructure->idToVertex( mExistingLine1, mPosInExistingLine1 ); //the shared ptr to the vertex has expired, try with the existing id;

    if ( !vert1 ) //nothing exist,we need to create it
    {
      vert1 = mStructure->createVertex( mIdToAdd, 1 );
      if ( mOnBoundary && vert0 )
      {
        int pos = mStructure->mBoundariesVertex.indexOf( vert0.get() );
        mStructure->mBoundariesVertex.insert( pos + 1, vert1.get() );
      }
    }
  }

  vert0->attachLine( mIdToAdd, 0 );
  vert1->attachLine( mIdToAdd, 1 );
  mStructure->mSegments.insert( mIdToAdd, {vert0, vert1} );

  mVert0 = vert0;
  mVert1 = vert1;
}

void ReosPolylineStructureVectorLayerUndoCommandAddLine::undo()
{
  VertexS vert0 = mVert0.lock();
  VertexS vert1 = mVert1.lock();

  if ( !vert0 )
    vert0 = mStructure->idToVertex( mIdToAdd, 0 );

  vert0->detachLine( mIdToAdd );
  if ( mOnBoundary && !vert0->hasLineAttached() )
    mStructure->mBoundariesVertex.removeOne( vert0.get() );

  if ( !vert1 )
    vert1 = mStructure->idToVertex( mIdToAdd, 1 );

  vert1->detachLine( mIdToAdd );
  if ( mOnBoundary && !vert1->hasLineAttached() )
    mStructure->mBoundariesVertex.removeOne( vert1.get() );

  mStructure->mSegments.remove( mIdToAdd );

  mVert0 = vert0;
  mVert1 = vert1;
}
