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
#include <qgspolygon.h>
#include <qgsgeometryutils.h>

#include "reosmapextent.h"
#include "reosexception.h"


ReosGeometryStructure_p::ReosGeometryStructure_p( const  QString &type, const QString &wktCrs )
  : mVectorLayer( new QgsVectorLayer( type
                                      + QStringLiteral( "?crs=" )
                                      + wktCrs
                                      + QStringLiteral( "&index=yes" )
                                      , QStringLiteral( "internalLayer" ),
                                      QStringLiteral( "memory" ) ) )
{
}

QgsPointXY ReosGeometryStructure_p::toLayerCoordinates( const ReosSpatialPosition &position ) const
{
  QgsCoordinateReferenceSystem crs;
  crs.fromWkt( position.crs() );

  QgsCoordinateTransform transform( crs, mVectorLayer->crs(), QgsProject::instance() );

  return transformCoordinates( position.position(), toLayerTransform( position.crs() ) );
}

QgsPointXY ReosGeometryStructure_p::transformCoordinates( const QPointF &position, const QgsCoordinateTransform &transform ) const
{
  return transformCoordinates( QgsPointXY( position ), transform );
}


const QgsCoordinateTransform ReosGeometryStructure_p::toLayerTransform( const QString &crs ) const
{
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.fromWkt( crs );

  return QgsCoordinateTransform( qgsCrs, mVectorLayer->crs(), QgsProject::instance() );
}

const QgsCoordinateTransform ReosGeometryStructure_p::toDestinationTransform( const QString &destinationCrs ) const
{
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.fromWkt( destinationCrs );

  return QgsCoordinateTransform( mVectorLayer->crs(), qgsCrs, QgsProject::instance() );
}

ReosMapExtent ReosGeometryStructure_p::extent( const QString &destinationCrs ) const
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

QString ReosGeometryStructure_p::crs() const
{
  return mVectorLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}


QgsPointXY ReosGeometryStructure_p::transformCoordinates( const QgsPointXY &position, const QgsCoordinateTransform &transform ) const
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


ReosPolylineStructureVectorLayer::ReosPolylineStructureVectorLayer( const QString &wktCrs )
  : ReosGeometryStructure_p( QStringLiteral( "LineString" ), wktCrs )
{
  init();
}

void ReosPolylineStructureVectorLayer::init()
{
  mVectorLayer->startEditing();
  mVectorLayer->extent();

  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, [this]
  {
    mRawLinesDirty = true;
  } );
  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );

  mVerticesBoundaryRequest.setMaxCost( 2000 );
}

VertexP ReosPolylineStructureVectorLayer::boundaryVertex( VertexP vert ) const
{
  if ( !vert )
    return nullptr;

  VertexW *wv = mVerticesBoundaryRequest.object( vert );
  VertexS vertS = wv ? wv->lock() : nullptr;
  if ( vertS && vertS.get() == vert )
    return vert;

  if ( !mBoundariesVertex.contains( vert ) )
  {
    mVerticesBoundaryRequest.insert( vert, nullptr );
    return nullptr;
  }

  std::unique_ptr<VertexW> newWeak( new VertexW );
  *newWeak = sharedVertex( vert );
  mVerticesBoundaryRequest.insert( vert, newWeak.release() );

  return vert;
}

ReosPolylineStructureVectorLayer::~ReosPolylineStructureVectorLayer()
{
  disconnect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
}

VertexS ReosPolylineStructureVectorLayer::purposeVertex( const QgsPointXY &point, double toleranceInLayerSystem )
{
  const QgsRectangle sr( point.x() - toleranceInLayerSystem, point.y() - toleranceInLayerSystem, point.x() + toleranceInLayerSystem, point.y() + toleranceInLayerSystem );
  QgsFeatureIterator fit0 = closeLinesInLayerCoordinate( sr );
  VertexS vert = searchForVertexPrivate( fit0, sr );

  if ( !vert )
  {
    double dist = 0;
    SegmentId closestLineId;
    fit0 = closeLinesInLayerCoordinate( sr );
    if ( closestLine( fit0, sr, closestLineId, &dist ) && dist < toleranceInLayerSystem )
    {
      vert = insertVertexPrivate( sr.center(), closestLineId );
    }
  }

  return vert;
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

  Data data;
  data.vertices = boundary;
  data.boundaryPointCount = boundary.count();

  buildGeometry( data );
}

void ReosPolylineStructureVectorLayer::buildGeometry( const ReosPolylinesStructure::Data &data )
{
  if ( data.boundaryPointCount < 3 )
    return;

  mVectorLayer->undoStack()->blockSignals( true );

  const QVector<QPointF> &vertices = data.vertices;

  //create boundary
  QgsPointXY point0( data.vertices.at( 0 ) );
  QgsPointXY point1( vertices.at( 1 ) );
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

  for ( int i = 2; i < data.boundaryPointCount ; ++i )
  {
    vert0 = vert1;
    point0 = point1;
    point1 = QgsPointXY( vertices.at( i ) );
    const QgsGeometry geomSegment( new QgsLineString( {point0, point1} ) );
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

  //create internal lines
  for ( const QVector<int> &line : data.internalLines )
  {
    const QgsPointXY point0 = vertices.at( line.at( 0 ) );
    const QgsPointXY point1 = vertices.at( line.at( 1 ) );

    VertexS vert0 = purposeVertex( point0, mTolerance );
    VertexS vert1 = purposeVertex( point1, mTolerance );

    const QgsGeometry geomSegment( new QgsLineString( {point0, point1} ) );
    QgsFeature feat;
    feat.setGeometry( geomSegment );
    mVectorLayer->addFeature( feat );

    if ( !vert0 )
      vert0 = createVertex( feat.id(), 0 );
    else
      vert0->attachLine( feat.id(), 0 );

    if ( !vert1 )
      vert1 = createVertex( feat.id(), 1 );
    else
      vert1->attachLine( feat.id(), 1 );

    mSegments.insert( feat.id(), {vert0, vert1} );
  }

  mVectorLayer->undoStack()->clear();
  mVectorLayer->undoStack()->blockSignals( false );
}

ReosPolylineStructureVectorLayer::ReosPolylineStructureVectorLayer( const ReosEncodedElement &encodedElement )
{
  QString wktCrs;
  encodedElement.getData( QStringLiteral( "crs" ), wktCrs );
  mVectorLayer.reset( new QgsVectorLayer( QStringLiteral( "Linestring?crs=" )
                                          + wktCrs
                                          + QStringLiteral( "&index=yes" )
                                          , QStringLiteral( "internalLayer" ),
                                          QStringLiteral( "memory" ) ) );

  init();

  Data data;
  encodedElement.getData( "boundary-vertices-count", data.boundaryPointCount );
  encodedElement.getData( QStringLiteral( "vertices" ), data.vertices );
  encodedElement.getData( QStringLiteral( "internal-lines" ), data.internalLines );
  encodedElement.getData( QStringLiteral( "tolerance" ), mTolerance );

  buildGeometry( data );
}

ReosEncodedElement ReosPolylineStructureVectorLayer::encode() const
{
  ReosEncodedElement element( QStringLiteral( "polyline-structure-vector-layer" ) );
  Data data = structuredLinesData();
  element.addData( "boundary-vertices-count", data.boundaryPointCount );
  element.addData( QStringLiteral( "vertices" ), data.vertices );
  element.addData( QStringLiteral( "internal-lines" ), data.internalLines );
  element.addData( QStringLiteral( "tolerance" ), mTolerance );
  element.addData( QStringLiteral( "crs" ), crs() );
  return element;
}

VertexS ReosPolylineStructureVectorLayer::createVertex( QgsFeatureId id, int positionInFeature )
{
  return std::make_shared<ReosStructureVertexHandler_p>( mVectorLayer.get(), id, positionInFeature );
}

VertexS ReosPolylineStructureVectorLayer::insertVertexPrivate( const QgsPointXY &point, qint64 lineId )
{
  QgsFeature oldFeature = mVectorLayer->getFeature( lineId );
  // get the projected point on the line
  QgsPointXY projPoint;
  int vi;
  oldFeature.geometry().closestSegmentWithContext( point, projPoint, vi );

  Segment seg = idToSegment( lineId );

  bool onBoundary = isOnBoundary( seg );

  QgsPointXY point0 = oldFeature.geometry().vertexAt( 0 );
  QgsPointXY point1 = oldFeature.geometry().vertexAt( 1 );

  mVectorLayer->beginEditCommand( "Insert vertex" );
  mVectorLayer->undoStack()->push( //remove the old line
    new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( lineId, this ) );
  mVectorLayer->deleteFeature( lineId );

  QgsFeature feat0;
  feat0.setGeometry( QgsGeometry( new QgsLineString( {point0, projPoint} ) ) );
  QgsFeature feat1;
  feat1.setGeometry( QgsGeometry( new QgsLineString( {projPoint, point1} ) ) );
  mVectorLayer->addFeature( feat0 );
  mVectorLayer->addFeature( feat1 );
  mVectorLayer->undoStack()->push( //add first line
    new ReosPolylineStructureVectorLayerUndoCommandAddLine(
      feat0.id(),
      seg.at( 0 ),
      nullptr,
      onBoundary,
      this ) );

  VertexS createdVertex =  oppositeVertex( seg.at( 0 ).get(), feat0.id() );

  mVectorLayer->undoStack()->push( // add second line
    new ReosPolylineStructureVectorLayerUndoCommandAddLine(
      feat1.id(),
      createdVertex,
      seg.at( 1 ),
      onBoundary,
      this ) );

  mVectorLayer->endEditCommand();

  return createdVertex;
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

bool ReosPolylineStructureVectorLayer::isSegmentExisting( VertexP vert0, VertexP vert1 ) const
{
  const QSet<SegmentId> ids = vert0->attachedLines();
  for ( const SegmentId &segId : ids )
  {
    Segment seg = idToSegment( segId );
    for ( int i = 0; i < 2; ++i )
      if ( seg.at( i ).get() == vert1 )
        return true;
  }

  return false;
}

VertexS ReosPolylineStructureVectorLayer::sharedVertex( VertexP vertex ) const
{
  if ( !vertex )
    return VertexS();

  int pos = 0;
  SegmentId sid = vertex->firstLinkedLine( pos );
  if ( sid == InvalidSegment )
  {
    pos = 1;
    sid = vertex->firstLinkedLine( pos );
  }

  if ( sid == InvalidSegment )
    return ( VertexS() );

  const Segment &seg = mSegments.value( sid );
  return seg.at( pos );
}

VertexP ReosPolylineStructureVectorLayer::oppositeVertexPointer( VertexP other, SegmentId sid ) const
{
  return oppositeVertex( other, sid ).get();
}

VertexS ReosPolylineStructureVectorLayer::oppositeVertex( VertexP other, SegmentId sid ) const
{
  const Segment &seg = idToSegment( sid );
  return oppositeVertex( other, seg );
}

VertexS ReosPolylineStructureVectorLayer::oppositeVertex( VertexP other, const Segment &seg ) const
{
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
    if ( boundaryVertex( vertices.at( i ) ) )
    {
      if ( vertices.at( i )->posInLine( segs.at( i ) ) == 0 )
        segId0 = segs.at( i );
      else
        segId1 = segs.at( i );
    }
  }
  return QPair<SegmentId, SegmentId>( segId0, segId1 );
}

void ReosPolylineStructureVectorLayer::addPolylines( const QPolygonF &polyline, const QList<double> &tolerances, const QString &sourceCrs )
{
  QgsCoordinateTransform transform = toLayerTransform( sourceCrs );
  QgsCoordinateReferenceSystem crs;
  crs.createFromWkt( sourceCrs );

  QgsUnitTypes::DistanceUnit unitSource = crs.mapUnits();
  QgsUnitTypes::DistanceUnit structureUnit = mVectorLayer->crs().mapUnits();
  double convertToleranceFactor = QgsUnitTypes::fromUnitToUnitFactor( unitSource, structureUnit );


  mVectorLayer->beginEditCommand( "Add lines" );

  QgsGeometry exterior( new QgsPolygon( QgsLineString::fromQPolygonF( boundary() ) ) );

  bool somethingDone = false;

  for ( int i = 0; i < polyline.count() - 1; ++i )
  {
    QPointF pt0 = polyline.at( i );
    QPointF pt1 = polyline.at( i + 1 );


    QgsPointXY pointXY0 = transformCoordinates( pt0, transform );
    QgsPointXY pointXY1 = transformCoordinates( pt1, transform );

    double tol = ( !tolerances.empty() && tolerances.at( i ) > 0 ) ?
                 tolerances.at( i ) * convertToleranceFactor : mTolerance;

    VertexS vert0 = purposeVertex( pointXY0, tol );
    VertexS vert1 = purposeVertex( pointXY1, tol );

    if ( vert0 && vert1 && isSegmentExisting( vert0.get(), vert1.get() ) )
      continue;

    if ( vert0 )
      pointXY0 = vert0->position();

    if ( vert1 )
      pointXY1 = vert1->position();

    if ( ( !vert0 && !exterior.contains( &pointXY0 ) ) ||
         ( !vert1 && !exterior.contains( &pointXY1 ) ) )
      continue;

    QgsFeature feature;
    feature.setGeometry( QgsGeometry( new QgsLineString( {pointXY0, pointXY1} ) ) );
    mVectorLayer->addFeature( feature );

    mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayerUndoCommandAddLine(
                                       feature.id(),
                                       vert0,
                                       vert1,
                                       false,
                                       this ) );
    somethingDone = true;
  }

  if ( !somethingDone )
  {
    mVectorLayer->destroyEditCommand();
    mVectorLayer->undoStack()->canRedoChanged( false );
  }
  else
    mVectorLayer->endEditCommand();
}

QPolygonF ReosPolylineStructureVectorLayer::polyline( const QString &destinationCrs, const QString &id ) const
{

}

QLineF ReosPolylineStructureVectorLayer::line( qint64 lineId, const QString &destinationCrs ) const
{
  QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );

  QgsGeometry geom = mVectorLayer->getGeometry( lineId );

  if ( geom.isNull() || geom.isEmpty() )
    return QLineF();

  if ( transform.isValid() )
  {
    try
    {
      geom.transform( transform );
    }
    catch ( QgsCsException &e )
    {
      geom = mVectorLayer->getFeature( lineId ).geometry();
    }
  }

  QgsPolylineXY poly = geom.asPolyline();

  if ( poly.count() != 2 )
    return QLineF();

  return QLineF( poly.at( 0 ).toQPointF(), poly.at( 1 ).toQPointF() );

}

QPolygonF ReosPolylineStructureVectorLayer::boundary( const QString &destinationCrs ) const
{
  if ( mBoundariesVertex.size() > 0 )
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

  return QPolygonF();
}

void ReosPolylineStructureVectorLayer::removeAll()
{
  if ( !mVectorLayer )
    return;

  mVectorLayer->undoStack()->blockSignals( true );

  QgsFeatureIterator it = mVectorLayer->getFeatures();
  QgsFeature feat;
  while ( it.nextFeature( feat ) )
    mVectorLayer->deleteFeature( feat.id() );

  mBoundariesVertex.clear();
  mSegments.clear();
  mRawLinesDirty = true;

  mVectorLayer->undoStack()->blockSignals( false );

  mVectorLayer->undoStack()->clear();
}

bool ReosPolylineStructureVectorLayer::vertexCanBeMoved( ReosGeometryStructureVertex *geometryVertex, const ReosSpatialPosition &newPosition ) const
{
  VertexP vertex = static_cast<VertexP>( geometryVertex );
  bool boundaryVertex = isOnBoundary( vertex );
  QList<SegmentId> ids;
  const QList<VertexP> neighbors = neighorsVertices( vertex, ids );
  QgsPointXY newPosInLayer = toLayerCoordinates( newPosition );
  double x = newPosInLayer.x();
  double y = newPosInLayer.y();

  ReosMapExtent searchExtent( x - mTolerance, y - mTolerance, x + mTolerance, y + mTolerance );
  VertexP closeVertex = static_cast<VertexP >( searchForVertex( searchExtent ) );

  if ( closeVertex && boundaryVertex  && isOnBoundary( closeVertex ) )
  {
    const QgsPointXY closeVertexPosition = closeVertex->position();
    if ( closeVertexPosition.distance( newPosInLayer ) < mTolerance )
      if ( closeVertex->attachedLines().count() > 1 || vertex->attachedLines().count() > 1 )
        return false;
  }

  if ( closeVertex )
  {
    for ( VertexP nv : std::as_const( neighbors ) )
    {
      if ( isSegmentExisting( nv, closeVertex ) )
        return false;
    }
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

  if ( boundaryVertex )
  {
    int vertesPos = mBoundariesVertex.indexOf( vertex );
    QPolygonF exteriorF = boundary();
    exteriorF.replace( vertesPos, newPosInLayer.toQPointF() );
    QgsGeometry exterior( new QgsPolygon( QgsLineString::fromQPolygonF( exteriorF ) ) );

    QSet<VertexP> checkedVert;
    for ( const Segment &seg : mSegments )
    {
      VertexP vertToCheck = seg.at( 0 ).get();
      if ( !checkedVert.contains( vertToCheck ) )
      {
        if ( !isOnBoundary( vertToCheck ) )
        {
          QgsPointXY pt( vertToCheck->position() );
          if ( !exterior.contains( &pt ) )
            return false;
        }
      }
      checkedVert.insert( vertToCheck );
    }
  }
  else
  {
    QgsGeometry exterior( new QgsPolygon( QgsLineString::fromQPolygonF( boundary() ) ) );
    if ( !isOnBoundary( closeVertex ) && !exterior.contains( &newPosInLayer ) )
      return false;
  }

  return true;
}

void ReosPolylineStructureVectorLayer::moveVertex( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition )
{
  QgsPointXY newLayerPosition = toLayerCoordinates( newPosition );

  VertexP vert = static_cast<ReosStructureVertexHandler_p *>( vertex );

  mVectorLayer->beginEditCommand( tr( "Move structure vertex" ) );
  VertexS newPos = purposeVertex( newLayerPosition, mTolerance );
  vert->move( newLayerPosition );
  if ( newPos )
  {
    VertexS vertexToKeep;
    VertexS vertexToRemove;

    if ( boundaryVertex( vert ) )
    {
      vertexToKeep = sharedVertex( vert );
      vertexToRemove = newPos;
    }
    else
    {
      vertexToKeep = newPos;
      vertexToRemove =  sharedVertex( vert );
    }
    mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayerUndoCommandMergeVertex(
                                       vertexToRemove, vertexToKeep, this ) );
  }
  mVectorLayer->endEditCommand();
}

ReosGeometryStructureVertex *ReosPolylineStructureVectorLayer::insertVertex( const ReosSpatialPosition &point, qint64 lineId )
{
  QgsPointXY pointI = toLayerCoordinates( point );
  return insertVertexPrivate( pointI, lineId ).get();
}

bool ReosPolylineStructureVectorLayer::vertexCanBeRemoved( ReosGeometryStructureVertex *vertex ) const
{
  VertexP vert = static_cast<VertexP>( vertex );
  if ( boundaryVertex( vert ) )
  {
    int vertexPos = mBoundariesVertex.indexOf( vert );
    int prevIndex = ( vertexPos - 1 + mBoundariesVertex.count() ) % mBoundariesVertex.count();
    int nextIndex = ( vertexPos + 1 ) % mBoundariesVertex.count();
    if ( isSegmentExisting( mBoundariesVertex.at( prevIndex ), mBoundariesVertex.at( nextIndex ) ) )
      return false;

    QPolygonF exteriorF = boundary();
    exteriorF.removeAt( vertexPos );
    QgsGeometry exterior( new QgsPolygon( QgsLineString::fromQPolygonF( exteriorF ) ) );

    QSet<VertexP> checkedVert;
    for ( const Segment &seg : mSegments )
    {
      for ( int i = 0; i < 2; ++i )
      {
        VertexP vertToCheck = seg.at( i ).get();
        if ( !checkedVert.contains( vertToCheck ) )
        {
          if ( !isOnBoundary( vertToCheck ) )
          {
            QgsPointXY pt( vertToCheck->position() );
            if ( !exterior.contains( &pt ) )
              return false;
          }
        }
        checkedVert.insert( vertToCheck );
      }
    }
  }

  return true;
}

void ReosPolylineStructureVectorLayer::removeVertex( ReosGeometryStructureVertex *vertex )
{
  VertexP vert = static_cast<VertexP>( vertex );

  bool onBoundary = isOnBoundary( vertex );

  const QSet<SegmentId> attachedLinesIds = vert->attachedLines();

  VertexS boundaryVert0;
  VertexS boundaryvert1;

  mVectorLayer->beginEditCommand( "Remove vertex" );

  for ( SegmentId id : attachedLinesIds )
  {
    const Segment &seg = idToSegment( id );
    if ( onBoundary && isOnBoundary( seg ) )
    {
      VertexS oppVertex = oppositeVertex( vert, seg );
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
      new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( id, this ) );
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

bool ReosPolylineStructureVectorLayer::lineCanBeRemoved( qint64 lineId ) const
{
  return !isOnBoundary( idToSegment( lineId ) );
}

void ReosPolylineStructureVectorLayer::removeLine( qint64 lineId )
{
  mVectorLayer->beginEditCommand( tr( "Remove line" ) );
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( lineId, this ) );
  mVectorLayer->deleteFeature( lineId );
  mVectorLayer->endEditCommand();
}



QgsFeatureIterator ReosPolylineStructureVectorLayer::closeLines( const ReosMapExtent &zone, QgsRectangle &rect ) const
{
  QgsCoordinateTransform transform = toLayerTransform( zone.crs() );

  if ( transform.isValid() )
  {
    try
    {
      rect = transform.transform( zone.toRectF() );
    }
    catch ( ... )
    {
      rect = QgsRectangle( zone.toRectF() );
    }
  }
  else
  {
    rect = QgsRectangle( zone.toRectF() );
  }

  return closeLinesInLayerCoordinate( rect );
}

QgsFeatureIterator ReosPolylineStructureVectorLayer::closeLinesInLayerCoordinate( const QgsRectangle &rectLayer ) const
{
  QgsFeatureRequest request;
  request.setFilterRect( rectLayer );

  return mVectorLayer->getFeatures( request );
}

bool ReosPolylineStructureVectorLayer::closestLine( QgsFeatureIterator &it, const QgsRectangle &rect, SegmentId &lineId, double *distance ) const
{
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
      lineId = feat.id();
      found = true;
      minDist = dist;
    }
  }

  if ( distance )
    *distance = minDist;

  return found;
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

  return closestLine( it, rect, id );
}

QPointF ReosPolylineStructureVectorLayer::vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const
{
  QgsCoordinateTransform transform = toDestinationTransform( crs );
  return static_cast<ReosStructureVertexHandler_p *>( vertex )->position( transform );
}

QPointF ReosPolylineStructureVectorLayer::projectedPoint( const QPointF &point, qint64 lineId, const QString &destinationCrs ) const
{
  QgsGeometry geom = mVectorLayer->getGeometry( lineId );
  if ( geom.isNull() || geom.isEmpty() )
    return QPointF();

  QgsPointXY projPoint;
  int vi;
  geom.closestSegmentWithContext( point, projPoint, vi );

  return transformCoordinates( projPoint, toDestinationTransform( destinationCrs ) ).toQPointF();
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

QList<QPointF> ReosPolylineStructureVectorLayer::intersectionPoints( const QLineF &line, const QString &crs, const QPolygonF &otherPoly ) const
{
  const QgsCoordinateTransform transform = toLayerTransform( crs );
  QgsGeometry geom = QgsGeometry::fromPolylineXY( {QgsPointXY( line.p1() ), QgsPointXY( line.p2() )} );

  QVector<QgsPointXY> intersectPoint;
  if ( transform.isValid() )
  {
    try
    {
      geom.transform( transform );
    }
    catch ( QgsCsException &e )
    {
      geom = QgsGeometry::fromPolylineXY( {QgsPointXY( line.p1() ), QgsPointXY( line.p2() )} );
    }
  }

  QgsFeatureRequest request( geom.boundingBox() );

  QgsFeatureIterator fit = mVectorLayer->getFeatures( request );
  QgsFeature feat;


  auto searchIntersection = [this, &geom, &intersectPoint]( const QgsGeometry & existingGeom )
  {
    const QgsGeometry intersectGeom = existingGeom.intersection( geom );

    for ( auto vertIt = intersectGeom.vertices_begin(); vertIt != intersectGeom.vertices_end(); ++vertIt )
      intersectPoint.append( ( *vertIt ) );

    if ( intersectGeom.isNull() || intersectGeom.isEmpty() )
    {
      //add eventually point closer than tolerance from the line
      for ( auto vertIt = existingGeom.vertices_begin(); vertIt != existingGeom.vertices_end(); ++vertIt )
      {
        QgsPointXY projPoint;
        int vi = 0;
        double dist = geom.closestSegmentWithContext( *vertIt, projPoint, vi );
        if ( dist > 0 && std::sqrt( dist ) < mTolerance )
          intersectPoint.append( projPoint );
      }
    }

  };

  while ( fit.nextFeature( feat ) )
  {
    const QgsGeometry &existingGeom = feat.geometry();
    if ( existingGeom.isEmpty() || existingGeom.isNull() )
      continue;

    searchIntersection( existingGeom );
  }

  //search intersection with other polyline
  const QgsGeometry &existingGeom = QgsGeometry( QgsLineString::fromQPolygonF( otherPoly ) );
  searchIntersection( existingGeom );

//sort points
  const QgsPointXY first = geom.vertexAt( 0 );
  auto firstLess = [ first ]( const QgsPointXY & pt1, const QgsPointXY & pt2 )
  {
    return pt1.distance( first ) < pt2.distance( first );
  };
  std::sort( intersectPoint.begin(), intersectPoint.end(), firstLess );

// remove points too close from extremity or from each other
  int pos = 0;
  while ( pos < intersectPoint.count() )
  {
    const QgsPointXY &pt = intersectPoint.at( pos );
    if ( pt.distance( geom.vertexAt( 0 ) ) < mTolerance || pt.distance( geom.vertexAt( 1 ) ) < mTolerance )
      intersectPoint.removeAt( pos );
    else
    {
      int nextPos = pos + 1;
      while ( nextPos < intersectPoint.count() && pt.distance( intersectPoint.at( nextPos ) ) < mTolerance )
        intersectPoint.removeAt( nextPos );

      pos++;
    }
  }

  QList<QPointF> ret;
  ret.reserve( intersectPoint.count() );
  const QgsCoordinateTransform transformToDestination = toDestinationTransform( crs );
  for ( const QgsPointXY &pt : intersectPoint )
    ret.append( transformCoordinates( pt, transformToDestination ).toQPointF() );

  return ret;
}

ReosMapExtent ReosPolylineStructureVectorLayer::extent( const QString &crs ) const
{
  return ReosGeometryStructure_p::extent( crs );
}

ReosPolylinesStructure::Data ReosPolylineStructureVectorLayer::structuredLinesData( const QString &destinationCrs ) const
{
  const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );
  Data data;
  QHash<VertexP, int> vertexPointerToIndex;

  data.boundaryPointCount = mBoundariesVertex.count();

  data.vertices.resize( data.boundaryPointCount );

  for ( int i = 0; i < mBoundariesVertex.count(); ++i )
  {
    VertexP vert = mBoundariesVertex.at( i );
    const QPointF position = mBoundariesVertex.at( i )->position( transform );
    vertexPointerToIndex.insert( vert, i );
    data.vertices[i] = position;
  }

  for ( const Segment &seg : std::as_const( mSegments ) )
  {
    std::array<VertexP, 2> vert = {seg.at( 0 ).get(), seg.at( 1 ).get()};
    std::array<int, 2> ind;

    for ( int i = 0; i < 2; ++i )
    {
      auto it = vertexPointerToIndex.find( vert[i] );
      if ( it == vertexPointerToIndex.end() )
      {
        ind[i] = data.vertices.count();
        vertexPointerToIndex.insert( vert[i],   ind[i] );
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

QVector<QLineF> ReosPolylineStructureVectorLayer::rawLines( const QString &destinationCrs ) const
{
  if ( mRawLinesDirty )
  {
    mRawLines.clear();
    mRawLines.reserve( mSegments.count() );
    mCurrentLineCrs = destinationCrs;
    const QgsCoordinateTransform &transform = toDestinationTransform( destinationCrs );
    for ( const Segment &seg : mSegments )
    {
      mRawLines.append( QLineF( seg.at( 0 )->position( transform ), seg.at( 1 )->position( transform ) ) );
    }

    mRawLinesDirty = false;
  }

  return mRawLines;
}


QUndoStack *ReosPolylineStructureVectorLayer::undoStack() const
{
  return mVectorLayer->undoStack();
}


bool ReosPolylineStructureVectorLayer::isOnBoundary( ReosGeometryStructureVertex *vertex ) const
{
  VertexP vert = boundaryVertex( static_cast<VertexP>( vertex ) );

  return vert != nullptr;
}

bool ReosPolylineStructureVectorLayer::isOnBoundary( const Segment &seg ) const
{
  if ( seg.at( 0 ) && seg.at( 1 ) )
  {
    int boundaryCount = mBoundariesVertex.count();
    int index0 = mBoundariesVertex.indexOf( seg.at( 0 ).get() );
    int index1 = mBoundariesVertex.indexOf( seg.at( 1 ).get() );
    return index0 != -1 && index1 != -1 && ( ( index1 + 1 ) % boundaryCount  == index0 || ( index0 + 1 ) % boundaryCount == index1 ) ;
  }

  return false;
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
  for ( const PositionInFeature &pos : std::as_const( mLinkedSegments ) )
    mSource->moveVertex( newPosition.x(), newPosition.y(), pos.fid, pos.pos );
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

int ReosStructureVertexHandler_p::posInLine( SegmentId id ) const
{
  int index = mLinkedSegments.indexOf( id );
  if ( index >= 0 )
    return mLinkedSegments.at( index ).pos;

  return -1;
}

ReosPolylineStructureVectorLayerUndoCommandRemoveLine::ReosPolylineStructureVectorLayerUndoCommandRemoveLine( QgsFeatureId id,
    ReosPolylineStructureVectorLayer *structure )
  : mId( id ), mStructure( structure )
{
  Segment seg = mStructure->idToSegment( mId );

  VertexP vert0 = seg.at( 0 ).get();
  VertexP vert1 = seg.at( 1 ).get();

  SegmentId sid = InvalidSegment;
  if ( vert0->oneOtherLine( mId, &sid ) && sid != InvalidSegment )
  {
    mExistingLine0 = sid;
    mPosInExistingLine0 = vert0->posInLine( sid );
  }

  sid = InvalidSegment;
  if ( vert1->oneOtherLine( mId, &sid ) && sid != InvalidSegment )
  {
    mExistingLine1 = sid;
    mPosInExistingLine1 = vert1->posInLine( sid );
  }

  mBoundaryPos0 = mStructure->mBoundariesVertex.indexOf( mStructure->idToVertex( id, 0 ).get() );
  mBoundaryPos1 = mStructure->mBoundariesVertex.indexOf( mStructure->idToVertex( id, 1 ).get() );

}

void ReosPolylineStructureVectorLayerUndoCommandRemoveLine::redo()
{
  Segment seg = mStructure->mSegments.take( mId );
  mVert0 = seg.at( 0 );
  mVert1 = seg.at( 1 );
  seg.at( 0 )->detachLine( mId );
  seg.at( 1 )->detachLine( mId );

  if ( mBoundaryPos0 != -1 && !seg.at( 0 )->hasLineAttached() )
    mStructure->mBoundariesVertex.removeOne( seg.at( 0 ).get() );

  if ( mBoundaryPos1 != -1 && !seg.at( 1 )->hasLineAttached() )
    mStructure->mBoundariesVertex.removeOne( seg.at( 1 ).get() );
}

void ReosPolylineStructureVectorLayerUndoCommandRemoveLine::undo()
{
  VertexS vert0 = mVert0.lock();
  VertexS vert1 = mVert1.lock();

  if ( !vert0 )
  {
    if ( mExistingLine0 != InvalidSegment )
      vert0 = mStructure->idToVertex( mExistingLine0, mPosInExistingLine0 );

    if ( !vert0 ) //nothing exist,we need to create it
      vert0 = mStructure->createVertex( mId, 0 );
  }

  if ( !vert1 )
  {
    if ( mExistingLine1 != InvalidSegment )
      vert1 = mStructure->idToVertex( mExistingLine1, mPosInExistingLine1 );

    if ( !vert1 ) //nothing exist,we need to create it
      vert1 = mStructure->createVertex( mId, 1 );
  }

  Segment seg( {vert0, vert1} );

  seg.at( 0 )->attachLine( mId, 0 );
  seg.at( 1 )->attachLine( mId, 1 );
  mStructure->mSegments.insert( mId, seg );

  if ( mBoundaryPos0 != -1 &&  !mStructure->mBoundariesVertex.contains( vert0.get() ) )
    mStructure->mBoundariesVertex.insert( mBoundaryPos0, vert0.get() );

  if ( mBoundaryPos1 != -1 && !mStructure->mBoundariesVertex.contains( vert1.get() ) )
    mStructure->mBoundariesVertex.insert( mBoundaryPos1, vert1.get() );
}

ReosPolylineStructureVectorLayerUndoCommandAddLine::ReosPolylineStructureVectorLayerUndoCommandAddLine( QgsFeatureId idLineToAdd, const VertexS &vert0, const VertexS &vert1, bool onBoundary, ReosPolylineStructureVectorLayer *structure )
  : mIdToAdd( idLineToAdd ), mVert0( vert0 ), mVert1( vert1 )
  , mStructure( structure )
  , mOnBoundary( onBoundary )
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

  mBoundaryPos0 = mStructure->mBoundariesVertex.indexOf( vert0.get() );
  mBoundaryPos1 = mStructure->mBoundariesVertex.indexOf( vert1.get() );

  if ( mBoundaryPos0 == -1 && mOnBoundary )
    mBoundaryPos0 = mBoundaryPos1;

  if ( mBoundaryPos1 == -1 && mOnBoundary )
    mBoundaryPos1 = mBoundaryPos0 + 1;

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
      vert0 = mStructure->createVertex( mIdToAdd, 0 );
  }

  if ( !vert1 )
  {
    if ( mExistingLine1 != InvalidSegment )
      vert1 = mStructure->idToVertex( mExistingLine1, mPosInExistingLine1 ); //the shared ptr to the vertex has expired, try with the existing id;

    if ( !vert1 ) //nothing exist,we need to create it
      vert1 = mStructure->createVertex( mIdToAdd, 1 );
  }

  vert0->attachLine( mIdToAdd, 0 );
  vert1->attachLine( mIdToAdd, 1 );
  mStructure->mSegments.insert( mIdToAdd, {vert0, vert1} );

  if ( mBoundaryPos0 != -1 && !mStructure->isOnBoundary( vert0.get() ) )
    mStructure->mBoundariesVertex.insert( mBoundaryPos0, vert0.get() );

  if ( mBoundaryPos1 != -1 && !mStructure->isOnBoundary( vert1.get() ) )
    mStructure->mBoundariesVertex.insert( mBoundaryPos1, vert1.get() );

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
  if ( mBoundaryPos0 != -1 && !vert0->hasLineAttached() )
    mStructure->mBoundariesVertex.removeOne( vert0.get() );

  if ( !vert1 )
    vert1 = mStructure->idToVertex( mIdToAdd, 1 );

  vert1->detachLine( mIdToAdd );
  if ( mBoundaryPos1 != -1 && !vert1->hasLineAttached() )
    mStructure->mBoundariesVertex.removeOne( vert1.get() );

  mStructure->mSegments.remove( mIdToAdd );

  mVert0 = vert0;
  mVert1 = vert1;
}


ReosPolylineStructureVectorLayerUndoCommandMergeVertex::ReosPolylineStructureVectorLayerUndoCommandMergeVertex( const VertexS &vertexToRemove, const VertexS &vertexToKeep, ReosPolylineStructureVectorLayer *structure )
  : mVertexToKeep( vertexToKeep )
  , mVertexToRemove( vertexToRemove )
  , mStructure( structure )
{
  mInitialLinks0 = vertexToKeep->mLinkedSegments;
  mInitialLinks1 = vertexToRemove->mLinkedSegments;
}

void ReosPolylineStructureVectorLayerUndoCommandMergeVertex::redo()
{
  VertexS vertexToKeep = mVertexToKeep.lock();
  VertexS vertexToRemove = mVertexToRemove.lock();

  if ( !vertexToKeep )
  {
    ReosStructureVertexHandler_p::PositionInFeature existingPosition = mInitialLinks0.first();
    vertexToKeep = mStructure->idToVertex( existingPosition.fid, existingPosition.pos );
  }

  if ( !vertexToRemove )
  {
    ReosStructureVertexHandler_p::PositionInFeature existingPosition = mInitialLinks1.first();
    vertexToRemove = mStructure->idToVertex( existingPosition.fid, existingPosition.pos );
  }

  QList<ReosStructureVertexHandler_p::PositionInFeature> mergedLinks = mInitialLinks0;

  for ( const ReosStructureVertexHandler_p::PositionInFeature &pi : std::as_const( mInitialLinks1 ) )
  {
    Segment seg = mStructure->idToSegment( pi.fid );
    seg.at( pi.pos ) = vertexToKeep;
    if ( !mergedLinks.contains( pi ) )
      mergedLinks.append( pi );

    mStructure->mSegments.insert( pi.fid, seg );
  }

  vertexToKeep->mLinkedSegments = mergedLinks;
}

void ReosPolylineStructureVectorLayerUndoCommandMergeVertex::undo()
{
  VertexS vertexToKeep = mVertexToKeep.lock();
  VertexS vertexToRemove = mVertexToRemove.lock();

  if ( !vertexToKeep )
  {
    ReosStructureVertexHandler_p::PositionInFeature existingPosition = mInitialLinks0.first();
    vertexToKeep = mStructure->idToVertex( existingPosition.fid, existingPosition.pos );
  }

  if ( !vertexToRemove )
  {
    ReosStructureVertexHandler_p::PositionInFeature existingPosition = mInitialLinks1.first();
    vertexToRemove = mStructure->createVertex( existingPosition.fid, existingPosition.pos );
  }


  vertexToKeep->mLinkedSegments = mInitialLinks0;
  vertexToRemove->mLinkedSegments = mInitialLinks1;

  for ( const ReosStructureVertexHandler_p::PositionInFeature &pi : std::as_const( mInitialLinks0 ) )
  {
    Segment seg = mStructure->idToSegment( pi.fid );
    seg.at( pi.pos ) = vertexToKeep;
    mStructure->mSegments.insert( pi.fid, seg );
  }

  for ( const ReosStructureVertexHandler_p::PositionInFeature &pi : std::as_const( mInitialLinks1 ) )
  {
    Segment seg = mStructure->idToSegment( pi.fid );
    seg.at( pi.pos ) = vertexToRemove;
    mStructure->mSegments.insert( pi.fid, seg );
  }
}

