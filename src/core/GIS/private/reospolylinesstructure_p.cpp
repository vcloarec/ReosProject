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
#include <QStack>

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
                                      , QStringLiteral( "internalLayer" )
                                      , QStringLiteral( "memory" ) ) )
{
}

QgsPointXY ReosGeometryStructure_p::toLayerCoordinates( const ReosSpatialPosition &position ) const
{
  QgsCoordinateReferenceSystem crs;
  crs.createFromWkt( position.crs() );

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
  qgsCrs.createFromWkt( crs );

  return QgsCoordinateTransform( qgsCrs, mVectorLayer->crs(), QgsProject::instance() );
}

const QgsCoordinateTransform ReosGeometryStructure_p::toDestinationTransform( const QString &destinationCrs ) const
{
  QgsCoordinateReferenceSystem qgsCrs;
  qgsCrs.createFromWkt( destinationCrs );

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

QgsRectangle ReosGeometryStructure_p::layerZone( const ReosMapExtent &zone ) const
{
  QgsCoordinateTransform transform = toLayerTransform( zone.crs() );

  QgsRectangle rect( zone.toRectF() );
  QgsRectangle layerRect = rect;

  if ( transform.isValid() )
  {
    try
    {
      layerRect = transform.transform( rect );
    }
    catch ( QgsCsException & )
    {
      layerRect = rect;
    }
  }
  return layerRect;
}

QString ReosGeometryStructure_p::crs() const
{
  return mVectorLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED );
}


QgsPointXY ReosGeometryStructure_p::transformCoordinates( const QgsPointXY &position, const QgsCoordinateTransform &transform ) const
{
  if ( transform.isValid() )
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
  else
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
  mVectorLayer->undoStack()->blockSignals( true );

  QgsField field;
  field.setType( QVariant::String );
  field.setName( QStringLiteral( "classId" ) );
  mVectorLayer->addAttribute( field );
  mVectorLayer->commitChanges( false );
  mVectorLayer->undoStack()->clear();
  mVectorLayer->undoStack()->blockSignals( false );

  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, [this]
  {
    mRawLinesDirty = true;
  } );
  connect( mVectorLayer->undoStack(), &QUndoStack::indexChanged, this, &ReosDataObject::dataChanged );
  connect( mVectorLayer.get(), &QgsVectorLayer::geometryChanged, this, &ReosPolylinesStructure::geometryChanged );

  mVerticesBoundaryRequest.setMaxCost( 2000 );
}

SegmentId ReosPolylineStructureVectorLayer::addSegmentToVectorLayer( const QgsPointXY &pt1, const QgsPointXY &pt2, QString clId )
{
  QgsGeometry geomSegment( new QgsLineString( {pt1, pt2} ) );
  QgsFeature feat;
  feat.setFields( mVectorLayer->fields(), true );
  feat.setAttribute( 0, clId );
  feat.setGeometry( geomSegment );
  bool success = mVectorLayer->addFeature( feat ) ;
  Q_ASSERT( success );

  return feat.id();
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
  data.extent = mVectorLayer->extent().toRectF();
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
  SegmentId sid = addSegmentToVectorLayer( point0, point1 );
  VertexS vert0 = createVertex( sid, 0 );
  VertexS firstVertex = vert0;

  VertexS vert1 = createVertex( sid, 1 );
  QgsPointXY firstPoint( point0 );
  mBoundariesVertex.append( vert0.get() );
  mBoundariesVertex.append( vert1.get() );
  mSegments.insert( sid, {vert0, vert1} );

  for ( int i = 2; i < data.boundaryPointCount ; ++i )
  {
    vert0 = vert1;
    point0 = point1;
    point1 = QgsPointXY( vertices.at( i ) );
    SegmentId sid = addSegmentToVectorLayer( point0, point1 );
    vert0->attachLine( sid, 0 );
    vert1 = createVertex( sid, 1 );
    mBoundariesVertex.append( vert1.get() );
    mSegments.insert( sid, {vert0, vert1} );
  }
  sid = addSegmentToVectorLayer( point1, firstPoint );
  vert1->attachLine( sid, 0 );
  firstVertex->attachLine( sid, 1 );
  mSegments.insert( sid, {vert1, firstVertex} );

  //create internal lines
  for ( const QVector<int> &line : data.internalLines )
  {
    const QgsPointXY point0 = vertices.at( line.at( 0 ) );
    const QgsPointXY point1 = vertices.at( line.at( 1 ) );

    VertexS vert0 = purposeVertex( point0, mTolerance );
    VertexS vert1 = purposeVertex( point1, mTolerance );
    SegmentId sid = addSegmentToVectorLayer( point0, point1 );

    if ( !vert0 )
      vert0 = createVertex( sid, 0 );
    else
      vert0->attachLine( sid, 0 );

    if ( !vert1 )
      vert1 = createVertex( sid, 1 );
    else
      vert1->attachLine( sid, 1 );

    mSegments.insert( sid, {vert0, vert1} );
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

  QVector<QPointF> holePoints;
  encodedElement.getData( QStringLiteral( "hole-points" ), holePoints );
  mHolePoints.clear();
  for ( const QPointF &pt : std::as_const( holePoints ) )
    mHolePoints.append( QgsPointXY( pt ) );

  buildGeometry( data );

  encodedElement.getData( QStringLiteral( "classes" ), mClassIds );

  QMap<int, QString> boundaryConditionId;
  encodedElement.getData( QStringLiteral( "boundary-condition" ), boundaryConditionId );
  const QList<int> indexes = boundaryConditionId.keys();
  mVectorLayer->undoStack()->blockSignals( true );
  for ( int ind : indexes )
  {
    const QString clId = boundaryConditionId.value( ind );
    SegmentId sid = 0;
    if ( segmentId( mBoundariesVertex.at( ind ), mBoundariesVertex.at( ( ind + 1 ) % mBoundariesVertex.count() ), sid ) )
    {
      mVectorLayer->changeAttributeValue( sid, 0, clId );
      if ( !mClassIds.contains( clId ) )
        mClassIds.insert( clId, QString() );
    }
  }
  mVectorLayer->undoStack()->clear();
  mVectorLayer->undoStack()->blockSignals( false );
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

  QMap<int, QString> boundaryConditionId;

  for ( int i = 0; i < mBoundariesVertex.count(); ++i )
  {
    SegmentId sid = 0;
    if ( segmentId( mBoundariesVertex.at( i ), mBoundariesVertex.at( ( i + 1 ) % mBoundariesVertex.count() ), sid ) )
    {
      QString clId = classId( sid );
      if ( !clId.isEmpty() )
        boundaryConditionId.insert( i, clId );
    }
  }

  element.addData( QStringLiteral( "boundary-condition" ), boundaryConditionId );
  element.addData( QStringLiteral( "classes" ), mClassIds );

  QVector<QPointF> holePoints;
  for ( const QgsPointXY &hpt : mHolePoints )
    holePoints.append( hpt.toQPointF() );
  element.addData( QStringLiteral( "hole-points" ), holePoints );
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
  QString clId = classId( lineId );

  bool onBoundary = isOnBoundary( seg );

  QgsPointXY point0 = oldFeature.geometry().vertexAt( 0 );
  QgsPointXY point1 = oldFeature.geometry().vertexAt( 1 );

  mVectorLayer->beginEditCommand( "Insert vertex" );
  mVectorLayer->undoStack()->push( //remove the old line
    new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( lineId, this ) );
  mVectorLayer->deleteFeature( lineId );

  SegmentId sid0 = addSegmentToVectorLayer( point0, projPoint, clId );
  SegmentId sid1 = addSegmentToVectorLayer( projPoint, point1, clId );
  mVectorLayer->undoStack()->push( //add first line
    new ReosPolylineStructureVectorLayerUndoCommandAddLine(
      sid0,
      seg.at( 0 ),
      nullptr,
      onBoundary,
      this ) );

  VertexS createdVertex =  oppositeVertex( seg.at( 0 ).get(), sid0 );

  mVectorLayer->undoStack()->push( // add second line
    new ReosPolylineStructureVectorLayerUndoCommandAddLine(
      sid1,
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
  fids = vert->attachedLines().values();

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

bool ReosPolylineStructureVectorLayer::segmentId( VertexP vert0, VertexP vert1, SegmentId &sid ) const
{
  const QSet<SegmentId> ids = vert0->attachedLines();
  for ( const SegmentId &segId : ids )
  {
    Segment seg = idToSegment( segId );
    for ( int i = 0; i < 2; ++i )
      if ( seg.at( i ).get() == vert1 )
      {
        sid = segId;
        return true;
      }
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
  {
    throw ReosException( QStringLiteral( "topologic error" ) );
  }
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

    if ( vert0 && vert1 && isOnBoundary( vert0.get() ) && isOnBoundary( vert1.get() ) )
    {
      QgsPointXY middle( ( pointXY0.x() + pointXY1.x() ) / 2, ( pointXY0.y() + pointXY1.y() ) / 2 );
      if ( !exterior.contains( &middle ) )
        continue;
    }

    if ( vert0 )
      pointXY0 = vert0->position();

    if ( vert1 )
      pointXY1 = vert1->position();

    if ( ( !vert0 && !exterior.contains( &pointXY0 ) ) ||
         ( !vert1 && !exterior.contains( &pointXY1 ) ) )
      continue;

    SegmentId sid = addSegmentToVectorLayer( pointXY0, pointXY1 );
    mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayerUndoCommandAddLine(
                                       sid,
                                       vert0,
                                       vert1,
                                       false,
                                       this ) );
    somethingDone = true;
  }

  if ( !somethingDone )
  {
    mVectorLayer->undoStack()->blockSignals( true );
    mVectorLayer->destroyEditCommand();
    mVectorLayer->undoStack()->canRedoChanged( false );
    mVectorLayer->undoStack()->blockSignals( false );
  }
  else
    mVectorLayer->endEditCommand();
}

QPolygonF ReosPolylineStructureVectorLayer::polyline( const QString &, const QString & ) const
{
  return QPolygonF();
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
    catch ( QgsCsException & )
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
    const QgsCoordinateTransform transform( toDestinationTransform( destinationCrs ) );
    for ( int i = 0; i < mBoundariesVertex.count(); ++i )
    {
      const VertexP &vert = mBoundariesVertex.at( i );
      ret[i] = vert->position( transform );
    }

    return ret;
  }

  return QPolygonF();
}


QString ReosPolylineStructureVectorLayer::boundaryClassId( int i ) const
{
  VertexP v1 = mBoundariesVertex.at( i );
  VertexP v2 = mBoundariesVertex.at( ( i + 1 ) % mBoundariesVertex.count() );

  SegmentId sid = 0;
  if ( !segmentId( v1, v2, sid ) )
    return QString();

  return classId( sid );
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

#if QT_VERSION < QT_VERSION_CHECK(5, 15, 0)
  QgsFeatureIterator fit = mVectorLayer->getFeatures( ids.toSet() );
#else
  QgsFeatureIterator fit = mVectorLayer->getFeatures( QSet<SegmentId>( ids.begin(), ids.end() ) );
#endif

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

  QString boundaryClass0;
  QString boundaryClass1;

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
      {
        boundaryVert0 = oppVertex;
        SegmentId sid = 0;
        if ( segmentId( oppVertex.get(), vert, sid ) )
          boundaryClass0 = classId( sid );
      }
      else
      {
        boundaryvert1 = oppVertex;
        SegmentId sid = 0;
        if ( segmentId( oppVertex.get(), vert, sid ) )
          boundaryClass1 = classId( sid );
      }
    }

    mVectorLayer->deleteFeature( id );
    mVectorLayer->undoStack()->push( //remove the old line
      new ReosPolylineStructureVectorLayerUndoCommandRemoveLine( id, this ) );
  }

  if ( onBoundary )
  {
    QgsPointXY pt0 = boundaryVert0->position();
    QgsPointXY pt1 = boundaryvert1->position();

    QString classId;
    if ( boundaryClass0 == boundaryClass1 )
      classId = boundaryClass0;

    QgsFeatureIds idForClass0 = classIdToSegments( boundaryClass0 );
    if ( idForClass0.isEmpty() )
    {
      mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeRemoveBoundaryCondition( boundaryClass0, this ) );
    }

    QgsFeatureIds idForClass1 = classIdToSegments( boundaryClass1 );
    if ( idForClass1.isEmpty() )
    {
      mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeRemoveBoundaryCondition( boundaryClass1, this ) );
    }

    SegmentId sid = addSegmentToVectorLayer( pt0, pt1, classId );
    mVectorLayer->undoStack()->push(
      new ReosPolylineStructureVectorLayerUndoCommandAddLine(
        sid,
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
  rect = layerZone( zone );
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
    catch ( QgsCsException & )
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

static double ccwAngle( const QgsVector &v1, const QgsVector &v2 )
{
  return  std::fmod( v1.angle() / M_PI * 180 + 360.0 - v2.angle() / M_PI * 180, 360.0 );
}

static double cwAngle( const QgsVector &v1, const QgsVector &v2 )
{
  return  std::fmod( v2.angle() / M_PI * 180 + 360.0 - v1.angle() / M_PI * 180, 360.0 );
}


QList<VertexP> ReosPolylineStructureVectorLayer::searchVerticesPolygon( const QgsPointXY &layerPoint, bool allowBoundary ) const
{
  QgsFeatureIterator it = mVectorLayer->getFeatures();
  QgsFeature feat;

  std::unique_ptr<QgsLineString> horizontalLine(
    new QgsLineString( {layerPoint, QgsPointXY( mVectorLayer->extent().xMaximum(),  layerPoint.y() )} ) );
  QgsGeometry horizontalLineGeom( horizontalLine.release() );

  QVector<QPair<QgsFeatureId, double>> featuresDistance;

  while ( it.nextFeature( feat ) )
  {
    QgsGeometry geom = feat.geometry();
    if ( geom.intersects( horizontalLineGeom ) )
    {
      double dist = geom.distance( QgsGeometry( new QgsPoint( layerPoint ) ) );
      featuresDistance.append( {feat.id(), dist} );
    }
  }

  if ( featuresDistance.isEmpty() )
    return QList<VertexP>();

  QList<VertexP> vertices;

  std::sort( featuresDistance.begin(), featuresDistance.end(),
  []( const QPair<QgsFeatureId, double> &fd1, const QPair<QgsFeatureId, double> &fd2 ) {return fd1.second < fd2.second;} );

  SegmentId lineId = 0;

  VertexP firstVertex = nullptr;
  VertexP prevVertex = nullptr;
  VertexP nextVertex = nullptr;

  bool ccw = false;

  QgsVector v1;
  QgsVector v2;

  if ( v1.length() > 0 && v2.length() > 0 )
  {
    v1 = v1.normalized();
    v2 = v2.normalized();
  }

  auto captureFirstLine = [&]
  {
    bool firstFound = false;
    vertices.clear();
    firstVertex = nullptr;
    while ( !featuresDistance.isEmpty() && !firstFound )
    {
      lineId = featuresDistance.takeFirst().first;
      Segment seg = idToSegment( lineId );
      firstVertex = seg.at( 0 ).get();
      nextVertex = seg.at( 1 ).get();

      if ( !allowBoundary && ( isOnBoundary( firstVertex ) || isOnBoundary( nextVertex ) ) )
        continue;

      firstFound = true;
      ccw = ccwAngle( QgsPointXY( nextVertex->position() ) - QgsPointXY( firstVertex->position() ),
                      layerPoint - QgsPointXY( firstVertex->position() ) ) > 180;

      QgsVector v1 = layerPoint - firstVertex->position();
      QgsVector v2 = QgsPointXY( nextVertex->position() ) - firstVertex->position();

      if ( v1.length() > 0 && v2.length() > 0 )
      {
        v1 = v1.normalized();
        v2 = v2.normalized();
      }
    }

    vertices.append( firstVertex );
  };

  auto cmp = [&prevVertex, &nextVertex, this, &ccw]( SegmentId id1, SegmentId id2 )
  {
    VertexP other1 = oppositeVertexPointer( nextVertex, id1 );
    QgsVector v = QgsPointXY( prevVertex->position() ) - nextVertex->position();
    QgsVector v1 = QgsPointXY( other1->position() ) - nextVertex->position();

    VertexP other2 = oppositeVertexPointer( nextVertex, id2 );
    QgsVector v2 = QgsPointXY( other2->position() ) - nextVertex->position();

    if ( ccw )
      return cwAngle( v, v1 ) < cwAngle( v, v2 );
    else
      return cwAngle( v, v1 ) >= cwAngle( v, v2 );
  };

  typedef QPair<SegmentId, VertexP> Line;
  QStack<Line> lineToThreat;

  QSet<VertexP> threatedVertices;
  threatedVertices.insert( firstVertex );
  prevVertex = firstVertex;
  bool found = false;
  while ( !found && !featuresDistance.isEmpty() )
  {
    threatedVertices.clear();
    captureFirstLine();
    if ( !firstVertex )
      return QList<VertexP>();
    prevVertex = firstVertex;

    while ( !threatedVertices.contains( nextVertex ) )
    {
      if ( !allowBoundary && isOnBoundary( nextVertex ) )
        return QList<VertexP>();

      QList<SegmentId> lines = qgis::setToList( nextVertex->attachedLines() );
      lines.removeOne( lineId );
      if ( !lines.isEmpty() )
      {
        vertices.append( nextVertex );
        std::sort( lines.begin(), lines.end(), cmp );
        for ( SegmentId sid : lines )
          lineToThreat.push( {sid, nextVertex} );
      }

      if ( !lineToThreat.isEmpty() )
      {
        const Line line = lineToThreat.pop();
        lineId = line.first;
        if ( !threatedVertices.contains( line.second ) )
          threatedVertices.insert( line.second );

        while ( !vertices.isEmpty() && vertices.last() != line.second )
        {
          threatedVertices.remove( vertices.last() );
          vertices.removeLast();
        }

        prevVertex = line.second;
        nextVertex = oppositeVertexPointer( line.second, line.first );
      }
      else
      {
        if ( featuresDistance.isEmpty() )
          return QList<VertexP>();
        else
          break;
      }
    }

    while ( !vertices.isEmpty() && vertices.first() != nextVertex )
      vertices.removeFirst();

    if ( vertices.count() > 2 )
    {
      QPolygonF points;
      for ( VertexP vert : std::as_const( vertices ) )
      {
        points.append( vert->position() );
      }
      points.append( points.first() );
      QgsGeometry geom = QgsGeometry::fromQPolygonF( points );
      found = geom.contains( &layerPoint );
    }
  }

  if ( !found )
    return QList<VertexP>();

  return vertices;
}

QString ReosPolylineStructureVectorLayer::classId( SegmentId id ) const
{
  QgsFeatureIterator it = mVectorLayer->getFeatures( QgsFeatureIds( {id} ) );

  QgsFeature feat;
  if ( it.nextFeature( feat ) )
    return feat.attribute( 0 ).toString();

  return QString();
}

QgsFeatureIds ReosPolylineStructureVectorLayer::classIdToSegments( const QString &classId )
{
  QgsFeatureIds ret;

  QgsFeatureIterator it = mVectorLayer->getFeatures();

  QgsFeature feat;
  while ( it.nextFeature( feat ) )
  {
    if ( feat.attribute( 0 ).toString() == classId )
      ret.insert( feat.id() );
  }

  return ret;
}

QList<VertexP> ReosPolylineStructureVectorLayer::boundaryFromClassId( const QString &clId ) const
{
  // search for the first with classId

  QList<VertexP> vertices;

  //firt find one that is not from the class to be sure to have all from the beginning of the classe
  int index = -1;
  for ( int i = 0; i < mBoundariesVertex.count(); ++i )
  {
    VertexP vert = mBoundariesVertex.at( i );
    const QSet<SegmentId> al = vert->attachedLines();
    bool found = false;
    for ( SegmentId sid : al )
      if ( classId( sid ) == clId )
      {
        found = true;
        break;
      }

    if ( !found )
    {
      index = i;
      break;
    }
  }

  if ( index == -1 )
  {
    if ( mBoundariesVertex.isEmpty() )
      return QList<VertexP>();
    else
      return mBoundariesVertex;
  }

  int start = index;

  //index = ( index + 1 ) % mBoundariesVertex.size();
  bool found = false;

  do
  {
    VertexP vert = mBoundariesVertex.at( index );
    const QSet<SegmentId> al = vert->attachedLines();
    bool inClass = false;
    for ( SegmentId sid : al )
      if ( classId( sid ) == clId )
      {
        vertices.append( vert );
        found = true;
        inClass = true;
      }

    if ( !inClass && found )
      break;

    index = ( index + 1 ) % mBoundariesVertex.size();
  }
  while ( index != start );

  return vertices;
}

QPolygonF ReosPolylineStructureVectorLayer::searchPolygon( const ReosSpatialPosition &position, bool allowBoundary ) const
{
  QgsPointXY layerPoint = toLayerCoordinates( position );

  QList<VertexP> vertices = searchVerticesPolygon( layerPoint, allowBoundary );
  const QgsCoordinateTransform toDest = toDestinationTransform( position.crs() );
  QPolygonF ret;
  for ( VertexP v : std::as_const( vertices ) )
    ret.append( v->position( toDest ) );

  return ret;
}

void ReosPolylineStructureVectorLayer::addHolePoint( const ReosSpatialPosition &position )
{
  mVectorLayer->beginEditCommand( tr( "Add hole" ) );
  const QgsPointXY pt = toLayerCoordinates( position );
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeAddHolePoint( pt, this ) );
  mVectorLayer->endEditCommand();
}

QList<QPointF> ReosPolylineStructureVectorLayer::holePoints( const QString &destinationCrs ) const
{
  QList<QPointF> ret;
  QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );
  for ( const QgsPointXY &pt : mHolePoints )
    ret.append( transformCoordinates( pt, transform ).toQPointF() );

  return ret;
}

int ReosPolylineStructureVectorLayer::searchHolePoint( const ReosMapExtent &zone ) const
{
  QgsRectangle searchZone = layerZone( zone );

  for ( int i = 0; i < mHolePoints.count(); ++i )
    if ( searchZone.contains( mHolePoints.at( i ) ) )
      return i;

  return -1;
}

void ReosPolylineStructureVectorLayer::moveHolePoint( int index, const ReosSpatialPosition &position )
{
  QgsPointXY newPosition = toLayerCoordinates( position );
  mVectorLayer->beginEditCommand( tr( "Move hole" ) );
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeMoveHolePoint( index, newPosition, this ) );
  mVectorLayer->endEditCommand();
}

void ReosPolylineStructureVectorLayer::removeHolePoint( int index )
{
  mVectorLayer->beginEditCommand( tr( "Remove hole" ) );
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeRemoveHolePoint( index, this ) );
  mVectorLayer->endEditCommand();
}


ReosPolylinesStructure::Data ReosPolylineStructureVectorLayer::structuredLinesData( const QString &destinationCrs ) const
{
  const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );
  Data data;
  QHash<VertexP, int> vertexPointerToIndex;
  QMap<QPair<int, int>, int> lineIndexes;

  data.extent = extent( destinationCrs ).toRectF();
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
    {
      data.internalLines.append( {ind[0], ind[1]} );
      lineIndexes.insert( {ind[0], ind[1]}, data.internalLines.count() - 1 );
    }
  }


  for ( const QgsPointXY &holePoint : mHolePoints )
  {
    const QList<VertexP> holeVertices = searchVerticesPolygon( holePoint, false );
    QVector<int> hole;
    int size = holeVertices.count();
    for ( int i = 0; i < size; ++i )
    {
      int v1 = vertexPointerToIndex.value( holeVertices.at( i ) );
      int v2 = vertexPointerToIndex.value( holeVertices.at( ( i + 1 ) % size ) );

      auto it = lineIndexes.find( {v1, v2} );
      if ( it == lineIndexes.end() )
        it = lineIndexes.find( {v2, v1} );

      if ( it != lineIndexes.end() )
        hole.append( it.value() );
    }

    data.holes.append( hole );
  }

  return data;
}

QVector<QLineF> ReosPolylineStructureVectorLayer::rawLines( const QString &destinationCrs ) const
{
  if ( mRawLinesDirty || destinationCrs != mCurrentLineCrs )
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

QPolygonF ReosPolylineStructureVectorLayer::linesOnBoundaryFromTo( ReosGeometryStructureVertex *vertexFrom, ReosGeometryStructureVertex *vertexTo, const QString &destinationCrs ) const
{
  VertexP v1 = static_cast<VertexP>( vertexFrom );
  VertexP v2 = static_cast<VertexP>( vertexTo );
  int indexFrom = mBoundariesVertex.indexOf( v1 );
  int indexTo = mBoundariesVertex.indexOf( v2 );

  if ( indexFrom < 0 || indexTo < 0 || indexFrom == indexTo )
  {
    return QPolygonF();
  }

  double lenght = 0;
  QVector<QgsPointXY> points;
  int index = indexFrom;
  int boundariesSize = mBoundariesVertex.count();
  QgsPointXY pt1 = v1->position();
  points.append( pt1 );

  while ( indexFrom != ( index + 1 ) % boundariesSize && indexTo != index % boundariesSize )
  {
    index++;
    QgsPointXY pt2 = mBoundariesVertex.at( index % boundariesSize )->position();
    lenght += pt1.distance( pt2 );
    points.append( pt2 );
    pt1 = pt2;
  }

  double lenghtOtherWise = 0;
  QVector<QgsPointXY> pointsOtherWise;
  index = indexFrom;

  pt1 = v1->position();
  pointsOtherWise.append( pt1 );

  while ( indexFrom != ( index - 1 + boundariesSize ) % boundariesSize &&
          indexTo != ( index + boundariesSize ) % boundariesSize )
  {
    index--;
    QgsPointXY pt2 = mBoundariesVertex.at( ( index + boundariesSize ) % boundariesSize )->position();
    lenghtOtherWise += pt1.distance( pt2 );
    if ( lenghtOtherWise > lenght )
      break;
    pointsOtherWise.append( pt2 );
    pt1 = pt2;
  }

  QVector<QgsPointXY> retPoints;
  if ( lenght < lenghtOtherWise )
    retPoints = points;
  else
    retPoints = pointsOtherWise;

  const QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );

  QPolygonF ret( retPoints.count() );

  for ( int i = 0; i < retPoints.count(); ++i )
    ret[i] = transformCoordinates( retPoints.at( i ), transform ).toQPointF();

  return ret;
}

QList<VertexP> ReosPolylineStructureVectorLayer::boundaryVerticesFromTo( VertexP v1, VertexP v2 ) const
{
  int indexFrom = mBoundariesVertex.indexOf( v1 );
  int indexTo = mBoundariesVertex.indexOf( v2 );

  if ( indexFrom < 0 || indexTo < 0 || indexFrom == indexTo )
  {
    return QList<VertexP>();
  }

  double lenght = 0;
  int index = indexFrom;
  int boundariesSize = mBoundariesVertex.count();
  QgsPointXY pt1 = v1->position();
  QList<VertexP> vertices;
  vertices.append( v1 );

  while ( indexFrom != ( index + 1 ) % boundariesSize && indexTo != index % boundariesSize )
  {
    index++;
    QgsPointXY pt2 = mBoundariesVertex.at( index % boundariesSize )->position();
    lenght += pt1.distance( pt2 );
    pt1 = pt2;
    vertices.append( mBoundariesVertex.at( index % boundariesSize ) );
  }

  double lenghtOtherWise = 0;
  QVector<QgsPointXY> pointsOtherWise;
  index = indexFrom;
  QList<VertexP> verticesOtherWise;
  verticesOtherWise.append( v1 );
  pt1 = v1->position();
  pointsOtherWise.append( pt1 );

  while ( indexFrom != ( index - 1 + boundariesSize ) % boundariesSize &&
          indexTo != ( index + boundariesSize ) % boundariesSize )
  {
    index--;
    QgsPointXY pt2 = mBoundariesVertex.at( ( index + boundariesSize ) % boundariesSize )->position();
    lenghtOtherWise += pt1.distance( pt2 );
    if ( lenghtOtherWise > lenght )
      break;
    pt1 = pt2;
    verticesOtherWise.append( mBoundariesVertex.at( ( index + boundariesSize ) % boundariesSize ) );
  }

  if ( lenghtOtherWise > lenght )
    return vertices;
  else
    return verticesOtherWise;
}

bool ReosPolylineStructureVectorLayer::canBoundaryConditionBeAdded( ReosGeometryStructureVertex *vertexFrom, ReosGeometryStructureVertex *vertexTo ) const
{
  VertexP v1 = static_cast<VertexP>( vertexFrom );
  VertexP v2 = static_cast<VertexP>( vertexTo );

  if ( !v1 )
    return false;

  const QSet<SegmentId> al1 = v1->attachedLines();
  for ( SegmentId sid : al1 )
    if ( !classId( sid ).isEmpty() )
      return false;

  if ( !v2 )
    return true;

  const QSet<SegmentId> al2 = v2->attachedLines();
  for ( SegmentId sid : al2 )
    if ( !classId( sid ).isEmpty() )
      return false;

  QList<VertexP> vertices = boundaryVerticesFromTo( v1, v2 );

  for ( int i = 1; i < vertices.count() - 1; ++i )
  {
    const QSet<SegmentId> al = vertices.at( i )->attachedLines();
    for ( SegmentId sid : al )
      if ( !classId( sid ).isEmpty() )
        return false;
  }

  return true;
}

void ReosPolylineStructureVectorLayer::addBoundaryCondition( ReosGeometryStructureVertex *vertexFrom, ReosGeometryStructureVertex *vertexTo, const QString &name )
{
  VertexP v1 = static_cast<VertexP>( vertexFrom );
  VertexP v2 = static_cast<VertexP>( vertexTo );

  QList<VertexP> newConditon = boundaryVerticesFromTo( v1, v2 );

  mVectorLayer->beginEditCommand( tr( "Add boundary condition" ) );
  QString newBoundaryId = QUuid::createUuid().toString();
  for ( int i = 0; i < newConditon.count() - 1; ++i )
  {
    SegmentId sid;
    if ( !segmentId( newConditon.at( i ), newConditon.at( i + 1 ), sid ) )
      return;

    mVectorLayer->changeAttributeValue( sid, 0, newBoundaryId );
  }
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeAddBoundaryCondition( newBoundaryId, name, this ) );

  mVectorLayer->endEditCommand();
}

void ReosPolylineStructureVectorLayer::removeBoundaryCondition( const QString &classID )
{
  mVectorLayer->beginEditCommand( tr( "Remove boundary condition" ) );

  const QgsFeatureIds fids = classIdToSegments( classID );
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeRemoveBoundaryCondition( classID, this ) );
  for ( QgsFeatureId fid : fids )
    mVectorLayer->changeAttributeValue( fid, 0, QString() );

  mVectorLayer->endEditCommand();
}

void ReosPolylineStructureVectorLayer::changeClassValue( const QString &classId, const QVariant &value )
{
  mVectorLayer->beginEditCommand( tr( "Rename class" ) );
  mVectorLayer->undoStack()->push( new ReosPolylineStructureVectorLayeChangeClassValue( classId, value, this ) );
  mVectorLayer->endEditCommand();
}

QStringList ReosPolylineStructureVectorLayer::classes() const
{
  return mClassIds.keys();
}

QRectF ReosPolylineStructureVectorLayer::classExtent( const QString &classId, const QString &destinationCrs ) const
{
  QgsFeatureIterator it = mVectorLayer->getFeatures();

  QgsFeature feat;
  QgsRectangle rect;
  rect.setMinimal();
  while ( it.nextFeature( feat ) )
  {
    if ( feat.attribute( 0 ).toString() == classId )
    {
      rect.include( feat.geometry().vertexAt( 0 ) );
      rect.include( feat.geometry().vertexAt( 1 ) );
    }
  }

  QRectF ret = rect.toRectF();
  QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );
  if ( transform.isValid() )
  {
    try
    {
      rect = transform.transform( rect );
      ret = rect.toRectF();
      return ret;
    }
    catch ( QgsCsException & )
    {
      return ret;
    }
  }

  return ret;
}

QPointF ReosPolylineStructureVectorLayer::boundaryConditionCenter( const QString &clId, const QString &destinationCrs ) const
{
  const QList<VertexP> vertices = boundaryFromClassId( clId );
  QgsCoordinateTransform transform = toDestinationTransform( destinationCrs );
  QgsPointSequence points;

  for ( VertexP vert : vertices )
    points.append( QgsPoint( vert->position( transform ) ) );

  QgsGeometry geom( new QgsLineString( points ) );
  double midLength = geom.length() / 2;

  return  geom.interpolate( midLength ).asPoint().toQPointF();
}

QVariant ReosPolylineStructureVectorLayer::value( const QString &classId ) const
{
  if ( !mClassIds.contains( classId ) || mClassIds.value( classId ).toString().isEmpty() )
    return QString( "Name not defined" );

  return mClassIds.value( classId );
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

double ReosPolylineStructureVectorLayer::tolerance( const QString &wktCrs ) const
{
  QgsCoordinateReferenceSystem destCrs;
  destCrs.createFromWkt( wktCrs );

  QgsUnitTypes::DistanceUnit destUnit = destCrs.mapUnits();
  QgsUnitTypes::DistanceUnit layerUnit = mVectorLayer->crs().mapUnits();

  return QgsUnitTypes::fromUnitToUnitFactor( layerUnit, destUnit ) * mTolerance;
}

QString ReosPolylineStructureVectorLayer::crs() const
{
  return mVectorLayer->crs().toWkt( QgsCoordinateReferenceSystem::WKT_PREFERRED_SIMPLIFIED );
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

  emit mStructure->geometryChanged();
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

  emit mStructure->geometryChanged();
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

  emit mStructure->geometryChanged();
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

  emit mStructure->geometryChanged();
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

  emit mStructure->geometryChanged();
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

  emit mStructure->geometryChanged();
}


ReosPolylineStructureVectorLayeRemoveHolePoint::ReosPolylineStructureVectorLayeRemoveHolePoint( int index, ReosPolylineStructureVectorLayer *structure )
  : mIndex( index )
  , mStructure( structure )
{
  mPosition = mStructure->mHolePoints.at( index );
}

void ReosPolylineStructureVectorLayeRemoveHolePoint::redo()
{
  mStructure->mHolePoints.removeAt( mIndex );

  emit mStructure->geometryChanged();
}

void ReosPolylineStructureVectorLayeRemoveHolePoint::undo()
{
  mStructure->mHolePoints.insert( mIndex, mPosition );

  emit mStructure->geometryChanged();
}

ReosPolylineStructureVectorLayeMoveHolePoint::ReosPolylineStructureVectorLayeMoveHolePoint( int index, const QgsPointXY &point, ReosPolylineStructureVectorLayer *structure )
  : mIndex( index )
  , mNewPosition( point )
  , mStructure( structure )
{
  mOldPosition = mStructure->mHolePoints.at( index );
}

void ReosPolylineStructureVectorLayeMoveHolePoint::redo()
{
  mStructure->mHolePoints[mIndex] = mNewPosition;

  emit mStructure->geometryChanged();
}

void ReosPolylineStructureVectorLayeMoveHolePoint::undo()
{
  mStructure->mHolePoints[mIndex] = mOldPosition;

  emit mStructure->geometryChanged();
}

ReosPolylineStructureVectorLayeAddHolePoint::ReosPolylineStructureVectorLayeAddHolePoint( const QgsPointXY &point, ReosPolylineStructureVectorLayer *structure )
  : mPosition( point )
  , mStructure( structure )
{}

void ReosPolylineStructureVectorLayeAddHolePoint::redo()
{
  mStructure->mHolePoints.append( mPosition );

  emit mStructure->geometryChanged();
}

void ReosPolylineStructureVectorLayeAddHolePoint::undo()
{
  mStructure->mHolePoints.removeLast();

  emit mStructure->geometryChanged();

}

ReosPolylineStructureVectorLayeAddBoundaryCondition::ReosPolylineStructureVectorLayeAddBoundaryCondition( const QString classId, const QVariant &value, ReosPolylineStructureVectorLayer *structure )
  : QUndoCommand()
  , mClassId( classId )
  , mValue( value )
  , mStructure( structure )
{

}

void ReosPolylineStructureVectorLayeAddBoundaryCondition::redo()
{
  mStructure->mClassIds.insert( mClassId, mValue );
  emit mStructure->classesChanged();
  emit mStructure->boundaryConditionAdded( mClassId );
}

void ReosPolylineStructureVectorLayeAddBoundaryCondition::undo()
{
  mStructure->mClassIds.remove( mClassId );
  emit mStructure->classesChanged();
  emit mStructure->boundaryConditionRemoved( mClassId );
}

ReosPolylineStructureVectorLayeRemoveBoundaryCondition::ReosPolylineStructureVectorLayeRemoveBoundaryCondition( const QString &classId, ReosPolylineStructureVectorLayer *structure )
  : mClassId( classId )
  , mStructure( structure )
{
  mValue = mStructure->mClassIds.value( classId );
}

void ReosPolylineStructureVectorLayeRemoveBoundaryCondition::redo()
{
  mStructure->mClassIds.remove( mClassId );
  emit mStructure->classesChanged();
  emit mStructure->boundaryConditionRemoved( mClassId );
}

void ReosPolylineStructureVectorLayeRemoveBoundaryCondition::undo()
{
  mStructure->mClassIds.insert( mClassId, mValue );
  emit mStructure->classesChanged();
  emit mStructure->boundaryConditionAdded( mClassId );
}

ReosPolylineStructureVectorLayeChangeClassValue::ReosPolylineStructureVectorLayeChangeClassValue( const QString &classId, const QVariant &newValue, ReosPolylineStructureVectorLayer *structure )
  : mClassId( classId )
  , mNewValue( newValue )
  , mStructure( structure )
{
  mOldValue = mStructure->mClassIds.value( classId );
}

void ReosPolylineStructureVectorLayeChangeClassValue::redo()
{
  mStructure->mClassIds.insert( mClassId, mNewValue );
  emit mStructure->classesChanged();
}

void ReosPolylineStructureVectorLayeChangeClassValue::undo()
{
  mStructure->mClassIds.insert( mClassId, mOldValue );
  emit mStructure->classesChanged();
}
