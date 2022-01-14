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

#include "reosmapextent.h"


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
}

bool ReosPolylineStructureVectorLayer::hasPolyline( const QString &id )
{
  return mReosToQgisId.contains( id );
}

QgsGeometry ReosPolylineStructureVectorLayer::toGeometry( const QPolygonF &polyline, const QString &sourceCrs ) const
{
  std::unique_ptr<QgsLineString> lineString( QgsLineString::fromQPolygonF( polyline ) );
  QgsCoordinateReferenceSystem crs;
  crs.fromWkt( sourceCrs );

  QgsCoordinateTransform transform( crs, mVectorLayer->crs(), QgsProject::instance() );
  if ( transform.isValid() )
  {
    try
    {
      lineString->transform( transform );
    }
    catch ( ... )
    {
      return QgsGeometry( QgsLineString::fromQPolygonF( polyline ) );
    }
  }

  return QgsGeometry( lineString.release() );
}

QPolygonF ReosPolylineStructureVectorLayer::toPolygonF( const QgsGeometry &geom, const QString &destinationCrs ) const
{
  if ( geom.isNull() )
    return QPolygonF();

  QgsCoordinateReferenceSystem crs;
  crs.fromWkt( destinationCrs );

  QgsGeometry destinationGeom = geom;

  QgsCoordinateTransform transform( mVectorLayer->crs(), crs, QgsProject::instance() );
  if ( transform.isValid() )
  {
    try
    {
      destinationGeom.transform( transform );
      return destinationGeom.asQPolygonF();
    }
    catch ( ... )
    {}
  }

  return geom.asQPolygonF();
}

ReosPolylineStructureVectorLayer *ReosPolylineStructureVectorLayer::clone()
{
  std::unique_ptr<ReosPolylineStructureVectorLayer> other( new ReosPolylineStructureVectorLayer );
  other->mReosToQgisId = mReosToQgisId;
  other->mVectorLayer.reset( mVectorLayer->clone() );

  return other.release();
}

void ReosPolylineStructureVectorLayer::addPolylines( const QPolygonF &polyline, const QString &sourceCrs, const QString &id )
{
//  QgsCoordinateReferenceSystem sourceQgsCrs;
//  sourceQgsCrs.createFromString( sourceCrs );
//  QgsGeometry geom( createQgsPolyline( polyline ) );

//  QgsCoordinateTransform transform( sourceQgsCrs, mVectorLayer->crs(), QgsProject::instance() );
//  if ( transform.isValid() )
//  {
//    try
//    {
//      geom.transform( transform );
//    }
//    catch ( QgsCsException &e )
//    {
//    }
//  }

//  QgsFeature feat;
//  feat.setGeometry( geom );
//  mVectorLayer->addFeature( feat );

//  if ( id.isEmpty() )
//  {
//    const QString strId = QUuid::createUuid().toString();
//    mReosToQgisId.insert( strId, feat.id() );
//    mQgisToReos.insert( feat.id(), strId );
//  }
//  else
//  {
//    mReosToQgisId.insert( id, feat.id() );
//    mQgisToReos.insert( feat.id(), id );
//  }
}

QPolygonF ReosPolylineStructureVectorLayer::polyline( const QString &destinationCrs, const QString &id ) const
{
//  QgsFeatureId fid = 0;;
//  if ( !id.isEmpty() )
//  {
//    auto it = mReosToQgisId.find( id );
//    if ( it == mReosToQgisId.end() )
//      return QPolygonF();

//    fid = it.value();
//  }
//  else
//  {
//    QgsFeatureIterator it = mVectorLayer->getFeatures();
//    QgsFeature feat;
//    if ( !it.nextFeature( feat ) )
//      return QPolygonF();

//    fid = feat.id();
//  }

//  QgsCoordinateReferenceSystem destQgsCrs;
//  destQgsCrs.createFromString( destinationCrs );

//  QgsGeometry geom = mVectorLayer->getGeometry( fid );

//  QgsCoordinateTransform transform( mVectorLayer->crs(), destQgsCrs, QgsProject::instance() );
//  if ( transform.isValid() )
//  {
//    try
//    {
//      geom.transform( transform );
//    }
//    catch ( QgsCsException &e )
//    {
//    }
//  }

//  return geom.asQPolygonF();
}

void ReosPolylineStructureVectorLayer::setBoundary( const QPolygonF &polyline, const QString &sourceCrs )
{
  QgsGeometry geom = toGeometry( polyline, sourceCrs );

  QgsGeometry existingBoundary = mVectorLayer->getGeometry( mBoundaryFeature );
  if ( existingBoundary.isNull() )
  {
    QgsFeature feat;
    feat.setGeometry( geom );
    mVectorLayer->addFeature( feat );
    mBoundaryFeature = feat.id();
  }
  else
  {
    mVectorLayer->changeGeometry( mBoundaryFeature, geom );
  }

  mBoundariesVertex.clear();

  int i = 0;
  for ( auto it = geom.vertices_begin(); it != geom.vertices_end(); ++it )
  {
    mBoundariesVertex.append( std::make_shared<ReosStructureVertexHandler_p>( mBoundaryFeature, i ) );
    ++i;
  }
}

QPolygonF ReosPolylineStructureVectorLayer::boundary( const QString &destinationCrs ) const
{
  return toPolygonF( mVectorLayer->getGeometry( mBoundaryFeature ), destinationCrs );
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

    //qDebug() << "found " << geom.vertexAt( closestIndex ).toQPointF();
    QPointF pos = geom.vertexAt( closestIndex ).toQPointF();
    if ( rect.contains( pos ) && distance < minDist )
    {
      selectedFid = feat.id();
      vertexIndex = closestIndex;
    }
  }

  if ( foundOne && selectedFid == mBoundaryFeature && vertexIndex >= 0 )
    return mBoundariesVertex.at( vertexIndex ).get();

  return nullptr;
}

QPointF ReosPolylineStructureVectorLayer::vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const
{
  QgsCoordinateReferenceSystem destinationCrs;
  destinationCrs.createFromWkt( crs );
  QgsCoordinateTransform transform( mVectorLayer->crs(), destinationCrs, QgsProject::instance() );

  return static_cast<ReosStructureVertexHandler_p *>( vertex )->position( mVectorLayer.get(), transform );
}


ReosStructureVertexHandler_p::ReosStructureVertexHandler_p( QgsFeatureId fid, int pos )
{
  mLinkedFeatures.append( PositionInFeature( {fid, pos} ) );
}

QPointF ReosStructureVertexHandler_p::position( QgsVectorLayer *source, const QgsCoordinateTransform &transform )
{
  if ( !source || mLinkedFeatures.isEmpty() )
    return QPointF();

  const PositionInFeature &pf = mLinkedFeatures.at( 0 );

  const QgsPoint &pt = source->getGeometry( pf.fid ).vertexAt( pf.pos );

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
