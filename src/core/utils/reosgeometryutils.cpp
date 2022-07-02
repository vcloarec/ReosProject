/***************************************************************************
                      reosgeometryutils.cpp
                     --------------------------------------
Date                 : 10-2020
Copyright            : (C) 2020 by Vincent Cloarec
email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include <math.h>
#include <qgsgeometry.h>
#include <qgslinestring.h>
#include <qgspolygon.h>
#include <qgsdistancearea.h>

#include "reosgeometryutils.h"


static QgsPolygon *createQgsPolygon( const QPolygonF &polygon )
{
  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( polygon ) );
  std::unique_ptr<QgsPolygon> qgsPolygon = std::make_unique<QgsPolygon>( linestring.release() );
  return qgsPolygon.release();
}

static QgsLineString *createQgsPolyline( const QPolygonF &polygon )
{
  std::unique_ptr<QgsLineString> linestring( QgsLineString::fromQPolygonF( polygon ) );
  return linestring.release();
}

QPolygonF ReosGeometryUtils::polygonSubtract( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  QgsGeometry result = geom1.difference( geom2 );

  return result.asQPolygonF();
}

ReosInclusionType ReosGeometryUtils::polygonIsInsidePolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );
  if ( geom2.contains( geom1 ) )
    return ReosInclusionType::Total;

  if ( geom1.intersection( geom2 ).area() > 0 )
    return ReosInclusionType::Partial;

  return ReosInclusionType::None;

}

bool ReosGeometryUtils::polygonIntersectPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  if ( !geom2.intersects( geom1 ) )
    return false;

  QgsGeometry intersection = geom2.intersection( geom1 );
  return intersection.type() == QgsWkbTypes::PolygonGeometry;

}

ReosInclusionType ReosGeometryUtils::polylineIsInsidePolygon( const QPolygonF &polyline, const QPolygonF &polygon )
{
  QgsGeometry qgsPolyline( createQgsPolyline( polyline ) );
  QgsGeometry qgsPolygon( createQgsPolygon( polygon ) );

  if ( qgsPolygon.contains( qgsPolyline ) )
    return ReosInclusionType::Total;

  if ( qgsPolyline.intersection( qgsPolygon ).length() > 0 )
    return ReosInclusionType::Partial;

  return ReosInclusionType::None;
}

bool ReosGeometryUtils::lineIntersectPolygon( const QPolygonF &line, const QPolygonF &polygon )
{
  QgsGeometry qgsLine( QgsLineString::fromQPolygonF( line ) );
  QgsGeometry qgsPolygon( createQgsPolygon( polygon ) );

  if ( !qgsPolygon.intersects( qgsLine ) )
    return false;

  QgsGeometry intersection = qgsPolygon.intersection( qgsLine );
  return intersection.type() == QgsWkbTypes::PolygonGeometry;
}

bool ReosGeometryUtils::pointIsInsidePolygon( const QPointF &point, const QPolygonF &polygon )
{
  QgsGeometry geom( createQgsPolygon( polygon ) );
  QgsPointXY pt( point );
  return geom.contains( &pt );
}

QPolygonF ReosGeometryUtils::polygonFitInPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  QgsGeometry intersection = geom1.intersection( geom2 );

  QPolygonF ret;

  if ( !intersection.isMultipart() )
    ret = intersection.asQPolygonF();
  else
  {
    // take the bigger part
    double area = 0;
    QgsGeometry selected;
    for ( QgsAbstractGeometry::const_part_iterator it = intersection.const_parts_begin();
          it != intersection.const_parts_end();
          ++it )
    {
      if ( ( *it )->area() > area )
      {
        QgsGeometry geom( ( *it )->clone() );
        ret = geom.asQPolygonF();
        area = ( *it )->area();
      }
    }
  }

  if ( !ret.isEmpty() && ret.last() == ret.first() )
    ret.removeLast();

  return ret;

}

QPolygonF ReosGeometryUtils::polygonCutByPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  QPolygonF ret = geom1.difference( geom2 ).asQPolygonF();

  if ( !ret.isEmpty() && ret.last() == ret.first() )
    ret.removeLast();

  return ret;
}

QPolygonF ReosGeometryUtils::polygonCutByPolygons( const QPolygonF &polygon1, const QList<QPolygonF> &polygons )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );

  if ( polygons.isEmpty() )
    return polygon1;

  // merge polygons
  QVector<QgsGeometry> polygonsVector( polygons.count() );

  for ( int i = 0; i < polygons.count(); ++i )
    polygonsVector[i] = QgsGeometry( createQgsPolygon( polygons.at( i ) ) );

  QgsGeometry mergedPolygons = QgsGeometry::unaryUnion( polygonsVector );


  QPolygonF polyResult = geom1.difference( mergedPolygons ).asQPolygonF();
  if ( polyResult.first() == polyResult.last() )
    polyResult.removeLast();

  return polyResult;
}

QPolygonF ReosGeometryUtils::polygonUnion( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  QPolygonF ret = geom1.combine( geom2 ).asQPolygonF();

  if ( !ret.isEmpty() && ret.last() == ret.first() )
    ret.removeLast();

  return ret;
}

int ReosGeometryUtils::closestSegment( const QPointF &point, const QPolygonF &polyline )
{
  QgsGeometry poly( createQgsPolyline( polyline ) );

  QgsPointXY returnPoint;
  int secondIndex;
  poly.closestSegmentWithContext( QgsPointXY( point ), returnPoint, secondIndex );

  return secondIndex;
}

double ReosGeometryUtils::projectedPointDistanceFromBegining( const QPointF &point, const QPolygonF &polyline )
{
  QgsGeometry polyGeom = QgsGeometry::fromQPolygonF( polyline );
  return polyGeom.lineLocatePoint( QgsGeometry::fromQPointF( point ) );
}

double ReosGeometryUtils::length( const QPolygonF &polyline )
{
  QgsGeometry polyGeom = QgsGeometry::fromQPolygonF( polyline );
  return polyGeom.length();
}

