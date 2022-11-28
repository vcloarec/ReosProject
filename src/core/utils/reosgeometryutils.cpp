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
#include <qgsgeometryutils.h>
#include <qgsgeometryengine.h>
#include "reosgeometryutils.h"
#include "reosprocess.h"


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
  const QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  QPolygonF ret = geom1.combine( geom2 ).asQPolygonF();

  if ( !ret.isEmpty() && ret.last() == ret.first() )
    ret.removeLast();

  return ret;
}

int ReosGeometryUtils::closestSegment( const QPointF &point, const QPolygonF &polyline )
{
  const QgsGeometry poly( createQgsPolyline( polyline ) );

  QgsPointXY returnPoint;
  int secondIndex;
  poly.closestSegmentWithContext( QgsPointXY( point ), returnPoint, secondIndex );

  return secondIndex;
}

double ReosGeometryUtils::projectedPointDistanceFromBegining( const QPointF &point, const QPolygonF &polyline )
{
  const QgsGeometry polyGeom = QgsGeometry::fromQPolygonF( polyline );
  return polyGeom.lineLocatePoint( QgsGeometry::fromQPointF( point ) );
}

double ReosGeometryUtils::length( const QPolygonF &polyline )
{
  QgsGeometry polyGeom = QgsGeometry::fromQPolygonF( polyline );
  return polyGeom.length();
}

static QVector<QPolygonF> sortMultiPolyline( const QgsGeometry &multiPolyline, const QgsGeometry &basePolyline, QVector<double> *distanceFromBegining )
{
  if ( distanceFromBegining )
    distanceFromBegining->clear();

  if ( multiPolyline.isMultipart() )
  {
    const QgsMultiPolylineXY &multiResult = multiPolyline.asMultiPolyline();
    QMap<double, QPolygonF> sortedResult;
    for ( const QgsPolylineXY &poly : multiResult )
    {
      if ( poly.size() > 1 )
      {
        double posFirst = basePolyline.lineLocatePoint( QgsGeometry::fromPointXY( poly.first() ) );
        double posLast = basePolyline.lineLocatePoint( QgsGeometry::fromPointXY( poly.last() ) );

        if ( posFirst > posLast )
        {
          int size = poly.size();
          QPolygonF reversed( size );
          for ( int i = 0; i < size; ++i )
            reversed[i] = poly.at( size - 1 - i ).toQPointF();
          sortedResult.insert( posLast, reversed );
        }
        else
        {
          int size = poly.size();
          QPolygonF polyF( size );
          for ( int i = 0; i < size; ++i )
            polyF[i] = poly.at( i ).toQPointF();
          sortedResult.insert( posFirst, polyF );
        }
      }
    }

    if ( distanceFromBegining )
    {
      *distanceFromBegining = sortedResult.keys().toVector();
    }
    return sortedResult.values().toVector();
  }

  QVector<QPolygonF> ret;
  ret.append( multiPolyline.asQPolygonF() );
  if ( distanceFromBegining )
    distanceFromBegining->append( 0 );

  return ret;
}

QVector<QPolygonF> ReosGeometryUtils::cutPolylineOutsidePolygon( const QPolygonF &polyline, const QPolygonF &polygon, QVector<double> *distanceFromBegining )
{
  const QgsGeometry polylineGeom( createQgsPolyline( polyline ) );
  const QgsGeometry polygonGeom( createQgsPolygon( polygon ) );

  QgsGeometry resultGeom = polygonGeom.intersection( polylineGeom );

  return sortMultiPolyline( resultGeom, polylineGeom, distanceFromBegining );
}

QVector<QPolygonF> ReosGeometryUtils::cutPolylineInsidePolygon( const QPolygonF &polyline, const QPolygonF &polygon, QVector<double> *distanceFromBegining )
{
  const QgsGeometry polylineGeom( createQgsPolyline( polyline ) );
  const QgsGeometry polygonGeom( createQgsPolygon( polygon ) );

  QgsGeometry resultGeom = polylineGeom.difference( polygonGeom );

  return sortMultiPolyline( resultGeom, polylineGeom, distanceFromBegining );
}

bool ReosGeometryUtils::segmentIntersect( const QPointF &pta1, const QPointF &pta2, const QPointF &ptb1, const QPointF &ptb2, QPointF &intersect )
{
  QgsPoint intersection;
  bool isIntersection = false;
  bool res = QgsGeometryUtils::segmentIntersection( QgsPoint( pta1 ), QgsPoint( pta2 ), QgsPoint( ptb1 ), QgsPoint( ptb2 ), intersection, isIntersection, 1e-8, true );
  intersect = intersection.toQPointF();

  return res;
}

QRectF ReosGeometryUtils::boundingBox( const QPolygonF &polygon, bool &ok )
{
  ok = false;
  double xMin = std::numeric_limits<double>::max();
  double yMin = std::numeric_limits<double>::max();
  double xMax = -std::numeric_limits<double>::max();
  double yMax = -std::numeric_limits<double>::max();

  for ( const QPointF &pt : polygon )
  {
    double x = pt.x();
    double y = pt.y();

    if ( std::isnan( x ) || std::isnan( y ) )
      continue;

    if ( x < xMin )
      xMin = x;
    if ( x > xMax )
      xMax = x;
    if ( y < yMin )
      yMin = y;
    if ( y > yMax )
      yMax = y;

    ok = true;
  }

  return QRectF( xMin, yMin, xMax - xMin, yMax - yMin );
}

ReosRasterMemory<double> ReosGeometryUtils::rasterizePolygon( const QPolygonF &polygon,
    const ReosRasterExtent &rasterExtent,
    ReosRasterExtent &finalRasterExtent,
    int &xOri,
    int &yOri,
    bool precise,
    ReosProcess *process )
{
  ReosRasterMemory<double> ret;

  QgsGeometry polygeom( createQgsPolygon( polygon ) );
  QgsRectangle bbox = polygeom.boundingBox();

  std::unique_ptr< QgsGeometryEngine > polyEngine( QgsGeometry::createGeometryEngine( polygeom.constGet( ) ) );
  if ( !polyEngine )
    return ret;
  polyEngine->prepareGeometry();

  QPoint minXminY = rasterExtent.mapToCell( QPointF( bbox.xMinimum(), bbox.yMinimum() ) );
  QPoint maxXmaxY = rasterExtent.mapToCell( QPointF( bbox.xMaximum(), bbox.yMaximum() ) );

  xOri = std::clamp( rasterExtent.xCellSize() > 0 ? minXminY.x() : maxXmaxY.x(), 0, rasterExtent.xCellCount() - 1 );;
  yOri = std::clamp( rasterExtent.yCellSize() > 0 ? minXminY.y() : maxXmaxY.y(), 0, rasterExtent.yCellCount() - 1 );;

  int xEnd =  std::clamp( rasterExtent.xCellSize() < 0 ? minXminY.x() : maxXmaxY.x(), 0, rasterExtent.xCellCount() - 1 );
  int yEnd =  std::clamp( rasterExtent.yCellSize() < 0 ? minXminY.y() : maxXmaxY.y(), 0, rasterExtent.yCellCount() - 1 );

  int colCount =  std::abs( xEnd - xOri )  + 1;
  int rowCount =  std::abs( yEnd - yOri )  + 1;

  double destXOri = rasterExtent.xMapOrigin() + xOri * rasterExtent.xCellSize();
  double destYOri = rasterExtent.yMapOrigin() + yOri * rasterExtent.yCellSize();

  finalRasterExtent = ReosRasterExtent( destXOri, destYOri, colCount, rowCount, rasterExtent.xCellSize(), rasterExtent.yCellSize() );

  ret = ReosRasterMemory<double>( rowCount, colCount );
  ret.reserveMemory();
  ret.fill( 0 );

  double cellArea = std::fabs( rasterExtent.xCellSize() * rasterExtent.yCellSize() );

  QgsGeometry pixelRectGeometry;
  for ( int xi = 0; xi < colCount; ++xi )
  {
    for ( int yi = 0; yi < rowCount; ++yi )
    {
      if ( process && process->isStop() )
        return ret;
      QgsPoint cellCenter( finalRasterExtent.cellCenterToMap( QPoint( xi, yi ) ) );
      if ( precise )
      {
        //from QGIS code ( QgsRasterAnalysisUtils::statisticsFromPreciseIntersection() )
        QgsRectangle cellRect( cellCenter.x() - rasterExtent.xCellSize() * 0.5,
                               cellCenter.y() - rasterExtent.yCellSize() * 0.5,
                               cellCenter.x() + rasterExtent.xCellSize() * 0.5,
                               cellCenter.y() + rasterExtent.yCellSize() * 0.5 );
        cellRect.normalize();
        pixelRectGeometry = QgsGeometry::fromRect( cellRect );
        QPolygonF polyTest = polygeom.asQPolygonF();
        QPolygonF rectTest = pixelRectGeometry.asQPolygonF();
        if ( !pixelRectGeometry.isNull() && polyEngine->intersects( pixelRectGeometry.constGet() ) )
        {
          //intersection
          const QgsGeometry intersectGeometry = pixelRectGeometry.intersection( polygeom );
          if ( !intersectGeometry.isEmpty() )
          {
            const double intersectionArea = intersectGeometry.area();
            if ( intersectionArea > 0.0 )
            {
              ret.setValue( yi, xi, intersectionArea / cellArea );
            }
          }
        }
      }
      else
      {
        if ( polyEngine->contains( &cellCenter ) )
        {
          ret.setValue( yi, xi, 1 );
        }
      }

    }
  }

  return ret;
}

