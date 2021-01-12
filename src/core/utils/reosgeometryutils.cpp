
#include <qgsgeometry.h>
#include <qgslinestring.h>
#include <qgspolygon.h>

#include "reosgeometryutils.h"


ReosGeometryUtils::ReosGeometryUtils()
{

}

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

  QPolygonF ret = intersection.asQPolygonF();

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

QPolygonF ReosGeometryUtils::polygonUnion( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );

  QPolygonF ret = geom1.combine( geom2 ).asQPolygonF();

  if ( !ret.isEmpty() && ret.last() == ret.first() )
    ret.removeLast();

  return ret;
}