
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

QPolygonF ReosGeometryUtils::polygonSubtract( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QVector<QgsPoint> poly1;
  for ( const QPointF &pt : polygon1 )
    poly1.append( QgsPoint( pt ) );
  QgsGeometry geom1( new QgsPolygon( new QgsLineString( poly1 ) ) );

  QVector<QgsPoint> poly2;
  for ( const QPointF &pt : polygon2 )
    poly2.append( QgsPoint( pt ) );
  QgsGeometry geom2( new QgsPolygon( new QgsLineString( poly1 ) ) );

  QgsGeometry result = geom1.difference( geom2 );

  return result.asQPolygonF();
}

bool ReosGeometryUtils::polygonIsInsidePolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 )
{
  QgsGeometry geom1( createQgsPolygon( polygon1 ) );
  QgsGeometry geom2( createQgsPolygon( polygon2 ) );
  return geom2.contains( geom1 );
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

bool ReosGeometryUtils::lineIntersectPolygon( const QPolygonF &line, const QPolygonF &polygon )
{
  QgsGeometry qgsLine( QgsLineString::fromQPolygonF( line ) );
  QgsGeometry qgsPlygon( createQgsPolygon( polygon ) );

  if ( !qgsPlygon.intersects( qgsLine ) )
    return false;

  QgsGeometry intersection = qgsPlygon.intersection( qgsLine );
  return intersection.type() == QgsWkbTypes::PolygonGeometry;
}

bool ReosGeometryUtils::pointIsInsidePolygon( const QPointF &point, const QPolygonF &polygon )
{
  QgsGeometry geom( createQgsPolygon( polygon ) );
  QgsPointXY pt( point );
  return geom.contains( &pt );
}
