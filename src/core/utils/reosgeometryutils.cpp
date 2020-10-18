
#include <qgsgeometry.h>
#include <qgslinestring.h>
#include <qgspolygon.h>

#include "reosgeometryutils.h"


ReosGeometryUtils::ReosGeometryUtils()
{

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
