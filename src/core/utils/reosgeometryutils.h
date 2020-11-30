#ifndef REOSGEOMETRYUTILS_H
#define REOSGEOMETRYUTILS_H

#include <QPolygonF>

class ReosGeometryUtils
{
  public:
    ReosGeometryUtils();

    //! Subtract \a polygon2 from \a polygon1
    static QPolygonF polygonSubtract( const QPolygonF &polygon1, const QPolygonF &polygon2 );

    static bool polygonIsInsidePolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static bool polygonIntersectPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static bool lineIntersectPolygon( const QPolygonF &line, const QPolygonF &polygon );
    static bool pointIsInsidePolygon( const QPointF &point, const QPolygonF &polygon );
};

#endif // REOSGEOMETRYUTILS_H
