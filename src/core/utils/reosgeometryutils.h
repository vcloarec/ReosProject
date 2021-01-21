#ifndef REOSGEOMETRYUTILS_H
#define REOSGEOMETRYUTILS_H

#include <QPolygonF>


enum class ReosInclusionType
{
  None,
  Partial,
  Total
};

class ReosGeometryUtils
{
  public:
    ReosGeometryUtils();

    //! Subtract \a polygon2 from \a polygon1
    static QPolygonF polygonSubtract( const QPolygonF &polygon1, const QPolygonF &polygon2 );

    static ReosInclusionType polygonIsInsidePolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static ReosInclusionType polylineIsInsidePolygon( const QPolygonF &polyline, const QPolygonF &polygon );
    static bool pointIsInsidePolygon( const QPointF &point, const QPolygonF &polygon );

    static bool polygonIntersectPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static bool lineIntersectPolygon( const QPolygonF &line, const QPolygonF &polygon );

    static QPolygonF polygonFitInPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static QPolygonF polygonCutByPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static QPolygonF polygonUnion( const QPolygonF &polygon1, const QPolygonF &polygon2 );

    //! Return the second vertex's index of the closest segment of \a polyline from \a point, returns -1 if none
    static int closestSegment( const QPointF &point, const QPolygonF &polyline );
};

#endif // REOSGEOMETRYUTILS_H
