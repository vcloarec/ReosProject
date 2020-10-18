#ifndef REOSGEOMETRYUTILS_H
#define REOSGEOMETRYUTILS_H

#include <QPolygonF>

class ReosGeometryUtils
{
  public:
    ReosGeometryUtils();

    //! Subtract \a polygon2 from \a polygon1
    static QPolygonF polygonSubtract( const QPolygonF &polygon1, const QPolygonF &polygon2 );

};

#endif // REOSGEOMETRYUTILS_H
