/***************************************************************************
                      reosgeometryutils.h
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

#ifndef REOSGEOMETRYUTILS_H
#define REOSGEOMETRYUTILS_H

#include <math.h>

#include <QPolygonF>

#include "reoscore.h"

enum class ReosInclusionType
{
  None,
  Partial,
  Total
};

class REOSCORE_EXPORT ReosGeometryUtils
{
  public:
    ReosGeometryUtils() = default;

    //! Subtract \a polygon2 from \a polygon1
    static QPolygonF polygonSubtract( const QPolygonF &polygon1, const QPolygonF &polygon2 );

    static ReosInclusionType polygonIsInsidePolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static ReosInclusionType polylineIsInsidePolygon( const QPolygonF &polyline, const QPolygonF &polygon );
    static bool pointIsInsidePolygon( const QPointF &point, const QPolygonF &polygon );

    static bool polygonIntersectPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static bool lineIntersectPolygon( const QPolygonF &line, const QPolygonF &polygon );

    static QPolygonF polygonFitInPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static QPolygonF polygonCutByPolygon( const QPolygonF &polygon1, const QPolygonF &polygon2 );
    static QPolygonF polygonCutByPolygons( const QPolygonF &polygon1, const QList<QPolygonF> &polygons );
    static QPolygonF polygonUnion( const QPolygonF &polygon1, const QPolygonF &polygon2 );

    //! Return the second vertex's index of the closest segment of \a polyline from \a point, returns -1 if none
    static int closestSegment( const QPointF &point, const QPolygonF &polyline );
};

#endif // REOSGEOMETRYUTILS_H
