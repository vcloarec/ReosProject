/***************************************************************************
  reospolygonwatershed.h - ReosPolygonWatershed

---------------------
begin                : 26.8.2026
copyright            : (C) 2026 by Vincent Cloarec
email                : vcloarec at gmail dot com
***************************************************************************
*                                                                         *
*   This program is free software; you can redistribute it and/or modify  *
*   it under the terms of the GNU General Public License as published by  *
*   the Free Software Foundation; either version 2 of the License, or     *
*   (at your option) any later version.                                   *
*                                                                         *
***************************************************************************/

#ifndef REOSPOLYGONWATERSHED_H
#define REOSPOLYGONWATERSHED_H

#define SIP_NO_FILE

#include <memory>

#include <QPolygonF>

#include "reosgeometrycomplex.h"

class ReosWatershed;


class ReosPolygonWatershed : public ReosGeometryComplex
{
  public:
    ReosPolygonWatershed();

    static ReosPolygonWatershed *createPolygonWatershed( const QString &wktCrs );

    virtual ReosPolygonWatershed *clone() const = 0;
    virtual void addWatershed( const QPolygonF &watershed, const QString &crs, const QString &id ) = 0;
    virtual void addWatershed( ReosWatershed *watershed ) = 0;
    virtual void removeWatershed( const QString &id ) = 0;

    virtual void setCrs( const QString &crs ) = 0;

    /**
     * Returns the position of the closest vertex to the given \a position within the specified tolerance of the watershed with id \a watershedId.
     * If a vertex is found, the function also returns the indices of the previous, current, and next vertices in the polygon.
     */
    virtual QPointF closestVertex( const QString &watershedId, const QPointF &position, const QString &destinationCrs, double tolerance, int &prevIndex, int &index, int &nextIndex ) const = 0;

    /**
     * Returns the position of the vertex at the given \a index within the watershed with id \a watershedId, transformed to the specified \a destinationCrs coordinate reference system.
     */
    virtual QPointF vertexPosition( const QString &watershedId, int index, const QString &destinationCrs ) const = 0;

    /**
     * Returns the extent of the watershed with id \a watershedId, transformed to the specified \a destinationCrs coordinate reference system.
     */
    virtual ReosMapExtent watershedExtent( const QString &watershedI, const QString &destinationCrs ) const = 0;

    /**
     * Returns the delineating polygon of the watershed with id \a watershedId, transformed to the specified \a destinationCrs coordinate reference system.
     */
    virtual QPolygonF watershedDelineating( const QString &watershedId, const QString &destinationCrs ) const = 0;

    /**
     * Returns the ID of the watershed that contains the given \a position in the specified \a destinationCrs coordinate reference system.
     */
    virtual QString watershedUnderPosition( const QPointF &position, const QString &destinationCrs ) const = 0;

    /**
     * Returns whether the vertex at the given \a index within the watershed with id \a watershedId can be moved
     * to the specified \a destinationPosition in the \a destinationCrs coordinate reference system.
     */

    virtual bool canVertexBeMoved( const QString &watershedId, int index, const QPointF &destinationPosition, const QString &destinationCrs ) const = 0;

    /**
     * Moves the vertex at the given \a index within the watershed with id \a watershedId to the specified \a destinationPosition in the \a destinationCrs coordinate reference system.
     */
    virtual void moveVertex( const QString &watershedId, int index, const QPointF &destinationPosition, const QString &destinationCrs ) = 0;

    virtual void clear() = 0;

    QPolygonF searchPolygon( const ReosSpatialPosition &, bool = true ) const override;
};

#endif // REOSPOLYGONWATERSHED_H
