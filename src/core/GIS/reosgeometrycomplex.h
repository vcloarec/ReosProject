/***************************************************************************
  reosgeometrycomplex.h - ReosGeometryStructure

 ---------------------
 begin                : 5.2.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSGEOMETRYCOMPLEX_H
#define REOSGEOMETRYCOMPLEX_H

#define SIP_NO_FILE

#include "reosdataobject.h"

class QPainter;

class ReosMapExtent;
class ReosSpatialPosition;

class ReosGeometryComplex : public ReosDataObject
{
    Q_OBJECT
  public:
    virtual ~ReosGeometryComplex() = default;
    virtual QObject *data() = 0;

    //! Returns the extent of the structure in \a crs coordinate system
    virtual ReosMapExtent extent( const QString &crs ) const = 0;

    /**
     * Searches a closed polygon containing \a position, returns the polygon in \a position coordinates.
     * If \a allowBoundary is false, return nothing if the polygon has a boundary vertex.
     */
    virtual QPolygonF searchPolygon( const ReosSpatialPosition &position, bool allowBoundary = true ) const = 0;

    virtual QString crs() const = 0;

    virtual void render( void *mapSettings, QPainter *painter, bool highlight, const QPointF &highlightPosition ) const = 0;

  signals:
    void geometryChanged();
};


#endif // REOSGEOMETRYCOMPLEX_H
