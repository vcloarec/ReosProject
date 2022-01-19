/***************************************************************************
  reospolylinesstructures.h - ReosPolylinesStructures

 ---------------------
 begin                : 10.1.2022
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
#ifndef REOSPOLYLINESSTRUCTURES_H
#define REOSPOLYLINESSTRUCTURES_H

#include <QPolygonF>
#include <QObject>

#include <memory>

#include "reosdataobject.h"

class ReosSpatialPosition;
class ReosMapExtent;
class QUndoStack;


class ReosGeometryStructureVertex
{
};

class ReosGeometryStructure : public ReosDataObject
{
  public:
    virtual ~ReosGeometryStructure() = default;
    virtual QObject *data() = 0;
};


class ReosPolylinesStructure : public ReosGeometryStructure
{
    Q_OBJECT
  public:

    //! Creates and returns polylines structure with specified \a crs
    static std::unique_ptr<ReosPolylinesStructure> createPolylineStructure( const QString &crs );

    //! Creates and returns polylines structure with specified \a crs
    static std::unique_ptr<ReosPolylinesStructure> createPolylineStructure( const QPolygonF &boundary, const QString &crs );

    //! Adds a \a polyline to the structure with an identifier \a id
    virtual void addPolylines( const QPolygonF &polyline, const QString &sourceCrs = QString(), const QString &id = QString() ) = 0;

    //! Returns the polyline with identifier \a id
    virtual QPolygonF polyline( const QString &destinationCrs = QString(), const QString &id = QString() ) const = 0;

    //! Returns the boundary of the structure in \a destinationCrs cordinate system
    virtual QPolygonF boundary( const QString &destinationCrs ) const = 0;

    //! Removes all the entities in the structure
    virtual void removeAll() = 0;

    //! Translates the polyline with \a id (boundary one if void string)
    virtual void translate( const QPointF &translation, const QString &crs, const QString &id = QString() ) = 0;

    //! Returns whether the \a vertex can be moves to the new position \a newPosition
    virtual bool vertexCanBeMoved( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition ) const = 0;

    //! Moves \a vertex to the new positon \a newPosition
    virtual void moveVertex( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition ) = 0;

    //! Inserts a vertex in the line with \a lineId at position \a point
    virtual void insertVertex( const ReosSpatialPosition &point, qint64 lineId ) = 0;

    //! Removes \a vertex
    virtual void removeVertex( ReosGeometryStructureVertex *vertex ) = 0 ;

    //! Returns the extent of the structure in \a crs coordinate system
    virtual ReosMapExtent extent( const QString &crs ) const = 0;

    //! Search the closest vertex of the center of \a zone and in this zone, returns a pointer to the vertex
    virtual ReosGeometryStructureVertex *searchForVertex( const ReosMapExtent &zone ) const = 0;

    //! Search and returns the closest line with \a id of the center of \a zone and in this zone, returns false if nothing found
    virtual bool searchForLine( const ReosMapExtent &zone, qint64 &id ) const = 0;

    //! Returns the \a vertex position in \a crs coordinate or in the strucure coordinate if \a crs is void
    virtual QPointF vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const = 0;

    //! Returns the neighbor vertices positions of \a vertex in \a crs coordinate or in the strucure coordinate if \a crs is void
    virtual QList<QPointF> neighborsPositions( ReosGeometryStructureVertex *vertex, const QString &crs ) const = 0;

    virtual QUndoStack *undoStack() const = 0;

};

#endif // REOSPOLYLINESSTRUCTURES_H
