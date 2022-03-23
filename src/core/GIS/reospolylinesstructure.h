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

#include "reosgeometrystructure.h"

class ReosSpatialPosition;
class QUndoStack;


class ReosPolylinesStructure : public ReosGeometryStructure
{
    Q_OBJECT
  public:

    struct Data
    {
      QVector<QPointF> vertices; //! all the vertices of the structure
      int boundaryPointCount; //! the count of boundary points that are the first in the array of point \a vertices
      QVector<QVector<int>> internalLines; //! all internal lines vertices index
      QVector<QVector<int>> holes; //! all holes internal lines index
      QRectF extent; //! Extent of the data
    };

    //! Creates and returns polylines structure with specified \a crs
    static std::unique_ptr<ReosPolylinesStructure> createPolylineStructure( const QString &crs );

    //! Creates and returns polylines structure with specified \a crs
    static std::unique_ptr<ReosPolylinesStructure> createPolylineStructure( const QPolygonF &boundary, const QString &crs );

    //! Creates and returns polylines structure with encoded \a encodedElement
    static std::unique_ptr<ReosPolylinesStructure> createPolylineStructure( const ReosEncodedElement &encodedElement );

    /**
     *  Adds a \a polyline to the structure with coordinates in \a sourcesCrs and a tolerance for each vertices \a tolerances in \a sourceCrs unit
     *  If the tolerance, is negative, the proper tolerance of the structure will be used
     */
    virtual void addPolylines( const QPolygonF &polyline,  const QList<double> &tolerances = QList<double>(), const QString &sourceCrs = QString() ) = 0;

    //! Returns the geometric line with \a id in the \a destinationCrs coordinate system
    virtual QLineF line( qint64 lineId, const QString &destinationCrs = QString() ) const = 0;

    //! Returns the structured lines data, that is positions of each vertices and the topology
    virtual Data structuredLinesData( const QString &destinationCrs = QString() ) const = 0;

    //! Returns all the lines of the structure in coordintate system \a destination CRS
    virtual QVector<QLineF> rawLines( const QString &destinationCrs = QString() ) const = 0;

    //! Returns the polyline with identifier \a id
    virtual QPolygonF polyline( const QString &destinationCrs = QString(), const QString &id = QString() ) const = 0;

    //! Returns the boundary of the structure in \a destinationCrs cordinate system
    virtual QPolygonF boundary( const QString &destinationCrs = QString() ) const = 0;

    //! Returns lines on boundary from the vertex \a vertexFrom to vertex \a vertexTo
    virtual QPolygonF linesOnBoundaryFromTo(
      ReosGeometryStructureVertex *vertexFrom,
      ReosGeometryStructureVertex *vertexTo,
      const QString &destinationCrs = QString() ) const = 0;

    //! Return the class id of the ith segment returned by boundary()
    virtual QString boundaryClassId( int i ) const = 0;

    //! Removes all the entities in the structure
    virtual void removeAll() = 0;

    //! Translates the polyline with \a id (boundary one if void string)
    virtual void translate( const QPointF &translation, const QString &crs, const QString &id = QString() ) = 0;

    //! Returns whether the \a vertex can be moves to the new position \a newPosition
    virtual bool vertexCanBeMoved( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition ) const = 0;

    //! Moves \a vertex to the new positon \a newPosition
    virtual void moveVertex( ReosGeometryStructureVertex *vertex, const ReosSpatialPosition &newPosition ) = 0;

    //! Inserts a vertex in the line with \a lineId at position \a point
    virtual ReosGeometryStructureVertex *insertVertex( const ReosSpatialPosition &point, qint64 lineId ) = 0;

    //! Returns whether the vertex can be removed
    virtual bool vertexCanBeRemoved( ReosGeometryStructureVertex *vertex ) const = 0;

    //! Removes \a vertex
    virtual void removeVertex( ReosGeometryStructureVertex *vertex ) = 0 ;

    //! Returns whether the vertex can be removed
    virtual bool lineCanBeRemoved( qint64 lineId ) const = 0;

    //! Removes line with id \a lineId
    virtual void removeLine( qint64 lineId ) = 0;

    //! Search the closest vertex of the center of \a zone and in this zone, returns a pointer to the vertex
    virtual ReosGeometryStructureVertex *searchForVertex( const ReosMapExtent &zone ) const = 0;

    //! Search and returns the closest line with \a id of the center of \a zone and in this zone, returns false if nothing found
    virtual bool searchForLine( const ReosMapExtent &zone, qint64 &id ) const = 0;

    //! Returns the \a vertex position in \a crs coordinate or in the strucure coordinate if \a crs is void
    virtual QPointF vertexPosition( ReosGeometryStructureVertex *vertex, const QString &crs ) const = 0;

    //! Returns a projected point on the line with \a id from the point  \a point;
    virtual QPointF projectedPoint( const QPointF &point, qint64 lineId, const QString &destinationCrs ) const = 0;

    //! Returns the neighbor vertices positions of \a vertex in \a crs coordinate or in the strucure coordinate if \a crs is void
    virtual QList<QPointF> neighborsPositions( ReosGeometryStructureVertex *vertex, const QString &crs ) const = 0;

    /**
     *  Returns the list of intersection points of \a line with the strucure ordered from the closest from firt point of \a line to the farthest.
     *  Caller can add another polyline \a otherPoly to also search interection of the \a line with
     */
    virtual  QList<QPointF> intersectionPoints( const QLineF &line, const QString &crs = QString(), const QPolygonF &otherPoly = QPolygonF() ) const = 0;

    //! Adds a point that represent a hole in the structure at \a position
    virtual void addHolePoint( const ReosSpatialPosition &position ) = 0;

    //! Returns position of points defining hole in the structure
    virtual QList<QPointF> holePoints( const QString &destinationCrs ) const = 0;

    //! Searches for an hole point in the \a zone and reyturn indexof this point, -1 if nothing
    virtual int searchHolePoint( const ReosMapExtent &zone ) const = 0;

    //! Moves the hole point with \a index to \a position
    virtual void moveHolePoint( int index, const ReosSpatialPosition &position ) = 0;

    //! Removes the hole point with \a index
    virtual void removeHolePoint( int index ) = 0;

    //! Returns whether the \a vertex is on boundary
    virtual bool isOnBoundary( ReosGeometryStructureVertex *vertex ) const = 0;

    /**
     * Returns whether a boundary condition can be added between \a vertexFrom and \a vertexTo.
     * If \a vertexTo is nullptr, tests if the boundary can contains \a
     */
    virtual bool canBoundaryConditionBeAdded( ReosGeometryStructureVertex *vertexFrom, ReosGeometryStructureVertex *vertexTo = nullptr ) const = 0;

    //! Adds a boundary condition between \a vertexFrom and \a vertexTo with \a name
    virtual void addBoundaryCondition( ReosGeometryStructureVertex *vertexFrom, ReosGeometryStructureVertex *vertexTo, const QString &name ) = 0;

    //! Remove the boundary condition identified by \a classId
    virtual void removeBoundaryCondition( const QString &classId ) = 0;

    //! Chneges the the \a value associated with the class identified by \a classId
    virtual void changeClassValue( const QString &classId, const QVariant &value ) = 0;

    //! Returns the selected class
    QString selectedClass() const;

    //! Sets the selected class
    void setSelectedClass( const QString &selectedClass );

    //! Returns the extent of all element that have the class identified by \a classId
    virtual QRectF classExtent( const QString &classId, const QString &destinationCrs ) const = 0;

    //! Returns all the clasees
    virtual QStringList classes() const = 0;

    //! Returns the value associated to the class identified by \a classId
    virtual QVariant value( const QString &classId ) const = 0;

    virtual QUndoStack *undoStack() const = 0;

    virtual ReosEncodedElement encode() const = 0;

  signals:
    void classesChanged();

  private:
    QString mSelectedClass;

};

#endif // REOSPOLYLINESSTRUCTURES_H
