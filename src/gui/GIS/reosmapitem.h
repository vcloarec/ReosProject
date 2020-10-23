/***************************************************************************
                      reosmapitem.h
                     --------------------------------------
Date                 : 17-09-2020
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

#ifndef REOSMAPITEM_H
#define REOSMAPITEM_H

#include <memory>

#include <QPolygonF>

class ReosMap;
class ReosMapPolygon_p;


class ReosMapPolygon
{
  public:
    //! Contructor
    ReosMapPolygon( ReosMap *map );

    //! Resets the polygon with \a polygon
    void resetPolygon( const QPolygonF &polygon = QPolygonF() );

    //! Returns the polygon
    QPolygonF mapPolygon() const;

    //! Move the point at \a index and update the map
    void movePoint( int pointIndex, const QPointF &p );

  private:
    std::unique_ptr<ReosMapPolygon_p> d;
};

class ReosMapPolyline
{
  public:
    //! Constructor
    ReosMapPolyline( ReosMap *map );

    //! Rsets the polyline with \a polyline
    void resetPolyline( const QPolygonF &polyline );

    //! Returns the map polyline
    QPolygonF mapPolyline() const;

    //! Move the point at \a index and update the map
    void movePoint( int pointIndex, const QPointF &p );

  private:
    std::unique_ptr<ReosMapPolygon_p> d;
};




#endif // REOSMAPITEM_H
