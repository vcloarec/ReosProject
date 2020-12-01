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
#include <QColor>
#include <QGraphicsScene>
#include <QPointer>

class ReosMap;
class ReosMapItem_p;
class ReosMapPolygon_p;
class ReosMapPolyline_p;

class ReosMapItem
{
  public:
    //! Construct a map item and link it with a map
    ReosMapItem( ReosMap *map );
    virtual ~ReosMapItem() = default;

    bool isItem( QGraphicsItem *item ) const;

    void setColor( const QColor &color );
    void setExternalColor( const QColor &color );
    void setWidth( double width );
    void setExternalWidth( double externalWidth );
    void setStyle( Qt::PenStyle style );

    //! Desscription to describe what is this item, could be convenient to retrieve particular item
    QString description() const;
    void setDescription( const QString &description );

  protected:
    bool isMapExist() const;
    QPointer<ReosMap> mMap;
    QString mDescription;

    //! Private item that represent the graphic representation on the map, access to id need to check isMapExist()
    ReosMapItem_p *d_;
};

class ReosMapPolygon : public ReosMapItem
{
  public:
    //! Contructor
    ReosMapPolygon( ReosMap *map );
    ReosMapPolygon( ReosMap *map, const QPolygonF &polygon );
    ~ReosMapPolygon();

    ReosMapPolygon( const ReosMapPolygon &other );

    //! Resets the polygon with \a polygon
    void resetPolygon( const QPolygonF &polygon = QPolygonF() );

    //! Returns the polygon
    QPolygonF mapPolygon() const;

    //! Move the point at \a index and update the map
    void movePoint( int pointIndex, const QPointF &p );
};

class ReosMapPolyline: public ReosMapItem
{
  public:
    //! Constructor
    ReosMapPolyline( ReosMap *map );
    ReosMapPolyline( ReosMap *map, const QPolygonF &polyline );
    ~ReosMapPolyline();

    ReosMapPolyline( const ReosMapPolyline &other );

    //! Rsets the polyline with \a polyline
    void resetPolyline( const QPolygonF &polyline = QPolygonF() );
    void resetPolygon( const QPolygonF &polygon = QPolygonF() ) = delete;

    //! Returns the map polyline
    QPolygonF mapPolyline() const;

    //! Move the point at \a index and update the map
    void movePoint( int pointIndex, const QPointF &p );

};




#endif // REOSMAPITEM_H
