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

#include "reosgui.h"
#include "reosmapextent.h"

class ReosMap;
class ReosMapItem_p;
class ReosMapPolygon_p;
class ReosMapPolyline_p;
class ReosMapMarkerFilledCircle_p;

class REOSGUI_EXPORT ReosMapItem
{
  public:
    //! Construct a map item and link it with a map
    ReosMapItem();
    ReosMapItem( ReosMap *map );
    ReosMapItem( const ReosMapItem &other );
    virtual ~ReosMapItem() = default;

    //! Returns whether graphic \a internal item correspond to \a this graphical item
    bool isItem( QGraphicsItem *item ) const;

    //! Returns whether internal item of \a item correspond to \a this internal item
    bool isItem( ReosMapItem *item ) const;

    void setColor( const QColor &color );
    void setExternalColor( const QColor &color );
    void setWidth( double width );
    void setExternalWidth( double externalWidth );
    void setStyle( Qt::PenStyle style );

    void setZValue( double Z );

    //! Desscription to describe what is this item, could be convenient to retrieve particular item
    QString description() const;
    void setDescription( const QString &description );

    void setVisible( bool visible );

    void setHovered( bool b );

    QGraphicsItem *graphicItem();

  protected:
    bool isMapExist() const;
    QPointer<ReosMap> mMap = nullptr;
    QString mDescription;

    //! Private item that represent the graphic representation on the map, access to it need to check isMapExist()
    ReosMapItem_p *d_ = nullptr;
};

class REOSGUI_EXPORT ReosMapMarker:  public ReosMapItem
{
  public:
    ReosMapMarker() {}
    ReosMapMarker( ReosMap *map ): ReosMapItem( map ) {}

    //! Resets the marker with \a point
    void resetPoint( const QPointF &point );

    //! Sets the marker empty
    void resetPoint();

    //! Returns the map position
    QPointF mapPoint() const;

    //! Moves the marker updates the map
    void move( const QPointF &p );

    bool isEmpty() const;
};

class REOSGUI_EXPORT ReosMapMarkerFilledCircle : public ReosMapMarker
{
  public:
    //! Contructor
    ReosMapMarkerFilledCircle();
    ReosMapMarkerFilledCircle( ReosMap *map );
    ReosMapMarkerFilledCircle( ReosMap *map, const QPointF &point );
    ~ReosMapMarkerFilledCircle();
};

class REOSGUI_EXPORT ReosMapMarkerEmptyCircle : public ReosMapMarker
{
  public:
    //! Contructor
    ReosMapMarkerEmptyCircle();
    ReosMapMarkerEmptyCircle( ReosMap *map );
    ReosMapMarkerEmptyCircle( ReosMap *map, const QPointF &point );
    ReosMapMarkerEmptyCircle( ReosMap *map, const ReosSpatialPosition &position );
    ~ReosMapMarkerEmptyCircle();
};

class REOSGUI_EXPORT ReosMapMarkerEmptySquare : public ReosMapMarker
{
  public:
    //! Contructor
    ReosMapMarkerEmptySquare();
    ReosMapMarkerEmptySquare( ReosMap *map );
    ReosMapMarkerEmptySquare( ReosMap *map, const QPointF &point );
    ~ReosMapMarkerEmptySquare();
};

class REOSGUI_EXPORT ReosMapMarkerSvg : public ReosMapMarker
{
  public:
    //! Contructor
    ReosMapMarkerSvg();
    ReosMapMarkerSvg( const QString &filePath, ReosMap *map );
    ReosMapMarkerSvg( const QString &filePath, ReosMap *map, const ReosSpatialPosition &position );
    ~ReosMapMarkerSvg();
};


class ReosMapPolygon : public ReosMapItem
{
  public:
    //! Contructor
    ReosMapPolygon();
    ReosMapPolygon( ReosMap *map );
    ReosMapPolygon( ReosMap *map, const QPolygonF &polygon );
    ~ReosMapPolygon();

    ReosMapPolygon( const ReosMapPolygon &other );

    void setFillColor( const QColor &color );

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
    ReosMapPolyline();
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

    void activeMarker( bool b );
    void setMarkerDistance( double d );
    void setMarkerArrow( bool b );

    void setExtremityDistance( double d );

};


class ReosMapPolylineFormater
{
  public:
    ReosMapPolylineFormater() = default;
    ReosMapPolyline &operator()( ReosMapPolyline &&polyline );
    ReosMapPolyline &operator()( ReosMapPolyline &polyline );


    QColor color() const;
    void setColor( const QColor &color );

    QColor externalColor() const;
    void setExternalColor( const QColor &externalColor );

    double width() const;
    void setWidth( double width );

    double externalWidth() const;
    void setExternalWidth( double externalWidth );

    Qt::PenStyle style() const;
    void setStyle( const Qt::PenStyle &style );

    double z() const;
    void setZ( double z );

    QString description() const;
    void setDescription( const QString &descritpion );

  private:
    QColor mColor;
    QColor mExternalColor;
    double mWidth = 0;
    double mExternalWidth = 0;
    Qt::PenStyle mStyle = Qt::SolidLine;
    double mZ = 0;
    QString mDescription;

};




#endif // REOSMAPITEM_H
