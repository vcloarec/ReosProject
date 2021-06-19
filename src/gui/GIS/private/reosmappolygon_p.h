/***************************************************************************
                      reosmappolygon.h
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

#ifndef REOSMAPPOLYGON_P_H
#define REOSMAPPOLYGON_P_H

#include <qgsmapcanvasitem.h>
#include <qgspointxy.h>

class ReosMapItem;

class ReosMapItem_p: public QgsMapCanvasItem
{
  public:
    ReosMapItem_p( QgsMapCanvas *canvas ): QgsMapCanvasItem( canvas ) {}
    virtual ReosMapItem_p *clone() = 0;

    virtual void setEditing( bool ) {}
    virtual void translate( const QPointF &translation ) = 0;
    virtual QPointF mapPos() const = 0;
    virtual void setMapPosition( const QgsPointXY & ) {};

    QColor color;
    QColor externalColor;
    double width = 0.0;
    double externalWidth = 0.0;
    Qt::PenStyle style = Qt::SolidLine;
    Qt::BrushStyle brushStyle = Qt::NoBrush;
    QColor fillColor;

    bool isHovered = false;

    ReosMapItem *base;
};

class ReosMapMarker_p: public ReosMapItem_p
{
  public:
    ReosMapMarker_p( QgsMapCanvas *canvas );
    void updatePosition() override;
    void translate( const QPointF &translation ) override;
    QPointF mapPos() const override;
    QRectF boundingRect() const override;
    void setMapPosition( const QgsPointXY &pos ) override;;

    QPointF mapPoint;
    bool isEmpty = true;

  protected:
    QPointF mViewPoint;

};

class ReosMapMarkerFilledCircle_p: public ReosMapMarker_p
{
  public:
    ReosMapMarkerFilledCircle_p( QgsMapCanvas *canvas );
    ReosMapMarkerFilledCircle_p *clone() override;
    QPainterPath shape() const override;

  protected:
    void paint( QPainter *painter ) override;
};


class ReosMapMarkerEmptySquare_p: public ReosMapMarker_p
{
  public:
    ReosMapMarkerEmptySquare_p( QgsMapCanvas *canvas );
    ReosMapMarkerEmptySquare_p *clone() override;
    QPainterPath shape() const override;

  protected:
    void paint( QPainter *painter ) override;
};

class ReosMapMarkerEmptyCircle_p: public ReosMapMarker_p
{
  public:
    ReosMapMarkerEmptyCircle_p( QgsMapCanvas *canvas );
    ReosMapMarkerEmptyCircle_p *clone() override;
    QPainterPath shape() const override;

  protected:
    void paint( QPainter *painter ) override;
};

class ReosMapPolygon_p: public ReosMapItem_p
{
  public:
    ReosMapPolygon_p( QgsMapCanvas *canvas );

    ReosMapPolygon_p *clone() override;
    QRectF boundingRect() const override;
    void updatePosition() override;
    QPainterPath shape() const override;
    void setEditing( bool b ) override;
    void translate( const QPointF &translation ) override;
    QPointF mapPos() const override;

    // Search a vertex in viex coordinate
    int findVertexInView( const QRectF &zone ) const;

    void activeMarker( bool b );
    void setMarkerDistance( double d );
    void setMarkerArrow( bool b );

    QPolygonF mapPolygon;

  protected:
    void paint( QPainter *painter ) override;
    QPolygonF mViewPolygon;
    bool mIsEditing = false;
    bool mIsMarkerActive = false;
    int mSegmentMarker = -1;
    bool mMarkerArrow = false;
    QPointF mMarkerposition;
    QPointF mMarkerPositionOnView;

  private:
    virtual void draw( QPainter *painter );
};

class ReosMapPolyline_p: public ReosMapPolygon_p
{
  public:
    ReosMapPolyline_p( QgsMapCanvas *canvas );
    ReosMapPolyline_p *clone() override;

    void setExtremityDistance( double d );

  private:
    void draw( QPainter *painter ) override;
    double mExtremityDistance = 0;
};

#endif // REOSMAPPOLYGON_P_H
