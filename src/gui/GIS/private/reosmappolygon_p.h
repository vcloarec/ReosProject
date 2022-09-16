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

#include <QSvgRenderer>

#include <qgsmapcanvasitem.h>
#include <qgsvectorlayer.h>
#include <qgspointxy.h>

class ReosMapItem;
class ReosPolylinesStructure;

class ReosMapItem_p: public QgsMapCanvasItem
{
  public:
    ReosMapItem_p( QgsMapCanvas *canvas ): QgsMapCanvasItem( canvas ) {}
    virtual ReosMapItem_p *clone() = 0;

    virtual void setEditing( bool ) {}
    virtual void translate( const QPointF &translation ) = 0;
    virtual QPointF mapPos() const = 0;
    virtual void setMapPosition( const QgsPointXY & ) {};

    QString crs() const;

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

class ReosMapMarkerSvg_p: public ReosMapMarker_p
{
  public:
    ReosMapMarkerSvg_p( QgsMapCanvas *canvas, const QString &filePath );
    ReosMapMarkerSvg_p *clone() override;
    QRectF boundingRect() const override;

  protected:
    void paint( QPainter *painter ) override;

  private:
    QString mFilePath;
    std::unique_ptr<QSvgRenderer> mSvgRenderer;
};


class ReosMapPolygonBase_p: public ReosMapItem_p
{
  public:
    ReosMapPolygonBase_p( QgsMapCanvas *canvas );
    QRectF boundingRect() const override;
    void updatePosition() override;
    QPainterPath shape() const override;
    void setEditing( bool b ) override;

    // Search a vertex in view coordinate
    int findVertexInView( const QRectF &zone ) const;

    void activeMarker( bool b );
    // Set the position of the marker with distance from begining in meter
    void setMarkerDistance( double d );
    void setMarkerArrow( bool b );
    void setMarkerAtMid();

    virtual void setGeometry( const QPolygonF &geom ) = 0;
    virtual void moveVertex( int index, const QPointF &newPosition ) = 0;
    virtual void insertVertex( int index, const QPointF &point ) = 0;
    virtual void removeVertex( int index ) = 0;

    virtual QPolygonF geometry() const = 0;

  protected:
    void paint( QPainter *painter ) override;

    ReosMapPolygonBase_p( ReosMapPolygonBase_p *other );
    QPolygonF mViewPolygon;
    bool mIsEditing = false;
    bool mIsMarkerActive = false;
    int mSegmentMarker = -1;
    bool mMarkerArrow = false;
    QPointF mMarkerposition;
    QPointF mMarkerPositionOnView;

    virtual  void draw( QPainter *painter );
};

class ReosMapPolygon_p: public ReosMapPolygonBase_p
{
  public:

    ReosMapPolygon_p( QgsMapCanvas *canvas );

    ReosMapPolygon_p *clone() override;

    void translate( const QPointF &translation ) override;
    QPointF mapPos() const override;
    void setGeometry( const QPolygonF &geom ) override;
    void moveVertex( int index, const QPointF &newPosition ) override;
    void insertVertex( int index, const QPointF &point ) override;
    void removeVertex( int index ) override;
    QPolygonF geometry() const override;

  protected:
    QPolygonF mMapPolygon;
  private:
    ReosMapPolygon_p( ReosMapPolygon_p *other );

};

class ReosMapStructureEnvelop_p: public ReosMapPolygonBase_p
{
  public:

    ReosMapStructureEnvelop_p( QgsMapCanvas *canvas );

    ReosMapStructureEnvelop_p *clone() override;

    void translate( const QPointF &translation ) override;
    QPointF mapPos() const override;
    void setGeometry( const QPolygonF &geom ) override;
    void moveVertex( int index, const QPointF &newPosition ) override;
    void insertVertex( int index, const QPointF &point ) override;
    void removeVertex( int index ) override;
    QPolygonF geometry() const override;

    void setStructrure( ReosPolylinesStructure *structure );

  private:
    ReosMapStructureEnvelop_p( ReosMapStructureEnvelop_p *other );
    ReosPolylinesStructure *mStructure = nullptr;
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
