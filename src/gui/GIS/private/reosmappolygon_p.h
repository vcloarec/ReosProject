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

class ReosMapItem;

class ReosMapItem_p: public QgsMapCanvasItem
{
  public:
    ReosMapItem_p( QgsMapCanvas *canvas ): QgsMapCanvasItem( canvas ) {}
    virtual ReosMapItem_p *clone() = 0;

    QColor color;
    QColor externalColor;
    double width = 0.0;
    double externalWidth = 0.0;
    Qt::PenStyle style = Qt::SolidLine;

    ReosMapItem *base;

};

class ReosMapPolygon_p: public ReosMapItem_p
{
  public:
    ReosMapPolygon_p( QgsMapCanvas *canvas );

    ReosMapPolygon_p *clone() override;
    QRectF boundingRect() const override;
    void updatePosition() override;

    QPolygonF mapPolygon;

  protected:
    void paint( QPainter *painter ) override;
    QPolygonF mViewPolygon;

  private:
    virtual void draw( QPainter *painter );
};

class ReosMapPolyline_p: public ReosMapPolygon_p
{
  public:
    ReosMapPolyline_p( QgsMapCanvas *canvas );

    ReosMapPolyline_p *clone() override;

  private:
    void draw( QPainter *painter ) override;
};

#endif // REOSMAPPOLYGON_P_H
