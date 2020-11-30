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

class ReosMapPolygon_p: public QgsMapCanvasItem
{
  public:
    ReosMapPolygon_p( QgsMapCanvas *canvas );
    virtual ~ReosMapPolygon_p() {}

    virtual ReosMapPolygon_p *clone();

    QRectF boundingRect() const override {return mViewPolygon.boundingRect();}
    void updatePosition() override;
    QPolygonF mapPolygon;

    QColor color;
    QColor externalColor;
    double width = 0.0;
    double externalWidth = 0.0;
    Qt::PenStyle style = Qt::SolidLine;

  protected:
    void paint( QPainter *painter ) override;
    QPolygonF mViewPolygon;
};

class ReosMapPolyline_p: public ReosMapPolygon_p
{
  public:
    ReosMapPolyline_p( QgsMapCanvas *canvas );
  protected:
    void paint( QPainter *painter ) override;
};

#endif // REOSMAPPOLYGON_P_H
