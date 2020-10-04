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

    QRectF boundingRect() const {return mViewPolygon.boundingRect();}
    void updatePosition();
    QPolygonF mapPolygon;

  protected:
    void paint( QPainter *painter );

  private:
    QPolygonF mViewPolygon;


};

#endif // REOSMAPPOLYGON_P_H
