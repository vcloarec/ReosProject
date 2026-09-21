/***************************************************************************
  reosmappolygoncomplex_p.h - ReosMapPolygonStructure_p

 ---------------------
 begin                : 6.2.2022
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
#ifndef REOSMAPPOLYGONSTRUCTURE_P_H
#define REOSMAPPOLYGONSTRUCTURE_P_H

#include <QPointer>
#include "reosmappolygon_p.h"

class ReosGeometryComplex;

class ReosMapPolygonComplex_p : public ReosMapItem_p
{
  public:
    ReosMapPolygonComplex_p( QgsMapCanvas *canvas );

    ReosMapItem_p *clone() override;

    void translate( const QPointF &translation ) override {}
    QPointF mapPos() const override;
    void updatePosition() override;
    QRectF boundingRect() const override;
    void setStructure( ReosGeometryComplex *structure );

    void setHovered( const QgsPointXY &position ) override;
    void clearHover() override;


  private:
    void paint( QPainter *painter ) override;
    QPointF mHoveredPosition;
    QPointer<ReosGeometryComplex> mGeometry;
    QRectF mBBox;
};

#endif // REOSMAPPOLYGONSTRUCTURE_P_H
