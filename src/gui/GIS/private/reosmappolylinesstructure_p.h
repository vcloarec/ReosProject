/***************************************************************************
  reosmappolylinesstructure_p.h - ReosMapPolylinesStructure_p

 ---------------------
 begin                : 12.1.2022
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
#ifndef REOSMAPPOLYLINESSTRUCTURE_P_H
#define REOSMAPPOLYLINESSTRUCTURE_P_H

#include "reosmappolygon_p.h"

class ReosMapStructureExteriorItem;
class QgsPointXY;

class ReosMapPolylinesStructure_p : public ReosMapItem_p
{
  public:
    ReosMapPolylinesStructure_p( QgsMapCanvas *canvas );

    ReosMapItem_p *clone();

    void translate( const QPointF &translation ) {}
    QPointF mapPos() const;

    void updatePosition() override;
    void setStructure( ReosPolylinesStructure *structure );

  protected:
    void paint( QPainter *painter );
    QPointF mOriginInView;
    ReosPolylinesStructure *mStructure = nullptr;
    ReosMapStructureExteriorItem *mExterior = nullptr;
};



class ReosMapStructureExteriorItem : public QGraphicsItem
{
  public:
    ReosMapStructureExteriorItem( ReosMapPolylinesStructure_p *parent );

    void updatePosition( const QPolygonF &poly, ReosMapPolylinesStructure_p *parent );

    QRectF boundingRect() const override;

    void paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * ) override;

  private:
    QPolygonF polyInLocalView;
    QRectF mBBox;
};

#endif // REOSMAPPOLYLINESSTRUCTURE_P_H
