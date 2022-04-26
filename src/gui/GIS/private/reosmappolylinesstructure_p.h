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
class ReosMapStructureLinesItem;
class ReosMapStructureHolePointsItem;
class QgsPointXY;

class ReosMapPolylinesStructure_p : public ReosMapItem_p
{
  public:
    ReosMapPolylinesStructure_p( QgsMapCanvas *canvas );

    ReosMapItem_p *clone() override;

    void translate( const QPointF &translation ) override {}
    QPointF mapPos() const override;

    void setLineWidth( double width );

    void updatePosition() override;
    void setStructure( ReosPolylinesStructure *structure );

  protected:
    void paint( QPainter *painter );
    QPointF mOriginInView;
    ReosPolylinesStructure *mStructure = nullptr;
    ReosMapStructureExteriorItem *mExterior = nullptr;
    ReosMapStructureLinesItem *mLines = nullptr;
    ReosMapStructureHolePointsItem *mHolePoints = nullptr;
};


class ReosMapStructureExteriorItem : public QGraphicsItem
{
  public:
    ReosMapStructureExteriorItem( ReosMapPolylinesStructure_p *parent );
    void updatePosition( const ReosPolylinesStructure *structure, ReosMapPolylinesStructure_p *parent, const QString &destinationCrs );
    QRectF boundingRect() const override;
    void paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * ) override;

    void setWidth( double baseWidth );

  private:
    QPolygonF mPolyInLocalView;
    QList<bool> mIsCondition;
    QList<bool> mIsSelected;
    QRectF mBBox;
    double mWidth = 5;
};

class ReosMapStructureLinesItem : public QGraphicsItem
{
  public:
    ReosMapStructureLinesItem( ReosMapPolylinesStructure_p *parent );
    void updatePosition( const ReosPolylinesStructure *structure, ReosMapPolylinesStructure_p *parent, const QString &destinationCrs );
    QRectF boundingRect() const override;
    void paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * ) override;

    void setWidth( double width );

  private:
    QList<QLineF> mLinesInLocalView;
    QRectF mBBox;
    double mWidth = 5;
};

class ReosMapStructureHolePointsItem : public QGraphicsItem
{
  public:
    ReosMapStructureHolePointsItem( ReosMapPolylinesStructure_p *parent );
    void updatePosition( const ReosPolylinesStructure *structure, ReosMapPolylinesStructure_p *parent, const QString &destinationCrs );

    QRectF boundingRect() const override;
    void paint( QPainter *painter, const QStyleOptionGraphicsItem *, QWidget * ) override;

  private:
    QList<QPointF> mViewPoints;
    QList<bool> mPointValidity;
    QRectF mBBox;
};


#endif // REOSMAPPOLYLINESSTRUCTURE_P_H
