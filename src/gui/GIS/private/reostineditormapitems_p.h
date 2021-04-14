/***************************************************************************
  reostineditormapitems_p.h - ReosTinEditorMapItems_p

 ---------------------
 begin                : 13.4.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#ifndef REOSTINEDITORMAPITEMS_P_H
#define REOSTINEDITORMAPITEMS_P_H

#include "reosmappolygon_p.h"
#include "reostriangularirregularnetwork.h"

#include <QPointer>
#include <QDebug>

class ReosTinEditorMapItems_p: public ReosMapItem_p
{
  public:
    ReosTinEditorMapItems_p( QgsMapCanvas *canvas );
    ReosTinEditorMapItems_p *clone() {return nullptr;}
    QRectF boundingRect() const override {return mBoundingRect;}
    void updatePosition() override;
    //QPainterPath shape() const override;
    void move( const QPointF &translation ) override {};
    QPointF mapPos() const override {return QPointF();}

    QPointer<ReosTriangularIrregularNetwork> mTriangulation;

  protected:

    struct PointView
    {
      QPointF posView;
      QPoint gridView;
    };

    QVector < QVector < int>> mGridView;

    void paint( QPainter *painter ) override;
    QVector<PointView> mVertexView;
    QRectF mBoundingRect;

    int resolReduction = 4;





};

#endif // REOSTINEDITORMAPITEMS_P_H
