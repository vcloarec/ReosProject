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
#include "reosmaptool_p.h"
#include "reostriangularirregularnetwork.h"

#include <QPointer>
#include <QDebug>

class ReosTinEditorMapItems_p: public ReosMapItem_p
{
  public:
    ReosTinEditorMapItems_p( QgsMapCanvas *canvas );
    ReosTinEditorMapItems_p *clone() override {return nullptr;}
    QRectF boundingRect() const override {return mBoundingRect;}
    void updatePosition() override;
    //QPainterPath shape() const override;
    void move( const QPointF & ) override {};
    QPointF mapPos() const override {return QPointF();}

    void highlightVertex( const QPointF &mapPoint, const QSizeF searchSize );
    int vertexIndexAt( const QPointF &mapPoint, const QSizeF searchSize );

    QPointer<ReosTriangularIrregularNetwork> mTriangulation;

  protected:
    void paint( QPainter *painter ) override;
  private:
    struct PointView
    {
      QPointF posView;
      QPoint gridView;
      int triangulationIndex;
    };

    QVector < QVector < QPair<int, int>>> mGridView;

    QVector<PointView> mVertexView;
    int mHighlightVertex = -1;
    QRectF mBoundingRect;

    int mMinResolReduction = 4;
    int mResolReduction = 4;

    int searchPointView( const QPointF &map, const QSizeF &searchSize ) const;

};


class ReosTinEditorMapToolSelectVertex_p: public ReosMapTool_p
{
    Q_OBJECT
  public:
    ReosTinEditorMapToolSelectVertex_p( QgsMapCanvas *map, ReosTinEditorMapItems_p *editoMapItems ):
      ReosMapTool_p( map )
      , mEditorMapItems( editoMapItems )
    {
    }

    void canvasMoveEvent( QgsMapMouseEvent *e ) override;
    void canvasReleaseEvent( QgsMapMouseEvent *e ) override;

  signals:
    void selectedVertexIndex( int index );

  protected:

  private:
    ReosTinEditorMapItems_p *mEditorMapItems = nullptr;

};


#endif // REOSTINEDITORMAPITEMS_P_H
