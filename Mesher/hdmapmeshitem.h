/***************************************************************************
                      hdmapmeshitem.h
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#ifndef HDMAPMESHITEM_H
#define HDMAPMESHITEM_H

#include <QPainter>

#include <qgsmapcanvas.h>
#include <qgsmapcanvasitem.h>

#include "hdmesheditor.h"

class HdMapMeshItem : public QgsMapCanvasItem
{
public:
    HdMapMeshItem(QgsMapCanvas *canvas): QgsMapCanvasItem(canvas){}

};


class HdMeshVertex: public HdMapMeshItem
{
public:
    HdMeshVertex(const QPointF &mapPosition,QgsMapCanvas *canvas):HdMapMeshItem(canvas)
    {
        setPosition(mapPosition);
    }

    void setPosition(const QPointF &pt)
    {
        mapPosition=pt;
        setPos(toCanvasCoordinates(pt));
    }
    // QgsMapCanvasItem interface
protected:
    void paint(QPainter *painter) override
    {
        painter->save();
        painter->setBrush(QBrush(Qt::red));
        painter->drawEllipse(-3,-3,6,6);
    }

private:
    QPointF mapPosition;

    // QgsMapCanvasItem interface
public:
    void updatePosition() override
    {
        prepareGeometryChange();
        setPos(toCanvasCoordinates(mapPosition));
    }
};

class HdMeshSegment: public HdMapMeshItem
{
public:
    HdMeshSegment(HdMeshVertex *n0,HdMeshVertex *n1,QgsMapCanvas *canvas):HdMapMeshItem (canvas),n0(n0),n1(n1)
    {}

private:
    HdMeshVertex *n0=nullptr;
    HdMeshVertex *n1=nullptr;

    // QgsMapCanvasItem interface
protected:
    void paint(QPainter *painter) override
    {
        painter->drawLine(n0->pos(),n1->pos());
    }
};

class HdMapMeshEditorItemDomain: public QGraphicsItemGroup{
public:
    HdMapMeshEditorItemDomain(QgsMapCanvas *canvas);

    int addVertex(const QPointF &p);
    int addSegmentHardLine(int n0, const QPointF &p);

    int verticesCount() const;
    int segmentCount() const;

    HdMeshVertex *vertex(int n) const;

    void setTINEditor(TINEditor *tinEditor)
    {
        if(tinEditor==mMeshEditor)
            return;

        mMeshEditor=tinEditor;
        clearDomain();
        populateDomain();

    }

private:
    TINEditor *mMeshEditor=nullptr;
    QgsMapCanvas *mCanvas=nullptr;
    QGraphicsItemGroup *verticesGroup=nullptr;
    QGraphicsItemGroup *segmentsGroup=nullptr;


    void addVertexToGroup(const QPointF &p);
    void addSegmentToGroup(int n0, int n1);

    void populateDomain();

    void clearDomain();
};


#endif // HDMAPMESHITEM_H
