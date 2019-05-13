/***************************************************************************
                      hdmapmeshitem.cpp
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

#include "hdmapmeshitem.h"



HdMapMeshEditorItemDomain::HdMapMeshEditorItemDomain(QgsMapCanvas *canvas):
    mCanvas(canvas),
    verticesGroup(new QGraphicsItemGroup(this)),
    segmentsGroup(new QGraphicsItemGroup(this))
{
    mCanvas->scene()->addItem(this);
}

int HdMapMeshEditorItemDomain::addVertex(const QPointF &p)
{
    addVertexToGroup(p);
    return verticesCount()-1;
}



int HdMapMeshEditorItemDomain::verticesCount() const {return verticesGroup->childItems().count();}

int HdMapMeshEditorItemDomain::segmentCount() const {return segmentsGroup->childItems().count();}

HdMeshVertex *HdMapMeshEditorItemDomain::vertex(int n) const
{
    return static_cast<HdMeshVertex*>(verticesGroup->childItems().at(n));
}


void HdMapMeshEditorItemDomain::addVertexToGroup(const QPointF &p)
{
    verticesGroup->addToGroup(new HdMeshVertex(p,mCanvas));
}

void HdMapMeshEditorItemDomain::addSegmentToGroup(int n0, int n1)
{
    segmentsGroup->addToGroup(new HdMeshSegment(vertex(n0),vertex(n1),mCanvas));
}


void HdMapMeshEditorItemDomain::clear()
{
    while (verticesCount()) {
        delete verticesGroup->childItems().at(0);
    }

    while (segmentCount()) {
        delete segmentsGroup->childItems().at(0);
    }
}

HdMeshVertex::HdMeshVertex(const QPointF &mapPosition, QgsMapCanvas *canvas):HdMapMeshItem(canvas)
{
    setPosition(mapPosition);
}

void HdMeshVertex::setPosition(const QPointF &pt)
{
    mapPosition=pt;
    setPos(toCanvasCoordinates(pt));
}

void HdMeshVertex::paint(QPainter *painter)
{
    painter->save();
    painter->setBrush(QBrush(Qt::red));
    painter->drawEllipse(-3,-3,6,6);
    painter->restore();
}

void HdMeshVertex::updatePosition()
{
    prepareGeometryChange();
    setPos(toCanvasCoordinates(mapPosition));
}

HdMeshSegment::HdMeshSegment(HdMeshVertex *n0, HdMeshVertex *n1, QgsMapCanvas *canvas):HdMapMeshItem (canvas),n0(n0),n1(n1)
{}

void HdMeshSegment::paint(QPainter *painter)
{
    painter->drawLine(n0->pos(),n1->pos());
}
