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
//    for (auto vert:editor.vertices())
//    {
//        QPointF position(vert.x(),vert.y());
//        addVertexToGroup(position);
//    }

}

int HdMapMeshEditorItemDomain::addVertex(const QPointF &p)
{
    if (!mMeshEditor)
        return -1;

    int index=mMeshEditor->addVertex(Vertex(p.x(),p.y()));
    if (index>=verticesGroup->childItems().count())
        addVertexToGroup(p);

    return index;
}

int HdMapMeshEditorItemDomain::addSegmentHardLine(int n0, const QPointF &p)
{
    if (!mMeshEditor)
        return -1;

    int n1=addVertex(p);
    if (n0>=0)
    {
        if (mMeshEditor->addSegment(n0,n1))
            addSegmentToGroup(n0,n1);
    }
    return n1;

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

void HdMapMeshEditorItemDomain::populateDomain()
{
    if(!mMeshEditor)
        return;

    for (const auto& v:mMeshEditor->vertices())
        addVertexToGroup(QPointF(v.x(),v.y()));

    for (const auto& s:mMeshEditor->segments())
        addSegmentToGroup(s.first,s.second);
}

void HdMapMeshEditorItemDomain::clearDomain()
{
    while (verticesCount()) {
        delete verticesGroup->childItems().at(0);
    }

    while (segmentCount()) {
        delete segmentsGroup->childItems().at(0);
    }
}
