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

#include "reosmapmeshitem.h"



ReosMapMeshEditorItemDomain::ReosMapMeshEditorItemDomain(QObject *parent, QgsMapCanvas *canvas):
    ReosMapItemDomain(parent,canvas),
    verticesDomain(new ReosMapItemDomain(this,canvas)),
    segmentsDomain(new ReosMapItemDomain(this,canvas))
{
    verticesDomain->setZValue(zValue()+2);
    segmentsDomain->setZValue(zValue()+1);
}

ReosMeshItemVertex *ReosMapMeshEditorItemDomain::addVertex(const QPointF &p)
{
    ReosMeshItemVertex* vert = new ReosMeshItemVertex(p,canvas());
    verticesDomain->addItem(vert);
    return vert;
}

ReosMeshItemSegment *ReosMapMeshEditorItemDomain::addSegmentHardLine(ReosMeshItemVertex *v1, ReosMeshItemVertex *v2)
{
    auto segment=new ReosMeshItemSegment(v1,v2,canvas());
    segmentsDomain->addItem(segment);
    return segment;
}



int ReosMapMeshEditorItemDomain::verticesCount() const {return verticesDomain->itemsCount();}

int ReosMapMeshEditorItemDomain::segmentCount() const {
    return segmentsDomain->itemsCount();
}

void ReosMapMeshEditorItemDomain::removeSegment(ReosMeshItemSegment *seg){
    if (!seg)
        return;
    seg->removeFromNode();
    segmentsDomain->removeItem(seg);
    delete seg;
}

ReosMeshItemVertex *ReosMapMeshEditorItemDomain::vertex(int n) const
{
    return static_cast<ReosMeshItemVertex*>(verticesDomain->item(n));
}

ReosMeshItemVertex *ReosMapMeshEditorItemDomain::vertex(const QRectF &rect) const
{
    return static_cast<ReosMeshItemVertex*>(verticesDomain->item(rect));
}



ReosMeshItemVertex::ReosMeshItemVertex(const QPointF &mapPosition, QgsMapCanvas *canvas):ReosMapItemNode(mapPosition,canvas)
{

    setBrush(QBrush(Qt::green));
    setSize(6);
}

ReosMeshItemVertex::~ReosMeshItemVertex(){}

void ReosMeshItemVertex::setRealWorldVertex(VertexPointer vertex) {

    mRealWorldVertex=vertex;
}

VertexPointer ReosMeshItemVertex::realWorldVertex() const {
    return mRealWorldVertex;
}


void ReosMeshItemVertex::addSegment(ReosMeshItemSegment *seg)
{
    ReosMapItemNode::addSegment(seg);
}


ReosMeshItemSegment *ReosMeshItemVertex::segment(int i) const {
    return static_cast<ReosMeshItemSegment*>(ReosMapItemNode::segment(i));
}

ReosMeshItemSegment *ReosMeshItemVertex::segment(ReosMeshItemVertex *otherVertex)
{
    return static_cast<ReosMeshItemSegment*>(ReosMapItemNode::segment(otherVertex));
}

void ReosMeshItemVertex::removeSegment(ReosMeshItemSegment *seg)
{
    ReosMapItemNode::removeSegment(seg);

}


ReosMeshItemSegment::ReosMeshItemSegment(ReosMeshItemVertex *v0, ReosMeshItemVertex *v1, QgsMapCanvas *canvas):ReosMapItemSegment(canvas,v0,v1)
{
    v0->addSegment(this);
    v1->addSegment(this);
    QPen pen(Qt::red);
    pen.setWidth(3);
    setPen(pen);
}

ReosMeshItemSegment::~ReosMeshItemSegment() {}



ReosMapMeshItem::ReosMapMeshItem(QgsMapCanvas *canvas): ReosMapItem (canvas){}

ReosMapMeshItem::~ReosMapMeshItem(){}

ReosMeshItemFace::~ReosMeshItemFace() {}
