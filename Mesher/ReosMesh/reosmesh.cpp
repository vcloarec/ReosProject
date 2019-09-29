/***************************************************************************
                      reosmesh.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email          : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmesh.h"
#include "reosvertexzspecifier.h"

Vertex::Vertex()
{
    mZSpecifier=std::make_unique<ReosVertexZSpecifierSimple>(this);
}

Vertex::Vertex(const Vertex &other)
{
    mGraphic=nullptr;
    mZUserDefined=other.mZUserDefined;
    if (mZSpecifier)
        mZSpecifier=std::unique_ptr<ReosVertexZSpecifier>(other.mZSpecifier->clone(this));
    else
        mZSpecifier=std::unique_ptr<ReosVertexZSpecifier>(nullptr);
}

Vertex::~Vertex() {}

double Vertex::z() {
    if (mZSpecifier)
        return mZSpecifier->zValue();
    else {
        return INVALID_VALUE;
    }
}

void *Vertex::graphicPointer() const {
    return mGraphic;
}


ReosMesh::~ReosMesh() {}

VertexPointer ReosMesh::vertex(double x, double y) const{
    return vertex(x,y,mTolerance);
}

bool ReosMesh::isDirty() const {return mDirty;}



MeshIO::~MeshIO() {}

Segment::Segment(VertexPointer v1, VertexPointer v2):mVertex1(v1),mVertex2(v2)
{}

VertexPointer Segment::first() const {return mVertex1;}

VertexPointer Segment::second() const {return mVertex2;}



bool Face::isVertexContained(VertexPointer vertex) const
{
    bool found=false;
    int i=0;
    while( !found && i<verticesCount())
    {
        found= vertex == vertexPointer(i);
        if (!found)
            ++i;
    }

    return found;
}

std::vector<double> Face::faceCentroid()
{
    double x=0;
    double y=0;
    int n=verticesCount();
    for (int i=0;i<n;++i)
    {
        x+=vertexPointer(i)->x();
        y+=vertexPointer(i)->y();
    }

    std::vector<double> centroid{x/n,y/n};

    return centroid;
}

void Vertex::setGraphicPointer(void *pointer)
{
    mGraphic=pointer;
}

void Vertex::setZUserDefined()
{
    mZUserDefined=true;
}

bool Vertex::isZUserDefined() const
{
    return mZUserDefined;
}

double Vertex::distanceFrom(const Vertex &other) const
{
    return sqrt(pow(x()-other.x(),2)+pow(y()-other.y(),2));
}

void Vertex::setZSpecifier(const ReosVertexZSpecifierFactory &zSpecifierFactory)
{
    mZSpecifier->hasToBeRemove();
    mZSpecifier=zSpecifierFactory.createZSpecifier(this);
    setDirty();
}

void Vertex::setZValue(double z)
{
    mZSpecifier=std::make_unique<ReosVertexZSpecifierSimple>(this,z);
    setDirty();
}


ReosVertexZSpecifier *Vertex::zSpecifier() const {return mZSpecifier.get();}

ReosVertexZSpecifier *Vertex::releaseZSpecifier()
{
    return mZSpecifier.release();
}

void Vertex::addDependentVertex(VertexPointer otherVertex)
{
    mDependentVertices.insert(otherVertex);
}

void Vertex::removeDependentVertex(VertexPointer otherVertex)
{
    mDependentVertices.erase(otherVertex);
}

void Vertex::setDependentVerticesDirty()
{
    for (auto v:mDependentVertices)
        v->setDirty();
}

void Vertex::hasToBeRemoved()
{
    mZSpecifier->hasToBeRemove();
    for (auto v:mDependentVertices)
        v->linkedVertexWillBeRemoved(this);
}

void Vertex::linkedVertexWillBeRemoved(VertexPointer vert)
{
    if (mZSpecifier)
        mZSpecifier=mZSpecifier->surrogateZSpecifier(vert);
}

void Vertex::setDirty()
{
    mZSpecifier->setDirty();
    for (auto dv:mDependentVertices)
        dv->setDirty();
}


