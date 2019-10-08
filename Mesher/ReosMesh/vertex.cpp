/***************************************************************************
                      vertex.cpp
                     --------------------------------------
Date                 : 01-09-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ******************************************************************************
 *                                                                            *
 *   This program is free software; you can redistribute it and/or modify     *
 *   it under the terms of the GNU General Public License as published by     *
 *   the Free Software Foundation; either version 2 of the License, or        *
 *   (at your option) any later version.                                      *
 *                                                                            *
 *****************************************************************************/

#include "vertex.h"

#include "reosmesh.h"
#include "reosvertexzspecifier.h"

Vertex::Vertex()
{
    mZSpecifier=std::make_unique<ReosVertexZSpecifierSimple>(this);
}

Vertex::Vertex(const Vertex &other)
{
    mGraphic=nullptr;
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


double Vertex::distanceFrom(const Vertex &other) const
{
    return sqrt(pow(x()-other.x(),2)+pow(y()-other.y(),2));
}

void Vertex::setZSpecifier(const ReosVertexZSpecifierFactory &zSpecifierFactory)
{
    if ( ! zSpecifierFactory.IsCompatibleZSpecifier(this))
        return;
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

bool Vertex::isSpecifierIsCompatible(const ReosVertexZSpecifierFactory &zSpecifierFactory)
{
    return zSpecifierFactory.IsCompatibleZSpecifier(this);
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
    auto v=mDependentVertices.begin();
    while (v!=mDependentVertices.end())
    {
        size_t dependentCount=mDependentVertices.size();
        (*v)->linkedVertexWillBeRemoved(this);
        if (dependentCount!=mDependentVertices.size())
            v=mDependentVertices.begin();
        else
            v++;
    }
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

