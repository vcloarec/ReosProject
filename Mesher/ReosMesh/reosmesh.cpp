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


Vertex::Vertex()
{
    mZSpecifier=std::make_unique<VertexZSpecifierSimple>(this);
}

Vertex::Vertex(const Vertex &other)
{
    mGraphic=nullptr;
    mZUserDefined=other.mZUserDefined;
    if (mZSpecifier)
        mZSpecifier=std::unique_ptr<VertexZSpecifier>(mZSpecifier->clone(this));
    else
        mZSpecifier=std::unique_ptr<VertexZSpecifier>(nullptr);
}

Vertex::~Vertex() {}

double Vertex::z() {
    if (mZSpecifier)
        return mZSpecifier->getZValue();
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

void Vertex::setZSpecifier(const VertexZSPecifierFactory &zSpecifierFactory)
{
    mZSpecifier=std::unique_ptr<VertexZSpecifier>(zSpecifierFactory.createZSpecifier(this));
}

void Vertex::setZValue(double z)
{
    mZSpecifier=std::make_unique<VertexZSpecifierSimple>(this,z);
}


VertexZSpecifier *Vertex::zSpecifier() const {return mZSpecifier.get();}

VertexZSpecifierSimple::VertexZSpecifierSimple(const VertexPointer associatedVertex):
    VertexZSpecifier(associatedVertex){}

VertexZSpecifierSimple::VertexZSpecifierSimple(const VertexPointer associatedVertex,double z):VertexZSpecifier(associatedVertex),mZValue(z)
{

}

VertexZSpecifier *VertexZSpecifierSimple::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifierSimple(associatedVertex,mZValue);
}

double VertexZSpecifierSimple::getZValue() const
{
    return mZValue;
}

VertexZSpecifier::~VertexZSpecifier(){}

VertexZSpecifierOtherVertexAndSlope::VertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex, VertexPointer otherVertex, double slope):
    VertexZSpecifierDependOnOtherVertex(associatedVertex,otherVertex),mSlope(slope)
{

}

VertexZSpecifier *VertexZSpecifierOtherVertexAndSlope::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifierOtherVertexAndSlope(associatedVertex,mOtherVertex,mSlope);
}

double VertexZSpecifierOtherVertexAndSlope::getZValue() const
{
    if(mOtherVertex)
        return mOtherVertex->z()+mSlope*mOtherVertex->distanceFrom(*mAssociatedVertex);
    else
        return INVALID_VALUE;

}

VertexZSpecifierOtherVertexAndGap::VertexZSpecifierOtherVertexAndGap(VertexPointer associatedVertex, VertexPointer otherVertex, double gap):
    VertexZSpecifierDependOnOtherVertex(associatedVertex,otherVertex),mGap(gap)
{

}

VertexZSpecifier *VertexZSpecifierOtherVertexAndGap::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifierOtherVertexAndGap(associatedVertex,mOtherVertex,mGap);
}

double VertexZSpecifierOtherVertexAndGap::getZValue() const
{
    if(mOtherVertex)
        return mOtherVertex->z()+mGap;
    else
        return INVALID_VALUE;

}
