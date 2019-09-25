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

void Vertex::setZSpecifier(const VertexZSpecifierFactory &zSpecifierFactory)
{
    mZSpecifier=zSpecifierFactory.createZSpecifier(this);
    setDirty();
}

void Vertex::setZValue(double z)
{
    mZSpecifier=std::make_unique<VertexZSpecifierSimple>(this,z);
    setDirty();
}


VertexZSpecifier *Vertex::zSpecifier() const {return mZSpecifier.get();}

VertexZSpecifier *Vertex::releaseZSpecifier()
{
    return mZSpecifier.release();
}

void Vertex::setDirty()
{
    mZSpecifier->setDirty(true);
    for (auto dv:mDependentVertices)
        dv->setDirty();
}

VertexZSpecifierSimple::VertexZSpecifierSimple(const VertexPointer associatedVertex):
    VertexZSpecifier(associatedVertex)
{
    mZValue=0;
    mDirty=false;
}

VertexZSpecifierSimple::VertexZSpecifierSimple(const VertexPointer associatedVertex,double z):VertexZSpecifier(associatedVertex)
{
    mZValue=z;
    mDirty=false;
}

VertexZSpecifier *VertexZSpecifierSimple::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifierSimple(associatedVertex,mZValue);
}


VertexZSpecifier::VertexZSpecifier(const VertexPointer associatedVertex):
    mAssociatedVertex(associatedVertex)
{

}

VertexZSpecifier::~VertexZSpecifier(){}

VertexZSpecifier *VertexZSpecifier::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifier(associatedVertex);
}

double VertexZSpecifier::zValue() const
{
    std::lock_guard<std::mutex> g(mMutex);
    if (mDirty)
        calculateZValue();
    mDirty=false;
    return mZValue;
}

void VertexZSpecifier::setDirty(bool b)
{
    std::lock_guard<std::mutex> g(mMutex);
    mDirty=b;
}


VertexZSpecifierOtherVertexAndSlope::VertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex, VertexPointer otherVertex, double slope):
    VertexZSpecifierDependOnOtherVertex(associatedVertex,otherVertex),mSlope(slope)
{

}

VertexZSpecifier *VertexZSpecifierOtherVertexAndSlope::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifierOtherVertexAndSlope(associatedVertex,mOtherVertex,mSlope);
}

void VertexZSpecifierOtherVertexAndSlope::calculateZValue() const
{
    if(mOtherVertex)
        mZValue=mOtherVertex->z()+mSlope*mOtherVertex->distanceFrom(*mAssociatedVertex);
    else
        mZValue=INVALID_VALUE;
}


VertexZSpecifierOtherVertexAndGap::VertexZSpecifierOtherVertexAndGap(VertexPointer associatedVertex, VertexPointer otherVertex, double gap):
    VertexZSpecifierDependOnOtherVertex(associatedVertex,otherVertex),mGap(gap)
{

}

VertexZSpecifier *VertexZSpecifierOtherVertexAndGap::clone(VertexPointer associatedVertex) const
{
    return new VertexZSpecifierOtherVertexAndGap(associatedVertex,mOtherVertex,mGap);
}


VertexZSpecifierDependOnOtherVertex::VertexZSpecifierDependOnOtherVertex(VertexPointer associatedVertex, VertexPointer otherVertex):
    VertexZSpecifier (associatedVertex),mOtherVertex(otherVertex)
{
    if (otherVertex && associatedVertex)
        otherVertex->addDependentVertex(associatedVertex);
}

VertexZSpecifierSimpleFactory::VertexZSpecifierSimpleFactory(double zValue):VertexZSpecifierFactory(),mZValue(zValue)
{

}

std::unique_ptr<VertexZSpecifier> VertexZSpecifierSimpleFactory::createZSpecifier(const VertexPointer associatedVertex) const
{
    return std::make_unique<VertexZSpecifierSimple>(associatedVertex,mZValue);
}

VertexZSpecifierInterpolationFactory::VertexZSpecifierInterpolationFactory() {}

VertexZSpecifierInterpolationFactory::VertexZSpecifierInterpolationFactory(VertexPointer firstVertex, VertexPointer secondVertex, bool hvf, bool hvs):
    VertexZSpecifierFactory(),mFirstVertex(firstVertex),mSecondVertex(secondVertex),mHardVertexFirst(hvf),mHardVertexSecond(hvs)
{

}

void VertexZSpecifierInterpolationFactory::setExtremitiesVertices(VertexPointer firstVertex, VertexPointer secondVertex)
{
    mFirstVertex=firstVertex;
    mSecondVertex=secondVertex;
    mHardVertexFirst=true;
    mHardVertexSecond=true;
}

void VertexZSpecifierInterpolationFactory::setHardVertexFirst(bool b)
{
    mHardVertexFirst=b;
}

void VertexZSpecifierInterpolationFactory::setHardVertexSecond(bool b)
{
    mHardVertexSecond=b;
}

std::unique_ptr<VertexZSpecifier> VertexZSpecifierInterpolationFactory::createZSpecifier(const VertexPointer associatedVertex) const
{
    if (associatedVertex==mFirstVertex || associatedVertex==mSecondVertex)
    {
        return std::unique_ptr<VertexZSpecifier>(associatedVertex->releaseZSpecifier());
    }

    return std::make_unique<VertexZSpecifierInterpolation>(associatedVertex,mFirstVertex,mSecondVertex,mHardVertexFirst,mHardVertexSecond);
}
