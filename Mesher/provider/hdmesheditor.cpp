/***************************************************************************
                      hdmesheditor.cpp
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

#include "hdmesheditor.h"

TINEditor::TINEditor(HdMesh &mesh, std::vector<Segment> &segments):
    mMesh(mesh),mSegments(segments)
{
}

VertexPointer TINEditor::addVertex(const Vertex &vert)
{
    return addVertex(vert.x(),vert.y());
}

VertexPointer TINEditor::addVertex(double x, double y)
{
    int index=mMesh.vertexIndex(x,y,tolerance());
    if (index==-1)
    {
        VertexPointer vert=Vertex::makeVertex(x,y);
        mMesh.addVertex(vert);
        return vert;
    }
    else {
        return mMesh.vertex(index);
    }
}

int TINEditor::vertexIndex(const Vertex &vert) const
{
    return mMesh.vertexIndex(vert.x(),vert.y(),tolerance());
}

bool TINEditor::addSegment(int n0, int n1)
{
    if (n0 >= verticesCount() || n1 >= verticesCount() || n0==n1)
        return false;

    int indexSegment=findSegmentWithVertex(n0,n1);

    if(indexSegment>=0)
        return false;

    mSegments.push_back({mMesh.vertex(n0),mMesh.vertex(n1)});

    return true;
}

int TINEditor::findSegmentWithVertex(int n0, int n1)
{
    if (n0 >= verticesCount() || n1 >= verticesCount() || n0==n1)
        return -1;

    bool found=false;
    size_t i=0;
    while (!found && i<mSegments.size())
    {
        found=(mSegments.at(i).first()==mMesh.vertex(n0) && mSegments.at(i).second()==mMesh.vertex(n1))||
                (mSegments.at(i).first()==mMesh.vertex(n1) && mSegments.at(i).second()==mMesh.vertex(n0));
        if (!found)
            ++i;
    }
    if (found)
        return int(i);
    else
        return -1;
}

VertexPointer TINEditor::vertex(int i) const {return mMesh.vertex(i);}



int TINEditor::verticesCount() const {
    return mMesh.verticesCount();
}

int TINEditor::segmentsCount() const
{
    return int(mSegments.size());
}


void TINEditor::addMeshGenerator(HdMeshGenerator *generator)
{
    mapGenerator[generator->getKey()]=generator;
}

bool TINEditor::containMeshGenerator(std::string key)
{
    return (mapGenerator.find(key)!=mapGenerator.end());
}

void TINEditor::setCurrentMeshGenerator(std::string key)
{
    if (containMeshGenerator(key))
        mCurrentMeshGenerator=mapGenerator[key];
    else {
        mCurrentMeshGenerator=nullptr;
    }
}

HdMeshGenerator *TINEditor::currentMeshGenerator() const {
    return mCurrentMeshGenerator;
}

bool TINEditor::generateMesh()
{
    if (!mCurrentMeshGenerator)
        return false;

    mCurrentMeshGenerator->clear();
    bool result=mCurrentMeshGenerator->triangulateTIN(mMesh,mSegments);

    return result;

}

int TINEditor::facesCount() const
{
    return mMesh.facesCount();
}



double TINEditor::tolerance() const
{
    return mTolerance;
}

void TINEditor::setTolerance(double tolerance)
{
    mTolerance = tolerance;
}

