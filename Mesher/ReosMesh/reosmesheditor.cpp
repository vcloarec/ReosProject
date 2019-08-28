/***************************************************************************
                      reosmesheditor.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmesheditor.h"


HdMeshEditor::HdMeshEditor(HdMeshBasic &mesh, std::vector<Segment> &segments):mMesh(mesh),mSegments(segments)
{

}

void HdMeshEditor::addMeshGenerator(HdMeshGenerator *generator)
{
    mapGenerator[generator->getKey()]=generator;
}

bool HdMeshEditor::containMeshGenerator(std::string key)
{
    return (mapGenerator.find(key)!=mapGenerator.end());
}

void HdMeshEditor::setCurrentMeshGenerator(std::string key)
{
    if (containMeshGenerator(key))
        mCurrentMeshGenerator=mapGenerator[key];
    else {
        mCurrentMeshGenerator=nullptr;
    }
}

HdMeshGenerator *HdMeshEditor::currentMeshGenerator() const {
    return mCurrentMeshGenerator;
}

VertexPointer HdMeshEditor::addVertex(const VertexBasic &vert)
{
    return addVertex(vert.x(),vert.y());
}

VertexPointer HdMeshEditor::addVertex(double x, double y)
{
    VertexPointer vert=mMesh.vertex(x,y,tolerance());
    if (vert)
    {
        return vert;
    }
    else {
        return mMesh.addVertex(x,y);
    }
}

bool HdMeshEditor::addSegment(int n0, int n1)
{
    if (n0 >= verticesCount() || n1 >= verticesCount() || n0==n1)
        return false;

    int indexSegment=findSegmentWithVertex(n0,n1);

    if(indexSegment>=0)
        return false;

    mSegments.push_back({mMesh.vertex(n0),mMesh.vertex(n1)});

    return true;
}

int HdMeshEditor::findSegmentWithVertex(int n0, int n1)
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

VertexPointer HdMeshEditor::vertex(double x, double y) const
{
    return mMesh.vertex(x,y,tolerance());
}

bool HdMeshEditor::generateMesh()
{
    if (!mCurrentMeshGenerator)
        return false;

    mCurrentMeshGenerator->clear();
    bool result=mCurrentMeshGenerator->triangulateTIN(mMesh,mSegments);

    return result;

}

int HdMeshEditor::facesCount() const
{
    return mMesh.facesCount();
}

int HdMeshEditor::verticesCount() const {
    return mMesh.verticesCount();
}

int HdMeshEditor::segmentsCount() const
{
    return int(mSegments.size());
}

double HdMeshEditor::tolerance() const
{
    return mTolerance;
}

void HdMeshEditor::setTolerance(double tolerance)
{
    mTolerance = tolerance;
}









