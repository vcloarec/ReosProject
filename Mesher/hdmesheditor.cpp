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

TINEditor::TINEditor(std::vector<Vertex> &inputVertices, std::vector<Segment> &segments, std::vector<Face> &meshFaces):
    mVertices(inputVertices),mSegments(segments),mMeshFaces(meshFaces)
{

}

int TINEditor::addVertex(const Vertex &vert)
{
    int index=vertexIndex(vert);
    if (index==-1)
    {
        mVertices.push_back(vert);
        return int(mVertices.size()-1);
    }
    else {
        return index;
    }
}

int TINEditor::vertexIndex(const Vertex &vert) const
{
    bool found=0;
    size_t i=0;

    while ( i<mVertices.size() && !found )
    {
        found=fabs(vert.x()-mVertices.at(i).x())<=tolerance() && fabs(vert.y()-mVertices.at(i).y())<=tolerance();
        if (!found)
            ++i;
    }
    if (found)
        return int(i);

    return -1;
}

bool TINEditor::addSegment(int n0, int n1)
{
    if (n0 >= verticesCount() || n1 >= verticesCount() || n0==n1)
        return false;

    int indexSegment=findSegmentWithVertex(n0,n1);

    if(indexSegment>=0)
        return false;

    mSegments.push_back({n0,n1});

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
        found=(mSegments.at(i).first==n0 && mSegments.at(i).second==n1)||
                (mSegments.at(i).first==n1 && mSegments.at(i).second==n0);
        if (!found)
            ++i;
    }
    if (found)
        return int(i);
    else
        return -1;
}



int TINEditor::verticesCount() const {
    return int(mVertices.size());
}

int TINEditor::segmentsCount() const
{
    return int(mSegments.size());
}

void TINEditor::setZValue(int vertIndex, double ZValue)
{
    if (vertIndex<verticesCount())
    {
        mVertices.at(size_t(vertIndex)).setZValue(ZValue);
    }

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
    bool result=mCurrentMeshGenerator->triangulateTIN(mVertices,mSegments,mMeshFaces);

    return result;

}

int TINEditor::facesCount() const
{
    return int(mMeshFaces.size());
}



double TINEditor::tolerance() const
{
    return mTolerance;
}

void TINEditor::setTolerance(double tolerance)
{
    mTolerance = tolerance;
}

