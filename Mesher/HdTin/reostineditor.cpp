/***************************************************************************
                      reostineditor.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reostineditor.h"

TINEditor::TINEditor(ReosTin &tin, std::vector<Segment> &segments):
    mTin(tin),mSegments(segments)
{
}

VertexPointer TINEditor::addVertex(double x, double y)
{
    mDirty=true;
    return mTin.addVertex(x,y);

}

VertexPointer TINEditor::vertex(double x, double y) const
{
    return mTin.vertex(x,y,tolerance());
}

std::list<VertexPointer> TINEditor::addSegment(VertexPointer v1, VertexPointer v2)
{
    mDirty=true;
    return mTin.addHardLine(v1,v2);
}

std::list<VertexPointer> TINEditor::hardNeighbours(VertexPointer vert) const
{
    return mTin.hardNeighbours(vert);
}

int TINEditor::facesCount() const
{
    return mTin.facesCount();
}

int TINEditor::verticesCount() const {
    return mTin.verticesCount();
}

int TINEditor::segmentsCount() const
{
    return int(mSegments.size());
}

double TINEditor::tolerance() const
{
    return mTolerance;
}

void TINEditor::setTolerance(double tolerance)
{
    mDirty=true;
    mTolerance = tolerance;
}

std::unique_ptr<MeshIO> TINEditor::getTinReader() const {return mTin.getReader();}

#include "reostineditor.h"
