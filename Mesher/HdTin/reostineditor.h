/***************************************************************************
                      hdtineditor.h
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


#ifndef HDTINEDITOR_H
#define HDTINEDITOR_H

#include "reostin.h"

class TINEditor
{
public:
    TINEditor(ReosTin &tin,std::vector<Segment> &segments);

    VertexPointer addVertex(double x, double y);
    VertexPointer vertex(double x, double y) const;


    void removeVertex(VertexPointer vert)
    {
        mTin.removeVertex(vert);
    }

    std::list<VertexPointer> removeHardLine(VertexPointer vert1, VertexPointer vert2)
    {
        return mTin.removeHardLine(vert1,vert2);
    }

    std::list<VertexPointer> addSegment(VertexPointer v1, VertexPointer v2);

    std::list<VertexPointer> hardNeighbours(VertexPointer vert) const;
    bool isVertexOnHarLine(VertexPointer vert) const {return mTin.isOnHardLine(vert);}

    int facesCount() const;
    int verticesCount() const;
    int segmentsCount() const;

    double tolerance() const;
    void setTolerance(double tolerance);

    std::unique_ptr<MeshIO> getTinReader() const;

    bool isDirty() const {return mDirty;}



private:
    ReosTin &mTin;
    std::vector<Segment> &mSegments;

    double mTolerance=0.01;

    bool mDirty=false;

};


#endif // HDTINEDITOR_H
