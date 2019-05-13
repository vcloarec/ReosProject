/***************************************************************************
                      hdmesheditor.h
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

#ifndef HDMESHEDITOR_H
#define HDMESHEDITOR_H


#include "hdmeshgenerator.h"


class TINEditor
{
public:
    TINEditor(HdMesh &mesh,std::vector<Segment> &segments);

    void addMeshGenerator(HdMeshGenerator* generator);
    bool containMeshGenerator(std::string key);
    void setCurrentMeshGenerator(std::string key);
    HdMeshGenerator* currentMeshGenerator() const;

    VertexPointer addVertex(const Vertex &vert);
    VertexPointer addVertex(double x, double y);
    int vertexIndex(const Vertex& vert) const;


    bool addSegment(int n0,int n1);
    int findSegmentWithVertex(int n0, int n1);

    VertexPointer vertex(int i) const;

    bool generateMesh();
    int facesCount() const;
    int verticesCount() const;
    int segmentsCount() const;

    double tolerance() const;
    void setTolerance(double tolerance);

private:
    HdMeshGenerator *mCurrentMeshGenerator=nullptr;

    std::map<std::string,HdMeshGenerator*> mapGenerator;

    HdMesh &mMesh;
    std::vector<Segment> &mSegments;

    double mTolerance=0.01;

};

#endif // HDMESHEDITOR_H
