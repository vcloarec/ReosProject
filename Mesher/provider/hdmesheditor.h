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
    TINEditor(std::vector<Vertex> &inputVertices, std::vector<Segment> &segments, std::vector<Face> &mMeshFaces);

    void addMeshGenerator(HdMeshGenerator* generator);
    bool containMeshGenerator(std::string key);
    void setCurrentMeshGenerator(std::string key);
    HdMeshGenerator* currentMeshGenerator() const;

    int addVertex(const Vertex &vert);
    int vertexIndex(const Vertex& vert) const;
    int ivertexCount()const;

    bool addSegment(int n0,int n1);

    int findSegmentWithVertex(int n0, int n1);


    bool generateMesh();
    int facesCount() const;
    int verticesCount() const;
    int segmentsCount() const;

    const std::vector<Vertex>& vertices() const {return mVertices;}
    const std::vector<Segment>& segments() const {return mSegments;}

    const std::vector<Face>& meshFaces() const {return mMeshFaces;}

    void setZValue(int vertIndex, double ZValue);

    double tolerance() const;
    void setTolerance(double tolerance);

private:
    HdMeshGenerator *mCurrentMeshGenerator=nullptr;

    std::map<std::string,HdMeshGenerator*> mapGenerator;

    std::vector<Vertex> &mVertices;
    std::vector<Segment> &mSegments;
    std::vector<Face> &mMeshFaces;

    double mTolerance=0.000001;

};

#endif // HDMESHEDITOR_H
