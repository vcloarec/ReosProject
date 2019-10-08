/***************************************************************************
                      vertex.h
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

#ifndef VERTEX_H
#define VERTEX_H

#include <vector>
#include <memory>
#include <set>

class ReosVertexZSpecifier;
class ReosVertexZSpecifierFactory;
class Vertex;

typedef Vertex* VertexPointer;

class Vertex
{
public:
    Vertex();
    Vertex(const Vertex &other);

    virtual ~Vertex();

    virtual double x() const =0;
    virtual double y() const =0;
    double z();


    void setGraphicPointer(void* pointer);
    void *graphicPointer() const;

    double distanceFrom(const Vertex &other) const;

    //Methods to deal with Z vlaue
    void setZSpecifier(const ReosVertexZSpecifierFactory &zSpecifierFactory);
    void setZValue(double z);
    ReosVertexZSpecifier* zSpecifier() const;
    ReosVertexZSpecifier* releaseZSpecifier();
    bool isSpecifierIsCompatible(const ReosVertexZSpecifierFactory &zSpecifierFactory);


    //Methods to deal with dependent vertex
    void addDependentVertex(VertexPointer otherVertex);
    void removeDependentVertex(VertexPointer otherVertex);
    void setDependentVerticesDirty();

    void hasToBeRemoved();

    virtual void linkedVertexWillBeRemoved(VertexPointer vert);

protected: //methode
    void setDirty();

private: //attribute
    void* mGraphic=nullptr;
    std::unique_ptr<ReosVertexZSpecifier> mZSpecifier;

    std::set<Vertex*> mDependentVertices;

};




class Segment
{
public:
    Segment(VertexPointer v1, VertexPointer v2);

    VertexPointer first() const;
    VertexPointer second() const;

private:
    VertexPointer mVertex1;
    VertexPointer mVertex2;
};

class Face
{
public:
    virtual ~Face() {}
    virtual void addVertex(VertexPointer vert)=0;
    virtual VertexPointer vertexPointer(int i) const =0;
    virtual int verticesCount() const=0;
    virtual void setNeighbor(int side, Face*) {}
    virtual bool isVertexContained(VertexPointer vertex) const;

    std::vector<double> faceCentroid();
};


typedef Face* FacePointer;


#endif // VERTEX_H
