#include "hdmesh.h"


Vertex::~Vertex() {}


HdMesh::~HdMesh() {}

MeshIO::~MeshIO() {}

Segment::Segment(VertexPointer v1, VertexPointer v2):mVertex1(v1),mVertex2(v2)
{}

VertexPointer Segment::first() const {return mVertex1;}

VertexPointer Segment::second() const {return mVertex2;}

void Face::addVertex(VertexPointer vert)
{
    mVertices.push_back(vert);
}

VertexPointer Face::vertex(int i) const
{
    if (i>=0 && i<int(mVertices.size()))
        return mVertices[size_t(i)];
    else {
        return VertexPointer();
    }
}

int Face::verticesCount() const  {return int(mVertices.size());}
