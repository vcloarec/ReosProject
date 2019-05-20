#ifndef HDMESH_H
#define HDMESH_H

#include <memory>
#include <vector>

class Vertex
{
public:

    virtual ~Vertex();

    virtual double x() const =0;
    virtual double y() const =0;
    virtual double z() const=0;

    virtual void setZValue(double ZValue)=0;

};

typedef Vertex* VertexPointer;


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
    void addVertex(VertexPointer vert);

    VertexPointer vertex(int i) const;

    int verticesCount() const;

private:
    std::vector<VertexPointer> mVertices;
};


typedef Face* FacePointer;

class MeshIO
{
public:
    virtual ~MeshIO();
    virtual int vertexCoordCount() const =0;
    virtual int verticesCount() const =0;
    virtual void readVertex (double *)=0;
    virtual void readOnlyVertex(double *) =0;

    virtual int currentFaceVerticesCount() const =0;
    virtual void readFace(int *)=0;

    virtual bool allVerticesReaden() const =0;
    virtual bool allFacesReaden() const =0;


};


class HdMesh
{
public:
    virtual ~HdMesh();
    virtual int verticesCount() const=0;
    virtual int facesCount() const=0;

    virtual VertexPointer vertex(double x, double y, double tolerance) const=0;

    virtual void clear()=0;
    virtual void clearFaces()=0;

    virtual VertexPointer addVertex(double x, double y)=0;

    virtual std::unique_ptr<MeshIO> getReader() const =0;


};






#endif // HDMESH_H
