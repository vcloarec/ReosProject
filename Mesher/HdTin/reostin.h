/***************************************************************************
                      reostin.h
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

#ifndef REOSTIN_H
#define REOSTIN_H

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Triangulation_2.h>
#include <CGAL/Constrained_Delaunay_triangulation_2.h>
#include <CGAL/Constrained_triangulation_plus_2.h>


#include "../HdMesh/reosmesh.h"



template < class Gt, class Vb = CGAL::Triangulation_vertex_base_2<Gt> >
class TinCGALVertex:public Vb, public Vertex
{
    typedef Vb Base;
public:
    typedef typename Vb::Vertex_handle      Vertex_handle;
    typedef typename Vb::Face_handle        Face_handle;
    typedef typename Vb::Point              Point;

    template< typename TDS2>
    struct Rebind_TDS {
        typedef typename Vb::template Rebind_TDS<TDS2>::Other Vb2;
        typedef  TinCGALVertex<Gt,Vb2> Other;
    };

    TinCGALVertex() : Base() {}
    TinCGALVertex(const Point & p) : Base(p) {}
    TinCGALVertex(const Point & p, Face_handle f) : Base(f,p) {}
    TinCGALVertex(Face_handle f) : Base(f) {}

    virtual double x() const override {return Vb::point().x();}
    virtual double y() const override {return Vb::point().y();}
    double z() const override {return z_;}
    void setZValue(double ZValue) override {z_=ZValue;}

private:
    double z_;
};

template < class Gt, class Fb = CGAL::Constrained_triangulation_face_base_2<Gt> >
class TinCGALFace:public Fb, public Face
{
    typedef Fb Base;
public:
    typedef typename Fb::Vertex_handle      Vertex_handle;
    typedef typename Fb::Face_handle        Face_handle;

    template< typename TDS2>
    struct Rebind_TDS {
        typedef typename Fb::template Rebind_TDS<TDS2>::Other Fb2;
        typedef  TinCGALFace<Gt,Fb2> Other;
    };

    TinCGALFace() : Base() {}
    TinCGALFace(Vertex_handle v0,Vertex_handle v1,Vertex_handle v2)
      : Base(v0,v1,v2) {}
    TinCGALFace(Vertex_handle v0,Vertex_handle v1,Vertex_handle v2,Face_handle n0,Face_handle n1,Face_handle n2)
      : Base(v0,v1,v2,n0,n1,n2) {}

private:
    void addVertex(VertexPointer vert) override {if(vert==nullptr){};}
    VertexPointer vertexPointer(int i) const override
    {
        i=i%3;
        return &(*this->vertex(i));
    }
    int verticesCount() const override
    {
        return 3;
    }

};



/////////////////////////////////
typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
////////////////////////////////////
////////////// Fake Tin vertex (not bind the Triangulation data structure)
typedef TinCGALVertex<K> FakeTinVertex;
typedef TinCGALFace<K> FakeTinFace;
////////////////////////////////////
////////////// Tin TDS bound Tin vertex, we have to use now Triangulation::Vertex to define the personnal type
typedef CGAL::Triangulation_data_structure_2<FakeTinVertex,FakeTinFace> Tds;

////////////////////////////////////
////////////// Personal TDS
typedef CGAL::Exact_predicates_tag Itag;

typedef CGAL::Constrained_Delaunay_triangulation_2<K, Tds,Itag> ConstrainTriangulation;

typedef CGAL::Constrained_triangulation_plus_2<ConstrainTriangulation> CgalTriangulation;


/////////////////////////////////////////////////
/// Choice of the CGAL triangulation type
///
typedef CgalTriangulation TinTriangulation;

typedef TinTriangulation::Vertex_handle VertexHandle;
typedef TinTriangulation::Face_handle FaceHandle;
typedef TinTriangulation::Vertex_iterator VertexIterator;
typedef TinTriangulation::Face_iterator FaceIterator;
typedef  TinTriangulation::Subconstraint_iterator SegmentIterator;

typedef TinTriangulation::Point CgalPoint;


VertexHandle oppositeVertex(VertexHandle vertex, const TinTriangulation::Edge &edge);

class TinReader:public MeshIO
{

public:
    TinReader(const TinTriangulation *triangulation);

    int vertexCoordCount() const override
    {
        return 3;
    }

    int verticesCount() const override
    {
        return int(mTriangulation->number_of_vertices());
    }


    int currentFaceVerticesCount() const override
    {
        return 3;
    }

    void readVertex(double *vert)  override
    {
        vert[0]=vIt->x();
        vert[1]=vIt->y();
        vert[2]=vIt->z();
        verticesIndex[vIt->handle()]=currentVertexIndex;
        vIt++;
        currentVertexIndex++;
    }

    void readOnlyVertex(double* vert) override
    {
        vert[0]=vIt->x();
        vert[1]=vIt->y();
        vert[2]=vIt->z();
        vIt++;
    }

    VertexPointer readVertexPointer() override
    {
        VertexPointer vert=static_cast<TinTriangulation::Vertex*>(&(*vIt));
        verticesIndex[vIt->handle()]=currentVertexIndex;
        vIt++;
        currentVertexIndex++;
        return vert;
    }

    void readFace(int *face) override
    {
        for (int i=0;i<3;++i)
        {
            face[i]=verticesIndex[fIt->vertex(i)];
        }
        fIt++;
    }

    bool allVerticesReaden() const override
    {
        return vIt==mTriangulation->finite_vertices_end();
    }

    bool allFacesReaden() const override
    {
        return fIt==mTriangulation->finite_faces_end();
    }

    void readSegment(int *sc) override
    {
        sc[0]=verticesIndex[sIt->first.first->handle()];
        sc[1]=verticesIndex[sIt->first.second->handle()];
        sIt++;
    }

    bool allSegmentsReaden() const override
    {
        return sIt==mTriangulation->subconstraints_end();
    }

private:
    FaceIterator fIt;
    VertexIterator vIt;
    SegmentIterator sIt;
    std::map<VertexHandle, int> verticesIndex;
    const TinTriangulation *mTriangulation;
    int currentVertexIndex=0;
};


class ReosTin: public HdMesh
{
public:
    ReosTin(){}

    int verticesCount() const override;
    int facesCount() const override;

    VertexPointer vertex(double x, double y, double tolerance) const override;
    FacePointer face(double x,double y) const override
    {
        auto faceHandle=triangulation.locate(CgalPoint(x,y));
        return &(*faceHandle);
    }

    void clear() override {}
    void clearFaces() override {}

    VertexPointer addVertex(double x, double y) override;
    void removeVertex(VertexPointer vertex);
    std::list<VertexPointer> removeVertexOnHardLine(VertexPointer vertex);

    std::list<VertexPointer> addHardLine(VertexPointer v1, VertexPointer v2) override;
    std::list<VertexPointer> hardNeighbours(VertexPointer vertex) const override;
    std::list<VertexPointer> removeHardLine(VertexPointer v1, VertexPointer v2) override;

    std::unique_ptr<MeshIO> getReader() const override;

    bool isOnHardLine(VertexPointer v1, VertexPointer v2) const;
    bool isOnHardLine(VertexPointer v1) const;

    virtual std::list<VertexPointer> neighboursVertices(VertexPointer vertex) const override;

    bool flipFaces(FacePointer f1, FacePointer f2)
    {
        auto face_1=faceHandle(static_cast<TinTriangulation::Face*>(f1));
        auto face_2=faceHandle(static_cast<TinTriangulation::Face*>(f2));

        int index=face_1->index(face_2);

        triangulation.flip(face_1,index);

        return true;
    }


private:
    TinTriangulation triangulation;

    FaceHandle faceHandle(TinTriangulation::Face* f)
    {
        if (!f)
            return nullptr;

        auto vert=f->vertex(0);
        auto cir=triangulation.incident_faces(vert);

        while (&(*cir)!=f)
        {
            cir++;
        }

        return cir;
    }

};



#endif // REOSTIN_H
