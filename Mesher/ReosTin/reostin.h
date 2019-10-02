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
#include <CGAL/Constrained_Delaunay_triangulation_2.h>
#include <CGAL/Constrained_triangulation_plus_2.h>

#include "../ReosMesh/reosmesh.h"

template<typename T>
class TD;

class TINVertex;

template < class Gt, class Vb = CGAL::Triangulation_vertex_base_2<Gt> >
class TinCGALVertex:public Vb//, public Vertex
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

    ~TinCGALVertex()
    {
        if (mTINVertex)
        {
            delete mTINVertex;
        }
    }
    //virtual double x() const override {return Vb::point().x();}
    //virtual double y() const override {return Vb::point().y();}

    TINVertex * tinVertex()
    {
        return mTINVertex;
    }

    void setTinVertex(VertexPointer *vert)
    {
        mTINVertex=vert;
    }

private:
    TINVertex *mTINVertex=nullptr;

    friend class TINVertex;

};



template < class Gt, typename Fb = CGAL::Constrained_triangulation_face_base_2<Gt> >
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

    TinCGALFace() : Base()
    {}
    TinCGALFace(Vertex_handle v0,Vertex_handle v1,Vertex_handle v2)
      : Base(v0,v1,v2)
    {}
    TinCGALFace(Vertex_handle v0,Vertex_handle v1,Vertex_handle v2,Face_handle n0,Face_handle n1,Face_handle n2)
      : Base(v0,v1,v2,n0,n1,n2)
    {}


    VertexPointer vertexPointer(int i) const override;


private:
    void addVertex(VertexPointer vert) override {if(vert==nullptr){};}

    int verticesCount() const override
    {
        return 3;
    }

    const void* self() const//Trick to acces to the methods of the Triangulation_ds_face_base_2 class (this classe is derived from). Necessary with MSVC not with GCC
    {
        return this;
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
//typedef CgalTriangulation TinTriangulation;
class TinTriangulation:public CgalTriangulation
{
public:
    //Necessary to reconstruct the hard lines when reading UGRID file
    //because the insert_subconstraint(Vertex_handle, Vertex_handle) is protected in CgalTriangulation
    void insertSubconstraint(Vertex_handle v1, Vertex_handle v2)
    {
        insert_subconstraint(v1,v2);
    }
};


typedef TinTriangulation::Vertex_handle VertexHandle;
typedef TinTriangulation::Face_handle FaceHandle;
typedef TinTriangulation::Vertex_iterator VertexIterator;
typedef TinTriangulation::Face_iterator FaceIterator;
typedef  TinTriangulation::Subconstraint_iterator SegmentIterator;
typedef  TinTriangulation::Constraint_iterator ConstraintIterator;

typedef TinTriangulation::Point CgalPoint;


VertexHandle oppositeVertex(VertexHandle vertex, const TinTriangulation::Edge &edge);

class TINVertex:public Vertex
{
public:
    TINVertex(VertexHandle cgalVert);

    // Vertex interface
    double x() const override
    {
        return mCgalVertex->point().x();
    }
    double y() const override
    {
        return mCgalVertex->point().y();
    }

    VertexHandle handle() const {return mCgalVertex;}

private:
    VertexHandle mCgalVertex;

};

template<class Gt, typename Fb>
VertexPointer TinCGALFace<Gt,Fb>::vertexPointer(int i) const
{
    auto f=static_cast<const TinTriangulation::Face*>(self());
    return f->vertex(i)->tinVertex();
}




class TinReader:public MeshIO
{

public:
    TinReader(const TinTriangulation *triangulation);

    int vertexCoordCount() const override;
    int verticesCount() const override;
    void readVertex(double *vert)  override;
    void readOnlyVertex(double* vert) override;
    VertexPointer readVertexPointer() override;
    bool allVerticesReaden() const override;

    void readNodePerFace(int &count) override;
    int currentFaceVerticesCount() const override;
    void readFace(int *face) override;
    bool allFacesReaden() const override;

    void readSegment(int *sc) override;
    bool allSegmentsReaden() const override;

    void readNeighbor(int *n) override;

    int hardlinesCount() const override;
    int hardlinesVerticesCount() const override;
    int currentHardlineVerticesCount() const override;
    void readHardlineVertices(int *) override;
    bool allHardLineReaden() const override;

    int boundariesCount() const override;
    void readBoundaryEdge(int *be) override;
    bool allBoundaryEdgesReaden() const override;

    int zSpecifierCount() const override;
    void readSpecifier(ReosVertexZSpecifier::Data &zSpecifierData) override
    {
        zSpecifierData=vItForZSpecifier->tinVertex()->zSpecifier()->data();

        zSpecifierData.verticesIndexes.push_back(verticesIndex[vItForZSpecifier->handle()]);
        for (auto v:zSpecifierData.otherVertices)
        {
            if (v)
                zSpecifierData.verticesIndexes.push_back(verticesIndex[static_cast<TINVertex*>(v)->handle()]);
            else
                zSpecifierData.verticesIndexes.push_back(-1);
        }

        vItForZSpecifier++;
        while(vItForZSpecifier!=mTriangulation->finite_vertices_end()
              && vItForZSpecifier->tinVertex()->zSpecifier()->type()==ReosVertexZSpecifier::Type::Simple)
        {
            vItForZSpecifier++;
        }
    }
    bool allZSpecifierReaden() const override
    {
        return vItForZSpecifier==mTriangulation->finite_vertices_end();
    }


    bool allNeighborReaden() const override;

private:
    FaceIterator fIt;
    VertexIterator vIt;
    SegmentIterator sIt;
    FaceIterator fItN;
    ConstraintIterator cIt;
    std::list<std::vector<int>>::iterator boundaryIterator;
    std::map<VertexHandle, int> verticesIndex;
    std::map<FaceHandle,int> facesIndex;
    std::list<std::vector<int>> boundariesList;
    const TinTriangulation *mTriangulation;
    int currentVertexIndex=0;
    int currentFaceIndex=0;
    int mZSpecifierCount=0;
    VertexIterator vItForZSpecifier;


};


class ReosTin: public ReosMesh
{
public:
    ReosTin(){}

    int verticesCount() const override;
    int facesCount() const override;

    VertexPointer vertex(double x, double y, double tolerance) const override;
    VertexPointer vertex(double x, double y) const override;
    FacePointer face(double x,double y) const override;

    void initialize(int verticesCount) override ;
    void clear() override;
    void clearFaces() override {}

    VertexPointer createVertex(double x,double y) override;
    VertexPointer insertVertex(double x,double y) override ;
    VertexPointer addVertex(double x, double y) override;
    int maxNodesPerFaces() const override {return 3;}

    FacePointer createFace(const std::vector<VertexPointer> &vertices);



    void removeVertex(VertexPointer vertex);
    std::list<VertexPointer> removeVertexOnHardLine(VertexPointer vertex);

    std::list<VertexPointer> addHardLine(VertexPointer v1, VertexPointer v2) override;
    std::list<VertexPointer> hardNeighbours(VertexPointer vertex) const override;
    std::list<VertexPointer> removeHardLine(VertexPointer v1, VertexPointer v2) override;

    std::unique_ptr<MeshIO> getReader() const override;

    bool isOnHardLine(VertexPointer v1, VertexPointer v2) const;
    bool isOnHardLine(VertexPointer v1) const;

    virtual std::list<VertexPointer> neighboursVertices(VertexPointer vertex) const override;

    bool isFlipable(FacePointer f1, FacePointer f2) const;

    /////////////////////////////////////////////////////////////
    /// \brief flipFaces
    /// \param f1 first face to flip
    /// \param f2 second face to flip
    /// \return return the two new faces
    ///
    std::vector<FacePointer> flipFaces(FacePointer f1, FacePointer f2);

    int readUGRIDFormat(std::string fileName) override;

private:
    TinTriangulation triangulation;

    FaceHandle faceHandle(TinTriangulation::Face* f) const;

};



#endif // REOSTIN_H
