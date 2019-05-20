#ifndef HDTIN_H
#define HDTIN_H

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Constrained_Delaunay_triangulation_2.h>


#include "../HdMesh/hdmesh.h"



template < class Gt, class Vb = CGAL::Triangulation_vertex_base_2<Gt> >
class TinCGALVertex:public Vb, public Vertex
{
public:
    template< typename TDS2>
    struct Rebind_TDS {
        typedef typename Vb::template Rebind_TDS<TDS2>::Other Vb2;
        typedef  TinCGALVertex<Gt,Vb2> Other;
    };

    virtual double x() const override {return Vb::point().x();}
    virtual double y() const override {return Vb::point().y();}
    double z() const override {return z_;}
    void setZValue(double ZValue) override {z_=ZValue;}

private:
    double z_;
};



typedef CGAL::Exact_predicates_inexact_constructions_kernel K;

typedef TinCGALVertex<K> TinVertex;
typedef CGAL::Triangulation_data_structure_2<TinVertex,CGAL::Constrained_triangulation_face_base_2<K>> Tds;
typedef CGAL::Exact_predicates_tag Itag;
typedef CGAL::Constrained_Delaunay_triangulation_2 <K, Tds, Itag> CgalTriangulation;
typedef CgalTriangulation::Point CgalPoint;
typedef CgalTriangulation::Vertex_handle VertexHandle;
typedef CgalTriangulation::Face_handle FaceHandle;
typedef CgalTriangulation::Vertex_iterator VertexIterator;
typedef CgalTriangulation::Face_iterator FaceIterator;

class VertexCGAL:public Vertex
{
public:
    VertexCGAL(VertexHandle handle):vertexHandle(handle)
    {}
    virtual ~VertexCGAL();

    virtual double x() const {return vertexHandle->point().x();}
    virtual double y() const {return vertexHandle->point().y();}

private:
    VertexHandle vertexHandle;


    // Vertex interface
public:

};


class TinReader:public MeshIO
{

public:
    TinReader(const CgalTriangulation *triangulation):mTriangulation(triangulation)
    {
        vIt=mTriangulation->finite_vertices_begin();
        fIt=mTriangulation->finite_faces_begin();
    }

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

private:
    FaceIterator fIt;
    VertexIterator vIt;
    std::map<VertexHandle, int> verticesIndex;
    const CgalTriangulation *mTriangulation;
    int currentVertexIndex=0;




};


class HdTin: public HdMesh
{
public:
    HdTin(){}

    int verticesCount() const override
    {
        return int(triangulation.number_of_vertices());
    }
    int facesCount() const override
    {
         return int(triangulation.number_of_faces());
    }

    VertexPointer vertex(double x, double y, double tolerance) const override
    {
        CgalPoint point(x,y);

        if (triangulation.dimension()<2)
        {
            VertexIterator it=triangulation.vertices_begin();
            VertexHandle nearerVert=nullptr;
            K::FT distMin=1e99;
            while(it!=triangulation.vertices_end())
            {
                K::FT dist=CGAL::squared_distance(point,it->point());
                if(dist<distMin)
                {
                    nearerVert=it->handle();
                    distMin=dist;
                }
                it++;
            }
            if (distMin<tolerance)
                return &(*nearerVert);
            else
                return nullptr;
        }

        FaceHandle face=triangulation.locate(point);
        if (face!=nullptr)
        {
            VertexHandle nearerVert;
            K::FT distMin=1e99;
            for (int i=0;i<3;++i)
            {
                VertexHandle vert=face->vertex(i);
                if (vert!=nullptr)
                {
                    K::FT dist=CGAL::squared_distance(point,vert->point());
                    if(dist<distMin)
                    {
                        nearerVert=vert;
                        distMin=dist;
                    }
                }
            }

            if (distMin<tolerance)
                return &(*nearerVert);
        }

        return nullptr;

    }

    void clear() override {}
    void clearFaces() override {}

    VertexPointer addVertex(double x, double y) override
    {
        auto vert= triangulation.insert(CgalPoint(x,y));
        std::cout<<"Dimension "<<triangulation.dimension()<<std::endl;
        return &(*vert);
    }

    std::unique_ptr<MeshIO> getReader() const override
    {
        return std::make_unique<TinReader>(&triangulation);
    }


private:
    CgalTriangulation triangulation;


};

#endif // HDTIN_H
