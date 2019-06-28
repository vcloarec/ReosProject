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

#include "reostin.h"

int ReosTin::verticesCount() const
{
    return int(triangulation.number_of_vertices());
}

int ReosTin::facesCount() const
{
    return int(triangulation.number_of_faces());
}

VertexPointer ReosTin::vertex(double x, double y, double tolerance) const
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

        if (std::sqrt(distMin)<tolerance)
            return &(*nearerVert);

    }
    return nullptr;
}

VertexPointer ReosTin::addVertex(double x, double y)
{
    auto vert= triangulation.insert(CgalPoint(x,y));
    return &(*vert);
}

void ReosTin::removeVertex(VertexPointer vertex)
{
    if (isOnHardLine(vertex))
        return;

    TinTriangulation::Vertex* vt=static_cast<TinTriangulation::Vertex*>(vertex);

    triangulation.remove(vt->handle());
}

std::list<VertexPointer> ReosTin::removeVertexOnHardLine(VertexPointer vertex)
{
    std::cout<<"***************************** sub Constraint remaining "<<triangulation.number_of_subconstraints()<<std::endl;

    TinTriangulation::Vertex* vt=static_cast<TinTriangulation::Vertex*>(vertex);

    bool notConstrainedAnymore=false;

    std::list<VertexPointer> list;

    //First remove the vertex from all constraint;
    //For remove all the constraint, remove the first and reinitialize the search because the structure seems to change after the first removal
    //need to store all the vertices in relation to the constraints

    while (!notConstrainedAnymore)
    {
        auto ie=triangulation.incident_edges(vt->handle());
        auto fie=ie;
        bool done=false;
        do
        {
            if (triangulation.is_constrained(*ie))
            {
                auto ov=oppositeVertex(vt->handle(),*ie);
                auto cst=triangulation.contexts_begin(vt->handle(),ov);

                while(!done)
                {
                    auto v=triangulation.vertices_in_constraint_begin(cst->id());
                    auto ve=triangulation.vertices_in_constraint_end(cst->id());

                    bool found=false;
                    while( v!=ve && !found)
                    {
                        found= vt->handle() != (*v)->handle();
                        if (!found)
                            v++;
                    }

                    if (v!=ve)
                    {
                        triangulation.remove_vertex_from_constraint(cst->id(),v);
                        done=true;
                    }
                    else
                    {
                        cst++;
                        done = cst==triangulation.contexts_end(vt->handle(),ov);
                    }
                }
            }

            ie++;
        }while (fie!=ie && !done);

        notConstrainedAnymore= fie==ie && !done;
    }

    std::cout<<"******************************** Subconstraint remaining "<<triangulation.number_of_subconstraints()<<std::endl;

    removeVertex(vertex);

    return list;
}

std::list<VertexPointer> ReosTin::removeHardLine(VertexPointer v1, VertexPointer v2)
{
    std::list<VertexPointer> verticesList;

    if (!isOnHardLine(v1,v2))
        return verticesList;

    auto vt1=static_cast<TinTriangulation::Vertex*>(v1);
    auto vt2=static_cast<TinTriangulation::Vertex*>(v2);
    auto itContext=triangulation.contexts_begin(vt1->handle(),vt2->handle());

    auto vcstBegin=triangulation.vertices_in_constraint_begin(itContext->id());
    auto vcstEnd=triangulation.vertices_in_constraint_end(itContext->id());

    for (auto v=vcstBegin;v!=vcstEnd;v++)
        verticesList.push_back(&(*(*v)->handle()));

    triangulation.remove_constraint(itContext->id());

    return verticesList;

}

std::unique_ptr<MeshIO> ReosTin::getReader() const
{
    return std::make_unique<TinReader>(&triangulation);
}

bool ReosTin::isOnHardLine(VertexPointer v1, VertexPointer v2) const
{
    if (triangulation.dimension()<1)
    {
        return false;
    }

    auto vertexHandle_1=static_cast<TinTriangulation::Vertex*>(v1)->handle();
    auto vertexHandle_2=static_cast<TinTriangulation::Vertex*>(v2)->handle();

    auto incidentEdgesOnVertex_1=vertexHandle_1->incident_edges();
    auto initEdge=incidentEdgesOnVertex_1;

    bool isOnHardLineValue=false;
    do
    {
        auto os=oppositeVertex(vertexHandle_1,(*incidentEdgesOnVertex_1));

        if(os==vertexHandle_2)
            isOnHardLineValue=triangulation.is_constrained(*incidentEdgesOnVertex_1);

        incidentEdgesOnVertex_1++;

    }while (initEdge!=incidentEdgesOnVertex_1 && !isOnHardLineValue);


    return isOnHardLineValue;
}

bool ReosTin::isOnHardLine(VertexPointer v1) const
{
    if (triangulation.dimension()<1)
    {
        return false;
    }

    auto vertexHandle_1=static_cast<TinTriangulation::Vertex*>(v1)->handle();

    auto incidentVertex=vertexHandle_1->incident_edges();
    auto initEdge=incidentVertex;

    bool isOnHardLineValue=false;
    do
    {
        isOnHardLineValue|=triangulation.is_constrained(*incidentVertex);
        incidentVertex++;

    }while (initEdge!=incidentVertex && !isOnHardLineValue);


    return isOnHardLineValue;
}

std::list<VertexPointer> ReosTin::neighboursVertices(VertexPointer vertex) const
{
    std::list<VertexPointer> neighbours;

    if (!vertex)
        return neighbours;

    auto vertexHandle=static_cast<TinTriangulation::Vertex*>(vertex)->handle();

    auto ce=triangulation.incident_edges(vertexHandle);
    auto initCe=ce;

    do
    {
        VertexHandle n=oppositeVertex(vertexHandle,(*ce));
        if (!triangulation.is_infinite(n))
            neighbours.push_back(&(*n));
        ce++;
    }while (ce!=initCe);

    return neighbours;

}

bool ReosTin::isFlipable(FacePointer f1, FacePointer f2) const
{
    auto face_1=faceHandle(static_cast<TinTriangulation::Face*>(f1));
    auto face_2=faceHandle(static_cast<TinTriangulation::Face*>(f2));


    if (!face_1->has_neighbor(face_2))
        return false;



    //test if the  face are convex;
    int i1=face_1->index(face_2);
    int i2=face_2->index(face_1);

    CGAL::Segment_2<K> oppositeSegement(face_1->vertex(i1)->point(),face_2->vertex(i2)->point());
    CGAL::Segment_2<K> commonSegment(face_1->vertex((i1+1)%3)->point(),face_1->vertex((i1+2)%3)->point());

    auto intersect=CGAL::intersection(oppositeSegement,commonSegment);

    if (intersect)
        return true;
    else
        return false;

}

std::vector<FacePointer> ReosTin::flipFaces(FacePointer f1, FacePointer f2)
{
    auto face_1=faceHandle(static_cast<TinTriangulation::Face*>(f1));
    auto face_2=faceHandle(static_cast<TinTriangulation::Face*>(f2));

    int index_1=face_1->index(face_2);
    auto v1=face_1->vertex(index_1);

    int index_2=face_2->index(face_1);
    auto v2=face_2->vertex(index_2);

    triangulation.flip(face_1,index_1);

    auto e=v1->incident_edges();
    auto ie=e;

    bool found=false;
    do
    {
        found= oppositeVertex(v1->handle(),(*e))==v2;
        if (!found)
            e++;
    }while (ie!=e && !found);


    auto otherEdge=triangulation.mirror_edge((*e));

    std::vector<FacePointer> newFaces;

    newFaces.push_back(&(*(e->first)));
    newFaces.push_back(&(*otherEdge.first));

    return newFaces;
}


std::list<VertexPointer> ReosTin::addHardLine(VertexPointer v1, VertexPointer v2)
{
    auto handle_1=static_cast<TinTriangulation::Vertex*>(v1)->handle();
    auto handle_2=static_cast<TinTriangulation::Vertex*>(v2)->handle();

    TinTriangulation::Constraint_id cid=triangulation.insert_constraint(handle_1,handle_2);

    std::list<VertexPointer> constraintVertices;

    if (cid==nullptr)
        return constraintVertices;

    typedef TinTriangulation::Vertices_in_constraint VerticesConstraint;
    VerticesConstraint begin=triangulation.vertices_in_constraint_begin(cid);
    VerticesConstraint end=triangulation.vertices_in_constraint_end(cid);


    for (VerticesConstraint it=begin;
         it!=end;
         it++)
    {
        VertexHandle handle=*it;
        constraintVertices.push_back(&(*handle));
    }
    return constraintVertices;
}

std::list<VertexPointer> ReosTin::hardNeighbours(VertexPointer vertex) const
{
    auto vertexHandle=static_cast<TinTriangulation::Vertex*>(vertex)->handle();

    std::list<VertexPointer> hardNeighbours;

    if (triangulation.dimension()<1)
    {
        return hardNeighbours;
    }

    auto incidentEdges=vertexHandle->incident_edges();
    auto initEdge=incidentEdges;

    do
    {
        if(triangulation.is_constrained(*incidentEdges))
        {
            auto opposVertex=oppositeVertex(vertexHandle,*incidentEdges);
            hardNeighbours.push_back(&(*opposVertex));
        }

        incidentEdges++;
    }while (initEdge!=incidentEdges);

    return hardNeighbours;
}

TinReader::TinReader(const TinTriangulation *triangulation):mTriangulation(triangulation)
{
    vIt=mTriangulation->finite_vertices_begin();
    fIt=mTriangulation->finite_faces_begin();
    sIt=mTriangulation->subconstraints_begin();
}

VertexHandle oppositeVertex(VertexHandle vertex, const TinTriangulation::Edge &edge)
{
    auto face=edge.first;
    auto oppositeVertexIndex=(edge.second+1)%3;
    auto oppositeVertexHandle=face->vertex(oppositeVertexIndex);

    if (oppositeVertexHandle==vertex)
    {
        oppositeVertexIndex=(edge.second-1)%3;
        oppositeVertexHandle=face->vertex(oppositeVertexIndex);
    }

    return oppositeVertexHandle;
}
