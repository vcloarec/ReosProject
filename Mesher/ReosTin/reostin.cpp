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
            return nearerVert->tinVertex();//&(*nearerVert);
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
            return nearerVert->tinVertex();//&(*nearerVert);

    }
    return nullptr;
}

VertexPointer ReosTin::vertex(double x, double y) const
{
    return ReosMesh::vertex(x,y);
}

FacePointer ReosTin::face(double x, double y) const
{
    auto faceHandle=triangulation.locate(CgalPoint(x,y));

    std::cout<<"X : "<<x<<"   Y : "<<y<<std::endl;

    if (triangulation.is_infinite(faceHandle))
        return nullptr;

    return &(*faceHandle);
}

void ReosTin::initialize(int verticesCount)
{

    if (verticesCount<=2)
    {
        clear();
        //the vertices will be simply inserted because no face to insert
    }
    else {
        //need to reconstruct the mesh from a clear triangulation data structure and not insert vertex but create them
        //because of the arrangment of the faces not Delaunay conforming
        triangulation.tds().clear();
        triangulation.set_infinite_vertex(triangulation.tds().create_vertex());
        triangulation.tds().set_dimension(2);
        //doesn't create the infinite faces because they will be constructed with boundaries treatment
    }
}

void ReosTin::clear()
{
    triangulation.clear();
}

VertexPointer ReosTin::createVertex(double x, double y)
{
    auto v=triangulation.tds().create_vertex();
    v->set_point(CgalPoint(x,y));

    return new TINVertex(v);
}

VertexPointer ReosTin::insertVertex(double x, double y)
{
    CgalPoint pt(x,y);
    auto v=triangulation.insert(pt);

    return new TINVertex(v);
}

VertexPointer ReosTin::addVertex(double x, double y)
{
    auto vertFound=vertex(x,y);
    if (vertFound)
        return vertFound;

    auto vert= triangulation.insert(CgalPoint(x,y));
    return new TINVertex(vert);//&(*vert);
}

void ReosTin::removeVertex(VertexPointer vertex)
{
    if (isOnHardLine(vertex))
        return;

    //TinTriangulation::Vertex* vt=static_cast<TinTriangulation::Vertex*>(vertex);
    auto vert=static_cast<TINVertex*>(vertex);
    //triangulation.remove(vt->handle());
    triangulation.remove(vert->handle());

}

std::list<VertexPointer> ReosTin::removeVertexOnHardLine(VertexPointer vertex)
{
    std::cout<<"***************************** sub Constraint remaining "<<triangulation.number_of_subconstraints()<<std::endl;

    //TinTriangulation::Vertex* vt=static_cast<TinTriangulation::Vertex*>(vertex);
    auto vt=static_cast<TINVertex*>(vertex);
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

    auto vt1=static_cast<TINVertex*>(v1);
    auto vt2=static_cast<TINVertex*>(v2);
    auto itContext=triangulation.contexts_begin(vt1->handle(),vt2->handle());

    auto vcstBegin=triangulation.vertices_in_constraint_begin(itContext->id());
    auto vcstEnd=triangulation.vertices_in_constraint_end(itContext->id());

    for (auto v=vcstBegin;v!=vcstEnd;v++)
        verticesList.push_back((*v)->handle()->tinVertex());

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

    auto vertexHandle_1=static_cast<TINVertex*>(v1)->handle();
    auto vertexHandle_2=static_cast<TINVertex*>(v2)->handle();

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

    auto vertexHandle_1=static_cast<TINVertex*>(v1)->handle();

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

    auto vertexHandle=static_cast<TINVertex*>(vertex)->handle();

    auto ce=triangulation.incident_edges(vertexHandle);
    auto initCe=ce;

    do
    {
        VertexHandle n=oppositeVertex(vertexHandle,(*ce));
        if (!triangulation.is_infinite(n))
            neighbours.push_back((*n).tinVertex());
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

    int i1=face_1->index(face_2);
    int i2=face_2->index(face_1);

    if(triangulation.is_constrained(CgalTriangulation::Edge(face_1,i1)))
        return false;

    //test if the  face are convex;
    CGAL::Segment_2<K> oppositeSegment(face_1->vertex(i1)->point(),face_2->vertex(i2)->point());
    CGAL::Segment_2<K> commonSegment(face_1->vertex((i1+1)%3)->point(),face_1->vertex((i1+2)%3)->point());

    auto intersect=CGAL::intersection(oppositeSegment,commonSegment);

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

int ReosTin::readUGRIDFormat(std::string fileName)
{
    int ncId;
    nc_open(fileName.c_str(),NC_NOWRITE,&ncId);

    size_t nodeCount;
    size_t faceCount;
    size_t maxNodesPerFace;
    size_t hardlinesVerticesCount;
    int error=0;

    //read dimensions
    int ncIdDimensionNode;
    error=nc_inq_dimid(ncId,"TIN_Node",&ncIdDimensionNode);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    error=nc_inq_dimlen(ncId,ncIdDimensionNode,&nodeCount);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncIdDimensionFace;
    error=nc_inq_dimid(ncId,"TIN_Face",&ncIdDimensionFace);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }
    error=nc_inq_dimlen(ncId,ncIdDimensionFace,&faceCount);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncIdDimensionMaxNodesPerFace;
    error=nc_inq_dimid(ncId,"Max_Node_Per_Faces",&ncIdDimensionMaxNodesPerFace);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }
    error=nc_inq_dimlen(ncId,ncIdDimensionMaxNodesPerFace,&maxNodesPerFace);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncIdDimensionHardlines;
    error=nc_inq_dimid(ncId,"HardLines_dimension",&ncIdDimensionHardlines);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }
    error=nc_inq_dimlen(ncId,ncIdDimensionHardlines,&hardlinesVerticesCount);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    initialize(int(nodeCount));

    //read coordinate system
    std::string coordinateSystemVariableName("Coordinate system");
    int ncCRSVariableId;
    error=nc_inq_varid(ncId,coordinateSystemVariableName.c_str(),&ncCRSVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    size_t len;
    error=nc_inq_attlen( ncId, ncCRSVariableId,"wkt", &len ) ;
    if (error)
    {
        nc_close(ncId);
        return error;
    }

    char *strCRS=static_cast<char *>( malloc( len + 1 ) );

    error=nc_get_att(ncId,ncCRSVariableId,"wkt",strCRS);
    strCRS[len]='\0';

    setCrs(std::string(strCRS));

    //read nodes
    std::string meshNodeXVariableName("TIN_node_x");
    std::string meshNodeYVariableName("TIN_node_y");
    std::string meshNodeZVariableName("TIN_altitude");

    int ncNodeXVariableId;
    error=nc_inq_varid(ncId,meshNodeXVariableName.c_str(),&ncNodeXVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncNodeYVariableId;
    error=nc_inq_varid(ncId,meshNodeYVariableName.c_str(),&ncNodeYVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncNodeZVariableId;
    error=nc_inq_varid(ncId,meshNodeZVariableName.c_str(),&ncNodeZVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    size_t bufferSize=100;
    std::vector<double> nodeX(bufferSize);
    std::vector<double> nodeY(bufferSize);
    std::vector<double> nodeZ(bufferSize);

    std::vector<VertexPointer> nodes(nodeCount);
    std::vector<FaceHandle> faces(faceCount);

    size_t i=0;

    while (i<nodeCount)
    {
        size_t j=bufferSize;
        if (i+j>nodeCount)
            j=nodeCount-i;

        error=nc_get_vara(ncId,ncNodeXVariableId,&i,&j,nodeX.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }
        error=nc_get_vara(ncId,ncNodeYVariableId,&i,&j,nodeY.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }
        error=nc_get_vara(ncId,ncNodeZVariableId,&i,&j,nodeZ.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        for (size_t k=0;k<j;++k)
        {
            //CgalTriangulation::Vertex_handle v;
            VertexPointer vt;
            if(nodeCount<3)
            {
                //insert point to handle with the infinite faces when there are less than 3 points;
                //v=triangulation.insert(pt);
                vt=insertVertex(nodeX[k],nodeY[k]);
            }
            else
            {
                //create point to not create face for eache vertex, that will be done after
//                v=triangulation.tds().create_vertex();
//                v->set_point(pt);
                vt=createVertex(nodeX[k],nodeY[k]);
            }


            vt->setZValue(nodeZ[k]);
            nodes[i+k]=vt;
        }

        i+=j;
    }

    //read faces
    int ncFacesVariableId;
    error=nc_inq_varid(ncId,"TIN_face_node_connectivity",&ncFacesVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    std::vector<int> faceBuffer(bufferSize*maxNodesPerFace);

    size_t valueStart[2];
    size_t valueCount[2];
    valueStart[1]=0;
    valueCount[1]=maxNodesPerFace;

    i=0;
    while (i<faceCount)
    {
        size_t j=bufferSize;
        if (i+j>faceCount)
            j=faceCount-i;

        valueStart[0]=i;
        valueCount[0]=j;
        error=nc_get_vara(ncId,ncFacesVariableId,valueStart,valueCount,faceBuffer.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        for (size_t k=0;k<j;k++)
        {
            auto f=triangulation.tds().create_face();
            for (size_t l=0;l<3;++l)
            {
                auto v=static_cast<TINVertex*>(nodes[static_cast<size_t>(faceBuffer[k*3+l])]);
                if (v!=nullptr)
                {
                    f->set_vertex(int(l),v->handle());
                    v->handle()->set_face(f);
                }
            }
            faces[i+k]=f;
        }
        i+=j;
    }

    //read neighbors
    int ncNeighborsVariableId;
    error=nc_inq_varid(ncId,"face_neighbors",&ncNeighborsVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    std::vector<int> neighborsBuffer(bufferSize*maxNodesPerFace);
    valueStart[1]=0;
    valueCount[1]=maxNodesPerFace;
    std::stack<std::pair<size_t,FaceHandle>> boundaries;

    i=0;
    while (i<faceCount)
    {
        size_t j=bufferSize;
        if (i+j>faceCount)
            j=faceCount-i;

        valueStart[0]=i;
        valueCount[0]=j;
        error=nc_get_vara_int(ncId,ncNeighborsVariableId,valueStart,valueCount,neighborsBuffer.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        for (size_t k=0;k<j;k++)
        {
            auto f=faces[i+k];
            for (size_t l=0;l<static_cast<size_t>(maxNodesPerFace);++l)
            {
                int index=neighborsBuffer[k*maxNodesPerFace+l];
                if (index>=0)
                {
                    auto fn=faces[static_cast<size_t>(index)];
                    f->set_neighbor(int(l),fn);
                }
                else
                {
                    boundaries.push(std::pair<int,FaceHandle>(l,f));
                }
            }

        }
        i+=j;
    }

    while (!boundaries.empty())
    {
        auto p=boundaries.top();
        auto f=p.second;
        auto l=p.first;
        auto v1=f->vertex(f->cw(int(l)));
        auto v2=f->vertex(f->ccw(int(l)));
        auto inf_face=triangulation.create_face(triangulation.infinite_vertex(),v1,v2);
        f->set_neighbor(int(l),inf_face);
        inf_face->set_neighbor(inf_face->index(triangulation.infinite_vertex()),f);
        triangulation.infinite_vertex()->set_face(inf_face);

        // need to find the infinite vertices which are neighbor
        // First turn arround the vertex v1
        auto nextVertex=&TinTriangulation::Face::cw;
        if (f->index(v2)==f->ccw(f->index(v1)))
        {
            nextVertex=&TinTriangulation::Face::ccw;
        }

        auto vs2=f->index(v2);
        auto nb=f;
        bool infiniteVertexFound(false), nothingFound(false);
        do
        {
            auto vtemp=nb->vertex(nextVertex(vs2));
            nb=nb->neighbor(vs2);
            if (nb!=nullptr)
            {
                vs2=nb->index(vtemp);
                infiniteVertexFound=nb->vertex(nextVertex(vs2))==triangulation.infinite_vertex();
            }
            else
                nothingFound=true;
        } while (!infiniteVertexFound && !nothingFound);
        if (infiniteVertexFound)
        {
            inf_face->set_neighbor(inf_face->index(v2),nb);
            nb->set_neighbor(vs2,inf_face);
        }

        //First turn arround the vertex v2
        if (f->index(v1)==f->ccw(f->index(v2)))
        {
            nextVertex=&TinTriangulation::Face::ccw;
        }
        auto vs1=f->index(v1);
        nb=f;
        infiniteVertexFound=false;
        nothingFound=false;
        do
        {
            auto vtemp=nb->vertex(nextVertex(vs1));
            nb=nb->neighbor(vs1);
            if (nb!=nullptr)
            {
                vs1=nb->index(vtemp);
                infiniteVertexFound=nb->vertex(nextVertex(vs1))==triangulation.infinite_vertex();
            }

            else
                nothingFound=true;
        } while (!infiniteVertexFound && !nothingFound);
        if (infiniteVertexFound)
        {
            inf_face->set_neighbor(inf_face->index(v1),nb);
            nb->set_neighbor(vs1,inf_face);
        }

        boundaries.pop();
    }


    //read hardline
    int ncHardlinesVariableId;
    error=nc_inq_varid(ncId,"hardlines",&ncHardlinesVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }


    i=0;
    bool allHardlinesReaden= 0==hardlinesVerticesCount;
    while(!allHardlinesReaden)
    {
        size_t valueCount=1;
        int verticesCount;
        error=nc_get_vara_int(ncId,ncHardlinesVariableId,&i,&valueCount,&verticesCount);
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }
        i+=valueCount;

        valueCount=static_cast<size_t>(verticesCount);
        std::vector<int> indexes(valueCount);
        error=nc_get_vara_int(ncId,ncHardlinesVariableId,&i,&valueCount,indexes.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        // this implementation is not correct because it changes the faces wich had been flipped
//        std::vector<CgalPoint> points(valueCount);
//        for (size_t k=0;k<valueCount;++k)
//        {
//            points[k]=nodes[static_cast<size_t>(indexes[k])]->point();
//        }

//        triangulation.insert_constraint(points.begin(),points.end());


        // Need this implementation with a lot af static_cast but TINVertex* has to be replaced by VertexPointer in a futur factorization
        if (valueCount>1)
        {
            auto v1=static_cast<TINVertex*>(nodes[static_cast<size_t>(indexes[0])]);
            auto v2=static_cast<TINVertex*>(nodes[static_cast<size_t>(indexes[1])]);
            auto ca=triangulation.hierarchy_ref().insert_constraint(v1->handle(),v2->handle());
            triangulation.insertSubconstraint(v1->handle(),v2->handle());

            if (valueCount>2)
            {
                for(size_t k=1;k<valueCount-1;++k)
                {
                    v1=static_cast<TINVertex*>(nodes[static_cast<size_t>(indexes[k])]);
                    v2=static_cast<TINVertex*>(nodes[static_cast<size_t>(indexes[k+1])]);
                    triangulation.hierarchy_ref().append_constraint(ca,v1->handle(),v2->handle());
                    triangulation.insertSubconstraint(v1->handle(),v2->handle());
                }

            }

        }

        ////////////////////


        i+=valueCount;
        allHardlinesReaden= i>=hardlinesVerticesCount;

    }

    nc_close(ncId);
    return error;
}

int ReosTin::writeUGRIDFormat(std::string fileName)
{
    int ncId;
    int error;

    if (fileName.empty())
        return 1;

    error=nc_create(fileName.c_str(),NC_CLOBBER|NC_NETCDF4,&ncId);
    if (error!=NC_NOERR)
        return error;

    std::unique_ptr<MeshIO> reader=getReader();

    std::string meshNodeXVariableName("TIN_node_x");
    std::string meshNodeYVariableName("TIN_node_y");
    std::string meshNodeZVariableName("TIN_altitude");
    std::string meshFaceNodeConnectivity("TIN_face_node_connectivity");
    std::string coordinateSystemVariableName("Coordinate system");

    //***********************************
    //define dimensions

    //node dimension
    int ncIdDimensionMeshNode;
    error=nc_def_dim(ncId,"TIN_Node",static_cast<size_t>(verticesCount()),&ncIdDimensionMeshNode);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //face dimension
    int ncIdDimensionMeshFace;
    error=nc_def_dim(ncId,"TIN_Face",static_cast<size_t>(facesCount()),&ncIdDimensionMeshFace);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }


    //max node per faces dimension
    int ncIdDimensionMaxNodesPerFaces;
    size_t maxNode=static_cast<size_t>(maxNodesPerFaces());
    error=nc_def_dim(ncId,"Max_Node_Per_Faces",maxNode,&ncIdDimensionMaxNodesPerFaces);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //hardline dimension
    size_t hardLineVerticesCount=static_cast<size_t>(reader->hardlinesVerticesCount());
    size_t hardLineCount=static_cast<size_t>(reader->hardlinesCount());
    int ncIdDimensionHardLines;
    error=nc_def_dim(ncId,"HardLines_dimension",hardLineVerticesCount+hardLineCount,&ncIdDimensionHardLines);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //define variable for coordinate system
    int ncIdCRSVariable;
    error=nc_def_var(ncId,coordinateSystemVariableName.c_str(),NC_BYTE,0,nullptr,&ncIdCRSVariable);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    const char* crsChar=crs().c_str();
    error=nc_put_att_text(ncId,ncIdCRSVariable,"wkt",std::strlen(crsChar),crsChar);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //define the dummy variable
    int ncIdMesh;
    error=nc_def_var(ncId,"TIN",NC_BYTE,0,nullptr,&ncIdMesh);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    ////////////////////////////////
    // fill the attributes
    //cf role
    const char* cfRole="mesh_topology";
    error=nc_put_att_text(ncId,ncIdMesh,"cf_role",std::strlen(cfRole),cfRole);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //topology dimension
    int topology_dimension=2;
    error=nc_put_att_int(ncId,ncIdMesh,"topology_dimension",NC_INT,1,&topology_dimension);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //node Coordinate Location
    std::string ncl=meshNodeXVariableName+" "+meshNodeYVariableName;
    const char* nodeCoordinatesLocation=ncl.c_str();
    error=nc_put_att_text(ncId,ncIdMesh,"node_coordinates",std::strlen(nodeCoordinatesLocation),nodeCoordinatesLocation);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //face node connectivity
    const char* faceNodeConnectivityLocation=meshFaceNodeConnectivity.c_str();
    error=nc_put_att_text(ncId,ncIdMesh,"face_node_connectivity",std::strlen(faceNodeConnectivityLocation),faceNodeConnectivityLocation);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //////////////////////
    // defines and fills node coordinate variable
    int ncNodeXVariableId;
    error=nc_def_var(ncId,meshNodeXVariableName.c_str(),NC_DOUBLE,1,&ncIdDimensionMeshNode,&ncNodeXVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    error=nc_put_att_text(ncId,ncNodeXVariableId,"grid_mapping",coordinateSystemVariableName.size(),coordinateSystemVariableName.c_str());
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncNodeYVariableId;
    error=nc_def_var(ncId,meshNodeYVariableName.c_str(),NC_DOUBLE,1,&ncIdDimensionMeshNode,&ncNodeYVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    error=nc_put_att_text(ncId,ncNodeYVariableId,"grid_mapping",coordinateSystemVariableName.size(),coordinateSystemVariableName.c_str());
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int ncNodeZVariableId;
    error=nc_def_var(ncId,meshNodeZVariableName.c_str(),NC_DOUBLE,1,&ncIdDimensionMeshNode,&ncNodeZVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }
    //fill the Z value variable attribute to be UGRID compliant
    const char* alt="altitude";
    error=nc_put_att_text(ncId,ncNodeZVariableId,"standard_name",std::strlen(alt),alt);
    const char* meshName="TIN";
    error=nc_put_att_text(ncId,ncNodeZVariableId,"mesh",std::strlen(meshName),meshName);
    const char* loc="node";
    error=nc_put_att_text(ncId,ncNodeZVariableId,"location",std::strlen(loc),loc);

    size_t bufferSize=100;
    std::vector<double> nodeX(bufferSize);
    std::vector<double> nodeY(bufferSize);
    std::vector<double> nodeZ(bufferSize);

    size_t i=0;
    while (!reader->allVerticesReaden())
    {

        size_t j=0;
        while (!reader->allVerticesReaden() && j<bufferSize)
        {
            double vert[3];
            reader->readVertex(vert);
            nodeX[j]=vert[0];
            nodeY[j]=vert[1];
            nodeZ[j]=vert[2];
            ++j;
        }
        error=nc_put_vara(ncId,ncNodeXVariableId,&i,&j,nodeX.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        error=nc_put_vara(ncId,ncNodeYVariableId,&i,&j,nodeY.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        error=nc_put_vara(ncId,ncNodeZVariableId,&i,&j,nodeZ.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }

        i+=j;
    }


    //////////////////////
    // defines and fills faces variable
    int ncFacesVariableId;

    int facesDimension[2];
    facesDimension[0]=ncIdDimensionMeshFace;
    facesDimension[1]=ncIdDimensionMaxNodesPerFaces;

    error=nc_def_var(ncId,meshFaceNodeConnectivity.c_str(),NC_INT,2,facesDimension,&ncFacesVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    //start index
    int startIndex=0;
    error=nc_put_att_int(ncId,ncFacesVariableId,"start_index",NC_INT,1,&startIndex);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    int fillValue=-1;
    error=nc_put_att_int(ncId,ncFacesVariableId,"_FillValue",NC_INT,1,&fillValue);
    std::vector<int> facesBuffer(bufferSize*maxNode);
    size_t valueCount[2];
    valueCount[1]=maxNode;
    size_t valueStart[2];
    valueStart[1]=0;
    i=0;
    while (!reader->allFacesReaden())
    {
        size_t j=0;
        while (!reader->allFacesReaden() && j<bufferSize)
        {
            int nodeCount;
            reader->readNodePerFace(nodeCount);
            std::vector<int> indexes(static_cast<size_t>(nodeCount));
            reader->readFace(indexes.data());
            size_t k=0;
            for (;k<indexes.size();k++)
            {
                facesBuffer[j*maxNode+k]=indexes[k];
            }

            for (;k<maxNode;++k)
            {
                facesBuffer[j*maxNode+k]=fillValue;
            }
            j++;
        }
        valueCount[0]=j;
        valueStart[0]=i;
        error=nc_put_vara_int(ncId,ncFacesVariableId,valueStart,valueCount,facesBuffer.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }
        i+=j;
    }


    //////////////////////
    // defines and fills neighbors variable
    int ncNeighborVariableId;

    error=nc_def_var(ncId,"face_neighbors",NC_INT,2,facesDimension,&ncNeighborVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    error=nc_put_att_int(ncId,ncNeighborVariableId,"_FillValue",NC_INT,1,&fillValue);
    int neighborCount;
    reader->readNodePerFace(neighborCount);
    std::vector<int> indexes(static_cast<size_t>(neighborCount));
    std::vector<int> neighborsBuffer(bufferSize*static_cast<size_t>(neighborCount));
    i=0;
    while (!reader->allNeighborReaden())
    {
        size_t j=0;
        while (!reader->allNeighborReaden() && j<bufferSize)
        {
            reader->readNeighbor(indexes.data());
            size_t k=0;
            for (;k<indexes.size();k++)
            {
                if (indexes[k]!=-1)
                    neighborsBuffer[j*maxNode+k]=indexes[k];
                else {
                    neighborsBuffer[j*maxNode+k]=fillValue;
                }
            }

            for (;k<maxNode;++k)
            {
                neighborsBuffer[j*maxNode+k]=fillValue;
            }

            j++;
        }

        valueCount[0]=j;
        valueStart[0]=i;
        error=nc_put_vara_int(ncId,ncNeighborVariableId,valueStart,valueCount,neighborsBuffer.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }
        i+=j;
    }

    //////////////////////
    // defines and fills hardlines variable
    int ncHardlinesVariableId;

    error=nc_def_var(ncId,"hardlines",NC_INT,1,&ncIdDimensionHardLines,&ncHardlinesVariableId);
    if (error!=NC_NOERR)
    {
        nc_close(ncId);
        return error;
    }

    i=0;
    while (!reader->allHardLineReaden())
    {
        size_t verticesCount=static_cast<size_t>(reader->currentHardlineVerticesCount());
        size_t valueCount=verticesCount+1;
        std::vector<int> indexes(valueCount);
        indexes[0]=int(verticesCount);
        reader->readHardlineVertices(&(indexes[1]));
        error=nc_put_vara_int(ncId,ncHardlinesVariableId,&i,&valueCount,indexes.data());
        if (error!=NC_NOERR)
        {
            nc_close(ncId);
            return error;
        }
        i+=valueCount;
    }

    return nc_close(ncId);

}

FaceHandle ReosTin::faceHandle(TinTriangulation::Face *f) const
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


std::list<VertexPointer> ReosTin::addHardLine(VertexPointer v1, VertexPointer v2)
{
    //auto handle_1=static_cast<TinTriangulation::Vertex*>(v1)->handle();
    //auto handle_2=static_cast<TinTriangulation::Vertex*>(v2)->handle();
    auto handle_1=static_cast<TINVertex*>(v1)->handle();
    auto handle_2=static_cast<TINVertex*>(v2)->handle();

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
        //constraintVertices.push_back(&(*handle));
        if (handle->tinVertex())
            constraintVertices.push_back(handle->tinVertex());
        else
            constraintVertices.push_back(new TINVertex(handle));
    }
    return constraintVertices;
}

std::list<VertexPointer> ReosTin::hardNeighbours(VertexPointer vertex) const
{
    auto vertexHandle=static_cast<TINVertex*>(vertex)->handle();

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
            hardNeighbours.push_back(opposVertex->tinVertex());
        }

        incidentEdges++;
    }while (initEdge!=incidentEdges);

    return hardNeighbours;
}

TinReader::TinReader(const TinTriangulation *triangulation):mTriangulation(triangulation)
{
    vIt=mTriangulation->finite_vertices_begin();
    fIt=mTriangulation->finite_faces_begin();
    fItN=mTriangulation->finite_faces_begin();
    sIt=mTriangulation->subconstraints_begin();
    boundaryIterator=boundariesList.begin();
    cIt=mTriangulation->constraints_begin();
}

int TinReader::vertexCoordCount() const
{
    return 3;
}

int TinReader::verticesCount() const
{
    return int(mTriangulation->number_of_vertices());
}

int TinReader::currentFaceVerticesCount() const
{
    return 3;
}

void TinReader::readVertex(double *vert)
{
    vert[0]=vIt->tinVertex()->x();
    vert[1]=vIt->tinVertex()->y();
    vert[2]=vIt->tinVertex()->z();
    verticesIndex[vIt->handle()]=currentVertexIndex;
    vIt++;
    currentVertexIndex++;
}

void TinReader::readOnlyVertex(double *vert)
{
    vert[0]=vIt->tinVertex()->x();
    vert[1]=vIt->tinVertex()->y();
    vert[2]=vIt->tinVertex()->z();
    vIt++;
}

VertexPointer TinReader::readVertexPointer()
{
    VertexPointer vert=vIt->tinVertex();
    verticesIndex[vIt->handle()]=currentVertexIndex;
    vIt++;
    currentVertexIndex++;
    return vert;
}

void TinReader::readFace(int *face)
{
    //the index of vertices are ordered counter clockwise
    int index=fIt->ccw(2);

    for (int i=0;i<3;++i)
    {
        face[i]=verticesIndex[fIt->vertex(index)];
        index=fIt->ccw(index);
    }
    facesIndex[fIt]=currentFaceIndex;
    if (fIt!=mTriangulation->finite_faces_end())
        fIt++;
    currentFaceIndex++;
}

void TinReader::readNodePerFace(int &count)
{
    count=3;
}

bool TinReader::allVerticesReaden() const
{
    return vIt==mTriangulation->finite_vertices_end();
}

bool TinReader::allFacesReaden() const
{
    return fIt==mTriangulation->finite_faces_end();
}

void TinReader::readSegment(int *sc)
{
    sc[0]=verticesIndex[sIt->first.first->handle()];
    sc[1]=verticesIndex[sIt->first.second->handle()];
    sIt++;
}

void TinReader::readNeighbor(int *n)
{
    for (int i=0;i<3;++i)
    {
        auto neighbor=fItN->neighbor(i);
        if (mTriangulation->is_infinite(neighbor))
        {
            n[i]=-1;
            auto v1=fItN->vertex(fItN->ccw(i));
            auto v2=fItN->vertex(fItN->cw(i));
            std::vector<int> boundary;
            boundary.push_back(verticesIndex[v1]);
            boundary.push_back(verticesIndex[v2]);
            boundariesList.push_back(boundary);
        }
        else
            n[i]=facesIndex[neighbor];
    }
    fItN++;
    boundaryIterator=boundariesList.begin();
}

int TinReader::hardlinesCount() const
{
    auto hl=mTriangulation->constraints_begin();
    auto hle=mTriangulation->constraints_end();
    int count(0);
    while (hl!=hle)
    {
        count++;
        hl++;
    }

    return count;
}

int TinReader::hardlinesVerticesCount() const
{
    auto hl=mTriangulation->constraints_begin();
    auto hle=mTriangulation->constraints_end();
    int count(0);
    while (hl!=hle)
    {
        auto hlv=mTriangulation->vertices_in_constraint_begin(*hl);
        auto hlve=mTriangulation->vertices_in_constraint_end(*hl);
        while (hlv!=hlve)
        {
            count++;
            hlv++;
        }
        hl++;
    }

    return count;
}

int TinReader::currentHardlineVerticesCount() const
{
    int count(0);
    if (allHardLineReaden())
        return 0;
    auto hlv=mTriangulation->vertices_in_constraint_begin(*cIt);
    auto hlve=mTriangulation->vertices_in_constraint_end(*cIt);
    while (hlv!=hlve)
    {
        count++;
        hlv++;
    }

    return count;
}

void TinReader::readHardlineVertices(int *hlvIndex)
{
    auto hlv=mTriangulation->vertices_in_constraint_begin(*cIt);
    size_t i(0);
    while (hlv!=mTriangulation->vertices_in_constraint_end(*cIt))
    {
        hlvIndex[i]=verticesIndex[*hlv];
        hlv++;
        i++;
    }
    cIt++;
}

bool TinReader::allHardLineReaden() const
{
    return cIt==mTriangulation->constraints_end();
}

int TinReader::boundariesCount() const
{
    return static_cast<int>(boundariesList.size());
}

void TinReader::readBoundaryEdge(int *be)
{
    if (boundaryIterator!=boundariesList.end())
    {
        be[0]=(*boundaryIterator)[0];
        be[1]=(*boundaryIterator)[1];
    }
    boundaryIterator++;

}

bool TinReader::allBoundaryEdgesReaden() const
{
    return boundaryIterator==boundariesList.end();
}

bool TinReader::allSegmentsReaden() const
{
    return sIt==mTriangulation->subconstraints_end();
}

bool TinReader::allNeighborReaden() const
{
    return fItN==mTriangulation->finite_faces_end();
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

TINVertex::TINVertex(VertexHandle cgalVert):mCgalVertex(cgalVert)
{
    cgalVert->mTINVertex=this;
}
