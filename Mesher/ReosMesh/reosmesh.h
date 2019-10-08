/***************************************************************************
                      reosmesh.h
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2018 by Vincent Cloarec
email                : vcloarec at gmail dot com / projetreos at gmail dot com
 ******************************************************************************
 *                                                                            *
 *   This program is free software; you can redistribute it and/or modify     *
 *   it under the terms of the GNU General Public License as published by     *
 *   the Free Software Foundation; either version 2 of the License, or        *
 *   (at your option) any later version.                                      *
 *                                                                            *
 *****************************************************************************/


#ifndef REOSMESH_H
#define REOSMESH_H

#include <memory>
#include <math.h>
#include <vector>
#include <list>
#include <set>
#include <fstream>
#include <iostream>
#include <cstring>
#include <mutex>

#include <netcdf.h>

#include "vertex.h"
#include "reosvertexzspecifier.h"
#include "../../Reos/reosutils.h"


class MeshIO
{
public:
    virtual ~MeshIO();
    virtual int vertexCoordCount() const =0;
    virtual int verticesCount() const =0;
    virtual void readVertex (double *)=0;
    virtual void readOnlyVertex(double *) =0;
    virtual VertexPointer readVertexPointer() =0;

    virtual int currentFaceVerticesCount() const =0;
    virtual void readFace(int *)=0;
    virtual void readNodePerFace(int &count)=0;
    virtual void readSegment(int *)=0;
    virtual void readNeighbor(int *)=0;
    virtual int boundariesCount() const =0;
    virtual void readBoundaryEdge(int *)=0;


    virtual int hardlinesCount() const=0;
    virtual int hardlinesVerticesCount() const=0;
    virtual int currentHardlineVerticesCount() const =0;
    virtual void readHardlineVertices(int *)=0;

    virtual int zSpecifierCount() const=0;
    virtual void readSpecifier(ReosVertexZSpecifier::Data &zSpecifierData)=0;
    virtual bool allZSpecifierReaden() const =0;

    virtual bool allVerticesReaden() const =0;
    virtual bool allFacesReaden() const =0;
    virtual bool allSegmentsReaden() const =0;
    virtual bool allNeighborReaden() const =0;
    virtual bool allBoundaryEdgesReaden() const =0;
    virtual bool allHardLineReaden() const =0;



};


class ReosMesh
{
public:
    virtual ~ReosMesh();
    virtual int verticesCount() const=0;
    virtual int facesCount() const=0;



    virtual VertexPointer vertex(int) const {return nullptr;}
    virtual VertexPointer vertex(double x, double y, double tolerance) const=0;
    virtual VertexPointer vertex(double x, double y) const;
    virtual FacePointer face(double x,double y) const=0;

    //////////////////////////////////////////////////////
    /// \brief clear
    /// clear faces and vertices ni the mesh
    virtual void clear()=0;

    ///////////////////////////////////////////////////////
    /// \brief clearFaces
    /// clear only the faces
    virtual void clearFaces()=0;


    virtual VertexPointer addVertex(double x, double y)=0;
    virtual std::list<VertexPointer> addHardLine(VertexPointer v1, VertexPointer v2)=0;
    virtual std::list<VertexPointer> hardNeighbours(VertexPointer vertex) const=0;
    virtual std::list<VertexPointer> removeHardLine(VertexPointer v1, VertexPointer v2)=0;

    virtual std::unique_ptr<MeshIO> getReader() const =0;
    virtual std::list<VertexPointer> neighboursVertices(VertexPointer vertex) const =0;
    virtual int maxNodesPerFaces() const=0;

    virtual int writeUGRIDFormat(std::string fileName);

    virtual int readUGRIDFormat(std::string fileName)=0;

    bool isDirty() const;

    //////////////////////////////////////////////////////
    /// \brief crs
    /// \return
    ///
    /// EPSG:code
    std::string crs() const;
    void setCrs(int EPSG_code)
    {
        mCrs="EPSG:";
        mCrs.append(std::to_string(EPSG_code));
    }

    void setCrs(std::string crsStr)
    {
        mCrs=crsStr;
    }

protected:
    double mTolerance=0.01;

    ///////////////////////////////////////////////
    /// \brief initialize
    /// Initialize the Mesh before reading from a file
    /// \param verticesCount
    /// The veritices count
    ///
    virtual void initialize(int verticesCount) =0;

    //////////////////////////////////////////////////////
    /// \brief createVertex
    /// Create a new vertex without creating new faces. Method used when reading a file
    /// \param x
    /// \param y
    /// \return
    ///
    virtual VertexPointer createVertex(double x,double y)=0;

    //////////////////////////////////////////////////////////////
    /// \brief insertVertex
    /// Create a new vertex with faces. Used internally to add a vertex without control if a vertex is present at the position (x,y).
    /// Normally, it wuold better to use the addVertex(x,y) method
    /// \param x
    /// \param y
    /// \return
    ///
    virtual VertexPointer insertVertex(double x,double y)=0;

private:

    bool mDirty=false;
    std::string mCrs;

};



#endif // REOSMESH_H
