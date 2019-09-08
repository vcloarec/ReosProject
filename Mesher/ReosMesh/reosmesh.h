/***************************************************************************
                      reosmesh.h
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


#ifndef REOSMESH_H
#define REOSMESH_H

#include <memory>
#include <math.h>
#include <vector>
#include <list>
#include <fstream>
#include <iostream>
#include <cstring>

#include <netcdf.h>

#include "../../Reos/reosutils.h"

#define INVALID_VALUE -999999;

class VertexZSpecifier;
class VertexZSPecifierFactory;

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

    void setZUserDefined();
    bool isZUserDefined() const;

    double distanceFrom(const Vertex &other) const
    {
        return sqrt(pow(x()-other.x(),2)+pow(y()-other.y(),2));
    }

    void setZSpecifier(const VertexZSPecifierFactory &zSpecifierFactory);
    void setZValue(double z);

    VertexZSpecifier* zSpecifier() const;

private:
    void* mGraphic=nullptr;
    bool mZUserDefined=false;
    std::unique_ptr<VertexZSpecifier> mZSpecifier;

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
    virtual ~Face() {}
    virtual void addVertex(VertexPointer vert)=0;
    virtual VertexPointer vertexPointer(int i) const =0;
    virtual int verticesCount() const=0;
    virtual bool isVertexContained(VertexPointer vertex) const;

    std::vector<double> faceCentroid();
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

    virtual void clear()=0;
    virtual void clearFaces()=0;

    virtual VertexPointer addVertex(double x, double y)=0;
    virtual std::list<VertexPointer> addHardLine(VertexPointer v1, VertexPointer v2)=0;
    virtual std::list<VertexPointer> hardNeighbours(VertexPointer vertex) const=0;
    virtual std::list<VertexPointer> removeHardLine(VertexPointer v1, VertexPointer v2)=0;

    virtual std::unique_ptr<MeshIO> getReader() const =0;
    virtual std::list<VertexPointer> neighboursVertices(VertexPointer vertex) const =0;
    virtual int maxNodesPerFaces() const=0;

    virtual int writeUGRIDFormat(std::string fileName)=0;

    virtual int readUGRIDFormat(std::string fileName)=0;

    bool isDirty() const;
protected:
    double mTolerance=0.01;

private:

    bool mDirty=false;


};


class VertexZSpecifier
{
public:
    VertexZSpecifier(const VertexPointer associatedVertex):
        mAssociatedVertex(associatedVertex)
    {

    }
    virtual ~VertexZSpecifier();

    virtual VertexZSpecifier *clone(VertexPointer associatedVertex) const
    {
        return new VertexZSpecifier(associatedVertex);
    }

    virtual double getZValue() const {return 0;}

protected:
    const VertexPointer mAssociatedVertex;
};

class VertexZSpecifierSimple : public VertexZSpecifier
{
public:
    VertexZSpecifierSimple(const VertexPointer associatedVertex);
    VertexZSpecifierSimple(const VertexPointer associatedVertex,double z);

    VertexZSpecifier *clone(VertexPointer associatedVertex) const override;

    double getZValue() const override;
private:
    double mZValue=0;



};


class VertexZSpecifierDependOnOtherVertex : public VertexZSpecifier
{
public:
    VertexZSpecifierDependOnOtherVertex(VertexPointer associatedVertex,VertexPointer otherVertex):
        VertexZSpecifier (associatedVertex),mOtherVertex(otherVertex)
    {

    }


protected:
    VertexPointer mOtherVertex=nullptr;

};

class VertexZSpecifierOtherVertexAndSlope : public VertexZSpecifierDependOnOtherVertex
{
public:
    VertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex,VertexPointer otherVertex,double slope);
    VertexZSpecifier *clone(VertexPointer associatedVertex) const override;

    double getZValue() const override;

private:
    double mSlope;


};

class VertexZSpecifierOtherVertexAndGap : public VertexZSpecifierDependOnOtherVertex
{
public:
    VertexZSpecifierOtherVertexAndGap(VertexPointer associatedVertex,VertexPointer otherVertex,double gap);
    VertexZSpecifier *clone(VertexPointer associatedVertex) const override;

    double getZValue() const override;

private:
    double mGap;
};




class VertexZSPecifierFactory
{
public:
    virtual ~VertexZSPecifierFactory() {}
private:
    virtual   VertexZSpecifier* createZSpecifier(const VertexPointer associatedVertex) const =0;

    friend class Vertex;
};

class VertexZSpecifierSimpleFactory:public VertexZSPecifierFactory
{
public:
    VertexZSpecifierSimpleFactory(){}
    VertexZSpecifierSimpleFactory(double zValue):VertexZSPecifierFactory(),mZValue(zValue)
    {

    }

    void setZValue(double z){mZValue=z;}

private:
    double mZValue=0;

    VertexZSpecifier *createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return new VertexZSpecifierSimple(associatedVertex,mZValue);
    }
};

class VertexZSpecifierDependOnOtherVertexFactory:public VertexZSPecifierFactory
{
public:
    VertexZSpecifierDependOnOtherVertexFactory(){}
    VertexZSpecifierDependOnOtherVertexFactory(VertexPointer otherVertex):VertexZSPecifierFactory(),mOtherVertex(otherVertex)
    {

    }
    virtual ~VertexZSpecifierDependOnOtherVertexFactory() {}

    void setOtherVertex(VertexPointer otherVertex)
    {
        mOtherVertex=otherVertex;
    }

protected:
    VertexPointer mOtherVertex=nullptr;


};

class VertexZSpecifierOtherVertexAndSlopeFactory:public VertexZSpecifierDependOnOtherVertexFactory
{
public:
    VertexZSpecifierOtherVertexAndSlopeFactory(){}
    VertexZSpecifierOtherVertexAndSlopeFactory(VertexPointer otherVertex, double slope):
        VertexZSpecifierDependOnOtherVertexFactory(otherVertex),mSlope(slope)
    {}

    VertexZSpecifier *createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return new VertexZSpecifierOtherVertexAndSlope(associatedVertex,mOtherVertex,mSlope);
    }

    void setSlope(double slope)
    {
        mSlope=slope;
    }

private:
    double mSlope=0;

};

class VertexZSpecifierOtherVertexAndGapFactory:public VertexZSpecifierDependOnOtherVertexFactory
{
public:
    VertexZSpecifierOtherVertexAndGapFactory(){}
    VertexZSpecifierOtherVertexAndGapFactory(VertexPointer otherVertex, double gap):
        VertexZSpecifierDependOnOtherVertexFactory(otherVertex),mGap(gap)
    {}

    VertexZSpecifier *createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return new VertexZSpecifierOtherVertexAndGap(associatedVertex,mOtherVertex,mGap);
    }

    void setGap(double slope)
    {
        mGap=slope;
    }

private:
    double mGap=0;

};





#endif // REOSMESH_H
