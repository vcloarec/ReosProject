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
#include <mutex>

#include <netcdf.h>

#include "../../Reos/reosutils.h"

#define INVALID_VALUE -999999;

class VertexZSpecifier;
class VertexZSpecifierFactory;

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

    void setZSpecifier(const VertexZSpecifierFactory &zSpecifierFactory);
    void setZValue(double z);

    VertexZSpecifier* zSpecifier() const;
    VertexZSpecifier* releaseZSpecifier();

    void addDependentVertex(Vertex* otherVertex)
    {
        mDependentVertices.push_back(otherVertex);
    }
    void removeDependentVertex(Vertex* otherVertex)
    {
        mDependentVertices.remove(otherVertex);
    }

protected: //methode
    void setDirty();

private: //attribute
    void* mGraphic=nullptr;
    bool mZUserDefined=false;
    std::unique_ptr<VertexZSpecifier> mZSpecifier;

    std::list<Vertex*> mDependentVertices;

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

    virtual int writeUGRIDFormat(std::string fileName)=0;

    virtual int readUGRIDFormat(std::string fileName)=0;

    bool isDirty() const;

    //////////////////////////////////////////////////////
    /// \brief crs
    /// \return
    ///
    /// EPSG:code
    std::string crs() const {return mCrs;}
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

//***********************************************************************************
class VertexZSpecifier
{
public:
    VertexZSpecifier(const VertexPointer associatedVertex);
    virtual ~VertexZSpecifier();

    virtual VertexZSpecifier *clone(VertexPointer associatedVertex) const;
    double zValue() const;
    void setDirty(bool b);

protected:
    const VertexPointer mAssociatedVertex;
    mutable double mZValue=0;
    mutable bool mDirty=true;
    mutable std::mutex mMutex;

private:
    virtual void calculateZValue() const {}

};

class VertexZSpecifierFactory
{
public:
    virtual ~VertexZSpecifierFactory() {}
private:
    virtual   std::unique_ptr<VertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const =0;

    friend class Vertex;
};

//***********************************************************************************

class VertexZSpecifierSimple : public VertexZSpecifier
{
public:
    VertexZSpecifierSimple(const VertexPointer associatedVertex);
    VertexZSpecifierSimple(const VertexPointer associatedVertex,double z);

    VertexZSpecifier *clone(VertexPointer associatedVertex) const override;

};

class VertexZSpecifierSimpleFactory:public VertexZSpecifierFactory
{
public:
    VertexZSpecifierSimpleFactory(){}
    VertexZSpecifierSimpleFactory(double zValue);

    void setZValue(double z){mZValue=z;}

private:
    double mZValue=0;

    std::unique_ptr<VertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;
};

//***********************************************************************************

class VertexZSpecifierDependOnOtherVertex : public VertexZSpecifier
{
public:
    VertexZSpecifierDependOnOtherVertex(VertexPointer associatedVertex,VertexPointer otherVertex);
    virtual ~VertexZSpecifierDependOnOtherVertex()
    {
        if (mOtherVertex)
            mOtherVertex->removeDependentVertex(mAssociatedVertex);
    }
protected:
    VertexPointer mOtherVertex=nullptr;

};


class VertexZSpecifierDependOnOtherVertexFactory:public VertexZSpecifierFactory
{
public:
    VertexZSpecifierDependOnOtherVertexFactory(){}
    VertexZSpecifierDependOnOtherVertexFactory(VertexPointer otherVertex):VertexZSpecifierFactory(),mOtherVertex(otherVertex)
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

//***********************************************************************************

class VertexZSpecifierOtherVertexAndSlope : public VertexZSpecifierDependOnOtherVertex
{
public:
    VertexZSpecifierOtherVertexAndSlope(VertexPointer associatedVertex,VertexPointer otherVertex,double slope);
    VertexZSpecifier *clone(VertexPointer associatedVertex) const override;

private:
    double mSlope;


    // VertexZSpecifier interface
private:
    void calculateZValue()  const override;
};


class VertexZSpecifierOtherVertexAndSlopeFactory:public VertexZSpecifierDependOnOtherVertexFactory
{
public:
    VertexZSpecifierOtherVertexAndSlopeFactory(){}
    VertexZSpecifierOtherVertexAndSlopeFactory(VertexPointer otherVertex, double slope):
        VertexZSpecifierDependOnOtherVertexFactory(otherVertex),mSlope(slope)
    {}

    std::unique_ptr<VertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return std::make_unique<VertexZSpecifierOtherVertexAndSlope>(associatedVertex,mOtherVertex,mSlope);
    }

    void setSlope(double slope)
    {
        mSlope=slope;
    }

private:
    double mSlope=0;

};

//***********************************************************************************

class VertexZSpecifierOtherVertexAndGap : public VertexZSpecifierDependOnOtherVertex
{
public:
    VertexZSpecifierOtherVertexAndGap(VertexPointer associatedVertex,VertexPointer otherVertex,double gap);
    VertexZSpecifier *clone(VertexPointer associatedVertex) const override;

private:
    double mGap;

    // VertexZSpecifier interface
private:
    void calculateZValue() const override
    {
        if(mOtherVertex)
            mZValue=mOtherVertex->z()+mGap;
        else
            mZValue=INVALID_VALUE;
    }
};


class VertexZSpecifierOtherVertexAndGapFactory:public VertexZSpecifierDependOnOtherVertexFactory
{
public:
    VertexZSpecifierOtherVertexAndGapFactory(){}
    VertexZSpecifierOtherVertexAndGapFactory(VertexPointer otherVertex, double gap):
        VertexZSpecifierDependOnOtherVertexFactory(otherVertex),mGap(gap)
    {}

    std::unique_ptr<VertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override
    {
        return std::make_unique<VertexZSpecifierOtherVertexAndGap>(associatedVertex,mOtherVertex,mGap);
    }

    void setGap(double slope)
    {
        mGap=slope;
    }

private:
    double mGap=0;

};

//***********************************************************************************

class VertexZSpecifierInterpolation:public VertexZSpecifier
{
public:
    VertexZSpecifierInterpolation(const VertexPointer associatedVertex,VertexPointer firstVertex, VertexPointer secondVertex,bool hvf=true, bool hvs=true):
        VertexZSpecifier(associatedVertex),
        mFirstVertex(firstVertex),mSecondVertex(secondVertex),mHardVertexFirst(hvf),mHardVertexSecond(hvs)
    {}

private:
    VertexPointer mFirstVertex;
    VertexPointer mSecondVertex;
    bool mHardVertexFirst;
    bool mHardVertexSecond;


    // VertexZSpecifier interface
public:
    VertexZSpecifier *clone(VertexPointer associatedVertex) const override
    {
        return new VertexZSpecifierInterpolation(associatedVertex,mFirstVertex,mSecondVertex,mHardVertexFirst,mHardVertexSecond);
    }

private:
    void calculateZValue() const override
    {
        std::list<VertexPointer> vertices=verticesList();

        double distance=0;
        auto pv=vertices.begin();
        auto p=pv;
        p++;
        while(p!=vertices.end())
        {
            distance+=(*pv)->distanceFrom(*(*p));
            pv=p;
            p++;
        }


        double firstZValue=vertices.front()->z();
        double gap=firstZValue-vertices.back()->z();
        double slope=gap/distance;

        pv=vertices.begin();
        p=pv;
        p++;
        auto lp=vertices.end();
        lp--;
        double cumul=0;
        while(p!=lp)
        {
            auto currentSpecifier=static_cast<VertexZSpecifierInterpolation*>((*p)->zSpecifier());
            if ((*p)!=mAssociatedVertex)
                std::lock_guard<std::mutex> g(currentSpecifier->mMutex);
            currentSpecifier->mDirty=false;
            cumul+=(*pv)->distanceFrom(*(*p));
            currentSpecifier->mZValue=firstZValue-cumul*slope;
            pv=p;
            p++;
        }

    }

    std::list<VertexPointer> verticesList() const
    {
        std::list<VertexPointer> vertices;
        vertices.push_back(mAssociatedVertex);
        vertices.push_front(mFirstVertex);
        vertices.push_back(mSecondVertex);


        bool continuePreviously= (!mHardVertexFirst);
        bool continueNext= (!mHardVertexSecond);

        VertexZSpecifierInterpolation *currentSpecifier=nullptr;
        while(continuePreviously)
        {
           currentSpecifier=dynamic_cast<VertexZSpecifierInterpolation*>(vertices.front()->zSpecifier());
           if (!currentSpecifier)
               break;
           vertices.push_front(currentSpecifier->mFirstVertex);
           continuePreviously= (!currentSpecifier->mHardVertexFirst);
        }

        while(continueNext)
        {
            currentSpecifier=dynamic_cast<VertexZSpecifierInterpolation*>(vertices.back()->zSpecifier());
            if (!currentSpecifier)
                break;
            vertices.push_back((currentSpecifier->mSecondVertex));
            continueNext= (!currentSpecifier->mHardVertexSecond);
        }

        return vertices;
    }


};


class VertexZSpecifierInterpolationFactory:public VertexZSpecifierFactory
{
public:
    VertexZSpecifierInterpolationFactory();

    VertexZSpecifierInterpolationFactory(VertexPointer firstVertex, VertexPointer secondVertex,bool hvf=true, bool hvs=true);

    void setExtremitiesVertices(VertexPointer firstVertex, VertexPointer secondVertex);
    void setHardVertexFirst(bool b);
    void setHardVertexSecond(bool b);


private:
    std::unique_ptr<VertexZSpecifier> createZSpecifier(const VertexPointer associatedVertex) const override;

    VertexPointer mFirstVertex;
    VertexPointer mSecondVertex;
    bool mHardVertexFirst;
    bool mHardVertexSecond;
};


#endif // REOSMESH_H
