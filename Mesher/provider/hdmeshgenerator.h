/***************************************************************************
                      hdmeshgenerator.h
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

#ifndef HDMESHGENERATOR_H
#define HDMESHGENERATOR_H

#include <iostream>
#include <sstream>
#include <fstream>

#include <QFileInfo>
#include <QDebug>

#include <qgspoint.h>

#define TRIANGLE_FILENAME "tinTriangle"



class Vertex
{
public:
    Vertex(double x, double y):x_(x),y_(y) {}

    double x() const {return x_;}
    double y() const {return y_;}
    double z() const {return z_;}

    void setZValue(double ZValue)
    {
        z_=ZValue;
    }

    static std::shared_ptr<Vertex> makeVertex(double x, double y) {return std::make_shared<Vertex>(x,y);}

private:
    double x_=0;
    double y_=0;
    double z_=0;
};

typedef std::shared_ptr<Vertex> VertexPointer;

class Segment
{
public:
    Segment(VertexPointer v1, VertexPointer v2):mVertex1(v1),mVertex2(v2)
    {}

    VertexPointer first() const {return mVertex1;}
    VertexPointer second() const {return mVertex2;}

private:
    VertexPointer mVertex1;
    VertexPointer mVertex2;
};

class Face
{
public:
    void addVertex(VertexPointer vert)
    {
        mVertices.push_back(vert);
    }

    VertexPointer vertex(int i) const
    {
        if (i>=0 && i<int(mVertices.size()))
            return mVertices[size_t(i)];
        else {
            return VertexPointer();
        }
    }

    int verticesCount() const  {return int(mVertices.size());}

private:
    std::vector<VertexPointer> mVertices;
};


typedef std::shared_ptr<Face> FacePointer;


class HdMesh
{
public:

    int verticesCount() const;
    int facesCount() const;

    int index(VertexPointer v) const;
    int vertexIndex(double x, double y, double tolerance) const;
    int index(FacePointer f) const;

    VertexPointer vertex(int index) const;
    FacePointer face(int index) const;

    void clear()
    {
        mVertices.clear();
        clearFaces();
    }

    void clearFaces()
    {
        mFaces.clear();
    }

    void addVertex(VertexPointer vert)
    {
        mVertices.push_back(vert);
    }

    void addFace(const std::vector<int> &faceInt)
    {
        auto face=std::make_shared<Face>();
        for (auto i:faceInt)
            face->addVertex(vertex(i));
        mFaces.push_back(face);
    }

    double az=4.123456789;

private:
    std::vector<VertexPointer> mVertices;
    std::vector<FacePointer> mFaces;


};




std::vector<std::string> splitString(std::string str,char sep);


class HdMeshGenerator{
public:
    virtual ~HdMeshGenerator() {}

    virtual void clear()=0;

    virtual bool triangulateMesh(const HdMesh& inputMesh,  HdMesh& outpuMesh)=0;


    virtual bool triangulateTIN(HdMesh& mesh,const std::vector<Segment> &inputSegments)=0;
    std::string getError() const;

    virtual std::string getKey() const =0;

protected:
    virtual void getVertices(HdMesh &mesh)=0;
    virtual void getFaces(HdMesh &mesh)=0;
    virtual void getMesh(HdMesh &mesh)=0;

    bool generatorError()
    {
        setError("GENERATOR_ERROR");
        return false;
    }

    void setError(const std::string &value);

    std::vector<Vertex> vertices;


private:
    std::vector<Vertex> resultVertexes;  //not necessary for the mesh generator with Triangle program because the data are stored in file
    std::vector<Face> resultfaces;

    std::string error;
};



class HdMeshGeneratorTriangleFile:public HdMeshGenerator{
public:
    ~HdMeshGeneratorTriangleFile() override {}

    virtual void clear() override;

    virtual bool triangulateMesh(const HdMesh& inputMesh,
                                 HdMesh& outpuMesh) override;

    virtual bool triangulateTIN(HdMesh& mesh,const std::vector<Segment> &inputSegments) override;


    //
    static std::string incFileName(std::string fileName);

    std::string getKey() const override{
        return "TriangleFile";
    }

private:

    virtual void getVertices(HdMesh &mesh) override;
    virtual void getFaces(HdMesh &mesh) override;
    virtual void getMesh(HdMesh &mesh) override;

    std::string triangleCallNodeInput(std::string argument, const HdMesh &mesh);
    std::string triangleCallNodePolyInput(std::string argument, const HdMesh &mesh,const std::vector<Segment> &segments);
    std::string populateNodeFile(std::string name,const HdMesh& mesh);
    std::string populatePolyFile(std::string name,const HdMesh& mesh,const std::vector<Segment> &segments);
    bool triangleCall(std::string argument,std::string inputFile);
    int getCountInFile(std::string fileName) const;

    void removeFile(std::string name);

    std::string outputName;
};

#endif // HDMESHGENERATOR_H
