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

#include <qgspoint.h>

#include "hdmesh.h"

#define TRIANGLE_FILENAME "tinTriangle"


class VertexBasic:public Vertex
{
public:
    VertexBasic(double x, double y);
    ~VertexBasic();

    double x() const {return x_;}
    double y() const {return y_;}
    double z() const {return z_;}

    void setZValue(double ZValue)
    {
        z_=ZValue;
    }

private:
    double x_=0;
    double y_=0;
    double z_=0;

};




class HdMeshBasic:public HdMesh
{
public:
    ~ HdMeshBasic() override
    {
        for (auto v:mVertices)
            delete v;
        for (auto f:mFaces)
            delete f;
    }

    int verticesCount() const override;
    int facesCount() const override;

    int index(VertexPointer v) const;
    int index(FacePointer f) const;

    VertexPointer vertex(int index) const;
    VertexPointer vertex(double x, double y, double tolerance) const override;
    FacePointer face(int index) const;

    void clear() override
    {
        mVertices.clear();
        clearFaces();
    }

    void clearFaces() override
    {
        mFaces.clear();
    }

    virtual VertexPointer addVertex(double x, double y) override
    {
        VertexPointer vert=new VertexBasic(x,y);
        mVertices.push_back(vert);
        return vert;
    }

    void addFace(const std::vector<int> &faceInt)
    {
        auto face=new Face();
        for (auto i:faceInt)
            face->addVertex(vertex(i));
        mFaces.push_back(face);
    }

private:
    std::vector<VertexPointer> mVertices;
    std::vector<FacePointer> mFaces;


    // HdMesh interface
public:
    std::unique_ptr<MeshIO> getReader() const override {return  std::unique_ptr<MeshIO>();}
};






std::vector<std::string> splitString(std::string str,char sep);


class HdMeshGenerator{
public:
    virtual ~HdMeshGenerator() {}

    virtual void clear()=0;

    virtual bool triangulateMesh(const HdMeshBasic& inputMesh,  HdMeshBasic& outpuMesh)=0;

    virtual bool triangulateTIN(HdMeshBasic& mesh,const std::vector<Segment> &inputSegments)=0;
    std::string getError() const;

    virtual std::string getKey() const =0;

protected:
    virtual void getVertices(HdMeshBasic &mesh)=0;
    virtual void getFaces(HdMeshBasic &mesh)=0;
    virtual void getMesh(HdMeshBasic &mesh)=0;

    bool generatorError()
    {
        setError("GENERATOR_ERROR");
        return false;
    }

    void setError(const std::string &value);

    std::vector<VertexBasic> vertices;


private:
    std::vector<VertexBasic> resultVertexes;  //not necessary for the mesh generator with Triangle program because the data are stored in file
    std::vector<Face> resultfaces;

    std::string error;
};

#ifdef __linux
static std::string triangleExecutable="triangle";
static std::string triangleExecutableCall="./"+triangleExecutable;
#endif
#ifdef _WIN64
static std::string triangleExecutable="triangle.exe";
static std::string triangleExecutableCall="triangle.exe";
#endif



class HdMeshGeneratorTriangleFile:public HdMeshGenerator{
public:
    ~HdMeshGeneratorTriangleFile() override {}

    virtual void clear() override;

    virtual bool triangulateMesh(const HdMeshBasic& inputMesh,
                                 HdMeshBasic& outpuMesh) override;

    virtual bool triangulateTIN(HdMeshBasic& mesh,const std::vector<Segment> &inputSegments) override;

    static std::string incFileName(std::string fileName);

    std::string getKey() const override{
        return "TriangleFile";
    }

private:

    virtual void getVertices(HdMeshBasic &mesh) override;
    virtual void getFaces(HdMeshBasic &mesh) override;
    virtual void getMesh(HdMeshBasic &mesh) override;

    std::string triangleCallNodeInput(std::string argument, const HdMeshBasic &mesh);
    std::string triangleCallNodePolyInput(std::string argument, const HdMeshBasic &mesh,const std::vector<Segment> &segments);
    std::string populateNodeFile(std::string name,const HdMeshBasic& mesh);
    std::string populatePolyFile(std::string name,const HdMeshBasic& mesh,const std::vector<Segment> &segments);
    bool triangleCall(std::string argument,std::string inputFile);
    int getCountInFile(std::string fileName) const;

    void removeFile(std::string name);

    std::string outputName;
};

#endif // HDMESHGENERATOR_H
