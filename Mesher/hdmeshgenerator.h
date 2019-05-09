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

private:
    double x_=0;
    double y_=0;
    double z_=2;
};



typedef std::pair<int,int> Segment;
typedef std::vector<int> Face;




std::vector<std::string> splitString(std::string str,char sep);


class HdMeshGenerator{
public:
    virtual ~HdMeshGenerator() {}

    virtual void clear()=0;

    virtual bool triangulateMesh(const std::vector<Vertex> &inputVerties, std::vector<Vertex> &outputVertices, std::vector<Face> &outputFaces)=0;


    virtual bool triangulateTIN(const std::vector<Vertex> &inputVertices,
                                 const std::vector<Segment> &inputSegments,
                                 std::vector<Face> &outputFaces)=0;
    std::string getError() const;

    virtual std::string getKey() const =0;

protected:
    virtual void getVertices(std::vector<Vertex> &verticies)=0;
    virtual void getFaces(std::vector<Face> &faces)=0;

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

    virtual bool triangulateMesh(const std::vector<Vertex> &inputVerties,
                                 std::vector<Vertex> &outputVertices,
                                 std::vector<Face> &outputFaces) override;

    virtual bool triangulateTIN(const std::vector<Vertex> &inputVertices,
                                 const std::vector<Segment> &inputSegments,
                                 std::vector<Face> &outputFaces) override;


    //
    static std::string incFileName(std::string fileName);

    std::string getKey() const override{
        return "TriangleFile";
    }

private:

    virtual void getVertices(std::vector<Vertex> &verticies) override;
    virtual void getFaces(std::vector<Face> &faces) override;


    std::string triangleCallNodeInput(std::string argument, const std::vector<Vertex> &nodes);
    std::string triangleCallNodePolyInput(std::string argument, const std::vector<Vertex> &nodes,const std::vector<Segment> &segments);
    std::string populateNodeFile(std::string name,const std::vector<Vertex> &points);
    std::string populatePolyFile(std::string name,const std::vector<Segment> &segments);
    bool triangleCall(std::string argument,std::string inputFile);
    int getCountInFile(std::string fileName) const;

    void removeFile(std::string name);

    std::string outputName;
};

#endif // HDMESHGENERATOR_H
