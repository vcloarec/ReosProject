/***************************************************************************
                      hdmeshgenerator.cpp
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

#include "hdmeshgenerator.h"

void HdMeshGeneratorTriangleFile::clear()
{
    setError("");
    outputName="";
}



bool HdMeshGeneratorTriangleFile::triangulateMesh(const HdMesh& inputMesh, HdMesh &outpuMesh)
{
    if (inputMesh.verticesCount()<3)
    {
        setError("NOT_ENOUGHT_DATA");
        return false;
    }

    outputName=triangleCallNodeInput("",inputMesh);

    if (outputName!="")
    {
        outputName=incFileName(outputName);
        getMesh(outpuMesh);
        std::remove((outputName+".node").c_str());
        std::remove((outputName+".ele").c_str());
        std::remove((outputName+".poly").c_str());
        return true;
    }
    else {
        return false;
    }
}

bool HdMeshGeneratorTriangleFile::triangulateTIN(HdMesh &mesh, const std::vector<Segment> &inputSegments)
{
    if (mesh.verticesCount()<3)
    {
        setError("NOT_ENOUGHT_DATA");
        return false;
    }

    outputName=triangleCallNodePolyInput("pc",mesh,inputSegments);

    if (outputName!=""){
        outputName=incFileName(outputName);
        getFaces(mesh);
        removeFile(outputName);
        return true;
    }
    else {
        return false;
    }

}


std::string HdMeshGenerator::getError() const
{
    return error;
}

std::string HdMeshGeneratorTriangleFile::incFileName(std::string fileName)
{
    auto splitFileName=splitString(fileName,'.');

    if (splitFileName.size()==0)
        return "";

    if (splitFileName.size()==1)
        return fileName+".1";

    if (splitFileName.size()==2)
        return splitFileName.at(0)+".1."+splitFileName.at(1);

    if (splitFileName.size()==3)
    {
        int c=std::stoi(splitFileName.at(1));
        c++;
        return splitFileName.at(0)+"."+std::to_string(c)+"."+splitFileName.at(2);
    }


    return fileName;
}

std::string HdMeshGeneratorTriangleFile::triangleCallNodeInput(std::string argument, const HdMesh& mesh)
{
    std::string inputName=TRIANGLE_FILENAME;
    std::string inputFileName=populateNodeFile(inputName,mesh);

    if(triangleCall(argument,inputFileName))
    {
        removeFile(inputName);
        return inputName;
    }

    else {
        removeFile(inputName);
        return "";
    }

}



std::string HdMeshGeneratorTriangleFile::triangleCallNodePolyInput(std::string argument, const HdMesh &mesh, const std::vector<Segment> &segments)
{
    std::string inputName=TRIANGLE_FILENAME;
    populateNodeFile(inputName,mesh);
    std::string inputFileName=populatePolyFile(inputName,mesh,segments);

    if(triangleCall(argument,inputFileName))
    {
        removeFile(inputName);
        return inputName;
    }

    else {
        removeFile(inputName);
        return "";
    }
}

std::string HdMeshGeneratorTriangleFile::populateNodeFile(std::string name, const HdMesh &mesh)
{
    std::string nodeFileName(name.append(".node"));
    std::ofstream nodeFile(nodeFileName);

    if (!nodeFile.is_open())
    {
        generatorError();
    }

    int count=mesh.verticesCount();

    nodeFile<<mesh.verticesCount()<<" "<<"2"<<std::endl;

    for (int i=0;i<count;++i)
    {
        VertexPointer vert=mesh.vertex(i);
        nodeFile<<i<<" "<<vert->x()<<" "<<vert->y()<<std::endl;
    }
    nodeFile.close();

    return nodeFileName;
}

std::string HdMeshGeneratorTriangleFile::populatePolyFile(std::string name, const HdMesh& mesh,const std::vector<Segment> &segments)
{
    std::string polyFileName(name.append(".poly"));
    std::ofstream polyFile(polyFileName);

    if (!polyFile.is_open())
    {
        generatorError();
    }

    polyFile<<"0"<<std::endl;
    polyFile<<segments.size()<<" "<<"0"<<std::endl;

    int i=0;
    for (auto seg:segments)
    {
        polyFile<<i++<<" "<<mesh.index(seg.first())<<" "<<mesh.index(seg.second())<<std::endl;
    }

    //hole
    polyFile<<"0"<<std::endl;;

    polyFile.close();

    return polyFileName;
}

bool HdMeshGeneratorTriangleFile::triangleCall(std::string argument, std::string inputFile)
{
    QFileInfo triangleFileInfo("triangle.exe");


    if (!triangleFileInfo.exists())
    {
        generatorError();
        return false;
    }


    if (argument!="")
        argument.insert(0,"-");

    std::string triangleCall="triangle.exe "+argument+" "+inputFile;
    system(triangleCall.c_str());

    return true;
}

int HdMeshGeneratorTriangleFile::getCountInFile(std::string fileName) const
{
    std::ifstream file(fileName);

    if (!file.is_open())
        return 0;


    std::string line;
    std::getline(file,line);

    auto splitLine=splitString(line,' ');

    if (splitLine.size()>0)
        return std::stoi(splitLine.at(0));
    else {
        return 0;
    }
}

void HdMeshGeneratorTriangleFile::removeFile(std::string name)
{
    std::remove((name+".node").c_str());
    std::remove((name+".ele").c_str());
    std::remove((name+".poly").c_str());
}

void HdMeshGeneratorTriangleFile::getVertices(HdMesh &mesh)
{
    mesh.clear();

    std::string fileName=outputName+".node";

    std::ifstream nodeFile(fileName);

    if (!nodeFile.is_open())
    {
        setError("NO_FILE_ACCESS");
        return;
    }

    std::string line;
    std::getline(nodeFile,line);

    auto splitLine=splitString(line,' ');

    int verticesCount;
    if (splitLine.size()>0)
        verticesCount=std::stoi(splitLine.at(0));
    else {
        verticesCount=0;
    }

    for (int i=0;i<verticesCount;++i)
    {
        std::getline(nodeFile,line);
        auto splitVerticesLine=splitString(line,' ');
        if(splitVerticesLine.size()>2)
        {
            mesh.addVertex(Vertex::makeVertex(std::stod(splitVerticesLine.at(1)),
                                              std::stod(splitVerticesLine.at(2))));
        }
    }


}

void HdMeshGeneratorTriangleFile::getFaces(HdMesh &mesh)
{
    mesh.clearFaces();

    std::string fileName=outputName+".ele";

    std::ifstream faceFile(fileName);

    if (!faceFile.is_open())
    {
        setError("NO_FILE_ACCESS");
        return;
    }

    std::string line;
    std::getline(faceFile,line);

    auto splitLine=splitString(line,' ');

    int facesCount;
    if (splitLine.size()>0)
        facesCount=std::stoi(splitLine.at(0));
    else {
        facesCount=0;
    }

    for (int i=0;i<facesCount;++i)
    {
        std::getline(faceFile,line);
        auto splitVerticesLine=splitString(line,' ');
        if(splitVerticesLine.size()>3)
        {
            std::vector<int> f({std::stoi(splitVerticesLine.at(1)),
                                std::stoi(splitVerticesLine.at(2)),
                                std::stoi(splitVerticesLine.at(3))});
            mesh.addFace(f);
        }
    }


}

void HdMeshGeneratorTriangleFile::getMesh(HdMesh &mesh)
{
    getVertices(mesh);
    getFaces(mesh);
}

void HdMeshGenerator::setError(const std::string &value)
{
    error = value;
}

std::vector<std::string> splitString(std::string str, char sep)
{
    std::istringstream is(str);
    std::vector<std::string> split;
    std::string s;
    while (std::getline(is,s,sep))
    {
        if (!s.empty())
            split.push_back(s);
    }


    return split;
}

int HdMesh::index(VertexPointer v) const
{
    bool found=false;
    size_t index=0;
    size_t count=mVertices.size();

    while( !found && index< count )
    {
        found = mVertices[index]==v;
        if( ! found )
            index++;
    }

    if (found)
        return int(index);
    else {
        return -1;
    }
}

int HdMesh::vertexIndex(double x, double y, double tolerance) const
{
    bool found=0;
    int i=0;
    while ( i<verticesCount() && !found )
    {
        VertexPointer vert=vertex(i);
        found=fabs(x-vert->x())<=tolerance && fabs(y-vert->y())<=tolerance;
        if (!found)
            ++i;
    }
    if (found)
        return int(i);

    return -1;
}


int HdMesh::index(FacePointer f) const
{
    bool found=false;
    size_t index=0;
    size_t count=mFaces.size();

    while( !found && index< count )
    {
        found = mFaces[index]==f;
        if( ! found )
            index++;
    }

    if (found)
        return int(index);
    else {
        return -1;
    }
}

int HdMesh::verticesCount() const {return int(mVertices.size());}

int HdMesh::facesCount() const {return int(mFaces.size());}

VertexPointer HdMesh::vertex(int index) const
{
    if ( index>=0 && index<verticesCount())
        return mVertices[size_t(index)];
    else {
        return VertexPointer();
    }
}

FacePointer HdMesh::face(int index) const
{
    if ( index>= 0 && index<facesCount())
        return mFaces[size_t(index)];
    else {
        return FacePointer();
    }
}

