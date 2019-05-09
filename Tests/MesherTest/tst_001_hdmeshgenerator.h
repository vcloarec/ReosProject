#ifndef TST_001_HDMESHGENERATOR_H
#define TST_001_HDMESHGENERATOR_H

#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "../../Mesher/hdmeshgenerator.h"

using namespace testing;

class MeshGeneratorTesting:public Test{
public:

    HdMeshGeneratorTriangleFile meshGeneratorTriangleFile;

    void populateWithVertex()
    {
        verticesList.push_back(Vertex(5,15));
        verticesList.push_back(Vertex(10,15));
        verticesList.push_back(Vertex(5,10));
        verticesList.push_back(Vertex(10,10));
        verticesList.push_back(Vertex(10,5));
    }

    void populateSegment()
    {
        populateWithVertex();
        segmentList.push_back(Segment(0,4));
    }

    void triangulateWithVertex(std::vector<Vertex> &oVert,std::vector<Face> &oFaces)
    {
        populateWithVertex();
        meshGeneratorTriangleFile.triangulateMesh(verticesList,oVert,oFaces);
    }

    void triangulateWithSegment(std::vector<Vertex> &oVert,std::vector<Face> &oFaces)
    {
        populateSegment();
        meshGeneratorTriangleFile.triangulateTIN(verticesList,segmentList,oFaces);
    }

    std::vector<Vertex> verticesList;
    std::vector<Segment> segmentList;

    std::vector<Vertex> outputVertices;
    std::vector<Face> outputFaces;
    std::vector<Segment> outputSegments;
};

TEST_F(MeshGeneratorTesting, triangulateFailNotEnoughtPoint)
{
    std::vector<Vertex> onePoint({Vertex(0,1)});

    ASSERT_FALSE(meshGeneratorTriangleFile.triangulateMesh(onePoint,outputVertices,outputFaces));
    ASSERT_THAT(meshGeneratorTriangleFile.getError(),Eq("NOT_ENOUGHT_DATA"));
}


TEST_F(MeshGeneratorTesting, triangulateMeshSuccessWithVertex)
{
    populateWithVertex();
    ASSERT_TRUE(meshGeneratorTriangleFile.triangulateMesh(verticesList,outputVertices,outputFaces));
}

TEST_F(MeshGeneratorTesting, triangulateMeshSuccessWithSegment)
{
    populateSegment();
    ASSERT_TRUE(meshGeneratorTriangleFile.triangulateTIN(verticesList,segmentList,outputFaces));
}

TEST_F(MeshGeneratorTesting, incrementSimpleFileName)
{
    std::string filename="abcd";
    ASSERT_THAT(meshGeneratorTriangleFile.incFileName(filename),Eq("abcd.1"));
}

TEST_F(MeshGeneratorTesting, incrementIncrementedFileName)
{
    std::string filename="abcd.1.node";
    ASSERT_THAT(meshGeneratorTriangleFile.incFileName(filename),Eq("abcd.2.node"));
}

TEST_F(MeshGeneratorTesting, FaceCount)
{
    triangulateWithVertex(outputVertices,outputFaces);

    ASSERT_THAT(outputFaces.size(),Eq(3));
}

TEST_F(MeshGeneratorTesting, VertexCount)
{
    triangulateWithVertex(outputVertices,outputFaces);

    ASSERT_THAT(outputVertices.size(),Eq(5));
}


TEST_F(MeshGeneratorTesting, getFaces)
{
    triangulateWithVertex(outputVertices,outputFaces);

    std::vector<std::vector<int>> facesToObtain;
    facesToObtain.push_back({0,2,3});
    facesToObtain.push_back({4,3,2});
    facesToObtain.push_back({3,1,0});

    ASSERT_THAT(outputFaces,Eq(facesToObtain));
}

TEST_F(MeshGeneratorTesting, getFacesWithTriangulateWithSegment)
{
    triangulateWithSegment(outputVertices,outputFaces);

    std::vector<std::vector<int>> facesToObtain;
    facesToObtain.push_back({2,4,0});
    facesToObtain.push_back({3,0,4});
    facesToObtain.push_back({3,1,0});

    ASSERT_THAT(outputFaces,Eq(facesToObtain));
}


#endif // TST_001_HDMESHGENERATOR_H
