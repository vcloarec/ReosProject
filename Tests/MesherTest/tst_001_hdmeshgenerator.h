#ifndef TST_001_HDMESHGENERATOR_H
#define TST_001_HDMESHGENERATOR_H

#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include "../../Mesher/provider/hdmeshgenerator.h"

using namespace testing;

class MeshGeneratorTesting:public Test{
public:

    HdMeshGeneratorTriangleFile meshGeneratorTriangleFile;

    void populateWithVertex()
    {
        inputMesh.addVertex(Vertex::makeVertex(5,15));
        inputMesh.addVertex(Vertex::makeVertex(10,15));
        inputMesh.addVertex(Vertex::makeVertex(5,10));
        inputMesh.addVertex(Vertex::makeVertex(10,10));
        inputMesh.addVertex(Vertex::makeVertex(10,5));
    }

    void populateSegment()
    {
        populateWithVertex();
        //segmentList.push_back(Segment(0,4));
    }

    void triangulateWithVertex()
    {
       populateWithVertex();
       meshGeneratorTriangleFile.triangulateMesh(inputMesh,outputMesh);
    }

    void triangulateWithSegment(std::vector<Vertex> &oVert,std::vector<Face> &oFaces)
    {
        populateSegment();
        ///TODO
       // meshGeneratorTriangleFile.triangulateTIN(verticesList,segmentList,oFaces);
    }

    HdMesh inputMesh;
    std::vector<Segment> segmentList;

    HdMesh outputMesh;
    std::vector<Segment> outputSegments;
};

TEST_F(MeshGeneratorTesting, triangulateFailNotEnoughtPoint)
{
    inputMesh.addVertex(Vertex::makeVertex(0,1));

    ASSERT_FALSE(meshGeneratorTriangleFile.triangulateMesh(inputMesh,outputMesh));
    ASSERT_THAT(meshGeneratorTriangleFile.getError(),Eq("NOT_ENOUGHT_DATA"));
}


TEST_F(MeshGeneratorTesting, triangulateMeshSuccessWithVertex)
{
    populateWithVertex();
    ASSERT_TRUE(meshGeneratorTriangleFile.triangulateMesh(inputMesh,outputMesh));
}

TEST_F(MeshGeneratorTesting, triangulateMeshSuccessWithSegment)
{
    populateSegment();
    ASSERT_TRUE(meshGeneratorTriangleFile.triangulateTIN(inputMesh,segmentList));
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
    triangulateWithVertex();

    ASSERT_THAT(outputMesh.facesCount(),Eq(3));
}

TEST_F(MeshGeneratorTesting, VertexCount)
{
    triangulateWithVertex();

    ASSERT_THAT(outputMesh.verticesCount(),Eq(5));
}


TEST_F(MeshGeneratorTesting, getFaces)
{
    triangulateWithVertex();

    std::vector<std::vector<int>> facesToObtain;
    facesToObtain.push_back({0,2,3});
    facesToObtain.push_back({4,3,2});
    facesToObtain.push_back({3,1,0});


    //ASSERT_THAT(outputFaces,Eq(facesToObtain));
}

TEST_F(MeshGeneratorTesting, getFacesWithTriangulateWithSegment)
{
    //triangulateWithSegment(outputVertices,outputFaces);

    std::vector<std::vector<int>> facesToObtain;
    facesToObtain.push_back({2,4,0});
    facesToObtain.push_back({3,0,4});
    facesToObtain.push_back({3,1,0});

    //ASSERT_THAT(outputFaces,Eq(facesToObtain));
}


#endif // TST_001_HDMESHGENERATOR_H
