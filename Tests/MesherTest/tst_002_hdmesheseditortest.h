#ifndef TST_HDMESHESEDITORTEST_H
#define TST_HDMESHESEDITORTEST_H

#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>


#include "../../Mesher/provider/meshdataprovider.h"


using namespace testing;

class MeshEditorTesting:public Test{
public:

    HdMesh mesh;
    std::vector<Segment> inputSegments;

    TINEditor meshEditor=TINEditor(mesh,inputSegments);

    void initializeMeshEditor()
    {
        meshEditor.addMeshGenerator(new HdMeshGeneratorTriangleFile());
        meshEditor.setCurrentMeshGenerator("TriangleFile");
    }

    void populateMeshEditorWithVertices()
    {
        meshEditor.addVertex(Vertex(5,15));
        meshEditor.addVertex(Vertex(10,15));
        meshEditor.addVertex(Vertex(5,10));
        meshEditor.addVertex(Vertex(10,10));
        meshEditor.addVertex(Vertex(10,5));
    }




};

TEST_F(MeshEditorTesting, inputVertexCount){

    meshEditor.addVertex(Vertex(1,1));
    ASSERT_THAT(meshEditor.verticesCount(),Eq(1));

}

TEST_F(MeshEditorTesting, containMeshGenerator)
{
    initializeMeshEditor();

    ASSERT_THAT(meshEditor.containMeshGenerator("TriangleFile"),Eq(true));
}

TEST_F(MeshEditorTesting, setCurrentMeshGeneratorFail)
{
    //mesheditor not initialize
    meshEditor.setCurrentMeshGenerator("TriangleFile");
    ASSERT_TRUE(meshEditor.currentMeshGenerator()==nullptr);
}

TEST_F(MeshEditorTesting, setCurrentMeshGeneratorSuccess)
{
    initializeMeshEditor();

    meshEditor.setCurrentMeshGenerator("TriangleFile");
    ASSERT_TRUE(meshEditor.currentMeshGenerator()!=nullptr);
}

TEST_F(MeshEditorTesting, generateMeshFail)
{
    initializeMeshEditor();
    meshEditor.addVertex(Vertex(1,1));

    ASSERT_FALSE(meshEditor.generateMesh());
}

TEST_F(MeshEditorTesting, generateMeshSucess)
{
    initializeMeshEditor();
    populateMeshEditorWithVertices();

    ASSERT_TRUE(meshEditor.generateMesh());
}


TEST_F(MeshEditorTesting, vertexIndex)
{
    initializeMeshEditor();
    meshEditor.setTolerance(0.01);

    meshEditor.addVertex(Vertex(10,15));

    Vertex other(10.0001,14.99999);

    ASSERT_THAT(meshEditor.vertexIndex(other),Eq(0));
}

TEST_F(MeshEditorTesting, addDupplicatePoint)
{
    initializeMeshEditor();
    meshEditor.setTolerance(0.01);

    meshEditor.addVertex(Vertex(10,15));
    meshEditor.addVertex(Vertex(5,10));
    meshEditor.addVertex(Vertex(10,10));

    meshEditor.addVertex(Vertex(10.0001,14.99999));

    ASSERT_THAT(meshEditor.verticesCount(),Eq(3));

}

TEST_F(MeshEditorTesting, addSegmentHardline)
{
    initializeMeshEditor();
    populateMeshEditorWithVertices();

    meshEditor.addSegment(1,2);

    ASSERT_THAT(meshEditor.segmentsCount(),Eq(1));
}

TEST_F(MeshEditorTesting, addDupplicateSegmentHardline)
{
    initializeMeshEditor();
    populateMeshEditorWithVertices();

    meshEditor.addSegment(1,2);
    meshEditor.addSegment(1,2);
    meshEditor.addSegment(2,1);

    ASSERT_THAT(meshEditor.segmentsCount(),Eq(1));
}



#endif // TST_HDMESHESEDITORTEST_H
