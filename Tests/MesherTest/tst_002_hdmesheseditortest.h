#ifndef TST_HDMESHESEDITORTEST_H
#define TST_HDMESHESEDITORTEST_H

#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>


#include "../../Mesher/HdMesh/reosmesheditor.h"
#include "../../Mesher/HdTin/reostineditor.h"
#include "../../Mesher/HdTin/test.h"


using namespace testing;

class MeshEditorTesting:public Test{
public:

    HdMeshBasic mesh;
    std::vector<Segment> inputSegments;

    HdMeshEditor meshEditor=HdMeshEditor(mesh,inputSegments);

    void initializeMeshEditor()
    {
        meshEditor.addMeshGenerator(new HdMeshGeneratorTriangleFile());
        meshEditor.setCurrentMeshGenerator("TriangleFile");
    }

    void populateMeshEditorWithVertices()
    {
        meshEditor.addVertex(5,15);
        meshEditor.addVertex(10,15);
        meshEditor.addVertex(5,10);
        meshEditor.addVertex(10,10);
        meshEditor.addVertex(10,5);
    }

};

TEST_F(MeshEditorTesting, inputVertexCount){

    meshEditor.addVertex(1,1);
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
    meshEditor.addVertex(VertexBasic(1,1));

    ASSERT_FALSE(meshEditor.generateMesh());
}

TEST_F(MeshEditorTesting, generateMeshSucess)
{
    initializeMeshEditor();
    populateMeshEditorWithVertices();

    ASSERT_TRUE(meshEditor.generateMesh());
}


TEST_F(MeshEditorTesting, searchVertex)
{
    initializeMeshEditor();
    meshEditor.setTolerance(0.01);

    VertexPointer vert=meshEditor.addVertex(VertexBasic(10,15));

    bool found=meshEditor.vertex(10.0001,14.99999)==vert;

    ASSERT_THAT(found,Eq(true));
}

TEST_F(MeshEditorTesting, addDupplicatePoint)
{
    initializeMeshEditor();
    meshEditor.setTolerance(0.01);

    meshEditor.addVertex(VertexBasic(10,15));
    meshEditor.addVertex(VertexBasic(5,10));
    meshEditor.addVertex(VertexBasic(10,10));

    meshEditor.addVertex(VertexBasic(10.0001,14.99999));

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


class TinEditorTesting:public Test{
public:
    ReosTin tin;


};

TEST_F(TinEditorTesting, tinCreation)
{
    ASSERT_THAT(tin.verticesCount(),Eq(0));
}

TEST_F(TinEditorTesting, addOneVertex)
{
    tin.addVertex(5,5);

    ASSERT_THAT(tin.verticesCount(),Eq(1));
}

TEST_F(TinEditorTesting, addVerticesToHaveAFace)
{
    tin.addVertex(5,5);
    tin.addVertex(10,5);
    tin.addVertex(5,10);

    ASSERT_THAT(tin.verticesCount(),Eq(3));
    ASSERT_THAT(tin.facesCount(),Eq(1));
}

TEST_F(TinEditorTesting, addVerticesToHaveTwoFace)
{
    tin.addVertex(5,5);
    tin.addVertex(6,5);
    tin.addVertex(7,10);
    tin.addVertex(10,10);

    ASSERT_THAT(tin.verticesCount(),Eq(4));
    ASSERT_THAT(tin.facesCount(),Eq(2));
}


TEST_F(TinEditorTesting, addDupplicatesVertices)
{
    tin.addVertex(5,5);
    tin.addVertex(5,5);
    tin.addVertex(7,10);
    tin.addVertex(10,10);

    ASSERT_THAT(tin.verticesCount(),Eq(3));
    ASSERT_THAT(tin.facesCount(),Eq(1));
}

TEST_F(TinEditorTesting, locateVertex)
{
    VertexPointer vert1=tin.addVertex(5,5);

    tin.addVertex(7,10);
    tin.addVertex(10,10);

    VertexPointer vert2=tin.vertex(4.999,5.001,0.01);

    ASSERT_TRUE(vert2==vert1);
}

TEST_F(TinEditorTesting, addHardLine)
{
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    ASSERT_THAT(tin.verticesCount(),Eq(4));
    ASSERT_THAT(tin.facesCount(),Eq(2));

    std::list<VertexPointer> constraintVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> constraintVertices_2=tin.addHardLine(vert3,vert4);

    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.facesCount(),Eq(4));

    auto vert5=tin.addVertex(1,0);
    auto vert6=tin.addVertex(1,5);

    std::list<VertexPointer> newVertices_3=tin.addHardLine(vert5,vert6);

    ASSERT_THAT(constraintVertices_1.size(),Eq(2));
    ASSERT_THAT(constraintVertices_2.size(),Eq(3));
    ASSERT_THAT(newVertices_3.size(),Eq(4));
}

TEST_F(TinEditorTesting, hasHardNeighbours)
{
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

    auto vert=newVertices_2.begin();
    vert++;
    std::list<VertexPointer> neighbours=tin.hardNeighbours((*vert));

    ASSERT_THAT(neighbours.size(),Eq(4));
}

TEST_F(TinEditorTesting, isSegmentOnHardline)
{
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

    auto centralVertex=newVertices_2.begin();
    centralVertex++;
    auto centralVertexPointer=static_cast<VertexPointer>((*centralVertex));

    ASSERT_THAT(tin.isOnHardLine(vert1,vert2),Eq(false));
    ASSERT_THAT(tin.isOnHardLine(vert1,vert3),Eq(false));
    ASSERT_THAT(tin.isOnHardLine(vert1,centralVertexPointer),Eq(true));
}


TEST_F(TinEditorTesting, removeFirstHardLineFromSegment)
{
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

    auto centralVertex=newVertices_2.begin();
    centralVertex++;
    auto centralVertexPointer=static_cast<VertexPointer>((*centralVertex));

    tin.removeHardLine(vert1,centralVertexPointer);

    std::list<VertexPointer> neighbours_1=tin.hardNeighbours(vert1);
    std::list<VertexPointer> neighbours_2=tin.hardNeighbours(vert2);
    std::list<VertexPointer> neighbours_3=tin.hardNeighbours(vert3);
    std::list<VertexPointer> neighbours_4=tin.hardNeighbours(vert4);
    std::list<VertexPointer> neighbours_central=tin.hardNeighbours(centralVertexPointer);
    ASSERT_THAT(neighbours_1.size(),Eq(0));
    ASSERT_THAT(neighbours_2.size(),Eq(0));
    ASSERT_THAT(neighbours_3.size(),Eq(1));
    ASSERT_THAT(neighbours_4.size(),Eq(1));
    ASSERT_THAT(neighbours_central.size(),Eq(2));
    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.hardNeighbours(centralVertexPointer).size(),Eq(2));

}

TEST_F(TinEditorTesting, removeFirstHardLineFromExtremities)
{
    /////////////////////////////////////////////////////////
    /// \brief Remove constraint from vertices wich are not neighbour is not permit, so, as vert1 and vert 2
    /// are note anymore neighbours because of the central vertex insertion, removing hardline is not permit
    ///
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

    auto centralVertex=newVertices_2.begin();
    centralVertex++;
    auto centralVertexPointer=static_cast<VertexPointer>((*centralVertex));

    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.hardNeighbours(centralVertexPointer).size(),Eq(4));

    tin.removeHardLine(vert1,vert2);

    std::list<VertexPointer> neighbours_1=tin.hardNeighbours(vert1);
    std::list<VertexPointer> neighbours_2=tin.hardNeighbours(vert2);
    std::list<VertexPointer> neighbours_3=tin.hardNeighbours(vert3);
    std::list<VertexPointer> neighbours_4=tin.hardNeighbours(vert4);
    std::list<VertexPointer> neighbours_central=tin.hardNeighbours(centralVertexPointer);
    ASSERT_THAT(neighbours_1.size(),Eq(1));
    ASSERT_THAT(neighbours_2.size(),Eq(1));
    ASSERT_THAT(neighbours_3.size(),Eq(1));
    ASSERT_THAT(neighbours_4.size(),Eq(1));
    ASSERT_THAT(neighbours_central.size(),Eq(4));
    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.hardNeighbours(centralVertexPointer).size(),Eq(4));


}


TEST_F(TinEditorTesting, removeVertex)
{

    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);

    ASSERT_THAT(tin.verticesCount(),Eq(4));

    tin.removeVertex(vert3);

    ASSERT_THAT(tin.verticesCount(),Eq(3));

}

TEST_F(TinEditorTesting, removeVertexOnHardLineWithRemoveVertex)
{
    /////////////////////////////////////////////////////////
    /// \brief Remove vertices on hardline is not permit It is necessary to use removeVertexOnConstraint
    ///
    ///
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.hardNeighbours(vert1).size(),Eq(1));

    auto centralVertex=newVertices_2.begin();
    centralVertex++;
    auto centralVertexPointer=static_cast<VertexPointer>((*centralVertex));

    tin.removeVertex(vert1);

    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.hardNeighbours(centralVertexPointer).size(),Eq(4));

}

TEST_F(TinEditorTesting, removeVertexOnHardLineWithRemoveVertexOnHardline)
{
//    auto vert1=tin.addVertex(0,5);
//    auto vert2=tin.addVertex(5,0);
//    auto vert3=tin.addVertex(0,0);
//    auto vert4=tin.addVertex(5,5);

//    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
//    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

//    ASSERT_THAT(tin.verticesCount(),Eq(5));
//    ASSERT_THAT(tin.hardNeighbours(vert1).size(),Eq(1));

//    auto centralVertex=newVertices_2.begin();
//    centralVertex++;
//    auto centralVertexPointer=static_cast<VertexPointer>((*centralVertex));

//    tin.removeVertexOnHardLine(centralVertexPointer);

//    ASSERT_THAT(tin.verticesCount(),Eq(4));
    //can't work while the potential CGAL issue is not resolved
//    ASSERT_THAT(tin.hardNeighbours(vert1).size(),Eq(1));
//    ASSERT_THAT(tin.hardNeighbours(vert2).size(),Eq(1));
//    ASSERT_THAT(tin.hardNeighbours(vert3).size(),Eq(1));
//    ASSERT_THAT(tin.hardNeighbours(vert4).size(),Eq(1));
}

TEST_F(TinEditorTesting, removeVertexOnHardLineWithRemoveVertexOnHardline_2)
{
//    auto vert1=tin.addVertex(0,0);
//    auto vert2=tin.addVertex(0,5);
//    auto vert3=tin.addVertex(6,0);
//    auto vert4=tin.addVertex(6,5);
//    auto vert5=tin.addVertex(-1,2.5);
//    auto vert6=tin.addVertex(7,2.5);

//    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
//    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

//    std::list<VertexPointer> newVertices_3=tin.addHardLine(vert5,vert6);

//    auto centralVertex=newVertices_3.begin();
//    centralVertex++;
//    auto centralVertexPointer=static_cast<VertexPointer>((*centralVertex));

//    tin.removeHardLine(vert1,vert2);
//    tin.removeVertexOnHardLine(centralVertexPointer);


}


TEST_F(TinEditorTesting, addRemoveHardLines)
{

    ASSERT_TRUE(testCDTP_4());
}



TEST_F(TinEditorTesting, flipFaces)
{
    auto vert1=tin.addVertex(0,0);
    auto vert2=tin.addVertex(2,2);
    auto vert3=tin.addVertex(3,0);
    auto vert4=tin.addVertex(5,2);

    auto face1=tin.face(1,0.3);
    auto face2=tin.face(3,1.9);


    auto neighbours1=tin.neighboursVertices(vert1);
    ASSERT_THAT(face1->isVertexContained(vert2),Eq(true));
    ASSERT_THAT(face2->isVertexContained(vert4),Eq(true));

    tin.flipFaces(face1,face2);

    auto face3=tin.face(1,0.3);
    auto face4=tin.face(3,1.9);


    ASSERT_THAT(face3->isVertexContained(vert4),Eq(true));
    ASSERT_THAT(face4->isVertexContained(vert2),Eq(true));

}



#endif // TST_HDMESHESEDITORTEST_H
