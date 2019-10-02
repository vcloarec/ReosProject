#pragma once

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>


#include "../../Mesher/ReosMesh/reosmesheditor.h"
#include "../../Mesher/ReosTin/reostin.h"
#include "../../Mesher/ReosTin/test.h"


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


class TinEditingTesting:public Test{
public:
    ReosTin tin;


};

TEST_F(TinEditingTesting, tinCreation)
{
    ASSERT_THAT(tin.verticesCount(),Eq(0));
}

TEST_F(TinEditingTesting, addOneVertex)
{
    tin.addVertex(5,5);

    ASSERT_THAT(tin.verticesCount(),Eq(1));
}

TEST_F(TinEditingTesting, addVerticesToHaveAFace)
{
    tin.addVertex(5,5);
    tin.addVertex(10,5);
    tin.addVertex(5,10);

    ASSERT_THAT(tin.verticesCount(),Eq(3));
    ASSERT_THAT(tin.facesCount(),Eq(1));
}

TEST_F(TinEditingTesting, addVerticesToHaveTwoFace)
{
    tin.addVertex(5,5);
    tin.addVertex(6,5);
    tin.addVertex(7,10);
    tin.addVertex(10,10);

    ASSERT_THAT(tin.verticesCount(),Eq(4));
    ASSERT_THAT(tin.facesCount(),Eq(2));
}


TEST_F(TinEditingTesting, addDupplicatesVertices)
{
    tin.addVertex(5,5);
    tin.addVertex(5,5);
    tin.addVertex(7,10);
    tin.addVertex(10,10);

    ASSERT_THAT(tin.verticesCount(),Eq(3));
    ASSERT_THAT(tin.facesCount(),Eq(1));
}

TEST_F(TinEditingTesting, locateVertex)
{
    VertexPointer vert1=tin.addVertex(5,5);

    tin.addVertex(7,10);
    tin.addVertex(10,10);

    VertexPointer vert2=tin.vertex(4.999,5.001,0.01);

    ASSERT_TRUE(vert2==vert1);
}

TEST_F(TinEditingTesting, addHardLine)
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

TEST_F(TinEditingTesting, hasHardNeighbours)
{
    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    auto vert4=tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);
    std::list<VertexPointer> newVertices_2=tin.addHardLine(vert3,vert4);

    auto vert=newVertices_2.begin();
    vert++;
    std::list<VertexPointer> neighbours=tin.hardNeighbours(*vert);

    ASSERT_THAT(neighbours.size(),Eq(4));
}

TEST_F(TinEditingTesting, isSegmentOnHardline)
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


TEST_F(TinEditingTesting, removeFirstHardLineFromSegment)
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

TEST_F(TinEditingTesting, removeFirstHardLineFromExtremities)
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


TEST_F(TinEditingTesting, removeVertex)
{

    auto vert1=tin.addVertex(0,5);
    auto vert2=tin.addVertex(5,0);
    auto vert3=tin.addVertex(0,0);
    tin.addVertex(5,5);

    std::list<VertexPointer> newVertices_1=tin.addHardLine(vert1,vert2);

    ASSERT_THAT(tin.verticesCount(),Eq(4));

    tin.removeVertex(vert3);

    ASSERT_THAT(tin.verticesCount(),Eq(3));

}

TEST_F(TinEditingTesting, removeVertexOnHardLineWithRemoveVertex)
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

TEST_F(TinEditingTesting, removeVertexOnHardLineWithRemoveVertexOnHardline)
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

TEST_F(TinEditingTesting, removeVertexOnHardLineWithRemoveVertexOnHardline_2)
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


TEST_F(TinEditingTesting, addRemoveHardLines)
{

    ASSERT_TRUE(testCDTP_4());
}



TEST_F(TinEditingTesting, flipFaces)
{
    auto vert1=tin.addVertex(0,0);
    auto vert2=tin.addVertex(2,2);
    tin.addVertex(3,0);
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

TEST_F(TinEditingTesting, writeUGRIDEmptyFile)
{

    ASSERT_THAT(tin.verticesCount(),Eq(0));
    ASSERT_THAT(tin.facesCount(),Eq(0));

    ASSERT_THAT(tin.writeUGRIDFormat("netCDFEmptyTest"),Eq(NC_NOERR));
}


TEST_F(TinEditingTesting, readUGRIDEmptyFile)
{
    ASSERT_THAT(tin.readUGRIDFormat("netCDFEmptyTest"),Eq(NC_NOERR));

    ASSERT_THAT(tin.verticesCount(),Eq(0));
    ASSERT_THAT(tin.facesCount(),Eq(0));
}


TEST_F(TinEditingTesting, readUGRIDEmptyFile_andAddVertex)
{
    ASSERT_THAT(tin.readUGRIDFormat("netCDFEmptyTest"),Eq(NC_NOERR));

    ASSERT_THAT(tin.verticesCount(),Eq(0));
    ASSERT_THAT(tin.facesCount(),Eq(0));

    tin.addVertex(1,0.5);
    tin.addVertex(1,0.5); //dupplicated vertex

    ASSERT_THAT(tin.verticesCount(),Eq(1));
    ASSERT_THAT(tin.facesCount(),Eq(0));
}

TEST_F(TinEditingTesting, writeUGRIDOneVertexFile)
{
    tin.addVertex(1,0.5);

    ASSERT_THAT(tin.verticesCount(),Eq(1));
    ASSERT_THAT(tin.facesCount(),Eq(0));

    ASSERT_THAT(tin.writeUGRIDFormat("netCDFOneVertTest"),Eq(NC_NOERR));
}

TEST_F(TinEditingTesting, readUGRIDOneVertexFile_andAddVertex)
{
    ASSERT_THAT(tin.readUGRIDFormat("netCDFOneVertTest"),Eq(NC_NOERR));

    ASSERT_THAT(tin.verticesCount(),Eq(1));
    ASSERT_THAT(tin.facesCount(),Eq(0));

    tin.addVertex(0,0);
    tin.addVertex(1,0.5); //dupplicated vertex

    ASSERT_THAT(tin.verticesCount(),Eq(2));
    ASSERT_THAT(tin.facesCount(),Eq(0));
}

TEST_F(TinEditingTesting, writeUGRIDTwoVertexFile)
{
    tin.addVertex(1,0.5);
    tin.addVertex(0,0);

    ASSERT_THAT(tin.verticesCount(),Eq(2));
    ASSERT_THAT(tin.facesCount(),Eq(0));

    ASSERT_THAT(tin.writeUGRIDFormat("netCDFTwoVertTest"),Eq(NC_NOERR));
}

TEST_F(TinEditingTesting, readUGRIDTwoVertexFile_andAddVertex)
{
    ASSERT_THAT(tin.readUGRIDFormat("netCDFTwoVertTest"),Eq(NC_NOERR));

    ASSERT_THAT(tin.verticesCount(),Eq(2));
    ASSERT_THAT(tin.facesCount(),Eq(0));

    tin.addVertex(1,0);
    tin.addVertex(1,0.5);//dupplicated vertex

    ASSERT_THAT(tin.verticesCount(),Eq(3));
    ASSERT_THAT(tin.facesCount(),Eq(1));
}

TEST_F(TinEditingTesting, writeUGRIDSmallFile)
{
    tin.addVertex(0,0);
    tin.addVertex(2,2);
    tin.addVertex(3,0);
    tin.addVertex(5,2);

    tin.setCrs("EPSG:32620");

    ASSERT_THAT(tin.verticesCount(),Eq(4));
    ASSERT_THAT(tin.facesCount(),Eq(2));

    ASSERT_THAT(tin.writeUGRIDFormat("netCDFTest"),Eq(NC_NOERR));
}


TEST_F(TinEditingTesting, readUGRIDSmallFile)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("netCDFTest"),Eq(NC_NOERR));

    EXPECT_THAT(tinToRead.verticesCount(),Eq(4));
    EXPECT_THAT(tinToRead.facesCount(),Eq(2));

    EXPECT_THAT(tinToRead.crs(),Eq("EPSG:32620"));
}

TEST_F(TinEditingTesting, writeUGRIDBigFile)
{
    int size=100;
   for (int i=0;i<size;++i)
   {
       for (int j=0;j<size;++j)
       {
           if (j%2==0)
            tin.addVertex(i,j);
           else {
               tin.addVertex(i+0.5,j);
           }
       }
   }

   ASSERT_THAT(tin.verticesCount(),Eq(10000));
   ASSERT_THAT(tin.facesCount(),Eq(19700));

   ASSERT_THAT(tin.writeUGRIDFormat("TINfile.tin"),Eq(NC_NOERR));
}

TEST_F(TinEditingTesting, readUGRIDBigFile)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("TINfile.tin"),Eq(NC_NOERR));

    ASSERT_THAT(tinToRead.verticesCount(),Eq(10000));
    ASSERT_THAT(tinToRead.facesCount(),Eq(19700));

    auto reader=tinToRead.getReader();

    while (!reader->allVerticesReaden())
    {
        auto vert=reader->readVertexPointer();
        ASSERT_FALSE(vert==nullptr);
    }

    while (!reader->allFacesReaden())
    {
        int f[3];
        reader->readFace(f);
    }
}

TEST_F(TinEditingTesting, readUGRIDSmallFile_andAddVertex)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("netCDFTest"),Eq(NC_NOERR));

    tinToRead.addVertex(1,0.5);

    ASSERT_THAT(tinToRead.verticesCount(),Eq(5));
    ASSERT_THAT(tinToRead.facesCount(),Eq(4));
}

TEST_F(TinEditingTesting, readUGRIDSmallFile_andRemoveVertex)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("netCDFTest"),Eq(NC_NOERR));

    auto vert=tinToRead.vertex(2,2,0.01);

    tinToRead.removeVertex(vert);


    ASSERT_THAT(tinToRead.verticesCount(),Eq(3));
    ASSERT_THAT(tinToRead.facesCount(),Eq(1));
}

TEST_F(TinEditingTesting, readUGRIDSmallFile_andFlipFace)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("netCDFTest"),Eq(NC_NOERR));


    auto vert2=tinToRead.vertex(2,2,0.01);
    auto vert4=tinToRead.vertex(5,2,0.01);

    auto face1=tinToRead.face(1,0.3);
    auto face2=tinToRead.face(3,1.9);


    ASSERT_THAT(face1->isVertexContained(vert2),Eq(true));
    ASSERT_THAT(face2->isVertexContained(vert4),Eq(true));

    tinToRead.flipFaces(face1,face2);

    auto face3=tinToRead.face(1,0.3);
    auto face4=tinToRead.face(3,1.9);


    ASSERT_THAT(face3->isVertexContained(vert4),Eq(true));
    ASSERT_THAT(face4->isVertexContained(vert2),Eq(true));


    ASSERT_THAT(tinToRead.verticesCount(),Eq(4));
    ASSERT_THAT(tinToRead.facesCount(),Eq(2));
}


TEST_F(TinEditingTesting, writeUGRIDSmallFile_WithHardLine)
{
    auto vert1=tin.addVertex(0,0);
    auto vert2=tin.addVertex(2,2);
    auto vert3=tin.addVertex(3,0);
    auto vert4=tin.addVertex(5,2);

    vert1->setZValue(7);
    vert2->setZValue(7);
    vert3->setZValue(7);
    vert4->setZValue(7);

    tin.addHardLine(vert1,vert4);
    tin.addHardLine(vert2,vert3);

    ASSERT_THAT(tin.verticesCount(),Eq(5));
    ASSERT_THAT(tin.facesCount(),Eq(4));

    ASSERT_THAT(tin.writeUGRIDFormat("netCDFTestHL"),Eq(NC_NOERR));
}

TEST_F(TinEditingTesting, readUGRIDSmallFile_WithHardLine)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("netCDFTestHL"),Eq(NC_NOERR));

    ASSERT_THAT(tinToRead.verticesCount(),Eq(5));
    ASSERT_THAT(tinToRead.facesCount(),Eq(4));

    ASSERT_TRUE(tinToRead.isOnHardLine(tinToRead.vertex(0,0,0.01)));
    ASSERT_TRUE(tinToRead.isOnHardLine(tinToRead.vertex(2,2,0.01)));
    ASSERT_TRUE(tinToRead.isOnHardLine(tinToRead.vertex(3,0,0.01)));
    ASSERT_TRUE(tinToRead.isOnHardLine(tinToRead.vertex(5,2,0.01)));
    ASSERT_TRUE(tinToRead.isOnHardLine(tinToRead.vertex(2.5,1,0.01)));
}


TEST_F(TinEditingTesting, flipFacesPersistentAfterSaving)
{
    tin.addVertex(0,0);
    auto vert2=tin.addVertex(2,2);
    tin.addVertex(3,0);
    auto vert4=tin.addVertex(5,2);

    auto face1=tin.face(1,0.3);
    auto face2=tin.face(3,1.9);

    ASSERT_THAT(face1->isVertexContained(vert2),Eq(true));
    ASSERT_THAT(face2->isVertexContained(vert4),Eq(true));

    tin.flipFaces(face1,face2);

    auto face3=tin.face(1,0.3);
    auto face4=tin.face(3,1.9);


    ASSERT_THAT(face3->isVertexContained(vert4),Eq(true));
    ASSERT_THAT(face4->isVertexContained(vert2),Eq(true));

    //save tin
    tin.writeUGRIDFormat("persistentFlipFaces");

    ReosTin tinToRead;
    tinToRead.readUGRIDFormat("persistentFlipFaces");

    face3=tinToRead.face(1,0.3);
    face4=tinToRead.face(3,1.9);

    auto newVert2=tinToRead.vertex(2,2,0.01);
    auto newVert4=tinToRead.vertex(5,2,0.01);

    ASSERT_THAT(face3->isVertexContained(newVert4),Eq(true));
    ASSERT_THAT(face4->isVertexContained(newVert2),Eq(true));

}

TEST_F(TinEditingTesting, flipFacesPersistentAfterSavingWithHardline)
{
    auto vert1=tin.addVertex(0,0);
    auto vert2=tin.addVertex(2,2);
    tin.addVertex(3,0);
    auto vert4=tin.addVertex(5,2);

    auto face1=tin.face(1,0.3);
    auto face2=tin.face(3,1.9);

    tin.addHardLine(vert1,vert2);

    ASSERT_THAT(face1->isVertexContained(vert2),Eq(true));
    ASSERT_THAT(face2->isVertexContained(vert4),Eq(true));

    tin.flipFaces(face1,face2);

    auto face3=tin.face(1,0.3);
    auto face4=tin.face(3,1.9);


    ASSERT_THAT(face3->isVertexContained(vert4),Eq(true));
    ASSERT_THAT(face4->isVertexContained(vert2),Eq(true));

    //save tin
    tin.writeUGRIDFormat("persistentFlipFaces");

    ReosTin tinToRead;
    tinToRead.readUGRIDFormat("persistentFlipFaces");

    face3=tinToRead.face(1,0.3);
    face4=tinToRead.face(3,1.9);

    auto newVert2=tinToRead.vertex(2,2,0.01);
    auto newVert4=tinToRead.vertex(5,2,0.01);

    ASSERT_THAT(face3->isVertexContained(newVert4),Eq(true));
    ASSERT_THAT(face4->isVertexContained(newVert2),Eq(true));

}

TEST_F(TinEditingTesting, writeUGRIDSmallFileWithZSpecifier)
{
    auto vert1=tin.addVertex(2,2);
    auto vert2=tin.addVertex(2,0);
    auto vert3=tin.addVertex(4,0);
    auto vert4=tin.addVertex(6,0);
    auto vert5=tin.addVertex(8,0);
    auto vert6=tin.addVertex(8,2);


    tin.setCrs("EPSG:32620");

    ReosVertexZSpecifierOtherVertexAndGapFactory gapFactory;
    ReosVertexZSpecifierOtherVertexAndSlopeFactory slopeFactory;
    ReosVertexZSpecifierInterpolationFactory interpolationFactory;

    vert1->setZValue(5);
    gapFactory.setOtherVertex(vert1);
    gapFactory.setGap(-2);
    vert2->setZSpecifier(gapFactory);

    vert6->setZValue(6);
    slopeFactory.setSlope(-1);
    slopeFactory.setOtherVertex(vert6);
    vert5->setZSpecifier(slopeFactory);

    interpolationFactory.setExtremitiesVertices(vert2,vert5);
    vert3->setZSpecifier(interpolationFactory);
    vert4->setZSpecifier(interpolationFactory);

    ASSERT_THAT(tin.writeUGRIDFormat("netCDFTest"),Eq(NC_NOERR));
}

TEST_F(TinEditingTesting, readUGRIDSmallFileWithZSpecifier)
{
    ReosTin tinToRead;

    ASSERT_THAT(tinToRead.readUGRIDFormat("netCDFTest"),Eq(NC_NOERR));

    EXPECT_THAT(tinToRead.verticesCount(),Eq(6));

    EXPECT_THAT(tinToRead.crs(),Eq("EPSG:32620"));

    auto vert1=tinToRead.vertex(2,2);
    auto vert2=tinToRead.vertex(2,0);
    auto vert3=tinToRead.vertex(4,0);
    auto vert4=tinToRead.vertex(6,0);
    auto vert5=tinToRead.vertex(8,0);
    auto vert6=tinToRead.vertex(8,2);

    EXPECT_THAT(vert1->z(),Eq(5));
    EXPECT_THAT(vert2->z(),Eq(3));
    EXPECT_TRUE(equality(vert3->z(),3.333,0.01));
    EXPECT_TRUE(equality(vert4->z(),3.666,0.01));
    EXPECT_THAT(vert5->z(),Eq(4));
    EXPECT_THAT(vert6->z(),Eq(6));

    vert1->setZValue(7);
    vert6->setZValue(8);

    EXPECT_THAT(vert2->z(),Eq(5));
    EXPECT_TRUE(equality(vert3->z(),5.333,0.01));
    EXPECT_TRUE(equality(vert4->z(),5.666,0.01));
    EXPECT_THAT(vert5->z(),Eq(6));


}
