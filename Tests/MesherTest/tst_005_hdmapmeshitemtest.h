#ifndef TST_005_HDMAPMESHITEMTEST_H
#define TST_005_HDMAPMESHITEMTEST_H


#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include <qgsapplication.h>
#include <qgsmapcanvas.h>

#include "../../Mesher/hdmapmeshitem.h"
#include "../../Mesher/meshdataprovider.h"


using namespace testing;

class MeshItemTesting : public Test{
public:

    std::vector<Vertex> vertices;
    std::vector<Segment> segments;
    std::vector<Face> meshFaces;

    QgsMapCanvas *mapCanvas;
    TINEditor tinEditor=TINEditor(vertices,segments,meshFaces);
    //TINProvider provider;

    void populateEditorWithVertex()
    {
        tinEditor.addVertex(Vertex(5,15));
        tinEditor.addVertex(Vertex(10,15));
        tinEditor.addVertex(Vertex(5,10));
        tinEditor.addVertex(Vertex(10,10));
        tinEditor.addVertex(Vertex(10,5));
    }

    void populateEditorWithVertexAndSegment()
    {
        tinEditor.addVertex(Vertex(5,15));
        tinEditor.addVertex(Vertex(10,15));
        tinEditor.addVertex(Vertex(5,10));
        tinEditor.addVertex(Vertex(10,10));
        tinEditor.addVertex(Vertex(10,5));

        tinEditor.addSegment(0,4);
    }

    // Test interface
protected:
    void SetUp() override
    {
        mapCanvas=new QgsMapCanvas();

    }

    void TearDown() override
    {
        delete mapCanvas;
    }
};


TEST_F(MeshItemTesting, creation)
{
    HdMapMeshEditorItemDomain domain(mapCanvas);

    ASSERT_THAT(domain.verticesCount(),Eq(0));
    ASSERT_THAT(domain.verticesCount(),Eq(tinEditor.verticesCount()));
}

TEST_F(MeshItemTesting, associateToDomain)
{
    populateEditorWithVertexAndSegment();
    HdMapMeshEditorItemDomain domain(mapCanvas);

    domain.setTINEditor(&tinEditor);

    ASSERT_THAT(domain.verticesCount(),Eq(5));
    ASSERT_THAT(domain.segmentCount(),Eq(1));

}

TEST_F(MeshItemTesting, associateNullEditorToDomain)
{
    populateEditorWithVertexAndSegment();
    HdMapMeshEditorItemDomain domain(mapCanvas);

    domain.setTINEditor(&tinEditor);
    domain.setTINEditor(nullptr);

    ASSERT_THAT(domain.verticesCount(),Eq(0));
    ASSERT_THAT(domain.segmentCount(),Eq(0));

}

TEST_F(MeshItemTesting, itemCount)
{
    populateEditorWithVertexAndSegment();
    HdMapMeshEditorItemDomain domain(mapCanvas);

    domain.setTINEditor(&tinEditor);

    ASSERT_THAT(domain.verticesCount(),Eq(tinEditor.verticesCount()));
    ASSERT_THAT(domain.segmentCount(),Eq(tinEditor.segmentsCount()));
}

TEST_F(MeshItemTesting, addVertex)
{
    populateEditorWithVertex();
    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
    editorDomain.setTINEditor(&tinEditor);

    QPointF pt(5,5);
    editorDomain.addVertex(pt);

    ASSERT_THAT(editorDomain.verticesCount(),Eq(6));
    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
}

TEST_F(MeshItemTesting, addVertexOrSegmentToVoidDomain)
{
    populateEditorWithVertex();
    HdMapMeshEditorItemDomain editorDomain(mapCanvas);

    QPointF pt1(5,5);
    QPointF pt2(10,5);
    int n1=editorDomain.addVertex(pt1);
    int n2=editorDomain.addSegmentHardLine(n1,pt2);

    ASSERT_THAT(n1,Eq(-1));
    ASSERT_THAT(n2,Eq(-1));
    ASSERT_THAT(editorDomain.verticesCount(),Eq(0));
}

TEST_F(MeshItemTesting, addDuplicateVertex)
{
    populateEditorWithVertex();
    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
    editorDomain.setTINEditor(&tinEditor);

    tinEditor.setTolerance(0.01);

    QPointF pt(4.999,10.001);
    editorDomain.addVertex(pt);

    ASSERT_THAT(editorDomain.verticesCount(),Eq(5));
    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
}

TEST_F(MeshItemTesting, addHardLine)
{
    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
    editorDomain.setTINEditor(&tinEditor);

    int n=editorDomain.addSegmentHardLine(-1,QPointF(0,0));
    editorDomain.addSegmentHardLine(n,QPointF(5,0));

    ASSERT_THAT(editorDomain.verticesCount(),Eq(2));
    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
    ASSERT_THAT(editorDomain.segmentCount(),Eq(1));

}

TEST_F(MeshItemTesting, addDupplicateHardLine)
{
    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
    editorDomain.setTINEditor(&tinEditor);

    int n=editorDomain.addSegmentHardLine(-1,QPointF(0,0));
    editorDomain.addSegmentHardLine(n,QPointF(5,0));

    int n2=editorDomain.addSegmentHardLine(-1,QPointF(5,0));
    editorDomain.addSegmentHardLine(n2,QPointF(0,0));

    ASSERT_THAT(editorDomain.verticesCount(),Eq(2));
    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
    ASSERT_THAT(editorDomain.segmentCount(),Eq(1));

}



#endif // TST_005_HDMAPMESHITEMTEST_H
