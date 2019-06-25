#ifndef TST_005_HDMAPMESHITEMTEST_H
#define TST_005_HDMAPMESHITEMTEST_H


#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include <qgsapplication.h>
#include <qgsmapcanvas.h>

#include "../../Mesher/tinEditorUi/reostineditorgraphic.h"
#include "../../Mesher/tinEditorUi/reosmapmeshitem.h"
#include "../../Mesher/provider/meshdataprovider.h"


using namespace testing;

class MeshItemTesting : public Test{
public:

    ReosMap * map;
    QgsMapCanvas *mapCanvas;
    ReosTinEditorUi *uiEditor;
    QgsMeshLayer *meshLayer;
    TINProvider *provider;
    HdManagerSIG *gismanager;

    std::vector<Segment> segments;

    //TINProvider provider;

    void populateEditorWithVertex()
    {
        uiEditor->newVertex(QPointF(5,15));
        uiEditor->newVertex(QPointF(10,15));
        uiEditor->newVertex(QPointF(5,10));
        uiEditor->newVertex(QPointF(10,10));
        uiEditor->newVertex(QPointF(10,5));
    }

    void populateEditorWithVertexAndSegment()
    {
        uiEditor->newVertex(QPointF(5,15));
        uiEditor->newVertex(QPointF(10,15));
        uiEditor->newVertex(QPointF(5,10));
        uiEditor->newVertex(QPointF(10,10));
        uiEditor->newVertex(QPointF(10,5));

        //tinEditor.addSegment(0,4);
    }

    // Test interface
protected:
    void SetUp() override
    {
        QgsProviderRegistry::instance()->registerProvider(new HdTinEditorProviderMetaData());
        map=new ReosMap(nullptr);
        mapCanvas=map->getMapCanvas();
        gismanager=new HdManagerSIG(map);
        uiEditor=new ReosTinEditorUi(gismanager);
        meshLayer=new QgsMeshLayer("-","Mesh editable","TIN");
        provider=static_cast<TINProvider*>(meshLayer->dataProvider());
    }

    void TearDown() override
    {
        delete mapCanvas;
    }
};


TEST_F(MeshItemTesting, domainContruction)
{
    ReosMapMeshEditorItemDomain domain(uiEditor,mapCanvas);

    ASSERT_THAT(domain.verticesCount(),Eq(0));
    ASSERT_THAT(domain.verticesCount(),Eq(provider->vertexCount()));
}

TEST_F(MeshItemTesting, associateMeshToEditor)
{
    uiEditor->setMeshLayer(meshLayer);
    populateEditorWithVertex();

    ReosMapMeshEditorItemDomain* domain=uiEditor->domain();;


    ASSERT_THAT(domain->verticesCount(),Eq(5));
}

TEST_F(MeshItemTesting, associateNullMeshToEditor)
{
    uiEditor->setMeshLayer(meshLayer);
    populateEditorWithVertex();

    uiEditor->setMeshLayer(nullptr);


    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(0));
}

TEST_F(MeshItemTesting, associateNewMeshAfterPopulateToEditor)
{
    uiEditor->setMeshLayer(meshLayer);
    populateEditorWithVertex();

    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(5));

    uiEditor->setMeshLayer(nullptr);

    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(0));

    uiEditor->setMeshLayer(meshLayer);

    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(5));
}


//TEST_F(MeshItemTesting, itemCount)
//{
//    populateEditorWithVertexAndSegment();
//    HdMapMeshEditorItemDomain domain(mapCanvas);

//    domain.setTINEditor(&tinEditor);

//    ASSERT_THAT(domain.verticesCount(),Eq(tinEditor.verticesCount()));
//    ASSERT_THAT(domain.segmentCount(),Eq(tinEditor.segmentsCount()));
//}

//TEST_F(MeshItemTesting, addVertex)
//{
//    populateEditorWithVertex();
//    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
//    editorDomain.setTINEditor(&tinEditor);

//    tinEditor.addVertex(5,5);

//    ASSERT_THAT(editorDomain.verticesCount(),Eq(6));
//    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
//}

//TEST_F(MeshItemTesting, addVertexOrSegmentToVoidDomain)
//{
//    populateEditorWithVertex();
//    HdMapMeshEditorItemDomain editorDomain(mapCanvas);

//    VertexPointer v1=tinEditor.addVertex(5,5);
//    VertexPointer v2=tinEditor.addVertex(10,5);

//    ASSERT_THAT(v1,Eq(nullptr));
//    ASSERT_THAT(v2,Eq(nullptr));
//    ASSERT_THAT(editorDomain.verticesCount(),Eq(0));
//}

//TEST_F(MeshItemTesting, addDuplicateVertex)
//{
//    populateEditorWithVertex();
//    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
//    editorDomain.setTINEditor(&tinEditor);

//    tinEditor.setTolerance(0.01);
//    tinEditor.addVertex(4.999,10.001);

//    ASSERT_THAT(editorDomain.verticesCount(),Eq(5));
//    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
//}

//TEST_F(MeshItemTesting, addHardLine)
//{
//    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
//    editorDomain.setTINEditor(&tinEditor);



//    ASSERT_THAT(editorDomain.verticesCount(),Eq(2));
//    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
//    ASSERT_THAT(editorDomain.segmentCount(),Eq(1));

//}

//TEST_F(MeshItemTesting, addDupplicateHardLine)
//{
//    HdMapMeshEditorItemDomain editorDomain(mapCanvas);
//    editorDomain.setTINEditor(&tinEditor);


//    ASSERT_THAT(editorDomain.verticesCount(),Eq(2));
//    ASSERT_THAT(editorDomain.verticesCount(),Eq(tinEditor.verticesCount()));
//    ASSERT_THAT(editorDomain.segmentCount(),Eq(1));

//}



#endif // TST_005_HDMAPMESHITEMTEST_H
