#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include <qgsmeshdataprovider.h>
#include <qgsproviderregistry.h>

#include "../../Mesher/tinEditorUi/reostineditorgraphic.h"
#include "../../Mesher/provider/meshdataprovider.h"

using namespace testing;

class UIMeshEditorTesting : public Test{
public:
    ReosMap * map;
    QgsMapCanvas *mapCanvas;
    ReosTinEditorUi *uiEditor;
    QgsMeshLayer *meshLayer;
    TINProvider *provider;
    ReosGisManager *gismanager;


    bool testUiEditorActionEnable()
    {
        QList<QAction*> actionsList=uiEditor->getActions();
        bool active=true;
        for (auto a:actionsList)
            active &= a->isEnabled();

        active|=(actionsList.isEmpty());

        return active;
    }

    void removeVertex(VertexPointer vert)
    {
        uiEditor->removeVertex(vert);
    }

    void removeSegment(VertexPointer v1,VertexPointer v2)
    {
        uiEditor->removeHardLine(v1,v2);
    }

    int editorVertexCount()
    {
        return uiEditor->mTIN->verticesCount();

    }

    int editorFaceCount()
    {
        return uiEditor->mTIN->facesCount();
    }

    QgsDataProvider *getProvider()
    {
        return uiEditor->mMeshLayer->dataProvider();
    }

    bool reloadMeshLayer()
    {
        try
        {
         uiEditor->mMeshLayer->reload();
         return true;
        }
        catch(...)
        {
            return false;
        }
    }

    void TEST_NEW_TIN(QString fileName)
    {

        QFileInfo fileInfo(fileName);

        if (fileInfo.exists())
        {
            QFile file(fileName);
            file.remove();
        }

        QgsCoordinateReferenceSystem crs("EPSG:32620");
        ASSERT_TRUE(crs.isValid());

        uiEditor->newTin(fileName,"tin",crs);

        ASSERT_THAT(uiEditor->domain()->verticesCount(),0);
    }

    // Test interface
protected:
    void SetUp() override
    {
        QgsProviderRegistry::instance()->registerProvider(new HdTinEditorProviderMetaData());
        map=new ReosMap(nullptr);
        mapCanvas=map->getMapCanvas();
        gismanager=new ReosGisManager(map);
        uiEditor=new ReosTinEditorUi(gismanager);
        meshLayer=new QgsMeshLayer("-","Mesh editable","TIN");
        provider=static_cast<TINProvider*>(meshLayer->dataProvider());
    }

    void TearDown() override
    {
        delete map;
        delete uiEditor;
        //delete  meshLayer;
    }

};


TEST_F(UIMeshEditorTesting, newTIN)
{
    TEST_NEW_TIN("TinUi.tin");
}


TEST_F(UIMeshEditorTesting, saveTIN)
{

    TEST_NEW_TIN("TinUi.tin");


    uiEditor->newVertex(QPointF(2,10));
    uiEditor->newVertex(QPointF(4,10));
    uiEditor->newVertex(QPointF(4,0));
    uiEditor->newVertex(QPointF(2,0));


    auto vert1=uiEditor->mapVertex(QPointF(2,10));
    auto vert2=uiEditor->mapVertex(QPointF(4,10));
    auto vert4=uiEditor->mapVertex(QPointF(4,0));
    auto vert5=uiEditor->mapVertex(QPointF(2,0));

    uiEditor->newSegment(vert1,vert2);
    uiEditor->newSegment(vert2,vert4);
    uiEditor->newSegment(vert4,vert5);
    uiEditor->newSegment(vert5,vert1);

    ASSERT_TRUE(uiEditor->saveTin());
}

TEST_F(UIMeshEditorTesting, openTIN)
{
    QString fileName="TinUi.tin";
    QFileInfo fileInfo(fileName);

    ASSERT_TRUE(fileInfo.exists());

    ASSERT_TRUE(uiEditor->openTinWithFileName(fileName));

    ASSERT_THAT(editorVertexCount(),Eq(4));
    ASSERT_THAT(editorFaceCount(),Eq(2));

    ASSERT_TRUE(reloadMeshLayer());

    ASSERT_THAT(getProvider()->crs(),Eq(QgsCoordinateReferenceSystem("EPSG:32620")));

}

TEST_F(UIMeshEditorTesting,actionsAreDisable)
{
    uiEditor->setMeshLayer(nullptr);
    ASSERT_THAT(testUiEditorActionEnable(),Eq(false));
}

TEST_F(UIMeshEditorTesting,actionsAreEnable)
{
    uiEditor->setMeshLayer(meshLayer);
    ASSERT_THAT(testUiEditorActionEnable(),Eq(true));
}

TEST_F(UIMeshEditorTesting,actionsAreDisableAgain)
{
    uiEditor->setMeshLayer(meshLayer);
    uiEditor->setMeshLayer(nullptr);

    ASSERT_THAT(testUiEditorActionEnable(),Eq(false));
}

TEST_F(UIMeshEditorTesting,addVertex)
{
    uiEditor->setMeshLayer(meshLayer);

    uiEditor->newVertex(QPointF(5,5));

    ASSERT_THAT(provider->vertexCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(1));

//    uiEditor->undo();
//    ASSERT_THAT(provider->vertexCount(),Eq(0));
//    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(0));

    uiEditor->redo();
    ASSERT_THAT(provider->vertexCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(1));

}

TEST_F(UIMeshEditorTesting,addVertexToVoidEditor)
{
    uiEditor->setMeshLayer(nullptr);

    uiEditor->newVertex(QPointF(5,5));

    ASSERT_THAT(provider->vertexCount(),Eq(0));
}

TEST_F(UIMeshEditorTesting,addDuplicateVertices)
{
    uiEditor->setMeshLayer(meshLayer);

    uiEditor->newVertex(QPointF(5,5));
    uiEditor->newVertex(QPointF(5,5));
    uiEditor->newVertex(QPointF(4.999,5.001));
    uiEditor->newVertex(QPointF(4.9,5.1));

    ASSERT_THAT(provider->vertexCount(),Eq(2));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(2));

/*    uiEditor->undo();
    ASSERT_THAT(provider->vertexCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(1))*/;

    uiEditor->redo();
    ASSERT_THAT(provider->vertexCount(),Eq(2));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(2));
}


TEST_F(UIMeshEditorTesting, addSegments)
{
    uiEditor->setMeshLayer(meshLayer);

    uiEditor->newVertex(QPointF(0,5));
    uiEditor->newVertex(QPointF(5,0));
    uiEditor->newVertex(QPointF(0,0));

    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(3));

    //add segement with not existing second vertex
    auto firstVertex=uiEditor->mapVertex(QPointF(0,0));
    uiEditor->newSegment(firstVertex,QPointF(5,5));

    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(4));

//    uiEditor->undo();
//    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(0));
//    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(3));

    uiEditor->redo();
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(4));


    //add segement with not existing second vertex
    auto secondVertex=uiEditor->mapVertex(QPointF(5,0));
    uiEditor->newSegment(firstVertex,secondVertex);

    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(2));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(4));

//    uiEditor->undo();
//    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(1));
//    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(4));

    uiEditor->redo();
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(2));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(4));
}



TEST_F(UIMeshEditorTesting, addIntersectedSegments)
{

    uiEditor->setMeshLayer(meshLayer);

    uiEditor->newVertex(QPointF(2,10));
    uiEditor->newVertex(QPointF(4,10));
    uiEditor->newVertex(QPointF(6,5));
    uiEditor->newVertex(QPointF(4,0));
    uiEditor->newVertex(QPointF(2,0));
    uiEditor->newVertex(QPointF(0,5));

    auto vert1=uiEditor->mapVertex(QPointF(2,10));
    auto vert2=uiEditor->mapVertex(QPointF(4,10));
    auto vert3=uiEditor->mapVertex(QPointF(6,5));
    auto vert4=uiEditor->mapVertex(QPointF(4,0));
    auto vert5=uiEditor->mapVertex(QPointF(2,0));
    auto vert6=uiEditor->mapVertex(QPointF(0,5));

    //-------(0)----------(2)------------(4)--------(6)-------------
    // (10)                1              2
    // (9)
    // (8)
    // (7)
    // (6)
    // (5)    6**************************************3
    // (4)
    // (3)
    // (2)
    // (1)
    // (0)                 5              4

    uiEditor->newSegment(vert6,vert3);
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(1));


    //-------(0)----------(2)-------(4)-------------(6)-------------
    // (10)                1         2
    // (9)                  *
    // (8)                   *
    // (7)                    *
    // (6)                     *
    // (5)    6*****************7********************3
    // (4)                       *
    // (3)                        *
    // (2)                         *
    // (1)                          *
    // (0)                 5         4

    uiEditor->newSegment(vert1,vert4);
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(4));
    ASSERT_THAT(vert1->segmentsCount(),Eq(1));
    ASSERT_THAT(vert2->segmentsCount(),Eq(0));
    ASSERT_THAT(vert3->segmentsCount(),Eq(1));
    ASSERT_THAT(vert4->segmentsCount(),Eq(1));
    ASSERT_THAT(vert5->segmentsCount(),Eq(0));
    ASSERT_THAT(vert6->segmentsCount(),Eq(1));

    //undo not available still potential CGAL issue nor resolved.
//    uiEditor->undo();
//    ASSERT_THAT(vert1->segmentsCount(),Eq(0));
//    ASSERT_THAT(vert2->segmentsCount(),Eq(0));
//    ASSERT_THAT(vert3->segmentsCount(),Eq(1));
//    ASSERT_THAT(vert4->segmentsCount(),Eq(0));
//    ASSERT_THAT(vert5->segmentsCount(),Eq(0));
//    ASSERT_THAT(vert6->segmentsCount(),Eq(1));
//    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(1));



//    //-------(0)----------(2)-------(4)-------------(6)-------------
//    // (10)                1         2
//    // (9)                  *       *
//    // (8)                   *     *
//    // (7)                    *   *
//    // (6)                     * *
//    // (5)    6*****************7********************3
//    // (4)                     * *
//    // (3)                    *   *
//    // (2)                   *     *
//    // (1)                  *       *
//    // (0)                 5         4

    uiEditor->newSegment(vert5,vert2);
    ASSERT_THAT(vert1->segmentsCount(),Eq(1));
    ASSERT_THAT(vert2->segmentsCount(),Eq(1));
    ASSERT_THAT(vert3->segmentsCount(),Eq(1));
    ASSERT_THAT(vert4->segmentsCount(),Eq(1));
    ASSERT_THAT(vert5->segmentsCount(),Eq(1));
    ASSERT_THAT(vert6->segmentsCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(6));

//    //-------(0)----------(2)-------(4)--------(6)-------------
//    // (10)                1         2
//    // (9)                 **       *
//    // (8)                 * *     *
//    // (7)                 *  *   *
//    // (6)                 *   * *
//    // (5)    6************8****7********************3
//    // (4)                 *   * *
//    // (3)                 *  *   *
//    // (2)                 * *     *
//    // (1)                 **       *
//    // (0)                 5         4

    uiEditor->newSegment(vert5,vert1);
    uiEditor->newSegment(vert6,vert3);
    ASSERT_THAT(vert1->segmentsCount(),Eq(2));
    ASSERT_THAT(vert2->segmentsCount(),Eq(1));
    ASSERT_THAT(vert3->segmentsCount(),Eq(1));
    ASSERT_THAT(vert4->segmentsCount(),Eq(1));
    ASSERT_THAT(vert5->segmentsCount(),Eq(2));
    ASSERT_THAT(vert6->segmentsCount(),Eq(1));
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(9));


//    //-------(0)----------(2)-------(4)-------------(6)-------------
//    // (10)                1         2
//    // (9)                 *   ******
//    // (8)                *9*10    *
//    // (7)           ***** *  *   *
//    // (6)      *****      *   * *
//    // (5)    6************8****7********************3
//    // (4)                 *   * *
//    // (3)                 *  *   *
//    // (2)                 * *     *
//    // (1)                 **       *
//    // (0)                 5         4

    uiEditor->newSegment(vert5,vert1);
    uiEditor->newSegment(vert3,vert6);
    uiEditor->newSegment(vert2,vert6);
    ASSERT_THAT(vert1->segmentsCount(),Eq(2));
    ASSERT_THAT(vert2->segmentsCount(),Eq(2));
    ASSERT_THAT(vert3->segmentsCount(),Eq(1));
    ASSERT_THAT(vert4->segmentsCount(),Eq(1));
    ASSERT_THAT(vert5->segmentsCount(),Eq(2));
    ASSERT_THAT(vert6->segmentsCount(),Eq(2));
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(14));

//    //-------(0)----(1)---(2)-------(4)---(5)-------(6)-------------
//    // (10)                1         2
//    // (9)                 *   ******
//    // (8)                *9*10    *
//    // (7)           ***** *  *   *
//    // (6)      *****      *   * *
//    // (5)    6*****11*****8****7**********12********3
//    // (4)                 *   * *
//    // (3)                 *  *   *
//    // (2)                 * *     *
//    // (1)                 **       *
//    // (0)                 5         4



    uiEditor->newVertex(QPointF(1,5));
    uiEditor->newVertex(QPointF(5,5));

    auto vert11=uiEditor->mapVertex(QPointF(1,5));
    auto vert12=uiEditor->mapVertex(QPointF(5,5));

    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(14)); //the segments 6-8 and 7-3 are not splitted by the new vertex

    uiEditor->newSegment(vert11,vert12);

    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(16));//segment 11-12 intersects and is on the segments 11-8, 8-7, 7-12

}

TEST_F(UIMeshEditorTesting, removeVertex)
{
    uiEditor->setMeshLayer(meshLayer);

    uiEditor->newVertex(QPointF(2,10));
    uiEditor->newVertex(QPointF(4,10));
    uiEditor->newVertex(QPointF(6,5));
    uiEditor->newVertex(QPointF(4,0));
    uiEditor->newVertex(QPointF(2,0));
    uiEditor->newVertex(QPointF(0,5));


    auto vert1=uiEditor->mapVertex(QPointF(2,10));
    auto vert2=uiEditor->mapVertex(QPointF(4,10));
    auto vert3=uiEditor->mapVertex(QPointF(6,5));
    auto vert4=uiEditor->mapVertex(QPointF(4,0));
    auto vert5=uiEditor->mapVertex(QPointF(2,0));
    auto vert6=uiEditor->mapVertex(QPointF(0,5));
    uiEditor->newSegment(vert6,vert3);
    uiEditor->newSegment(vert1,vert4);
    uiEditor->newSegment(vert5,vert2);

    uiEditor->newVertex(QPointF(0,9));
    auto vert8=uiEditor->mapVertex(QPointF(0,9));


    //-------(0)----------(2)-------(4)-------------(6)-------------
    // (10)                1         2
    // (9)    8             *       *
    // (8)                   *     *
    // (7)                    *   *
    // (6)                     * *
    // (5)    6*****************7********************3
    // (4)                     * *
    // (3)                    *   *
    // (2)                   *     *
    // (1)                  *       *
    // (0)                 5         4

    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(6));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(8));

    removeVertex(vert8->realWorldVertex());

    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(7));

    removeVertex(vert6->realWorldVertex());

    //vert6 is on a hardline, so can't be removed before the hardline was removed
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(7));
    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(6));

}


TEST_F(UIMeshEditorTesting, removeSegment)
{
    uiEditor->setMeshLayer(meshLayer);

    uiEditor->newVertex(QPointF(2,10));
    uiEditor->newVertex(QPointF(4,10));
    uiEditor->newVertex(QPointF(4,0));
    uiEditor->newVertex(QPointF(2,0));


    auto vert1=uiEditor->mapVertex(QPointF(2,10));
    auto vert2=uiEditor->mapVertex(QPointF(4,10));
    auto vert4=uiEditor->mapVertex(QPointF(4,0));
    auto vert5=uiEditor->mapVertex(QPointF(2,0));

    uiEditor->newSegment(vert1,vert2);
    uiEditor->newSegment(vert2,vert4);
    uiEditor->newSegment(vert4,vert5);
    uiEditor->newSegment(vert5,vert1);

    //-------(0)----------(2)-------(4)-------------(6)-------------
    // (10)                1*********2
    // (9)                 *         *
    // (8)                 *         *
    // (7)                 *         *
    // (6)                 *         *
    // (5)                 *         *
    // (4)                 *         *
    // (3)                 *         *
    // (2)                 *         *
    // (1)                 *         *
    // (0)                 5*********4

    ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(4));
    ASSERT_THAT(uiEditor->domain()->verticesCount(),Eq(4));

    int count=4;
    while(count>0)
    {
        ReosMeshItemSegment *seg=uiEditor->domain()->segment(0);

        removeSegment(seg->vertex1()->realWorldVertex(),seg->vertex2()->realWorldVertex());
        count--;
        ASSERT_THAT(uiEditor->domain()->segmentCount(),Eq(count));
    }
}
