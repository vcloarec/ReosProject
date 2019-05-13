#ifndef TST_006_HDTINEDITORGRAPHICTEST_H
#define TST_006_HDTINEDITORGRAPHICTEST_H


#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include <qgsmeshdataprovider.h>
#include <qgsproviderregistry.h>

#include "../../Mesher/tinEditorUi/hdtineditorgraphic.h"
#include "../../Mesher/provider/meshdataprovider.h"

using namespace testing;

class UIMeshEditorTesting : public Test{
public:
    HdMap * map;
    QgsMapCanvas *mapCanvas;
    HdTinEditorUi *uiEditor;
    QgsMeshLayer *meshLayer;
    TINProvider *provider;
    HdManagerSIG *gismanager;


    bool testUiEditorActionEnable()
    {
        QList<QAction*> actionsList=uiEditor->getActions();
        bool active=true;
        for (auto a:actionsList)
            active &= a->isEnabled();

        active|=(actionsList.isEmpty());

        return active;
    }

    // Test interface
protected:
    void SetUp() override
    {

        QgsProviderRegistry::instance()->registerProvider(new HdTinEditorProviderMetaData());
        map=new HdMap(nullptr);
        mapCanvas=map->getMapCanvas();
        gismanager=new HdManagerSIG(map);
        uiEditor=new HdTinEditorUi(gismanager);
        meshLayer=new QgsMeshLayer("-","Mesh editable","TIN");
        provider=static_cast<TINProvider*>(meshLayer->dataProvider());
    }

    void TearDown() override
    {
        delete map;
        delete uiEditor;
        delete  meshLayer;
    }

};

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
}



#endif // TST_006_HDTINEDITORGRAPHICTEST_H
