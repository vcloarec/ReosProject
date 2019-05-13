#ifndef TST_HDEDITABLEMESHLAYERTEST_H
#define TST_HDEDITABLEMESHLAYERTEST_H

#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include <qgsproviderregistry.h>

#include "../../Mesher/provider/meshdataprovider.h"


using namespace testing;

class EditableMeshLayerTesting: public Test{
public:
    QgsProviderRegistry *providerRegistery=QgsProviderRegistry::instance();
    QgsMeshLayer *layer=nullptr;

    void initializeLayerWithOneVertex()
    {
        layer=new QgsMeshLayer("path to mesh","nom","TIN");
        TINProvider *provider=static_cast<TINProvider*>(layer->dataProvider());
        provider->editor()->addVertex(Vertex(0,0));
    }

    void initializeLayerWithSeveralVertices()
    {
        layer=new HdEditableMeshLayer();
        TINProvider *provider=static_cast<TINProvider*>(layer->dataProvider());

        provider->editor()->addVertex(Vertex(0,0));
        provider->editor()->addVertex(Vertex(0,5));
        provider->editor()->addVertex(Vertex(5,5));
        provider->editor()->addVertex(Vertex(5,0));
        provider->editor()->addVertex(Vertex(10,5));
    }

    bool generateMesh()
    {
        if (!layer)
            return false;

        TINProvider *provider=static_cast<TINProvider*>(layer->dataProvider());

        return provider->editor()->generateMesh();

    }

    void registerProvider()
    {
        providerRegistery->registerProvider(new HdTinEditorProviderMetaData());
    }



    // Test interface
protected:
    void SetUp() override
    {
        providerRegistery->registerProvider(new HdTinEditorProviderMetaData());
    }
};

TEST_F(EditableMeshLayerTesting,registerProvider)
{
    registerProvider();
    ASSERT_TRUE(providerRegistery->providerList().contains("TIN"));
}

TEST_F(EditableMeshLayerTesting,createProvider)
{
    registerProvider();
    QgsDataProvider *provider=providerRegistery->createProvider("TIN","");
    ASSERT_TRUE(provider->isValid());
}

TEST_F(EditableMeshLayerTesting, layerHasProvider)
{
    registerProvider();
    HdEditableMeshLayer *layer=new HdEditableMeshLayer();

    ASSERT_TRUE(layer->dataProvider()!=nullptr);
}

TEST_F(EditableMeshLayerTesting, layerIsValid)
{
    registerProvider();
    HdEditableMeshLayer *layer=new HdEditableMeshLayer();

    ASSERT_TRUE(layer->isValid());
}

TEST_F(EditableMeshLayerTesting, generateFailWithNotEnoughtData)
{
    registerProvider();
    initializeLayerWithOneVertex();

    ASSERT_FALSE(generateMesh());

}

TEST_F(EditableMeshLayerTesting, generateSucces)
{
    registerProvider();
    initializeLayerWithSeveralVertices();


    ASSERT_TRUE(generateMesh());
}


TEST_F(EditableMeshLayerTesting, verticesAfterGenerate)
{
    registerProvider();
    initializeLayerWithSeveralVertices();

    generateMesh();

    ASSERT_THAT(layer->dataProvider()->vertexCount(),Eq(5));
}

TEST_F(EditableMeshLayerTesting, facesBeforeGenerate)
{
    registerProvider();
    initializeLayerWithSeveralVertices();

    ASSERT_THAT(layer->dataProvider()->faceCount(),Eq(0));
}

TEST_F(EditableMeshLayerTesting, facesAfterGenerate)
{
    registerProvider();
    initializeLayerWithSeveralVertices();

    generateMesh();

    ASSERT_THAT(layer->dataProvider()->faceCount(),Eq(3));
}

TEST_F(EditableMeshLayerTesting, populateMeshBeforeGenerate)
{
    registerProvider();
    initializeLayerWithSeveralVertices();

    QgsMesh mesh;

    layer->dataProvider()->populateMesh(&mesh);

    ASSERT_THAT(mesh.faceCount(),Eq(0));
}

TEST_F(EditableMeshLayerTesting, populateMeshAfterGenerate)
{
    registerProvider();
    initializeLayerWithSeveralVertices();

    QgsMesh mesh;

    generateMesh();

    layer->dataProvider()->populateMesh(&mesh);

    ASSERT_THAT(mesh.faceCount(),Eq(3));
    ASSERT_THAT(mesh.vertexCount(),Eq(5));
}


TEST_F(EditableMeshLayerTesting, extentMesh)
{
    registerProvider();
    initializeLayerWithSeveralVertices();

    generateMesh();


    qDebug()<<layer->dataProvider()->extent().xMinimum();
    qDebug()<<layer->dataProvider()->extent().yMinimum();
    qDebug()<<layer->dataProvider()->extent().xMaximum();
    qDebug()<<layer->dataProvider()->extent().yMaximum();

    ASSERT_TRUE(layer->dataProvider()->extent().contains(QgsRectangle(0,0,10,5)));

}


#endif // TST_HDEDITABLEMESHLAYERTEST_H
