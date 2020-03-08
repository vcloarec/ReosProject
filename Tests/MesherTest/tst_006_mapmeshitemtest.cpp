#include <gtest/gtest.h>
#include <gmock/gmock-matchers.h>

#include <qgsapplication.h>
#include <qgsmapcanvas.h>

#include "../../Mesher/tinEditorUi/reostineditorgraphic.h"
#include "../../Mesher/tinEditorUi/reosmapmeshitem.h"
#include "../../Mesher/provider/meshdataprovider.h"
#include "../../GIS/hdgismanager.h"


using namespace testing;

class MeshItemTesting : public Test
{
  public:

    ReosMap *map;
    QgsMapCanvas *mapCanvas;
    ReosTinEditorUi *uiEditor;
    QgsMeshLayer *meshLayer;
    TINProvider *provider;
    ReosGisManager *gismanager;

    std::vector<Segment> segments;

    //TINProvider provider;

    void populateEditorWithVertex()
    {
      uiEditor->newVertex( QPointF( 5, 15 ) );
      uiEditor->newVertex( QPointF( 10, 15 ) );
      uiEditor->newVertex( QPointF( 5, 10 ) );
      uiEditor->newVertex( QPointF( 10, 10 ) );
      uiEditor->newVertex( QPointF( 10, 5 ) );
    }

    // Test interface
  protected:
    void SetUp() override
    {
      QgsProviderRegistry::instance()->registerProvider( new HdTinEditorProviderMetaData() );
      map = new ReosMap();
      mapCanvas = map->getMapCanvas();
      gismanager = new ReosGisManager( map );
      uiEditor = new ReosTinEditorUi( gismanager );
      meshLayer = new QgsMeshLayer( "-", "Mesh editable", "TIN" );
      provider = static_cast<TINProvider *>( meshLayer->dataProvider() );
    }

    void TearDown() override
    {
      delete mapCanvas;
    }
};


TEST_F( MeshItemTesting, domainContruction )
{
  ReosMapMeshEditorItemDomain domain( uiEditor, mapCanvas );

  ASSERT_THAT( domain.verticesCount(), Eq( 0 ) );
  ASSERT_THAT( domain.verticesCount(), Eq( provider->vertexCount() ) );
}

TEST_F( MeshItemTesting, associateMeshToEditor )
{
  uiEditor->setMeshLayer( meshLayer );
  populateEditorWithVertex();

  ReosMapMeshEditorItemDomain *domain = uiEditor->domain();;


  ASSERT_THAT( domain->verticesCount(), Eq( 5 ) );
}

TEST_F( MeshItemTesting, associateNullMeshToEditor )
{
  uiEditor->setMeshLayer( meshLayer );
  populateEditorWithVertex();

  uiEditor->setMeshLayer( nullptr );


  ASSERT_THAT( uiEditor->domain()->verticesCount(), Eq( 0 ) );
}

TEST_F( MeshItemTesting, associateNewMeshAfterPopulateToEditor )
{
  uiEditor->setMeshLayer( meshLayer );
  populateEditorWithVertex();

  ASSERT_THAT( uiEditor->domain()->verticesCount(), Eq( 5 ) );

  uiEditor->setMeshLayer( nullptr );

  ASSERT_THAT( uiEditor->domain()->verticesCount(), Eq( 0 ) );

  uiEditor->setMeshLayer( meshLayer );

  ASSERT_THAT( uiEditor->domain()->verticesCount(), Eq( 5 ) );
}


