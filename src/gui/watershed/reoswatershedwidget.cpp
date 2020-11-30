#include "reoswatershedtreeview.h"
#include "ui_reoswatershedtreeview.h"

#include "reoswatershedtree.h"

ReosWatershedWidget::ReosWatershedWidget( ReosMap *map, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosWatershedTreeView ),
  mMap( map )
{
  ui->setupUi( this );
}

ReosWatershedWidget::~ReosWatershedWidget()
{
  delete ui;
}

void ReosWatershedWidget::setModel( ReosWatershedItemModel *model )
{
  ui->treeView->setModel( model );
  mModelWatershed = model;
  connect( model, &ReosWatershedItemModel::watershedAdded, this, &ReosWatershedWidget::onWatershedAdded );
  connect( model, &ReosWatershedItemModel::watershedAdded, ui->treeView, &QTreeView::expand );
}

void ReosWatershedWidget::onWatershedAdded( const QModelIndex &index )
{
  updateMapWatershed();
}

void ReosWatershedWidget::updateMapWatershed()
{
  mMapWatersheds.clear();

  const QList<ReosWatershed *> watersheds = mModelWatershed->allWatersheds();
  for ( ReosWatershed *ws : watersheds )
  {
    ReosMapPolygon wsPolygon( mMap, ws->delineating() );
    mMapWatersheds.append( formatWatershedPolygon( wsPolygon ) );
  }
}

ReosMapPolygon ReosWatershedWidget::formatWatershedPolygon( ReosMapPolygon &watershedPolygon )
{
  watershedPolygon.setWidth( 3 );
  watershedPolygon.setColor( QColor( 0, 200, 100 ) );
  watershedPolygon.setExternalWidth( 5 );
  return watershedPolygon;
}
