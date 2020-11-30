#include "reoswatershedwidget.h"
#include "ui_reoswatershedwidget.h"

#include "reossettings.h"
#include "reoswatershedmodule.h"
#include "reoswatershedtree.h"
#include "reosdelineatingwatershedwidget.h"

ReosWatershedWidget::ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosWatershedWidget ),
  mMap( map ),
  mDelineatingWidget( new ReosDelineatingWatershedWidget( module->delineatingModule(), map, this ) )
{
  ReosSettings settings;
  ui->setupUi( this );
  mModelWatershed = new ReosWatershedItemModel( module->watershedTree(), this );

  ui->treeView->setModel( mModelWatershed );
  ui->mToolButtonDelineate->setCheckable( true );

  connect( ui->mToolButtonDelineate, &QToolButton::toggled, this, &ReosWatershedWidget::onButtonDelineateClicked );
  ui->mToolButtonDelineate->setChecked( settings.value( QStringLiteral( "/Windows/WatershedDelineateWidget/Open" ) ).toBool() );
  connect( mDelineatingWidget, &ReosDelineatingWatershedWidget::closed, this, [ = ]
  {
    ui->mToolButtonDelineate->setChecked( false );
  } );
  onButtonDelineateClicked();
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

void ReosWatershedWidget::onButtonDelineateClicked()
{
  if ( ui->mToolButtonDelineate->isChecked() )
    mDelineatingWidget->show();
  else
    mDelineatingWidget->close();

  ReosSettings settings;
  settings.setValue( QStringLiteral( "/Windows/WatershedDelineateWidget/Open" ), ui->mToolButtonDelineate->isChecked() );

}

ReosMapPolygon ReosWatershedWidget::formatWatershedPolygon( ReosMapPolygon &watershedPolygon )
{
  watershedPolygon.setWidth( 3 );
  watershedPolygon.setColor( QColor( 0, 200, 100 ) );
  watershedPolygon.setExternalWidth( 5 );
  return watershedPolygon;
}
