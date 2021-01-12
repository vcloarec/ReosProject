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
  mActionDelineateWatershed( new QAction( QPixmap( ":/images/delineateWatershed.svg" ), tr( "Delineate watershed" ), this ) ),
  mDelineatingWidget( new ReosDelineatingWatershedWidget( module, map, this ) ),
  mCurrentMapOutlet( map )
{
  ui->setupUi( this );
  setModel( new ReosWatershedItemModel( module->watershedTree(), this ) );

  mActionDelineateWatershed->setCheckable( true );
  QToolBar *toolBar = new QToolBar( this );
  toolBar->addAction( mActionDelineateWatershed );
  static_cast<QBoxLayout *>( layout() )->insertWidget( 0, toolBar );
  mDelineatingWidget->setAction( mActionDelineateWatershed );

  ui->mToolButtonDelineate->setCheckable( true );

  //connect( ui->mToolButtonDelineate, &QToolButton::toggled, this, &ReosWatershedWidget::onButtonDelineateClicked );
  connect( ui->treeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosWatershedWidget::onCurrentWatershedChange );

  mCurrentMapOutlet.setWidth( 4 );
  mCurrentMapOutlet.setExternalWidth( 6 );
  mCurrentMapOutlet.setColor( QColor( 0, 100, 250 ) );
  mCurrentMapOutlet.setExternalColor( Qt::white );
  mCurrentMapOutlet.setZValue( 10 );

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
  clearSelection();
  ReosWatershed *ws = mModelWatershed->indexToWatershed( index );
  if ( !ws )
    return;
  ReosMapPolygon wsPolygon( mMap, ws->delineating() );
  mMapWatersheds.insert( ws, formatWatershedPolygon( wsPolygon ) );
  ui->treeView->selectionModel()->select( index, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows );
  ui->treeView->setCurrentIndex( index );
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

void ReosWatershedWidget::onCurrentWatershedChange( const QItemSelection &selected, const QItemSelection &deselected )
{
  ReosWatershed *currentWatershed = nullptr;
  if ( selected.indexes().count() > 0 )
    currentWatershed = mModelWatershed->indexToWatershed( selected.indexes().at( 0 ) );
  ReosWatershed *previousWatershed = nullptr;
  if ( deselected.indexes().count() > 0 )
    previousWatershed = mModelWatershed->indexToWatershed( deselected.indexes().at( 0 ) );

  QMap<ReosWatershed *, ReosMapPolygon>::iterator it = mMapWatersheds.find( previousWatershed );
  if ( it != mMapWatersheds.end() )
  {
    it.value().setFillColor( QColor() );
  }

  it = mMapWatersheds.find( currentWatershed );
  if ( it != mMapWatersheds.end() )
  {
    it.value().setFillColor( QColor( 0, 255, 0, 30 ) );
    mCurrentMapOutlet.resetPoint( currentWatershed->outletPoint() );
  }
  else
  {
    ReosMapPolygon wsPolygon( mMap, currentWatershed->delineating() );
    mMapWatersheds.insert( currentWatershed, formatWatershedPolygon( wsPolygon ) );
    onCurrentWatershedChange( selected, deselected );
  }
}

ReosMapPolygon ReosWatershedWidget::formatWatershedPolygon( ReosMapPolygon &watershedPolygon )
{
  watershedPolygon.setWidth( 3 );
  watershedPolygon.setColor( QColor( 0, 200, 100 ) );
  watershedPolygon.setExternalWidth( 5 );
  return watershedPolygon;
}

void ReosWatershedWidget::clearSelection()
{
  QMap<ReosWatershed *, ReosMapPolygon>::iterator it = mMapWatersheds.begin();
  while ( it != mMapWatersheds.end() )
    ( it++ ).value().setFillColor( QColor() );
}
