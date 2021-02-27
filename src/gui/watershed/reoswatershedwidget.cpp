#include "reoswatershedwidget.h"
#include "ui_reoswatershedwidget.h"

#include "reosmaptool.h"
#include "reossettings.h"
#include "reoswatershedmodule.h"
#include "reoswatershedtree.h"
#include "reosdelineatingwatershedwidget.h"
#include "reoslongitudinalprofilewidget.h"
#include "reosconcentrationtimewidget.h"
#include "reosmenupopulator.h"
#include "reosmeteorologicmodel.h"
#include "reosmeteorologicmodelwidget.h"
#include "reosrunoffhydrographwidget.h"

#include <QMessageBox>

ReosWatershedWidget::ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, QWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosWatershedWidget ),
  mMap( map ),
  mActionSelectWatershed( new QAction( QPixmap( QStringLiteral( ":/images/selectWatershed.svg" ) ), tr( "Select watershed on map" ), this ) ),
  mMapToolSelectWatershed( new ReosMapToolSelectMapItem( map, QStringLiteral( "Watershed" ) ) ),
  mActionRemoveWatershed( new QAction( QPixmap( QStringLiteral( ":/images/removeWatershed.svg" ) ), tr( "Remove watershed" ), this ) ),
  mActionDelineateWatershed( new QAction( QPixmap( QStringLiteral( ":/images/delineateWatershed.svg" ) ), tr( "Delineate watershed" ), this ) ),
  mDelineatingWidget( new ReosDelineatingWatershedWidget( module, map, this ) ),
  mActionLongitudinalProfile( new QAction( QPixmap( QStringLiteral( ":/images/longProfile.svg" ) ), tr( "Longitudinal profile" ) ) ),
  mLongitudinalProfileWidget( new ReosLongitudinalProfileWidget( map, this ) ),
  mActionConcentrationTime( new QAction( QPixmap( QStringLiteral( ":/images/concentrationTimeWatershed.svg" ) ), tr( "Concentration time" ), this ) ),
  mConcentrationTimeWidget( new ReosConcentrationTimeWidget( this ) ),
  mActionMeteorologicModel( new QAction( QPixmap( QStringLiteral( ":/images/meteoModel.svg" ) ), tr( "Meteorologic models" ), this ) ),
  mActionRunoffHydrograph( new QAction( QPixmap( QStringLiteral( ":/images/runoffHydrograph.svg" ) ), tr( "Runoff hydrograph" ), this ) ),
  mRunoffHydrographWidget( new ReosRunoffHydrographWidget( module, this ) ),
  mCurrentMapOutlet( map ),
  mCurrentStreamLine( map )
{
  ui->setupUi( this );
  setWatershedModel( new ReosWatershedItemModel( module->watershedTree(), this ) );

  mMeteorolocicModelWidget = new ReosMeteorologicModelWidget( mModelWatershed, module->meteoModelsCollection(), this );

  ui->mParameterAreaWidget->setDefaultName( tr( "Area" ) );
  ui->mParameterSlopeWidget->setDefaultName( tr( "Average Slope" ) );
  ui->mParameterNameWidget->setDefaultName( tr( "Watershed name" ) );

  mActionDelineateWatershed->setCheckable( true );
  mActionLongitudinalProfile->setCheckable( true );
  mActionConcentrationTime->setCheckable( true );
  mActionMeteorologicModel->setCheckable( true );
  mActionRunoffHydrograph->setCheckable( true );
  QToolBar *toolBar = new QToolBar( this );

  toolBar->addAction( mActionSelectWatershed );
  toolBar->addAction( mActionDelineateWatershed );
  toolBar->addAction( mActionRemoveWatershed );
  toolBar->addAction( mActionLongitudinalProfile );
  toolBar->addAction( mActionConcentrationTime );
  toolBar->addAction( mActionMeteorologicModel );
  toolBar->addAction( mActionRunoffHydrograph );

  static_cast<QBoxLayout *>( layout() )->insertWidget( 0, toolBar );
  mDelineatingWidget->setAction( mActionDelineateWatershed );
  mLongitudinalProfileWidget->setAction( mActionLongitudinalProfile );
  mConcentrationTimeWidget->setAction( mActionConcentrationTime );
  mMeteorolocicModelWidget->setAction( mActionMeteorologicModel );
  mRunoffHydrographWidget->setAction( mActionRunoffHydrograph );

  mMapToolSelectWatershed->setAction( mActionSelectWatershed );
  mActionSelectWatershed->setCheckable( true );
  mMapToolSelectWatershed->setSearchUnderPoint( true );
  mMapToolSelectWatershed->setCursor( Qt::ArrowCursor );
  connect( mMapToolSelectWatershed, &ReosMapToolSelectMapItem::found, this, &ReosWatershedWidget::onWatershedSelectedOnMap );

  connect( ui->treeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosWatershedWidget::onCurrentWatershedChange );

  std::unique_ptr<ReosMenuPopulator> menuPopulator = std::make_unique<ReosMenuPopulator>();
  menuPopulator->addAction( mActionLongitudinalProfile );
  menuPopulator->addAction( mActionRemoveWatershed );
  menuPopulator->addAction( mActionConcentrationTime );
  menuPopulator->addAction( mActionRunoffHydrograph );
  mMapToolSelectWatershed->setContextMenuPopulator( menuPopulator.release() );

  mCurrentMapOutlet.setWidth( 4 );
  mCurrentMapOutlet.setExternalWidth( 6 );
  mCurrentMapOutlet.setColor( QColor( 0, 155, 242 ) );
  mCurrentMapOutlet.setExternalColor( Qt::white );
  mCurrentMapOutlet.setZValue( 10 );

  connect( this, &ReosWatershedWidget::currentWatershedChanged, mLongitudinalProfileWidget, &ReosLongitudinalProfileWidget::setCurrentWatershed );
  connect( this, &ReosWatershedWidget::currentWatershedChanged, mConcentrationTimeWidget, &ReosConcentrationTimeWidget::setCurrentWatershed );
  connect( this, &ReosWatershedWidget::currentWatershedChanged, mRunoffHydrographWidget, &ReosRunoffHydrographWidget::setCurrentWatershed );

  connect( module, &ReosWatershedModule::hasBeenReset, this, &ReosWatershedWidget::onModuleReset );

  connect( mActionRemoveWatershed, &QAction::triggered, this, &ReosWatershedWidget::onRemoveWatershed );

  connect( ui->mParameterNameWidget, &ReosParameterWidget::valueChanged, ui->treeView, [this]
  {
    ui->treeView->dataChanged( ui->treeView->currentIndex(), ui->treeView->currentIndex() );
  } );

  connect( mMeteorolocicModelWidget, &ReosMeteorologicModelWidget::currentModelChanged,
           mRunoffHydrographWidget, &ReosRunoffHydrographWidget::setCurrentMeteorologicModel );
}

ReosWatershedWidget::~ReosWatershedWidget()
{
  delete ui;
}

void ReosWatershedWidget::setWatershedModel( ReosWatershedItemModel *model )
{
  ui->treeView->setModel( model );
  mModelWatershed = model;
  connect( model, &ReosWatershedItemModel::watershedAdded, this, &ReosWatershedWidget::onWatershedAdded );
  connect( model, &ReosWatershedItemModel::watershedAdded, ui->treeView, &QTreeView::expand );
  connect( model, &ReosWatershedItemModel::dataChanged, this, &ReosWatershedWidget::onWatershedDataChanged );
}

void ReosWatershedWidget::onWatershedAdded( const QModelIndex &index )
{
  clearSelection();
  ReosWatershed *ws = mModelWatershed->indexToWatershed( index );
  if ( !ws )
    return;
  mMapWatersheds.insert( ws, formatWatershedPolygon( ReosMapPolygon( mMap, ws->delineating() ) ) );
  ui->treeView->selectionModel()->select( index, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows );
  ui->treeView->setCurrentIndex( index );
}

void ReosWatershedWidget::onWatershedSelectedOnMap( ReosMapItem *item, const QPointF &pos )
{
  QList<ReosWatershed *> keys = mMapWatersheds.keys();

  for ( ReosWatershed *ws : keys )
  {
    if ( mMapWatersheds[ws].isItem( item ) )
    {
      //Watershed found, return the more upstream under the point pos
      ReosWatershed *uws = ws->upstreamWatershed( pos, true );
      if ( uws )
        ws = uws;
      ui->treeView->setCurrentIndex( mModelWatershed->watershedToIndex( ws ) );
      return;
    }
  }
}

void ReosWatershedWidget::onRemoveWatershed()
{
  QModelIndex currentndex = ui->treeView->currentIndex();

  ReosWatershed *ws = mModelWatershed->indexToWatershed( currentndex );

  if ( !ws )
    return;

  if ( QMessageBox::warning( this, tr( "Removing watershed" ), tr( "Do you want to remove the current watershed '%1'?" ).arg( ws->name()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
    return;

  mModelWatershed->removeWatershed( currentndex );
  mMapWatersheds.remove( ws );
  mCurrentMapOutlet.resetPoint();
  emit currentWatershedChanged( nullptr );
}

void ReosWatershedWidget::onCurrentWatershedChange( const QItemSelection &selected, const QItemSelection &deselected )
{
  ReosWatershed *currentWatershed = nullptr;
  QModelIndex currentIndex;
  if ( selected.indexes().count() > 0 )
  {
    currentIndex = selected.indexes().at( 0 );
    currentWatershed = mModelWatershed->indexToWatershed( currentIndex );
  }
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
    mMapWatersheds.insert( currentWatershed, formatWatershedPolygon( ReosMapPolygon( mMap, currentWatershed->delineating() ) ) );
    onCurrentWatershedChange( selected, deselected );
    return;
  }

  if ( currentWatershed )
  {
    ui->mParameterNameWidget->setString( currentWatershed->name() );
    ui->mParameterAreaWidget->setArea( currentWatershed->area() );
    ui->mParameterSlopeWidget->setSlope( currentWatershed->slope() );
  }
  else
  {
    ui->mParameterNameWidget->setString( nullptr );
    ui->mParameterAreaWidget->setArea( nullptr );
    ui->mParameterSlopeWidget->setSlope( nullptr );
  }

  emit currentWatershedChanged( currentWatershed );
}

void ReosWatershedWidget::onWatershedDataChanged( const QModelIndex &index )
{
  if ( ui->treeView->currentIndex() == index )
  {
    ReosWatershed *currentWatershed = mModelWatershed->indexToWatershed( index );
    mCurrentMapOutlet.resetPoint( currentWatershed->outletPoint() );
  }
}

void ReosWatershedWidget::onModuleReset()
{
  mMapWatersheds.clear();

  const QList<ReosWatershed *> allWs = mModelWatershed->allWatersheds();

  for ( ReosWatershed *ws : allWs )
    mMapWatersheds.insert( ws, formatWatershedPolygon( ReosMapPolygon( mMap, ws->delineating() ) ) );

  mMeteorolocicModelWidget->setCurrentMeteorologicalModel( 0 );
}

ReosMapPolygon &ReosWatershedWidget::formatWatershedPolygon( ReosMapPolygon &&watershedPolygon )
{
  watershedPolygon.setDescription( QStringLiteral( "Watershed" ) );
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
