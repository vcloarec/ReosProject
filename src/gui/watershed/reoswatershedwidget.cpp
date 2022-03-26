#include "reoswatershedwidget.h"
#include "ui_reoswatershedwidget.h"

#include <QMenu>
#include <QMessageBox>

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
#include "reosexportwatershedtovectordialog.h"
#include "reosgaugedhydrographwidget.h"
#include "reoshydraulicnetwork.h"
#include "reoshydrographsource.h"
#include "reosdockwidget.h"
#include "reosstyleregistery.h"

ReosWatershedWidget::ReosWatershedWidget( ReosMap *map, ReosWatershedModule *module, ReosHydraulicNetwork *hydraulicNetwork, ReosDockWidget *parent ) :
  QWidget( parent ),
  ui( new Ui::ReosWatershedWidget ),
  mWatershdModule( module ),
  mMap( map ),
  mHydraulicNetwork( hydraulicNetwork ),
  mActionSelectWatershed( new QAction( QPixmap( QStringLiteral( ":/images/selectWatershed.svg" ) ), tr( "Select watershed on map" ), this ) ),
  mDescriptionKeyWatershed( QStringLiteral( "watershed:delineatingPolygon" ) ),
  mMapToolSelectWatershed( new ReosMapToolSelectMapItem( map, mDescriptionKeyWatershed ) ),
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
  mActionGaugedHydrograph( new QAction( QPixmap( QStringLiteral( ":/images/gaugedHydrograph.svg" ) ), tr( "Gauged hydrograph" ), this ) ),
  mGaugedHydrographWidget( new ReosWatershedGaugedHydrographWidget( map, this ) ),
  mActionExportToVectorLayer( new QAction( QPixmap( QStringLiteral( ":/images/exportWatershed.svg" ) ), tr( "Export watershed geometry to vector layer" ), this ) ),
  mActionZoomToWatershed( new QAction( QPixmap( QStringLiteral( ":/images/zoomToWatershed.svg" ) ), tr( "Zoom to watershed" ), this ) ),
  mMapToolEditDelineating( new ReosMapToolEditMapPolygon( map ) ),
  mMapToolMoveOutletPoint( new ReosMapToolMoveMapItem( map ) )
{
  ui->setupUi( this );

  connect( parent, &ReosDockWidget::shown, this, &ReosWatershedWidget::onOpened );
  connect( parent, &ReosDockWidget::closed, this, &ReosWatershedWidget::onClosed );

  setWatershedModel( new ReosWatershedItemModel( module->watershedTree(), this ) );

  mMeteorolocicModelWidget = new ReosMeteorologicModelWidget( mModelWatershed, module->meteoModelsCollection(), this );

  ui->mParameterAreaWidget->setDefaultName( tr( "Area" ) );
  ui->mParameterSlopeWidget->setDefaultName( tr( "Average Slope" ) );
  ui->mParameterNameWidget->setDefaultName( tr( "Watershed name" ) );
  ui->mParameterAverageElevationWidget->setDefaultName( tr( "Average elevation" ) );

  mActionDelineateWatershed->setCheckable( true );
  mActionLongitudinalProfile->setCheckable( true );
  mActionConcentrationTime->setCheckable( true );
  mActionMeteorologicModel->setCheckable( true );
  mActionRunoffHydrograph->setCheckable( true );
  mActionGaugedHydrograph->setCheckable( true );
  QToolBar *toolBar = new QToolBar( this );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

  toolBar->addAction( mActionSelectWatershed );
  toolBar->addAction( mActionDelineateWatershed );
  toolBar->addAction( mActionRemoveWatershed );
  toolBar->addAction( mActionLongitudinalProfile );
  toolBar->addAction( mActionConcentrationTime );
  toolBar->addAction( mActionMeteorologicModel );
  toolBar->addAction( mActionRunoffHydrograph );
  toolBar->addAction( mActionGaugedHydrograph );
  toolBar->addAction( mActionExportToVectorLayer );

  static_cast<QBoxLayout *>( layout() )->insertWidget( 0, toolBar );
  mDelineatingWidget->setAction( mActionDelineateWatershed );
  mDelineatingWidget->setEditingDelineatingMapTool( mMapToolEditDelineating );
  mDelineatingWidget->setMoveOutletPointMapTool( mMapToolMoveOutletPoint );
  mLongitudinalProfileWidget->setAction( mActionLongitudinalProfile );
  mConcentrationTimeWidget->setAction( mActionConcentrationTime );
  mMeteorolocicModelWidget->setAction( mActionMeteorologicModel );
  mRunoffHydrographWidget->setAction( mActionRunoffHydrograph );
  mGaugedHydrographWidget->setAction( mActionGaugedHydrograph );

  mMapToolSelectWatershed->setAction( mActionSelectWatershed );
  mActionSelectWatershed->setCheckable( true );
  mMapToolSelectWatershed->setSearchUnderPoint( true );
  mMapToolSelectWatershed->setCursor( Qt::ArrowCursor );
  connect( mMapToolSelectWatershed, &ReosMapToolSelectMapItem::found, this, &ReosWatershedWidget::onWatershedSelectedOnMap );

  ui->treeView->setContextMenuPolicy( Qt::CustomContextMenu );
  connect( ui->treeView->selectionModel(), &QItemSelectionModel::selectionChanged, this, &ReosWatershedWidget::onCurrentWatershedChanges );
  connect( ui->treeView, &QWidget::customContextMenuRequested, this, &ReosWatershedWidget::onTreeViewContextMenu );

  std::unique_ptr<ReosMenuPopulator> menuPopulator = std::make_unique<ReosMenuPopulator>();
  menuPopulator->addAction( mActionLongitudinalProfile );
  menuPopulator->addAction( mActionRemoveWatershed );
  menuPopulator->addAction( mActionConcentrationTime );
  menuPopulator->addAction( mActionRunoffHydrograph );
  menuPopulator->addAction( mActionGaugedHydrograph );
  mMapToolSelectWatershed->setContextMenuPopulator( menuPopulator.release() );

  connect( this, &ReosWatershedWidget::currentWatershedChanged, mLongitudinalProfileWidget, &ReosLongitudinalProfileWidget::setCurrentWatershed );
  connect( this, &ReosWatershedWidget::currentWatershedChanged, mConcentrationTimeWidget, &ReosConcentrationTimeWidget::setCurrentWatershed );
  connect( this, &ReosWatershedWidget::currentWatershedChanged, mRunoffHydrographWidget, &ReosRunoffHydrographWidget::setCurrentWatershed );
  connect( this, &ReosWatershedWidget::currentWatershedChanged, mGaugedHydrographWidget, &ReosWatershedGaugedHydrographWidget::setCurrentWatershed );

  connect( module, &ReosWatershedModule::hasBeenReset, this, &ReosWatershedWidget::onModuleReset );
  connect( mActionRemoveWatershed, &QAction::triggered, this, &ReosWatershedWidget::onRemoveWatershed );

  connect( ui->mParameterNameWidget, &ReosParameterWidget::valueChanged, ui->treeView, [this]
  {
    ui->treeView->dataChanged( ui->treeView->currentIndex(), ui->treeView->currentIndex() );
  } );

  connect( mMeteorolocicModelWidget, &ReosMeteorologicModelWidget::currentModelChanged,
           mRunoffHydrographWidget, &ReosRunoffHydrographWidget::setCurrentMeteorologicModel );

  mMapToolMoveOutletPoint->setMovingColor( QColor( 255, 0, 0, 150 ) );
  connect( mMapToolEditDelineating, &ReosMapToolEditMapPolygon::polygonEdited, this, [this]
  {
    MapWatersheds::iterator it = mMapWatersheds.find( currentWatershed() );
    if ( it != mMapWatersheds.end() && currentWatershed() )
    {
      currentWatershed()->setDelineating( it.value().delineating->mapPolygon() );
    }

  } );

  connect( mMapToolMoveOutletPoint, &ReosMapToolMoveMapItem::itemMoved, this, [this]
  {
    MapWatersheds::iterator it = mMapWatersheds.find( currentWatershed() );
    if ( it != mMapWatersheds.end() && currentWatershed() )
    {
      currentWatershed()->setOutletPoint( it.value().outletPoint->mapPoint() );
    }
  } );

  connect( mActionExportToVectorLayer, &QAction::triggered, this, &ReosWatershedWidget::onExportToVectorLayer );
  connect( mActionZoomToWatershed, &QAction::triggered, this, &ReosWatershedWidget::onZoomToWatershed );

  connect( ui->mAddRemoveHydraulicNetworkButton, &QPushButton::clicked, this, &ReosWatershedWidget::onAddRemoveNetwork );
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

ReosMapPolygon *ReosWatershedWidget::mapDelineating( ReosWatershed *ws )
{
  MapWatersheds::const_iterator it = mMapWatersheds.find( ws );
  if ( it != mMapWatersheds.end() )
    return it.value().delineating.get();
  else
    return nullptr;
}

void ReosWatershedWidget::onWatershedAdded( const QModelIndex &index )
{
  clearSelection();
  ReosWatershed *ws = mModelWatershed->indexToWatershed( index );
  if ( !ws )
    return;
  constructMapWatershed( ws );
//  mMapWatersheds[ws] = MapWatershed( mMap, ws->delineating(), ws->outletPoint() );
//  formatMapWatershed( mMapWatersheds[ws] );
  ui->treeView->selectionModel()->select( index, QItemSelectionModel::ClearAndSelect | QItemSelectionModel::Rows );
  ui->treeView->setCurrentIndex( index );
  updateNetworkButton();
}

void ReosWatershedWidget::onWatershedSelectedOnMap( ReosMapItem *item, const QPointF &pos )
{
  ui->treeView->setCurrentIndex( QModelIndex() );

  if ( !item )
    return;

  for ( ReosWatershed *ws : mMapWatersheds.keys() )
  {
    if ( mMapWatersheds.value( ws ).delineating->isItem( item ) )
    {
      //if the watershed is a residual one, start from its upstream
      if ( ws->watershedType() == ReosWatershed::Residual )
        ws = ws->downstreamWatershed();

      //Watershed found, return the more upstream under the point pos
      ReosWatershed *uws = ws->upstreamWatershed( pos, true );
      if ( uws )
        ui->treeView->setCurrentIndex( mModelWatershed->watershedToIndex( uws ) );
      else
        ui->treeView->setCurrentIndex( mModelWatershed->watershedToIndex( ws ) );
      return;
    }
  }
}

void ReosWatershedWidget::onRemoveWatershed()
{
  QModelIndex currentIndex = ui->treeView->currentIndex();

  ReosWatershed *ws = mModelWatershed->indexToWatershed( currentIndex );

  if ( !ws || ws->watershedType() == ReosWatershed::Residual )
    return;

  ReosWatershed *wsResid = ws->residualWatershed();

  if ( QMessageBox::warning( this, tr( "Removing watershed" ), tr( "Do you want to remove the current watershed '%1'?" ).arg( ws->watershedName()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
    return;

  emit currentWatershedChanged( nullptr );

  ReosWatershed *downstreamWatershed = ws->downstreamWatershed();
  ReosWatershed *downstreamResidualWatershed = nullptr;
  if ( downstreamWatershed )
    downstreamResidualWatershed = downstreamWatershed->residualWatershed(); //store it to update the map delineating later

  mMapToolEditDelineating->setMapPolygon( nullptr );

  if ( wsResid )
    mHydraulicNetwork->removeElement( associatedNetworkNode( wsResid ) );

  mHydraulicNetwork->removeElement( currentNetworkNode() );
  mModelWatershed->removeWatershed( currentIndex );
  MapWatersheds::iterator it = mMapWatersheds.find( ws );
  if ( it != mMapWatersheds.end() )
    mMapWatersheds.erase( it );

  it = mMapWatersheds.find( wsResid );
  if ( it != mMapWatersheds.end() )
    mMapWatersheds.erase( it );

  // update the residual of downstream if exist
  if ( downstreamWatershed && downstreamResidualWatershed )
  {
    // remove the old map polygon
    mMapWatersheds.remove( downstreamResidualWatershed );
    // add new one if still exist
    downstreamResidualWatershed = downstreamWatershed->residualWatershed();
    if ( downstreamResidualWatershed )
    {
      constructMapWatershed( downstreamResidualWatershed );
//      mMapWatersheds[downstreamResidualWatershed] = MapWatershed( mMap, downstreamResidualWatershed->delineating(), downstreamResidualWatershed->outletPoint() );
//      formatMapWatershed( mMapWatersheds[downstreamResidualWatershed] );
    }
  }

}

void ReosWatershedWidget::onCurrentWatershedChanges( const QItemSelection &selected, const QItemSelection &deselected )
{
  ReosWatershed *currentWatershed = nullptr;
  QModelIndex currentIndex;
  if ( selected.indexes().count() > 0 )
  {
    currentIndex = selected.indexes().at( 0 );
    currentWatershed = mModelWatershed->indexToWatershed( currentIndex );
  }

  mActionRemoveWatershed->setEnabled( currentWatershed && currentWatershed->watershedType() != ReosWatershed::Residual );

  ReosWatershed *previousWatershed = nullptr;
  if ( deselected.indexes().count() > 0 )
    previousWatershed = mModelWatershed->indexToWatershed( deselected.indexes().at( 0 ) );

  MapWatersheds::iterator it = mMapWatersheds.find( previousWatershed );
  if ( it != mMapWatersheds.end() )
  {
    formatUnselectedWatershed( it.value() );
  }

  it = mMapWatersheds.find( currentWatershed );
  if ( it != mMapWatersheds.end() )
  {
    if ( currentWatershed->watershedType() == ReosWatershed::Residual ) //update the delineating that could be changed if watershed was added
      it.value().delineating->resetPolygon( currentWatershed->delineating() );
    formatSelectedWatershed( it.value() );
    mMapToolEditDelineating->setMapPolygon( it.value().delineating.get() );
    mMapToolMoveOutletPoint->setCurrentMapItem( it.value().outletPoint.get() );
  }
  else if ( currentWatershed )
  {
    constructMapWatershed( currentWatershed );
    onCurrentWatershedChanges( selected, deselected );
    return;
  }

  if ( currentWatershed )
  {
    ui->mParameterNameWidget->setString( currentWatershed->watershedName() );
    ui->mParameterAreaWidget->setArea( currentWatershed->area() );
    ui->mParameterSlopeWidget->setSlope( currentWatershed->slope() );
    ui->mParameterAverageElevationWidget->setDouble( currentWatershed->averageElevation() );
  }
  else
  {
    ui->mParameterNameWidget->setString( nullptr );
    ui->mParameterAreaWidget->setArea( nullptr );
    ui->mParameterSlopeWidget->setSlope( nullptr );
    ui->mParameterAverageElevationWidget->setDouble( nullptr );
  }

  emit currentWatershedChanged( currentWatershed );
  updateNetworkButton();
}

void ReosWatershedWidget::onTreeViewContextMenu( const QPoint &pos )
{
  if ( !ui->treeView->currentIndex().isValid() )
    return;

  ReosWatershed *ws = currentWatershed();

  if ( !ws )
    return;

  QMenu contextMenu;
  contextMenu.addAction( mActionZoomToWatershed );
  contextMenu.addAction( mActionLongitudinalProfile );
  contextMenu.addAction( mActionConcentrationTime );
  contextMenu.addAction( mActionRunoffHydrograph );
  contextMenu.addAction( mActionGaugedHydrograph );
  if ( ws->watershedType() != ReosWatershed::Residual )
    contextMenu.addAction( mActionRemoveWatershed );

  contextMenu.exec( ui->treeView->viewport()->mapToGlobal( pos ) );
}

void ReosWatershedWidget::onWatershedDataChanged( const QModelIndex &index )
{
  ReosWatershed *ws = mModelWatershed->indexToWatershed( index );

  if ( !ws )
    return;

  ReosMapPolygon *delineating = mapDelineating( ws );
  if ( delineating )
    delineating->resetPolygon( ws->delineating() );
}

void ReosWatershedWidget::onModuleReset()
{
  mMapToolEditDelineating->setMapPolygon( nullptr );
  emit currentWatershedChanged( nullptr );
  mMapWatersheds.clear();

  const QList<ReosWatershed *> allWs = mModelWatershed->allWatershedsFromDSToUS();
  for ( ReosWatershed *ws : allWs )
  {
    constructMapWatershed( ws );
  }

  mMeteorolocicModelWidget->setCurrentMeteorologicalModel( 0 );
}

void ReosWatershedWidget::onExportToVectorLayer()
{
  ReosExportWatershedToVectorDialog *dial = new ReosExportWatershedToVectorDialog( mModelWatershed->allWatershedsFromUSToDS(), mMap->engine()->crs(), this );
  dial->exec();

  dial->deleteLater();
}

void ReosWatershedWidget::onZoomToWatershed()
{
  ReosWatershed *ws = currentWatershed();
  if ( ws )
    mMap->setExtent( ws->extent() );
}

void ReosWatershedWidget::onAddRemoveNetwork()
{
  if ( !currentWatershed() )
    return;

  ReosWatershed *ws = currentWatershed();

  ReosHydrographNodeWatershed *hsw = currentNetworkNode();
  if ( hsw )
  {
    if ( QMessageBox::warning( this,
                               tr( "Remove Watershed Hydraulic Node" ),
                               tr( "This will permanently remove the hydraulic node %1. Do you want to proceed? " ).arg( hsw->elementName()->value() ),
                               QMessageBox::Yes, QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
      return;
    mHydraulicNetwork->removeElement( hsw );
  }
  else
  {
    const QList<ReosHydraulicNetworkElement *> watershedNodeElements = mHydraulicNetwork->getElements( ReosHydrographNodeWatershed::staticType() );
    const ReosWatershed *otherWatershed = nullptr;
    bool otherIsResidual = false;
    if ( ws->watershedType() == ReosWatershed::Residual )
    {
      otherIsResidual = false;
      otherWatershed = ws->downstreamWatershed();
    }
    else
    {
      otherIsResidual = true;
      otherWatershed = ws->residualWatershed();
    }

    for ( ReosHydraulicNetworkElement *hne : std::as_const( watershedNodeElements ) )
    {
      ReosHydrographNodeWatershed *wsn = qobject_cast<ReosHydrographNodeWatershed *>( hne );
      if ( wsn && wsn->watershed() == otherWatershed )
      {
        const QString text1 = tr( "downstream watershed" );
        const QString text2 = tr( "residual watershed" );
        QString part1, part2;
        if ( otherIsResidual )
        {
          part1 = text1;
          part2 = text2;
        }
        else
        {
          part2 = text1;
          part1 = text2;
        }
        QMessageBox::warning( this,
                              tr( "Add Watershed Hydraulic Node" ),
                              tr( "This %1 associated with this %2 has already a hydraulic watershed node linked with.\n\n"
                                  "It is not possible to have hydraulic watershed node for both downstream watershed and associated residual watershed" ).arg( part1, part2 ),
                              QMessageBox::Ok );
        return;
      }
    }

    mHydraulicNetwork->addElement( new ReosHydrographNodeWatershed( ws, mWatershdModule->meteoModelsCollection(), mHydraulicNetwork ) );
  }

  updateNetworkButton();
}

void ReosWatershedWidget::onClosed()
{
  setVisibleMapItems( false );
}

void ReosWatershedWidget::onOpened()
{
  setVisibleMapItems( true );
}

void ReosWatershedWidget::constructMapWatershed( ReosWatershed *watershed )
{
  MapWatershed mapWs( mMap, watershed->delineating(), watershed->outletPoint() );

  if ( watershed->watershedType() == ReosWatershed::Residual )
  {
    MapWatersheds::iterator dsMapWs = mMapWatersheds.find( watershed->downstreamWatershed() );
    if ( dsMapWs != mMapWatersheds.end() )
      mapWs.outletPoint = dsMapWs->outletPoint;
  }

  mMapWatersheds[watershed] = mapWs;
  formatMapWatershed( mMapWatersheds[watershed] );
}

ReosWatershed *ReosWatershedWidget::currentWatershed() const
{
  QModelIndex currentIndex = ui->treeView->currentIndex();
  return mModelWatershed->indexToWatershed( currentIndex );
}

void ReosWatershedWidget::formatMapWatershed( MapWatershed &mapWatershed )
{
  mapWatershed.delineating->setDescription( mDescriptionKeyWatershed );
  mapWatershed.delineating->setWidth( 3 );
  mapWatershed.delineating->setColor( QColor( 0, 200, 100 ) );
  mapWatershed.delineating->setExternalWidth( 5 );

  mapWatershed.outletPoint->setWidth( 4 );
  mapWatershed.outletPoint->setExternalWidth( 6 );
  mapWatershed.outletPoint->setColor( QColor( 0, 155, 242 ) );
  mapWatershed.outletPoint->setExternalColor( Qt::white );
  mapWatershed.outletPoint->setZValue( 10 );
}

void ReosWatershedWidget::formatSelectedWatershed( ReosWatershedWidget::MapWatershed &mapWatershed )
{
  mapWatershed.delineating->setFillColor( QColor( 0, 255, 0, 30 ) );
  mapWatershed.delineating->setFillStyle( Qt::SolidPattern );
  mapWatershed.outletPoint->setWidth( 8 );
  mapWatershed.outletPoint->setExternalWidth( 12 );
}

void ReosWatershedWidget::formatUnselectedWatershed( ReosWatershedWidget::MapWatershed &mapWatershed )
{
  mapWatershed.delineating->setFillColor( QColor() );
  mapWatershed.outletPoint->setWidth( 4 );
  mapWatershed.outletPoint->setExternalWidth( 6 );
}


void ReosWatershedWidget::clearSelection()
{
  MapWatersheds::iterator it = mMapWatersheds.begin();
  while ( it != mMapWatersheds.end() )
  {
    formatUnselectedWatershed( it.value() );
    it++;
  }
}

ReosHydrographNodeWatershed *ReosWatershedWidget::currentNetworkNode() const
{
  return associatedNetworkNode( currentWatershed() );
}

ReosHydrographNodeWatershed *ReosWatershedWidget::associatedNetworkNode( ReosWatershed *watershed ) const
{
  QList<ReosHydraulicNetworkElement *> watershedHydrographSource = mHydraulicNetwork->getElements( ReosHydrographNodeWatershed::staticType() );

  for ( ReosHydraulicNetworkElement *elem : watershedHydrographSource )
  {
    ReosHydrographNodeWatershed *hsw = qobject_cast<ReosHydrographNodeWatershed *>( elem );
    if ( hsw && hsw->watershed() == watershed )
    {
      ui->mAddRemoveHydraulicNetworkButton->setText( tr( "Remove watershed from network" ) );
      ui->mAddRemoveHydraulicNetworkButton->setEnabled( true );
      return hsw;
    }
  }

  return nullptr;
}

void ReosWatershedWidget::setVisibleMapItems( bool visible )
{
  for ( MapWatershed &mw : mMapWatersheds )
    mw.setVisible( visible );

  mLongitudinalProfileWidget->setVisibleStreamLine( visible );
}

void ReosWatershedWidget::updateNetworkButton()
{
  if ( !mHydraulicNetwork )
  {
    ui->mAddRemoveHydraulicNetworkButton->setText( tr( "No Hydraulic Network" ) );
    ui->mAddRemoveHydraulicNetworkButton->setEnabled( false );
    return;
  }

  if ( !currentWatershed() )
  {
    ui->mAddRemoveHydraulicNetworkButton->setText( tr( "No Watershed Selected" ) );
    ui->mAddRemoveHydraulicNetworkButton->setEnabled( false );
    return;
  }

  if ( currentNetworkNode() )
  {
    ui->mAddRemoveHydraulicNetworkButton->setText( tr( "Remove Watershed from Network" ) );
    ui->mAddRemoveHydraulicNetworkButton->setEnabled( true );
    return;
  }

  ui->mAddRemoveHydraulicNetworkButton->setText( tr( "Add Watershed to Network" ) );
  ui->mAddRemoveHydraulicNetworkButton->setEnabled( true );
}

ReosWatershedDockWidget::ReosWatershedDockWidget( ReosMap *map, ReosWatershedModule *module, ReosHydraulicNetwork *hydraulicNetwork, QWidget *parent )
  : ReosDockWidget( tr( "Watershed" ), parent )
  , mWatershedWidget( new ReosWatershedWidget( map, module, hydraulicNetwork, this ) )
{
  setWidget( mWatershedWidget );
}

ReosWatershedWidget::MapWatershed::MapWatershed( ReosMap *map, const QPolygonF &delineat, const QPointF &outletPt )
{
  delineating = std::make_shared<ReosMapPolygon>( map, delineat );
  outletPoint = std::make_shared<ReosMapMarkerFilledCircle>( map, outletPt );
}

void ReosWatershedWidget::MapWatershed::setVisible( bool b )
{
  delineating->setVisible( b );
  outletPoint->setVisible( b );
}
