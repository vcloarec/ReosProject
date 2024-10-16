/***************************************************************************
  reoshydraulicnetworkwidget.cpp - ReosHydraulicNetworkWidget

 ---------------------
 begin                : 20.5.2021
 copyright            : (C) 2021 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reoshydraulicnetworkwidget.h"
#include "ui_reoshydraulicnetworkwidget.h"

#include <QMessageBox>
#include <QMenu>

#include "reoshydrographsource.h"
#include "reoshydrographrouting.h"
#include "reoshydraulicstructure2d.h"
#include "reosmaptool.h"
#include "reoshydraulicelementpropertieswidget.h"
#include "reosstyleregistery.h"
#include "reoshydraulicscheme.h"
#include "reoshydraulicschemewidget.h"
#include "reosparameter.h"
#include "reosstructure2dtoolbar.h"
#include "reoshydraulicstructure2dproperties.h"
#include "reosimporthydraulicstructuredialog.h"
#include "reoshydraulicelementmodel.h"
#include "reosnetworkcompatibilitydialog.h"
#include "reosaddhydrographnodefromwidget.h"
#include "reoshydraulicschemewidget.h"


ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, const ReosGuiContext &context ) :
  QWidget( context.parent() )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mGuiContext( context )
  , mHydraulicNetwork( network )
  , mElementModel( new ReosHydraulicElementModel( network ) )
  , mMap( context.map() )
  , mStructure2dToolBar( new ReosStructure2dToolBar )
  , mActionSelectNetworkElement( new QAction( QIcon( QStringLiteral( ":/images/selectHydraulicElement.svg" ) ), tr( "Select Hydraulic Network Element" ), this ) )
  , mMapToolSelectNetworkElement( new ReosMapToolSelectMapItem( context.map(), ReosHydraulicNetworkElement::staticType() ) )
  , mActionAddHydrographJunction( new QAction( QIcon( QStringLiteral( ":/images/addHydrographJunction.svg" ) ), tr( "Add Junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
  , mActionAddHydrofraphJunctionFrom( new QAction( QIcon( QStringLiteral( ":/images/addHydrographJunction.svg" ) ), tr( "Add Junction From..." ), this ) )
  , mActionAddHydrographRouting( new QAction( QIcon( QStringLiteral( ":/images/addHydrographRouting.svg" ) ), tr( "Add Link" ), this ) )
  , mMapToolAddHydrographRouting( new ReosMapToolDrawHydrographRouting( mHydraulicNetwork, mMap ) )
  , mActionHydraulicNetworkProperties( new QAction( QIcon( QStringLiteral( ":/images/hydraulicProperties.svg" ) ), tr( "Hydraulic Element Properties" ), this ) )
  , mElementPropertiesWidget( new ReosHydraulicElementPropertiesActionWidget( network, ReosGuiContext( context, this ) ) )
  , mActionMoveHydrographJunction( new QAction( QIcon( QStringLiteral( ":/images/moveHydrographJunction.svg" ) ),  tr( "Move Junction" ), this ) )
  , mMapToolMoveHydrographJunction( new ReosMapToolMoveHydraulicNetworkElement( network, context.map() ) )
  , mActionNewStructure2D( new QAction( QIcon( QStringLiteral( ":/images/addHydraulicStructure2D.svg" ) ), tr( "Structure 2D" ), this ) )
  , mMapToolNewStructure2D( new ReosMapToolNewStructure2D( network, mMap ) )
  , mActionImportStructure2D( new QAction( QIcon( QStringLiteral( ":/images/importHydraulicStructure2D.svg" ) ), tr( "Import Structure 2D" ), this ) )
  , mActionRemoveElement( new QAction( QIcon( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Hydraulic Element" ), this ) )
  , mActionZoomToNetworkExtent( new QAction( QIcon( QStringLiteral( ":/images/zoomNetworkExtent.svg" ) ), tr( "Zoom to Network Extent" ), this ) )
{
  ui->setupUi( this );

  ReosHydraulicNetworkDockWidget *dockParent = qobject_cast<ReosHydraulicNetworkDockWidget *>( context.parent() );
  if ( dockParent )
  {
    connect( dockParent, &ReosHydraulicNetworkDockWidget::shown, this, &ReosHydraulicNetworkWidget::onOpened );
    connect( dockParent, &ReosHydraulicNetworkDockWidget::closed, this, &ReosHydraulicNetworkWidget::onClosed );
  }

  connect( mMap, &ReosMap::crsChanged, this, &ReosHydraulicNetworkWidget::onMapCrsChanged );

  ui->mElementListView->setModel( mElementModel );

  connect( ui->mElementListView, &QListView::clicked, this, [this]( const QModelIndex index )
  {
    ReosHydraulicNetworkElement *elem = mElementModel->indexToElement( index );
    if ( !elem )
      return;
    NetworkItem item = mMapItems.value( elem, nullptr );
    if ( item )
      onElementSelected( item.get() );
  } );


  QToolBar *toolBar = new QToolBar( this );
  ui->mElementToolBar->addWidget( toolBar );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );

  toolBar->addAction( mActionSelectNetworkElement );
  mActionSelectNetworkElement->setCheckable( true );
  mMapToolSelectNetworkElement->setAction( mActionSelectNetworkElement );
  mMapToolSelectNetworkElement->setSearchItemWhenMoving( true );
  mMapToolSelectNetworkElement->setCursor( Qt::ArrowCursor );

  mActionAddHydrographJunction->setCheckable( true );

  QStringList spatialHydrographProvider =
    ReosDataProviderRegistery::instance()->withCapabilities( ReosHydrograph::staticType(), ReosDataProvider::Spatial );
  if ( spatialHydrographProvider.isEmpty() )
    toolBar->addAction( mActionAddHydrographJunction );
  else
  {
    mActionAddHydrofraphJunctionFrom->setCheckable( true );
    QToolButton *mHydrographJunctionButton = new QToolButton( toolBar );
    mHydrographJunctionButton->setPopupMode( QToolButton::MenuButtonPopup );
    mHydrographJunctionButton->addAction( mActionAddHydrographJunction );
    mHydrographJunctionButton->setDefaultAction( mActionAddHydrographJunction );
    mHydrographJunctionButton->addAction( mActionAddHydrofraphJunctionFrom );
    toolBar->addWidget( mHydrographJunctionButton );
    ReosAddHydrographNodeFromWidget *addFromWidget = new ReosAddHydrographNodeFromWidget( network, ReosGuiContext( mGuiContext, this ) );
    addFromWidget->setAction( mActionAddHydrofraphJunctionFrom );
  }

  mMapToolAddHydrographJunction->setAction( mActionAddHydrographJunction );
  mActionAddHydrographRouting->setCheckable( true );
  toolBar->addAction( mActionAddHydrographRouting );
  mMapToolAddHydrographRouting->setAction( mActionAddHydrographRouting );
  mMapToolAddHydrographRouting->addSearchingItemDescription( ReosHydrographNode::staticType() );
  mMapToolAddHydrographRouting->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionMoveHydrographJunction );
  mActionMoveHydrographJunction->setCheckable( true );
  mMapToolMoveHydrographJunction->setAction( mActionMoveHydrographJunction );
  mMapToolMoveHydrographJunction->addSearchingItemDescription( ReosHydrographJunction::staticType() );
  mMapToolMoveHydrographJunction->setSearchItemWhenMoving( true );

  mActionNewStructure2D->setCheckable( true );
  mMapToolNewStructure2D->setAction( mActionNewStructure2D );
  if ( !ReosSimulationEngineRegistery::instance()->canImportSrtucture2D() )
  {
    toolBar->addAction( mActionNewStructure2D );
  }
  else
  {
    QToolButton *mStructure2DButton = new QToolButton( toolBar );
    mStructure2DButton->setPopupMode( QToolButton::MenuButtonPopup );
    mStructure2DButton->addAction( mActionNewStructure2D );
    mStructure2DButton->setDefaultAction( mActionNewStructure2D );
    mStructure2DButton->addAction( mActionImportStructure2D );
    toolBar->addWidget( mStructure2DButton );
  }

  toolBar->addAction( mActionHydraulicNetworkProperties );
  mActionHydraulicNetworkProperties->setCheckable( true );
  mElementPropertiesWidget->setAction( mActionHydraulicNetworkProperties );

  toolBar->addAction( mActionRemoveElement );
  mActionRemoveElement->setEnabled( false );
  connect( mActionRemoveElement, &QAction::triggered, this, &ReosHydraulicNetworkWidget::onSelectedElementRemoved );

  toolBar->addAction( mActionZoomToNetworkExtent );
  connect( mActionZoomToNetworkExtent, &QAction::triggered, this, &ReosHydraulicNetworkWidget::onZoomToNetworkExtent );

  connect( network, &ReosHydraulicNetwork::hasBeenReset, this, &ReosHydraulicNetworkWidget::onModuleReset );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicNetworkWidget::onElementAdded );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementWillBeRemoved, this, &ReosHydraulicNetworkWidget::onElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementPositionHasChanged, this, &ReosHydraulicNetworkWidget::onElementChanged );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::loaded, this, &ReosHydraulicNetworkWidget::onNetworkLoaded );

  connect( mMapToolAddHydrographJunction, &ReosMapToolDrawPoint::drawn, this, [this]( const QPointF & p )
  {
    ReosSpatialPosition sp( p, mMap->mapCrs() );
    ReosHydraulicNetworkElement *elem = mHydraulicNetwork->addElement( new ReosHydrographJunction( sp, mHydraulicNetwork ) );
    onElementSelected( mMapItems.value( elem ).get() );
  } );

  connect( mMapToolAddHydrographRouting, &ReosMapToolDrawHydrographRouting::finished, this, &ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish );

  connect( mMapToolSelectNetworkElement, &ReosMapToolSelectMapItem::found, this, &ReosHydraulicNetworkWidget::onElementSelected );
  connect( mMapToolSelectNetworkElement, &ReosMapToolSelectMapItem::foundDoubleClick, this, &ReosHydraulicNetworkWidget::onElementSelectedDoubleClick );
  connect( mMap, &ReosMap::mapItemFound, this, &ReosHydraulicNetworkWidget::onElementSelected );
  connect( mMap, &ReosMap::mapItemFoundDoubleClick, this, &ReosHydraulicNetworkWidget::onElementSelectedDoubleClick );

  connect( mActionImportStructure2D, &QAction::triggered, this, &ReosHydraulicNetworkWidget::onImportStructure2D );

  onElementSelected( nullptr );

  ui->mHydraulicSchemeCombo->setModel( mHydraulicNetwork->hydraulicSchemeCollection() );
  connect( ui->mHydraulicShcemeAddButton, &QToolButton::clicked, this, &ReosHydraulicNetworkWidget::onAddHydraulicScheme );
  connect( ui->mHydraulicShcemeRemoveButton, &QToolButton::clicked, this, &ReosHydraulicNetworkWidget::onRemoveHydraulicScheme );
  connect( ui->mHydraulicShcemeRenameButton, &QToolButton::clicked, this, &ReosHydraulicNetworkWidget::onRenameHydraulicScheme );
  connect( ui->mHydraulicSchemeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), this, &ReosHydraulicNetworkWidget::onCurrentSchemeChange );

  connect( mHydraulicNetwork, &ReosHydraulicNetwork::timeStepChanged, this, [this]
  {
    emit mapTimeStepChanged();
  } );

  mSchemeWidget = new ReosHydraulicSchemeWidget( network->context(), this );
  mSchemeWidget->hideName();
  QMenu *menu = new QMenu( ui->mSchemeSettingsButton );
  QWidgetAction *wa = new QWidgetAction( this );
  wa->setDefaultWidget( mSchemeWidget );
  menu->addAction( wa );
  ui->mSchemeSettingsButton->setMenu( menu );
  ui->mSchemeSettingsButton->setPopupMode( QToolButton::InstantPopup );
  connect( mSchemeWidget, &ReosHydraulicSchemeWidget::meteoModelChange, ui->mMeteoModelLabel, &QLabel::setText );

  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->schemeCount() > 1 );

  changeCurrentScheme( mHydraulicNetwork->currentScheme() );
}

ReosHydraulicNetworkWidget::~ReosHydraulicNetworkWidget()
{
  delete ui;
}

void ReosHydraulicNetworkWidget::closePropertiesWidget()
{
  unselectCurrentElement();
  mElementPropertiesWidget->setCurrentElement( nullptr, mGuiContext );
  mElementPropertiesWidget->close();
}

ReosDuration ReosHydraulicNetworkWidget::mapTimeStep() const
{
  if ( mCurrentSelectedElement )
    return mCurrentSelectedElement->mapTimeStep();

  return ReosDuration();
}

void ReosHydraulicNetworkWidget::onElementAdded( ReosHydraulicNetworkElement *elem, bool select )
{
  NetworkItem item;
  item.reset( mMapItemFactory.createMapItem( elem, mMap ) );
  if ( item )
    mMapItems[elem] =  item ;

  addGeometryStructure( elem );

  if ( select && elem->isAutoSelectable() )
    onElementSelected( item.get() );
}

void ReosHydraulicNetworkWidget::onElementRemoved( ReosHydraulicNetworkElement *elem )
{
  if ( mCurrentSelectedElement == elem )
    onElementSelected( nullptr );

  removeGeometryStructure( elem );
  mMapItems.remove( elem );
}

void ReosHydraulicNetworkWidget::onElementChanged( ReosHydraulicNetworkElement *elem )
{
  auto it = mMapItems.constFind( elem );
  if ( it != mMapItems.constEnd() )
    mMapItemFactory.updateMapItem( elem, it.value().get() );
}

void ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish()
{
  QList<ReosMapItem *> itemList = mMapToolAddHydrographRouting->linkedItems();
  Q_ASSERT( itemList.count() == 2 );

  ReosHydrographSource *source = qobject_cast<ReosHydrographSource *>( mHydraulicNetwork->getElement( itemList.at( 0 )->description() ) );
  Q_ASSERT( source );

  ReosHydrographNode *destination = qobject_cast<ReosHydrographNode *>( mHydraulicNetwork->getElement( itemList.at( 1 )->description() ) );
  Q_ASSERT( destination );

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->addElement( new ReosHydrographRoutingLink( source, destination, mHydraulicNetwork ) );

  onElementSelected( mMapItems.value( elem ).get() );
}

void ReosHydraulicNetworkWidget::onElementSelected( ReosMapItem *item )
{
  if ( item && !item->description().contains( ReosHydraulicNetworkElement::staticType() ) )
    return;

  unselectCurrentElement();
  mActionRemoveElement->setEnabled( item != nullptr );
  ReosHydraulicNetworkElement *elem = nullptr;

  if ( item )
    elem = mHydraulicNetwork->getElement( item->description() );

  if ( !elem )
  {
    mElementPropertiesWidget->setCurrentElement( nullptr, ReosGuiContext( this ) );
    mExtraItemSelection.reset( );
    ui->mElementListView->setCurrentIndex( QModelIndex() );
    emit mapTimeStepChanged();
    return;
  }

  ReosGuiContext guiContext = mGuiContext;
  guiContext.addMapItems( item );

  mMapItemFactory.selectItem( elem, item );
  mExtraItemSelection.reset( mMapItemFactory.createExtraItemSelected( elem, mMap ) );
  mActionRemoveElement->setEnabled( elem->isRemovable() );

  ui->mElementListView->blockSignals( true );
  ui->mElementListView->setCurrentIndex( mElementModel->elementToIndex( elem ) );
  ui->mElementListView->blockSignals( false );

  connect( elem, &ReosHydraulicNetworkElement::mapTimeStepChanged, this, &ReosHydraulicNetworkWidget::mapTimeStepChanged );

  mElementPropertiesWidget->setCurrentElement( elem, guiContext );
  mStructure2dToolBar->setCurrentStructure2DPropertiesWidget(
    qobject_cast<ReosHydraulicStructure2DProperties *>( mElementPropertiesWidget->currentElementWidget() ) );

  mCurrentSelectedElement = elem;

  emit mapTimeStepChanged();
}

void ReosHydraulicNetworkWidget::onElementSelectedDoubleClick( ReosMapItem *item )
{
  onElementSelected( item );
  if ( item && item->description().contains( ReosHydraulicNetworkElement::staticType() ) )
  {
    mActionHydraulicNetworkProperties->setChecked( true );
    mElementPropertiesWidget->show();
  }
}

void ReosHydraulicNetworkWidget::onSelectedElementRemoved()
{
  mMapToolSelectNetworkElement->clearHoveredItem();
  mMap->defaultMapTool()->clearHoveredItem();

  if ( !mCurrentSelectedElement )
    return;

  if ( QMessageBox::warning( this, tr( "Remove Hydraulic Network Element" ), tr( "This action will remove definitly the element \"%1\"\n"
                             "Do you want to proceed?" ).arg( mCurrentSelectedElement->elementNameParameter()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
    return;

  if ( mCurrentSelectedElement )
    mHydraulicNetwork->removeElement( mCurrentSelectedElement );

  mCurrentSelectedElement = nullptr;
  mActionRemoveElement->setEnabled( false );
}

void ReosHydraulicNetworkWidget::onZoomToNetworkExtent()
{
  mMap->setExtent( mHydraulicNetwork->networkExtent() );
}

void ReosHydraulicNetworkWidget::onImportStructure2D()
{
  ReosImportHydraulicStructureDialog *dial = new ReosImportHydraulicStructureDialog( mGuiContext );

  if ( dial->exec() )
    dial->createStructure2d( mHydraulicNetwork->context() );

  dial->deleteLater();
}

void ReosHydraulicNetworkWidget::onAddHydraulicScheme()
{
  ReosFormDialog *dia = new ReosFormDialog( this );

  std::unique_ptr<ReosHydraulicScheme> scheme = std::make_unique<ReosHydraulicScheme>();
  scheme->schemeName()->setValue( tr( "New Hydraulic Scheme" ) );
  const ReosHydraulicNetworkContext context = mHydraulicNetwork->context();
  scheme->setMeteoModel( context.watershedModule()->meteoModelsCollection()->meteorologicModel( 0 ) );
  ReosHydraulicSchemeWidget *widget = new ReosHydraulicSchemeWidget( context, dia );
  widget->setScheme( scheme.get() );
  dia->addWidget( widget );

  if ( dia->exec() )
  {
    mHydraulicNetwork->addExistingScheme( scheme.release() );
    ui->mHydraulicSchemeCombo->setCurrentIndex( mHydraulicNetwork->schemeCount() - 1 );
  }

  dia->deleteLater();

  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->schemeCount() > 1 );
}

void ReosHydraulicNetworkWidget::onRemoveHydraulicScheme()
{
  int currentSchemeIndex = ui->mHydraulicSchemeCombo->currentIndex();
  ReosHydraulicScheme *scheme = mHydraulicNetwork->scheme( currentSchemeIndex );
  if ( scheme )
  {
    if ( QMessageBox::warning( this, tr( "Remove Hydraulic Scheme" ),
                               tr( "Do you want to remove the hydraulic scheme: \n\n%1" ).arg( scheme->schemeName()->value() ),
                               QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
      return;

    mHydraulicNetwork->removeScheme( currentSchemeIndex );

    ui->mHydraulicSchemeCombo->blockSignals( true );
    if ( mHydraulicNetwork->schemeCount() > 0 )
    {
      mHydraulicNetwork->setCurrentScheme( 0 );
      ui->mHydraulicSchemeCombo->setCurrentIndex( 0 );
    }
    else
    {
      mHydraulicNetwork->setCurrentScheme( -1 );
      ui->mHydraulicSchemeCombo->setCurrentIndex( -1 );
    }
    ui->mHydraulicSchemeCombo->blockSignals( false );
  }

  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->schemeCount() > 1 );
}

void ReosHydraulicNetworkWidget::onRenameHydraulicScheme()
{
  if ( !mCurrentHydraulicScheme )
    return;
  ReosFormDialog *dial = new ReosFormDialog( this );
  ReosParameterString string( tr( "New scheme name" ) );
  string.setValue( mCurrentHydraulicScheme->schemeName()->value() );
  dial->addParameter( &string );

  if ( dial->exec() )
    mCurrentHydraulicScheme->schemeName()->setValue( string.value() );
}

void ReosHydraulicNetworkWidget::onCurrentSchemeChange( int index )
{
  int prvIndex = mHydraulicNetwork->currentSchemeIndex();

  ReosHydraulicScheme *newScheme = mHydraulicNetwork->scheme( index );
  ReosHydraulicNetworkElementCompatibilty compatibility = mHydraulicNetwork->checkSchemeCompatibility( newScheme );

  if ( !compatibility.isCompatible )
  {
    ReosNetworkCompatibilityDialog *diag =
      new ReosNetworkCompatibilityDialog( tr( "The new selected scheme is incompatible with the state"
                                          " of the hydraulic network for the following reason(s):" ),
                                          compatibility,
                                          tr( "If you continue, some elements of the network could be altered or removed definitively.\n"
                                              "Do you want to continue ?" ),
                                          ReosGuiContext( mGuiContext, this ) );
    if ( !diag->exec() )
    {
      ui->mHydraulicSchemeCombo->blockSignals( true );
      ui->mHydraulicSchemeCombo->setCurrentIndex( prvIndex );
      ui->mHydraulicSchemeCombo->blockSignals( false );
      diag->deleteLater();
      return;
    }
  }

  mHydraulicNetwork->changeScheme( index );
  changeCurrentScheme( mHydraulicNetwork->currentScheme() );
  emit mapTimeStepChanged();
}

void ReosHydraulicNetworkWidget::onNetworkLoaded()
{
  ui->mHydraulicSchemeCombo->setCurrentIndex( mHydraulicNetwork->currentSchemeIndex() );
  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->schemeCount() > 1 );
}

void ReosHydraulicNetworkWidget::onModuleReset()
{
  mCurrentSelectedElement = nullptr;
  mElementPropertiesWidget->setCurrentElement( nullptr, mGuiContext );
  mGeometryStructures.clear();
  mMapItems.clear();
}

void ReosHydraulicNetworkWidget::onClosed()
{
  unselectCurrentElement();
  closePropertiesWidget();
  setMapItemVisible( false );
}

void ReosHydraulicNetworkWidget::onOpened()
{
  setMapItemVisible( true );
}

void ReosHydraulicNetworkWidget::onMapCrsChanged()
{
  for ( auto it = mMapItems.begin(); it != mMapItems.end(); ++it )
    mMapItemFactory.updateMapItem( it.key(), it.value().get() );
}


ReosStructure2dToolBar *ReosHydraulicNetworkWidget::structure2dToolBar() const
{
  return mStructure2dToolBar;
}

void ReosHydraulicNetworkWidget::unselectCurrentElement()
{
  if ( mCurrentSelectedElement )
  {
    disconnect( mCurrentSelectedElement, &ReosHydraulicNetworkElement::mapTimeStepChanged, this, &ReosHydraulicNetworkWidget::mapTimeStepChanged );
    auto it = mMapItems.constFind( mCurrentSelectedElement );
    if ( it != mMapItems.constEnd() )
      mMapItemFactory.unselectItem( mCurrentSelectedElement, it.value().get() );
    mCurrentSelectedElement = nullptr;
    mStructure2dToolBar->setCurrentStructure2DPropertiesWidget( nullptr );
  }
}

void ReosHydraulicNetworkWidget::setMapItemVisible( bool visible )
{
  for ( NetworkItem &item : mMapItems )
    item->setVisible( visible );

  if ( mExtraItemSelection )
    mExtraItemSelection->setVisible( visible );

  if ( visible )
    for ( ReosGeometryStructure *structure : std::as_const( mGeometryStructures ) )
      mMap->addSnappableStructure( structure );
  else
    for ( ReosGeometryStructure *structure : std::as_const( mGeometryStructures ) )
      mMap->removeSnappableStructure( structure );
}

void ReosHydraulicNetworkWidget::addGeometryStructure( ReosHydraulicNetworkElement *elem )
{
  if ( elem->type() == ReosHydraulicStructure2D::staticType() )
  {
    ReosGeometryStructure *structure = qobject_cast<ReosHydraulicStructure2D *>( elem )->geometryStructure();
    if ( structure )
    {
      if ( isVisible() )
        mMap->addSnappableStructure( structure );
      mGeometryStructures.append( structure );
      connect( structure, &ReosDataObject::dataChanged, this, [this, elem] {onElementChanged( elem );} );
    }
  }
}

void ReosHydraulicNetworkWidget::removeGeometryStructure( ReosHydraulicNetworkElement *elem )
{
  if ( elem->type() == ReosHydraulicStructure2D::staticType() )
  {
    ReosGeometryStructure *structure = qobject_cast<ReosHydraulicStructure2D *>( elem )->geometryStructure();
    if ( structure )
    {
      mMap->removeSnappableStructure( qobject_cast<ReosHydraulicStructure2D *>( elem )->geometryStructure() );
    }
    mGeometryStructures.removeOne( structure );
  }
}

void ReosHydraulicNetworkWidget::changeCurrentScheme( ReosHydraulicScheme *scheme )
{
  mCurrentHydraulicScheme = scheme;

  mSchemeWidget->blockSignals( true );
  mSchemeWidget->setScheme( scheme );
  mSchemeWidget->blockSignals( false );

  if ( scheme && scheme->meteoModel() )
    ui->mMeteoModelLabel->setText( scheme->meteoModel()->name()->value() );
}

ReosHydraulicElementWidget::ReosHydraulicElementWidget( QWidget *parent )
  : QWidget( parent )
{}

ReosHydraulicNetworkDockWidget::ReosHydraulicNetworkDockWidget( ReosHydraulicNetwork *network, const ReosGuiContext &context )
  : ReosDockWidget( tr( "Hydraulic Network" ), context.parent() )
{
  DockWidgetFeatures feat = features();
  feat.setFlag( DockWidgetFeature::DockWidgetClosable, false );
  setFeatures( feat );
  mHydraulicNetworkWidget = new ReosHydraulicNetworkWidget( network, ReosGuiContext( context, this ) );
  setWidget( mHydraulicNetworkWidget );
}

void ReosHydraulicNetworkDockWidget::closePropertieWidget()
{
  mHydraulicNetworkWidget->closePropertiesWidget();
}

ReosHydraulicNetworkWidget *ReosHydraulicNetworkDockWidget::hydraulicNetworkWidget() const
{
  return mHydraulicNetworkWidget;
}
