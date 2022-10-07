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

ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosWatershedModule *watershedModule, const ReosGuiContext &context ) :
  QWidget( context.parent() )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mGuiContext( context )
  , mHydraulicNetwork( network )
  , mMap( context.map() )
  , mStructure2dToolBar( new ReosStructure2dToolBar )
  , mActionSelectNetworkElement( new QAction( QIcon( QStringLiteral( ":/images/selectHydraulicElement.svg" ) ), tr( "Select Hydraulic Network Element" ), this ) )
  , mMapToolSelectNetworkElement( new ReosMapToolSelectMapItem( context.map(), ReosHydraulicNetworkElement::staticType() ) )
  , mActionAddHydrographJunction( new QAction( QIcon( QStringLiteral( ":/images/addHydrographJunction.svg" ) ), tr( "Add Junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
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

  ui->mNameWidget->setDefaultName( tr( "Name" ) );

  ReosHydraulicNetworkDockWidget *dockParent = qobject_cast<ReosHydraulicNetworkDockWidget *>( context.parent() );
  if ( dockParent )
  {
    connect( dockParent, &ReosHydraulicNetworkDockWidget::shown, this, &ReosHydraulicNetworkWidget::onOpened );
    connect( dockParent, &ReosHydraulicNetworkDockWidget::closed, this, &ReosHydraulicNetworkWidget::onClosed );
  }

  QToolBar *toolBar = new QToolBar( this );
  ui->mainLayout->insertWidget( 0, toolBar );
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize( this ) );

  toolBar->addAction( mActionSelectNetworkElement );
  mActionSelectNetworkElement->setCheckable( true );
  mMapToolSelectNetworkElement->setAction( mActionSelectNetworkElement );
  mMapToolSelectNetworkElement->setSearchItemWhenMoving( true );
  mMapToolSelectNetworkElement->setCursor( Qt::ArrowCursor );

  toolBar->addAction( mActionAddHydrographJunction );
  mActionAddHydrographJunction->setCheckable( true );
  mMapToolAddHydrographJunction->setAction( mActionAddHydrographJunction );

  toolBar->addAction( mActionAddHydrographRouting );
  mActionAddHydrographRouting->setCheckable( true );
  mMapToolAddHydrographRouting->setAction( mActionAddHydrographRouting );
  mMapToolAddHydrographRouting->addSearchingItemDescription( ReosHydrographNode::staticType() );
  mMapToolAddHydrographRouting->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionMoveHydrographJunction );
  mActionMoveHydrographJunction->setCheckable( true );
  mMapToolMoveHydrographJunction->setAction( mActionMoveHydrographJunction );
  mMapToolMoveHydrographJunction->addSearchingItemDescription( ReosHydrographJunction::staticType() );
  mMapToolMoveHydrographJunction->setSearchItemWhenMoving( true );

  mActionNewStructure2D->setCheckable( true );
  if ( !ReosSimulationEngineRegistery::instance()->canImportSrtucture2D() )
  {
    toolBar->addAction( mActionNewStructure2D );
    mMapToolNewStructure2D->setAction( mActionNewStructure2D );
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
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosHydraulicNetworkWidget::onElementRemoved );
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
  connect( mMap, &ReosMap::mapItemFound, this, &ReosHydraulicNetworkWidget::onElementSelected );

  onElementSelected( nullptr );

  ui->mHydraulicSchemeCombo->setModel( mHydraulicNetwork->hydraulicSchemeCollection() );
  connect( ui->mHydraulicShcemeAddButton, &QToolButton::clicked, this, &ReosHydraulicNetworkWidget::onAddHydraulicScheme );
  connect( ui->mHydraulicShcemeRemoveButton, &QToolButton::clicked, this, &ReosHydraulicNetworkWidget::onRemoveHydraulicScheme );
  QMenu *schemeMenu = new QMenu( ui->mHydraulicSchemeSettingsButton );
  ReosHydraulicSchemeWidgetAction *wa = new ReosHydraulicSchemeWidgetAction( mHydraulicNetwork, schemeMenu );
  schemeMenu->addAction( wa );
  ui->mHydraulicSchemeSettingsButton->setMenu( schemeMenu );

  connect( ui->mHydraulicSchemeCombo, QOverload<int>::of( &QComboBox::currentIndexChanged ), wa, [wa, this]( int index )
  {
    mHydraulicNetwork->changeScheme( index );
    wa->setCurrentScheme( mHydraulicNetwork->currentScheme() );
    changeCurrentScheme( mHydraulicNetwork->currentScheme() );
    emit timeWindowChanged();
    emit mapTimeStepChanged();
  } );

  connect( mHydraulicNetwork, &ReosHydraulicNetwork::timeStepChanged, this, [this]
  {
    emit timeWindowChanged();
    emit mapTimeStepChanged();
  } );
  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() > 1 );

  connect( mMap, &ReosMap::crsChanged, this, &ReosHydraulicNetworkWidget::onMapCrsChanged );
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

ReosTimeWindow ReosHydraulicNetworkWidget::timeWindow() const
{
  ReosTimeWindow timeWindow;

  if ( mCurrentSelectedElement )
    timeWindow = mCurrentSelectedElement->timeWindow();

  if ( mHydraulicNetwork->currentScheme() )
    timeWindow = timeWindow.unite( mHydraulicNetwork->currentScheme()->timeWindow() );

  return timeWindow;
}

ReosDuration ReosHydraulicNetworkWidget::mapTimeStep() const
{
  if ( mCurrentSelectedElement )
    mCurrentSelectedElement->mapTimeStep();

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

  if ( !item )
  {
    mElementPropertiesWidget->setCurrentElement( nullptr, ReosGuiContext( this ) );
    ui->mNameWidget->setString( nullptr );
    mExtraItemSelection.reset( );
    emit timeWindowChanged();
    emit mapTimeStepChanged();
    return;
  }

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->getElement( item->description() );
  ReosGuiContext guiContext = mGuiContext;
  guiContext.addMapItems( item );

  if ( elem )
  {
    mMapItemFactory.selectItem( elem, item );
    ui->mNameWidget->setString( elem->elementName() );
    mExtraItemSelection.reset( mMapItemFactory.createExtraItemSelected( elem, mMap ) );
    mActionRemoveElement->setEnabled( elem->isRemovable() );

    connect( elem, &ReosHydraulicNetworkElement::timeWindowChanged, this, &ReosHydraulicNetworkWidget::timeWindowChanged );
    connect( elem, &ReosHydraulicNetworkElement::mapTimeStepChanged, this, &ReosHydraulicNetworkWidget::mapTimeStepChanged );
  }

  mElementPropertiesWidget->setCurrentElement( elem, guiContext );
  mStructure2dToolBar->setCurrentStructure2DPropertiesWidget(
    qobject_cast<ReosHydraulicStructure2DProperties *>( mElementPropertiesWidget->currentElementWidget() ) );

  mCurrentSelectedElement = elem;

  emit timeWindowChanged();
  emit mapTimeStepChanged();
}

void ReosHydraulicNetworkWidget::onSelectedElementRemoved()
{
  mMapToolSelectNetworkElement->clearHoveredItem();
  mMap->defaultMapTool()->clearHoveredItem();

  if ( !mCurrentSelectedElement )
    return;

  if ( QMessageBox::warning( this, tr( "Remove Hydraulic Network Element" ), tr( "This action will remove definitly the element \"%1\"\n"
                             "Do you want to proceed?" ).arg( mCurrentSelectedElement->elementName()->value() ),
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
}

void ReosHydraulicNetworkWidget::onAddHydraulicScheme()
{
  ReosFormDialog *dia = new ReosFormDialog( this );

  std::unique_ptr<ReosHydraulicScheme> scheme = std::make_unique<ReosHydraulicScheme>();
  scheme->schemeName()->setValue( tr( "New Hydraulic Scheme" ) );
  const ReosHydraulicNetworkContext context = mHydraulicNetwork->context();
  scheme->setMeteoModel( context.watershedModule()->meteoModelsCollection()->meteorologicModel( 0 ) );
  ReosHydraulicSchemeWidget *widget = new ReosHydraulicSchemeWidget( scheme.get(), context, dia );
  dia->addWidget( widget );

  if ( dia->exec() )
  {
    mHydraulicNetwork->hydraulicSchemeCollection()->addScheme( scheme.release() );
    ui->mHydraulicSchemeCombo->setCurrentIndex( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() - 1 );
  }

  dia->deleteLater();

  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() > 1 );
}

void ReosHydraulicNetworkWidget::onRemoveHydraulicScheme()
{
  int currentSchemeIndex = ui->mHydraulicSchemeCombo->currentIndex();
  ReosHydraulicScheme *scheme = mHydraulicNetwork->hydraulicSchemeCollection()->scheme( currentSchemeIndex );
  if ( scheme )
  {
    if ( QMessageBox::warning( this, tr( "Remove Hydraulic Scheme" ),
                               tr( "Do you want to remove the hydraulic scheme: \n\n%1" ).arg( scheme->schemeName()->value() ),
                               QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
      return;

    mHydraulicNetwork->hydraulicSchemeCollection()->removeScheme( currentSchemeIndex );

    ui->mHydraulicSchemeCombo->blockSignals( true );
    if ( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() > 0 )
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

  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() > 1 );
}

void ReosHydraulicNetworkWidget::onNetworkLoaded()
{
  ui->mHydraulicSchemeCombo->setCurrentIndex( mHydraulicNetwork->currentSchemeIndex() );
  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() > 1 );
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

void ReosHydraulicNetworkWidget::updateSchemeInfo()
{
  if ( mCurrentHydraulicScheme && mCurrentHydraulicScheme->meteoModel() )
    ui->mLabelMeteoModel->setText( mCurrentHydraulicScheme->meteoModel()->name()->value() );
  else
    ui->mLabelMeteoModel->setText( tr( "None" ) );

  if ( mCurrentHydraulicScheme )
  {
    ui->mLabelStartTime->setText( QLocale().toString( mCurrentHydraulicScheme->startTime()->value(), QLocale::LongFormat ) );
    ui->mLabelEndTime->setText( QLocale().toString( mCurrentHydraulicScheme->endTime()->value(), QLocale::LongFormat ) );
  }
  else
  {
    ui->mLabelStartTime->setText( tr( "Time not defined" ) );
    ui->mLabelEndTime->setText( tr( "Time not defined" ) );
  }
}

ReosStructure2dToolBar *ReosHydraulicNetworkWidget::structure2dToolBar() const
{
  return mStructure2dToolBar;
}

void ReosHydraulicNetworkWidget::unselectCurrentElement()
{
  if ( mCurrentSelectedElement )
  {
    disconnect( mCurrentSelectedElement, &ReosHydraulicNetworkElement::timeWindowChanged, this, &ReosHydraulicNetworkWidget::timeWindowChanged );
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
  if ( mCurrentHydraulicScheme )
    disconnect( mCurrentHydraulicScheme, &ReosDataObject::dataChanged, this, &ReosHydraulicNetworkWidget::updateSchemeInfo );

  mCurrentHydraulicScheme = scheme;

  updateSchemeInfo();

  if ( mCurrentHydraulicScheme )
    connect( mCurrentHydraulicScheme, &ReosDataObject::dataChanged, this, &ReosHydraulicNetworkWidget::updateSchemeInfo );
}

ReosHydraulicElementWidget::ReosHydraulicElementWidget( QWidget *parent )
  : QWidget( parent )
{}

ReosHydraulicNetworkDockWidget::ReosHydraulicNetworkDockWidget( ReosHydraulicNetwork *network, ReosWatershedModule *watershedModule, const ReosGuiContext &context )
  : ReosDockWidget( tr( "Hydraulic Network" ), context.parent() )
{
  DockWidgetFeatures feat = features();
  feat.setFlag( DockWidgetFeature::DockWidgetClosable, false );
  setFeatures( feat );
  mHydraulicNetworkWidget = new ReosHydraulicNetworkWidget( network, watershedModule, ReosGuiContext( context, this ) );
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
