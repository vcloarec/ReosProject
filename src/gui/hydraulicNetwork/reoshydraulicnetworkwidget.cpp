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
#include "reoswatershed.h"
#include "reosmaptool.h"
#include "reoshydraulicelementpropertieswidget.h"
#include "reosstyleregistery.h"
#include "reoshydraulicscheme.h"
#include "reoshydraulicschemewidget.h"
#include "reosparameter.h"

ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosWatershedModule *watershedModule, const ReosGuiContext &context ) :
  QWidget( context.parent() )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mGuiContext( context )
  , mHydraulicNetwork( network )
  , mMap( context.map() )
  , mActionSelectNetworkElement( new QAction( QPixmap( QStringLiteral( ":/images/selectHydraulicElement.svg" ) ), tr( "Select Hydraulic Network Element" ), this ) )
  , mMapToolSelectNetworkElement( new ReosMapToolSelectMapItem( context.map(), ReosHydraulicNetworkElement::staticType() ) )
  , mActionAddHydrographJunction( new QAction( QPixmap( QStringLiteral( ":/images/addHydrographJunction.svg" ) ), tr( "Add Junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
  , mActionAddHydrographRouting( new QAction( QPixmap( QStringLiteral( ":/images/addHydrographRouting.svg" ) ), tr( "Add Link" ), this ) )
  , mMapToolAddHydrographRouting( new ReosMapToolDrawHydrographRouting( mHydraulicNetwork, mMap ) )
  , mActionHydraulicNetworkProperties( new QAction( QPixmap( QStringLiteral( ":/images/hydraulicProperties.svg" ) ), tr( "Hydraulic Element Properties" ), this ) )
  , mElementPropertiesWidget( new ReosHydraulicElementPropertiesActionWidget( network, ReosGuiContext( context, this ) ) )
  , mActionMoveHydrographJunction( new QAction( QPixmap( QStringLiteral( ":/images/moveHydrographJunction.svg" ) ),  tr( "Move Junction" ), this ) )
  , mMapToolMoveHydrographJunction( new ReosMapToolMoveHydraulicNetworkElement( network, context.map() ) )
  , mActionNewStructure2D( new QAction( QPixmap( QStringLiteral( ":/images/addHydraulicStructure2D.svg" ) ), tr( "Structure 2D" ), this ) )
  , mMapToolNewStructure2D( new ReosMapToolNewStructure2D( network, mMap ) )
  , mActionRemoveElement( new QAction( QPixmap( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Hydraulic Element" ), this ) )
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
  toolBar->setIconSize( ReosStyleRegistery::instance()->toolBarIconSize() );

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
  mMapToolAddHydrographRouting->setSearchingItemDecription( ReosHydrographNode::staticType() );
  mMapToolAddHydrographRouting->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionMoveHydrographJunction );
  mActionMoveHydrographJunction->setCheckable( true );
  mMapToolMoveHydrographJunction->setAction( mActionMoveHydrographJunction );
  mMapToolMoveHydrographJunction->setSearchingItemDecription( ReosHydrographJunction::staticType() );
  mMapToolMoveHydrographJunction->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionNewStructure2D );
  mActionNewStructure2D->setCheckable( true );
  mMapToolNewStructure2D->setAction( mActionNewStructure2D );

  toolBar->addAction( mActionHydraulicNetworkProperties );
  mActionHydraulicNetworkProperties->setCheckable( true );
  mElementPropertiesWidget->setAction( mActionHydraulicNetworkProperties );

  toolBar->addAction( mActionRemoveElement );
  mActionRemoveElement->setEnabled( false );

  connect( network, &ReosHydraulicNetwork::hasBeenReset, this, &ReosHydraulicNetworkWidget::onModuleReset );

  connect( mActionRemoveElement, &QAction::triggered, this, &ReosHydraulicNetworkWidget::onSelectedElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicNetworkWidget::onElementAdded );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosHydraulicNetworkWidget::onElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementPositionHasChanged, this, &ReosHydraulicNetworkWidget::onElementChanged );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::loaded, this, &ReosHydraulicNetworkWidget::onNetworkLoaded );

  connect( mMapToolAddHydrographJunction, &ReosMapToolDrawPoint::drawn, this, [this]( const QPointF & p )
  {
    ReosHydraulicNetworkElement *elem = mHydraulicNetwork->addElement( new ReosHydrographJunction( p, mHydraulicNetwork ) );
    onElementSelected( mMapItems.value( elem ).get() );
  } );

  connect( mMapToolAddHydrographRouting, &ReosMapToolDrawHydrographRouting::finished, this, &ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish );

  connect( mMapToolSelectNetworkElement, &ReosMapToolSelectMapItem::found, this, &ReosHydraulicNetworkWidget::onElementSelected );

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
    mMap->setTimeStep( mHydraulicNetwork->currentTimeStep() );
  } );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::timeStepChanged, this, [this]
  {
    mMap->setTimeStep( mHydraulicNetwork->currentTimeStep() );
  } );
  ui->mHydraulicShcemeRemoveButton->setEnabled( mHydraulicNetwork->hydraulicSchemeCollection()->schemeCount() > 1 );
}

ReosHydraulicNetworkWidget::~ReosHydraulicNetworkWidget()
{
  delete ui;
}

void ReosHydraulicNetworkWidget::closePropertiesWidget()
{
  mElementPropertiesWidget->setCurrentElement( nullptr, mGuiContext );
  mElementPropertiesWidget->close();
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
  if ( mCurrentSelectedElement )
  {
    auto it = mMapItems.constFind( mCurrentSelectedElement );
    if ( it != mMapItems.constEnd() )
      mMapItemFactory.unselectItem( mCurrentSelectedElement, it.value().get() );
    mCurrentSelectedElement = nullptr;
  }

  mActionRemoveElement->setEnabled( item != nullptr );

  if ( !item )
  {
    mElementPropertiesWidget->setCurrentElement( nullptr, ReosGuiContext( this ) );
    ui->mNameWidget->setString( nullptr );
    mExtraItemSelection.reset( );
    return;
  }

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->getElement( item->description() );

  ReosGuiContext guiContext = mGuiContext;
  guiContext.addMapItems( item );
  mElementPropertiesWidget->setCurrentElement( elem, guiContext );

  if ( elem )
  {
    mMapItemFactory.selectItem( elem, item );
    ui->mNameWidget->setString( elem->elementName() );
    mExtraItemSelection.reset( mMapItemFactory.createExtraItemSelected( elem, mMap ) );
    mActionRemoveElement->setEnabled( elem->isRemovable() );
  }

  mCurrentSelectedElement = elem;
}

void ReosHydraulicNetworkWidget::onSelectedElementRemoved()
{
  mMapToolSelectNetworkElement->clearHoveredItem();

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

void ReosHydraulicNetworkWidget::onHydraulicSchemeChange( int index )
{
  ui->mHydraulicSchemeCombo->setCurrentIndex( index );
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
  setMapItemVisible( false );
}

void ReosHydraulicNetworkWidget::onOpened()
{
  setMapItemVisible( true );
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

ReosHydraulicElementWidget::ReosHydraulicElementWidget( QWidget *parent ):  QWidget( parent )
{}

ReosHydraulicNetworkDockWidget::ReosHydraulicNetworkDockWidget( ReosHydraulicNetwork *network, ReosWatershedModule *watershedModule, const ReosGuiContext &context )
  : ReosDockWidget( tr( "Hydraulic Network" ), context.parent() )
{
  mHydraulicNetworkWidget = new ReosHydraulicNetworkWidget( network, watershedModule, ReosGuiContext( context, this ) );
  setWidget( mHydraulicNetworkWidget );
}

void ReosHydraulicNetworkDockWidget::closePropertieWidget()
{
  mHydraulicNetworkWidget->closePropertiesWidget();
}
