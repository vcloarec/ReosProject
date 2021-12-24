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

#include "reoshydrographsource.h"
#include "reoshydrographtransfer.h"
#include "reoswatershed.h"
#include "reosmaptool.h"
#include "reoshydraulicelementpropertieswidget.h"

ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosMap *map, ReosWatershedModule *watershedModule, QWidget *parent ) :
  QWidget( parent )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mHydraulicNetwork( network )
  , mMap( map )
  , mActionSelectNetworkElement( new QAction( QPixmap( QStringLiteral( ":/images/neutral.svg" ) ), tr( "Select Hydraulic Network Element" ), this ) )
  , mMapToolSelectNetworkElement( new ReosMapToolSelectMapItem( map, ReosHydraulicNetworkElement::staticType() ) )
  , mActionAddHydrographJunction( new QAction( QPixmap( QStringLiteral( ":/images/addHydrographJunction.svg" ) ), tr( "Add Hydrograph Junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
  , mActionAddHydrographRouting( new QAction( QPixmap( QStringLiteral( ":/images/addHydrographRouting.svg" ) ), tr( "Add Hydrograph Routing" ), this ) )
  , mMapToolAddHydrographRouting( new ReosMapToolDrawHydrographRoutine( mHydraulicNetwork, mMap ) )
  , mActionHydraulicNetworkProperties( new QAction( QPixmap( QStringLiteral( ":/images/hydraulicProperties.svg" ) ), tr( "Hydraulic Element Properties" ), this ) )
  , mElementPropertiesWidget( new ReosHydraulicElementPropertiesWidget( watershedModule, this ) )
  , mActionMoveHydrographJunction( new QAction( QPixmap( QStringLiteral( ":/images/moveHydrographJunction.svg" ) ),  tr( "Move Hydrograph Junction" ), this ) )
  , mMapToolMoveHydrographJunction( new ReosMapToolMoveHydraulicNetworkElement( network, map ) )
  , mActionRemoveElement( new QAction( QPixmap( QStringLiteral( ":/images/remove.svg" ) ), tr( "Remove Hydraulic Element" ), this ) )
{
  ui->setupUi( this );

  QToolBar *toolBar = new QToolBar( this );
  layout()->addWidget( toolBar );

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
  mMapToolAddHydrographRouting->setSearchingItemDecription( ReosHydrographSource::staticType() );
  mMapToolAddHydrographRouting->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionMoveHydrographJunction );
  mActionMoveHydrographJunction->setCheckable( true );
  mMapToolMoveHydrographJunction->setAction( mActionMoveHydrographJunction );
  mMapToolMoveHydrographJunction->setSearchingItemDecription( ReosHydrographJunction::staticType() );
  mMapToolMoveHydrographJunction->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionHydraulicNetworkProperties );
  mActionHydraulicNetworkProperties->setCheckable( true );
  mElementPropertiesWidget->setAction( mActionHydraulicNetworkProperties );

  toolBar->addAction( mActionRemoveElement );
  mActionRemoveElement->setEnabled( false );

  connect( mActionRemoveElement, &QAction::triggered, this, &ReosHydraulicNetworkWidget::onSelectedElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicNetworkWidget::onElementAdded );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosHydraulicNetworkWidget::onElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementPositionHasChanged, this, &ReosHydraulicNetworkWidget::onElementChanged );

  connect( mMapToolAddHydrographJunction, &ReosMapToolDrawPoint::drawn, this, [this]( const QPointF & p )
  {
    ReosHydraulicNetworkElement *elem = mHydraulicNetwork->addElement( new ReosHydrographJunction( p, mHydraulicNetwork ) );
    onElementSelected( mMapItems.value( elem ).get() );
  } );

  connect( mMapToolAddHydrographRouting, &ReosMapToolDrawHydrographRoutine::finished, this, &ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish );

  connect( mMapToolSelectNetworkElement, &ReosMapToolSelectMapItem::found, this, &ReosHydraulicNetworkWidget::onElementSelected );
}

ReosHydraulicNetworkWidget::~ReosHydraulicNetworkWidget()
{
  delete ui;
}

ReosCalculationContext ReosHydraulicNetworkWidget::currentContext() const
{
  ReosCalculationContext context;
  context.setMeteorologicModel( meteoModelCollection->meteorologicModel( ui->comboBoxMeteo->currentIndex() ) );

  return context;
}

void ReosHydraulicNetworkWidget::setMeteoModelCollection( ReosMeteorologicModelsCollection *meteoCollection )
{
  meteoModelCollection = meteoCollection;
  ui->comboBoxMeteo->setModel( meteoCollection );
}

void ReosHydraulicNetworkWidget::onElementAdded( ReosHydraulicNetworkElement *elem )
{
  NetworkItem item;
  item.reset( mMapItemFactory.createMapItem( elem, mMap ) );
  if ( item )
    mMapItems[elem] =  item ;
}

void ReosHydraulicNetworkWidget::onElementRemoved( ReosHydraulicNetworkElement *elem )
{
  mMapItems.remove( elem );
  onElementSelected( nullptr );
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
    mElementPropertiesWidget->setCurrentElement( nullptr );
    return;
  }

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->getElement( item->description() );
  if ( elem )
    mMapItemFactory.selectItem( elem, item );
  mCurrentSelectedElement = elem;

  mElementPropertiesWidget->setCurrentElement( elem );
}

void ReosHydraulicNetworkWidget::onSelectedElementRemoved()
{
  mMapToolSelectNetworkElement->clearHoveredItem();

  if ( !mCurrentSelectedElement )
    return;
  if ( QMessageBox::warning( this, tr( "Remove Hydraulic Network Element" ), tr( "This action will remove definitly the element \"%1\"\n"
                             "Do you want to proceed?" ).arg( mCurrentSelectedElement->name()->value() ),
                             QMessageBox::Yes | QMessageBox::No, QMessageBox::No ) == QMessageBox::No )
    return;

  if ( mCurrentSelectedElement )
    mHydraulicNetwork->removeElement( mCurrentSelectedElement );

  mCurrentSelectedElement = nullptr;
  mActionRemoveElement->setEnabled( false );
}

