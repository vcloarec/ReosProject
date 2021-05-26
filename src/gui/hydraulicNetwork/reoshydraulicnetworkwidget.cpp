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

#include "reoshydrographsource.h"
#include "reoshydrographtransfer.h"
#include "reoswatershed.h"
#include "reosmaptool.h"
#include "reoshydraulicelementpropertieswidget.h"

ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosMap *map, QWidget *parent ) :
  QWidget( parent )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mHydraulicNetwork( network )
  , mMap( map )
  , mActionSelectNetworkElement( new QAction( tr( "Select hydraulic network element" ), this ) )
  , mMapToolSelectNetworkElement( new ReosMapToolSelectMapItem( map, ReosHydraulicNetworkElement::hydraulicElementType() ) )
  , mActionAddHydrographJunction( new QAction( tr( "Add hydrograph junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
  , mActionAddHydrographRouting( new QAction( tr( "Add hydrograph routing" ), this ) )
  , mMapToolAddHydrographRouting( new ReosMapToolDrawHydrographRouting( mHydraulicNetwork, mMap ) )
  , mActionHydraulicNetworkProperties( new QAction( tr( "Hydraulic element properties" ), this ) )
  , mElementPropertiesWidget( new ReosHydraulicElementPropertiesWidget( this ) )
{
  ui->setupUi( this );

  QToolBar *toolBar = new QToolBar( this );
  ui->groupBoxHydrographTransfer->layout()->addWidget( toolBar );

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
  mMapToolAddHydrographRouting->setSearchingItemDecription( ReosHydrographSource::hydrographSourceType() );
  mMapToolAddHydrographRouting->setSearchItemWhenMoving( true );

  toolBar->addAction( mActionHydraulicNetworkProperties );
  mActionHydraulicNetworkProperties->setCheckable( true );
  mElementPropertiesWidget->setAction( mActionHydraulicNetworkProperties );

  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicNetworkWidget::onElementAdded );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementRemoved, this, &ReosHydraulicNetworkWidget::onElementRemoved );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementPositionHasChanged, this, &ReosHydraulicNetworkWidget::onElementChanged );

  connect( mMapToolAddHydrographJunction, &ReosMapToolDrawPoint::drawn, this, [this]( const QPointF & p )
  {
    mHydraulicNetwork->addElement( new ReosHydrographJunction( p, mHydraulicNetwork ) );
  } );

  connect( mMapToolAddHydrographRouting, &ReosMapToolDrawHydrographRouting::finished, this, &ReosHydraulicNetworkWidget::onDrawHydrographRoutingFinish );

  connect( mMapToolSelectNetworkElement, &ReosMapToolSelectMapItem::found, this, &ReosHydraulicNetworkWidget::onElementSelected );
}

ReosHydraulicNetworkWidget::~ReosHydraulicNetworkWidget()
{
  delete ui;
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

  mHydraulicNetwork->addElement( new ReosHydrographRouting( source, destination, mHydraulicNetwork ) );
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

  if ( !item )
    return;

  ReosHydraulicNetworkElement *elem = mHydraulicNetwork->getElement( item->description() );
  if ( elem )
  {
    mMapItemFactory.selectItem( elem, item );
    mCurrentSelectedElement = elem;
    mElementPropertiesWidget->setCurrentElement( elem );
  }
}
