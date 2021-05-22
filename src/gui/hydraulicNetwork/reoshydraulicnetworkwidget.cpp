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

static ReosMapItem *createHydrographSourceWatershedItem( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographSourceWatershed *hws = qobject_cast<ReosHydrographSourceWatershed *>( elem );
  if ( hws )
  {
    std::unique_ptr<ReosMapMarkerEmptySquare> marker = std::make_unique<ReosMapMarkerEmptySquare>( map, hws->position() );
    marker->setWidth( 12 );
    marker->setExternalWidth( 20 );
    marker->setColor( QColor( 0, 155, 242 ) );
    marker->setExternalColor( Qt::white );
    marker->setZValue( 10 );
    marker->setDescription( hws->type() );
    return marker.release();
  }

  return nullptr;
}

static void updateHydrographSourceWatershedItem( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  ReosHydrographSourceWatershed *hws = qobject_cast<ReosHydrographSourceWatershed *>( elem );
  ReosMapMarker *markerItem = static_cast<ReosMapMarker *>( item );

  markerItem->resetPoint( hws->watershed()->outletPoint() );
}


static ReosMapItem *createHydrographJunctionItem( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographJunction *hj = qobject_cast<ReosHydrographJunction *>( elem );
  if ( hj )
  {
    std::unique_ptr<ReosMapMarkerEmptyCircle> marker = std::make_unique<ReosMapMarkerEmptyCircle>( map, hj->position() );
    marker->setWidth( 12 );
    marker->setExternalWidth( 22 );
    marker->setColor( QColor( 0, 155, 242 ) );
    marker->setExternalColor( Qt::white );
    marker->setZValue( 10 );
    marker->setDescription( hj->type() );
    return marker.release();
  }

  return nullptr;
}

ReosHydraulicNetworkWidget::ReosHydraulicNetworkWidget( ReosHydraulicNetwork *network, ReosMap *map, QWidget *parent ) :
  QWidget( parent )
  , ui( new Ui::ReosHydraulicNetworkWidget )
  , mHydraulicNetwork( network )
  , mMap( map )
  , mActionAddHydrographJunction( new QAction( tr( "Add hydrograph junction" ), this ) )
  , mMapToolAddHydrographJunction( new ReosMapToolDrawPoint( mMap ) )
{
  ui->setupUi( this );

  mMapItemFactory.addCreationFunction( QStringLiteral( "node:hydrograph:source:watershed" ), &createHydrographSourceWatershedItem );
  mMapItemFactory.addCreationFunction( QStringLiteral( "node:hydrograph:junction" ), &createHydrographJunctionItem );
  mMapItemFactory.addUpdateFunction( QStringLiteral( "node:hydrograph:source:watershed" ), &updateHydrographSourceWatershedItem );

  QToolBar *toolBar = new QToolBar( this );
  ui->groupBoxHydrographTransfer->layout()->addWidget( toolBar );

  toolBar->addAction( mActionAddHydrographJunction );
  mMapToolAddHydrographJunction->setAction( mActionAddHydrographJunction );
  mMapToolAddHydrographJunction->setSearchingItemDecription( QStringLiteral( "node:hydrograph" ) );
  mMapToolAddHydrographJunction->setSearchItemWhenMoving( true );

  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementAdded, this, &ReosHydraulicNetworkWidget::onElementAdded );
  connect( mHydraulicNetwork, &ReosHydraulicNetwork::elementPostionHasChanged, this, &ReosHydraulicNetworkWidget::onElementChanged );

  connect( mMapToolAddHydrographJunction, &ReosMapToolDrawPoint::drawn, this, [this]( const QPointF & p )
  {
    mHydraulicNetwork->addElement( new ReosHydrographJunction( p, mHydraulicNetwork ) );
  } );
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

void ReosHydraulicNetworkWidget::onElementChanged( ReosHydraulicNetworkElement *elem )
{
  auto it = mMapItems.constFind( elem );
  if ( it != mMapItems.constEnd() )
    mMapItemFactory.updateMapItem( elem, it.value().get() );
}
