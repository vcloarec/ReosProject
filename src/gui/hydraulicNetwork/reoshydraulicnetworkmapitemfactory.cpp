/***************************************************************************
  reoshydraulicnetworkmapitemfactory.cpp - ReosHydraulicNetworkMapItemFactory

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
#include "reoshydraulicnetworkmapitemfactory.h"
#include "reoshydrographsource.h"
#include "reoshydrographtransfer.h"

//****************************************************
// Hydrograph Source Watershed

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
    marker->setDescription( hws->id() );
    return marker.release();
  }

  return nullptr;
}

static void updateHydrographSourceWatershedItem( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  ReosHydrographSourceWatershed *hws = qobject_cast<ReosHydrographSourceWatershed *>( elem );
  ReosMapMarker *markerItem = static_cast<ReosMapMarker *>( item );

  markerItem->resetPoint( hws->watershed()->outletPoint() );

  for ( ReosHydraulicLink *link : hws->links() )
    link->positionChanged();
}


//****************************************************
// Hydrograph Source junction

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
    marker->setDescription( hj->id() );
    return marker.release();
  }

  return nullptr;
}


//****************************************************
// Hydrograph routing link

static ReosMapItem *createHydrographRoutingLink( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographRouting *hr = qobject_cast<ReosHydrographRouting *>( elem );
  if ( hr )
  {
    std::unique_ptr<ReosMapPolyline> line = std::make_unique<ReosMapPolyline>( map );

    if ( hr->firstNode() && hr->secondNode() )
    {
      QPolygonF mapLine;
      mapLine.append( hr->firstNode()->position() );
      mapLine.append( hr->secondNode()->position() );
      line->resetPolyline( mapLine );
    }

    line->setWidth( 3 );
    line->setExternalWidth( 8 );
    line->setColor( QColor( 0, 155, 242 ) );
    line->setExternalColor( Qt::white );
    line->setZValue( 9 );
    line->setDescription( hr->id() );
    return line.release();
  }

  return nullptr;
}

static void updateHydrographDirectTransferLink( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  ReosHydrographRouting *hr = qobject_cast<ReosHydrographRouting *>( elem );
  if ( !hr )
    return;
  ReosMapPolyline *line = static_cast<ReosMapPolyline *>( item );
  QPolygonF mapLine;
  if ( hr->firstNode() && hr->secondNode() )
  {
    mapLine.append( hr->firstNode()->position() );
    mapLine.append( hr->secondNode()->position() );
    line->resetPolyline( mapLine );
  }
}


//****************************************************
// Common

static void selectHydrographElement( ReosHydraulicNetworkElement *, ReosMapItem *item )
{
  if ( item )
    item->setColor( QColor( 250, 175, 100 ) );
}

static void unselectHydrographElement( ReosHydraulicNetworkElement *, ReosMapItem *item )
{
  if ( item )
    item->setColor( QColor( 0, 155, 242 ) );
}

ReosHydraulicNetworkMapItemFactory::ReosHydraulicNetworkMapItemFactory()
{
  mCreationFunctions.insert( ReosHydrographSourceWatershed::hydrographSourceWatershedType(), &createHydrographSourceWatershedItem );
  mCreationFunctions.insert( ReosHydrographJunction::hydrographJunctionType(), &createHydrographJunctionItem );
  mCreationFunctions.insert( ReosHydrographRouting::hydrographRoutingType(), &createHydrographRoutingLink );

  mUpdateFunctions.insert( ReosHydrographSourceWatershed::hydrographSourceWatershedType(), &updateHydrographSourceWatershedItem );
  mUpdateFunctions.insert( ReosHydrographRouting::hydrographRoutingType(), &updateHydrographDirectTransferLink );

  mSelectFunctions.insert( ReosHydrographSourceWatershed::hydrographSourceWatershedType(), &selectHydrographElement );
  mSelectFunctions.insert( ReosHydrographJunction::hydrographJunctionType(), &selectHydrographElement );
  mSelectFunctions.insert( ReosHydrographRouting::hydrographRoutingType(), &selectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographSourceWatershed::hydrographSourceWatershedType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographJunction::hydrographJunctionType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographRouting::hydrographRoutingType(), &unselectHydrographElement );
}


ReosMapItem *ReosHydraulicNetworkMapItemFactory::createMapItem( ReosHydraulicNetworkElement *element, ReosMap *map )
{
  auto it = mCreationFunctions.find( element->type() );
  if ( it != mCreationFunctions.end() )
    return it.value()( element, map );

  return nullptr;
}

void ReosHydraulicNetworkMapItemFactory::updateMapItem( ReosHydraulicNetworkElement *element, ReosMapItem *item )
{
  auto it = mUpdateFunctions.find( element->type() );
  if ( it != mUpdateFunctions.end() )
    return it.value()( element, item );
}

void ReosHydraulicNetworkMapItemFactory::selectItem( ReosHydraulicNetworkElement *element, ReosMapItem *item )
{
  auto it = mSelectFunctions.find( element->type() );
  if ( it != mSelectFunctions.end() )
    return it.value()( element, item );
}

void ReosHydraulicNetworkMapItemFactory::unselectItem( ReosHydraulicNetworkElement *element, ReosMapItem *item )
{
  auto it = mUnselectFunctions.find( element->type() );
  if ( it != mUnselectFunctions.end() )
    return it.value()( element, item );
}
