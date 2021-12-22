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

#include <QVector2D>

//****************************************************
// Hydrograph Source Watershed

static ReosMapItem *createHydrographSourceWatershedItem( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographNodeWatershed *hws = qobject_cast<ReosHydrographNodeWatershed *>( elem );
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
  ReosHydrographNodeWatershed *hws = qobject_cast<ReosHydrographNodeWatershed *>( elem );
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

static void updateHydrographJunctionItem( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  ReosHydrographJunction *hj = qobject_cast<ReosHydrographJunction *>( elem );
  ReosMapMarker *markerItem = static_cast<ReosMapMarker *>( item );

  markerItem->resetPoint( hj->position() );

  for ( ReosHydraulicLink *link : hj->links() )
    link->positionChanged();
}


//****************************************************
// Hydrograph routing link

static ReosMapItem *createHydrographRoutingLink( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographRoutingLink *hr = qobject_cast<ReosHydrographRoutingLink *>( elem );
  if ( hr )
  {
    std::unique_ptr<ReosMapPolyline> line = std::make_unique<ReosMapPolyline>( map );

    if ( hr->firstNode() && hr->secondNode() )
    {
      QPolygonF mapLine;
      QVector2D v = QVector2D( hr->firstNode()->position() - hr->secondNode()->position() );

      mapLine.append( hr->firstNode()->position() );
      mapLine.append( hr->secondNode()->position() );
      line->resetPolyline( mapLine );
      line->activeMarker( true );
      line->setMarkerArrow( true );
      line->setMarkerDistance( v.length() / 2 );
    }

    line->setWidth( 3 );
    line->setExternalWidth( 8 );
    line->setColor( QColor( 0, 155, 242 ) );
    line->setExternalColor( Qt::white );
    line->setZValue( 9 );
    line->setDescription( hr->id() );
    line->setExtremityDistance( 10 );
    return line.release();
  }

  return nullptr;
}

static void updateHydrographDirectTransferLink( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  ReosHydrographRoutingLink *hr = qobject_cast<ReosHydrographRoutingLink *>( elem );
  if ( !hr )
    return;
  ReosMapPolyline *line = static_cast<ReosMapPolyline *>( item );
  QPolygonF mapLine;
  if ( hr->firstNode() && hr->secondNode() )
  {
    QPolygonF mapLine;
    QVector2D v = QVector2D( hr->firstNode()->position() - hr->secondNode()->position() );
    mapLine.append( hr->firstNode()->position() );
    mapLine.append( hr->secondNode()->position() );
    line->resetPolyline( mapLine );
    line->setMarkerDistance( v.length() / 2 );
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
  mCreationFunctions.insert( ReosHydrographNodeWatershed::staticType(), &createHydrographSourceWatershedItem );
  mCreationFunctions.insert( ReosHydrographJunction::staticType(), &createHydrographJunctionItem );
  mCreationFunctions.insert( ReosHydrographRoutingLink::staticType(), &createHydrographRoutingLink );

  mUpdateFunctions.insert( ReosHydrographNodeWatershed::staticType(), &updateHydrographSourceWatershedItem );
  mUpdateFunctions.insert( ReosHydrographRoutingLink::staticType(), &updateHydrographDirectTransferLink );
  mUpdateFunctions.insert( ReosHydrographJunction::staticType(), &updateHydrographJunctionItem );

  mSelectFunctions.insert( ReosHydrographNodeWatershed::staticType(), &selectHydrographElement );
  mSelectFunctions.insert( ReosHydrographJunction::staticType(), &selectHydrographElement );
  mSelectFunctions.insert( ReosHydrographRoutingLink::staticType(), &selectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographNodeWatershed::staticType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographJunction::staticType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographRoutingLink::staticType(), &unselectHydrographElement );
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
