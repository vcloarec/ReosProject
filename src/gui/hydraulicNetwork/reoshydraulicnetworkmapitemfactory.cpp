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
#include "reoshydrographrouting.h"
#include "reoshydraulicstructure2d.h"
#include "reoshydraulicstructureboundarycondition.h"
#include "reosstyleregistery.h"
#include <QVector2D>

//****************************************************
// Hydrograph Source Watershed

static ReosMapItem *createHydrographSourceWatershedItem( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographNodeWatershed *hws = qobject_cast<ReosHydrographNodeWatershed *>( elem );
  if ( hws )
  {
    std::unique_ptr<ReosMapMarkerEmptySquare> marker = std::make_unique<ReosMapMarkerEmptySquare>( map, hws->position( map->mapCrs() ) );
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

static ReosMapItem *createExtraItemSelectedHydrographSourceWatershed( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographNodeWatershed *hws = qobject_cast<ReosHydrographNodeWatershed *>( elem );
  if ( hws )
  {
    std::unique_ptr<ReosMapPolygon> polygon = std::make_unique<ReosMapPolygon>( map, hws->watershed()->delineating() );
    polygon->setWidth( 1 );
    polygon->setColor( Qt::white );
    polygon->setFillColor( ReosStyleRegistery::instance()->orangeReos( 100 ) );
    polygon->setFillStyle( Qt::SolidPattern );
    polygon->setZValue( 9 );
    return polygon.release();
  }

  return nullptr;
}


//****************************************************
// Hydrograph Source junction

static ReosMapItem *createHydrographJunctionItem( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydrographJunction *hj = qobject_cast<ReosHydrographJunction *>( elem );
  if ( hj )
  {
    std::unique_ptr<ReosMapMarkerEmptyCircle> marker = std::make_unique<ReosMapMarkerEmptyCircle>( map, hj->position( map->mapCrs() ) );
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

  markerItem->resetPoint( hj->position( item->map()->mapCrs() ) );

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
    const QString mapCrs = map->mapCrs();
    if ( hr->firstNode() && hr->secondNode() )
    {
      QPolygonF mapLine;
      QVector2D v = QVector2D( hr->firstNode()->position( mapCrs ) - hr->secondNode()->position( mapCrs ) );

      mapLine.append( hr->firstNode()->position( mapCrs ) );
      mapLine.append( hr->secondNode()->position( mapCrs ) );
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
  const QString mapCrs = item->map()->mapCrs();
  if ( hr->firstNode() && hr->secondNode() )
  {
    QPolygonF mapLine;
    const QVector2D v = QVector2D( hr->firstNode()->position( mapCrs ) - hr->secondNode()->position( mapCrs ) );
    mapLine.append( hr->firstNode()->position( mapCrs ) );
    mapLine.append( hr->secondNode()->position( mapCrs ) );
    line->resetPolyline( mapLine );
    line->setMarkerDistance( v.length() / 2 );
  }
}

//****************************************************
// Structure 2D

static ReosMapItem *createStructure2D( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydraulicStructure2D *str2D = qobject_cast<ReosHydraulicStructure2D *>( elem );
  if ( str2D )
  {
    std::unique_ptr<ReosMapPolygon> poly( new ReosMapPolygon( map, str2D->geometryStructure() ) );
    poly->setWidth( 3 );
    poly->setColor( QColor( 0, 155, 242 ) );
    poly->setExternalWidth( 5 );
    poly->setExternalColor( Qt::white );
    poly->setFillColor( QColor( 0, 155, 242, 75 ) );
    poly->setFillStyle( Qt::DiagCrossPattern );
    poly->setDescription( str2D->id() );
    return poly.release();
  }

  return nullptr;
}

static void updateStructure2D( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  if ( item )
  {
    item->updatePosition();
  }
}

static void selectStructure2D( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  if ( item )
  {
    item->setColor( QColor( 250, 175, 100 ) );
  }

  ReosHydraulicStructure2D *str2D = qobject_cast<ReosHydraulicStructure2D *>( elem );
  if ( str2D && str2D->mesh() && item )
  {
    if ( str2D->mesh()->vertexCount() > 0 && str2D->mesh()->datasetIds().count() > 0 )
      static_cast<ReosMapPolygon *>( item )->setFillStyle( Qt::NoBrush );
    else
      static_cast<ReosMapPolygon *>( item )->setFillStyle( Qt::DiagCrossPattern );
  }
}

static void unselectStructure2D( ReosHydraulicNetworkElement *, ReosMapItem *item )
{
  if ( item )
  {
    item->setColor( QColor( 0, 155, 242 ) );
    static_cast<ReosMapPolygon *>( item )->setFillStyle( Qt::DiagCrossPattern );
  }
}

static ReosMapItem *createStructureBoundaryCondition( ReosHydraulicNetworkElement *elem, ReosMap *map )
{
  ReosHydraulicStructureBoundaryCondition *stdBc = qobject_cast<ReosHydraulicStructureBoundaryCondition *>( elem );
  if ( stdBc )
  {
    std::unique_ptr<ReosMapMarkerEmptySquare> marker( new ReosMapMarkerEmptySquare( map, stdBc->position( map->mapCrs() ) ) );
    marker->setWidth( 6 );
    marker->setExternalWidth( 16 );
    marker->setColor( QColor( 0, 155, 242 ) );
    marker->setExternalColor( Qt::white );
    marker->setZValue( 10 );
    marker->setDescription( stdBc->id() );
    return marker.release();
  }

  return nullptr;
}

static void updateStructureBoundaryCondition( ReosHydraulicNetworkElement *elem, ReosMapItem *item )
{
  ReosHydraulicStructureBoundaryCondition *stdBc = qobject_cast<ReosHydraulicStructureBoundaryCondition *>( elem );
  ReosMapMarker *markerItem = static_cast<ReosMapMarker *>( item );

  markerItem->resetPoint( stdBc->position( item->map()->mapCrs() ) );

  for ( ReosHydraulicLink *link : stdBc->links() )
    link->positionChanged();
}

//****************************************************
// Common

static void selectHydraulicElement( ReosHydraulicNetworkElement *, ReosMapItem *item )
{
  if ( item )
  {
    item->setColor( QColor( 250, 175, 100 ) );
  }
}

static void unselectHydrographElement( ReosHydraulicNetworkElement *, ReosMapItem *item )
{
  if ( item )
  {
    item->setColor( QColor( 0, 155, 242 ) );
  }
}

ReosHydraulicNetworkMapItemFactory::ReosHydraulicNetworkMapItemFactory()
{
  // creation function
  mCreationFunctions.insert( ReosHydrographNodeWatershed::staticType(), &createHydrographSourceWatershedItem );
  mCreationFunctions.insert( ReosHydrographJunction::staticType(), &createHydrographJunctionItem );
  mCreationFunctions.insert( ReosHydrographRoutingLink::staticType(), &createHydrographRoutingLink );
  mCreationFunctions.insert( ReosHydraulicStructure2D::staticType(), &createStructure2D );
  mCreationFunctions.insert( ReosHydraulicStructureBoundaryCondition::staticType(), &createStructureBoundaryCondition );

  // update function
  mUpdateFunctions.insert( ReosHydrographNodeWatershed::staticType(), &updateHydrographSourceWatershedItem );
  mUpdateFunctions.insert( ReosHydrographRoutingLink::staticType(), &updateHydrographDirectTransferLink );
  mUpdateFunctions.insert( ReosHydrographJunction::staticType(), &updateHydrographJunctionItem );
  mUpdateFunctions.insert( ReosHydraulicStructure2D::staticType(), &updateStructure2D );
  mUpdateFunctions.insert( ReosHydraulicStructureBoundaryCondition::staticType(), &updateStructureBoundaryCondition );

  // select function
  mSelectFunctions.insert( ReosHydrographNodeWatershed::staticType(), &selectHydraulicElement );
  mSelectFunctions.insert( ReosHydrographJunction::staticType(), &selectHydraulicElement );
  mSelectFunctions.insert( ReosHydrographRoutingLink::staticType(), &selectHydraulicElement );
  mSelectFunctions.insert( ReosHydraulicStructure2D::staticType(), &selectStructure2D );
  mSelectFunctions.insert( ReosHydraulicStructureBoundaryCondition::staticType(), &selectHydraulicElement );

  // unselect function
  mUnselectFunctions.insert( ReosHydrographNodeWatershed::staticType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographJunction::staticType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydrographRoutingLink::staticType(), &unselectHydrographElement );
  mUnselectFunctions.insert( ReosHydraulicStructure2D::staticType(), &unselectStructure2D );
  mUnselectFunctions.insert( ReosHydraulicStructureBoundaryCondition::staticType(), &unselectHydrographElement );

  mExtraItemSelectedFunctions.insert( ReosHydrographNodeWatershed::staticType(), &createExtraItemSelectedHydrographSourceWatershed );
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
    it.value()( element, item );
}

void ReosHydraulicNetworkMapItemFactory::unselectItem( ReosHydraulicNetworkElement *element, ReosMapItem *item )
{
  auto it = mUnselectFunctions.find( element->type() );
  if ( it != mUnselectFunctions.end() )
    it.value()( element, item );
}

ReosMapItem *ReosHydraulicNetworkMapItemFactory::createExtraItemSelected( ReosHydraulicNetworkElement *element, ReosMap *map )
{
  auto it = mExtraItemSelectedFunctions.find( element->type() );
  if ( it != mExtraItemSelectedFunctions.end() )
    return it.value()( element, map );

  return nullptr;

}
