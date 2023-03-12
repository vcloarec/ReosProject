/***************************************************************************
  resomaptoolhydraulicnetwork.cpp - %{Cpp:License:ClassName}

 ---------------------
 begin                : 24.5.2021
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

#include "reosmaptoolhydraulicnetwork.h"
#include "reoshydrographrouting.h"
#include "reoshydraulicstructure2d.h"
#include "reosmaptool_p.h"
#include "reosstyleregistery.h"
#include "reoshydraulicstructureboundarycondition.h"


ReosMapToolDrawHydraulicNetworkLink::ReosMapToolDrawHydraulicNetworkLink( ReosHydraulicNetwork *network, ReosMap *map )
  : ReosMapTool( map, map ), ReosMapToolHydraulicElement( network )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawHydraulicNetworkLink_p( canvas );
  d->mRubberBand->setWidth( 3 );
  d->mRubberBand->setLineStyle( Qt::DotLine );
  d->mRubberBand->setStrokeColor( QColor( 0, 155, 242 ) );
  d->mRubberBand->setSecondaryStrokeColor( Qt::white );

  connect( d, &ReosMapToolDrawHydraulicNetworkLink_p::itemSelected, this, &ReosMapToolDrawHydraulicNetworkLink::onItemSuggested );
}

ReosMapToolDrawHydraulicNetworkLink::~ReosMapToolDrawHydraulicNetworkLink()
{
  if ( d )
    d->deleteLater();
}

int ReosMapToolDrawHydraulicNetworkLink::itemsCount() const
{
  if ( d )
    return d->mLinkedItems.count();

  return 0;
}

QList<ReosMapItem *> ReosMapToolDrawHydraulicNetworkLink::linkedItems() const
{
  QList<ReosMapItem *> ret;

  if ( !d )
    return ret;

  for ( const ReosMapItem_p *itemPrivate : std::as_const( d->mLinkedItems ) )
    if ( itemPrivate->base )
      ret.append( itemPrivate->base );

  return ret;
}

void ReosMapToolDrawHydraulicNetworkLink::onItemSuggested( ReosMapItem_p *itemPrivate )
{
  if ( isFinished() )
    d->mLinkedItems.clear();

  ReosMapItem *item = nullptr;
  if ( itemPrivate )
    item = itemPrivate->base;

  if ( item && acceptItem( item ) )
  {
    d->appendItem( itemPrivate );
    emit itemAdded( itemPrivate->base );
  }

  if ( isFinished() )
  {
    d->mRubberBand->reset();
    emit finished();
  }
}

ReosMapTool_p *ReosMapToolDrawHydraulicNetworkLink::tool_p() const
{
  return d;
}

ReosMapToolDrawHydrographRouting::ReosMapToolDrawHydrographRouting( ReosHydraulicNetwork *network, ReosMap *map ):
  ReosMapToolDrawHydraulicNetworkLink( network, map )
{}

bool ReosMapToolDrawHydrographRouting::acceptItem( ReosMapItem *item )
{
  if ( !item )
    return false;

  ReosHydrographNode *nodeElem = qobject_cast<ReosHydrographNode *>( mNetwork->getElement( item->description() ) );

  if ( !nodeElem )
    return false;

  if ( itemsCount() == 0 && item->description().contains( ReosHydrographSource::staticType() ) )
  {
    ReosHydrographSource *source = qobject_cast<ReosHydrographSource *>( nodeElem );
    if ( !source || source->outputHydrographTransfer() )
      return false;
  }

  if ( itemsCount() == 1 && item->description().contains( ReosHydrographJunction::staticType() ) )
  {
    if ( item->description().contains( ReosHydraulicStructureBoundaryCondition::staticType() ) )
    {
      ReosHydraulicStructureBoundaryCondition *firstBoundary = qobject_cast<ReosHydraulicStructureBoundaryCondition *>
          ( mNetwork->getElement( linkedItems().at( 0 )->description() ) );
      if ( firstBoundary ) //first node of the link is a boundary
      {
        ReosHydraulicStructureBoundaryCondition *secondBoundary = static_cast<ReosHydraulicStructureBoundaryCondition *>( nodeElem );
        if ( firstBoundary->structure() == secondBoundary->structure() )
          return false;
      }
    }

    ReosHydrographJunction *firstJunction = qobject_cast<ReosHydrographJunction *>( mNetwork->getElement( linkedItems().at( 0 )->description() ) );
    ReosHydrographJunction *junction = qobject_cast<ReosHydrographJunction *>( nodeElem );

    if ( firstJunction == junction )
      return false;

    while ( junction )
    {
      ReosHydraulicLink *downstreamLink = junction->downstreamRouting();
      if ( !downstreamLink )
        return true;

      junction = qobject_cast<ReosHydrographJunction *>( downstreamLink->secondNode() );

      if ( junction == firstJunction )
        return false;
    }
  }

  if ( item->description().contains( ReosHydraulicStructureBoundaryCondition::staticType() ) )
  {
    return nodeElem != nullptr && nodeElem->canAcceptLink( ReosHydrographRoutingLink::staticType(), itemsCount() );
  }

  return true;
}

bool ReosMapToolDrawHydrographRouting::isFinished() const
{
  return itemsCount() == 2;
}

ReosMapToolMoveHydraulicNetworkElement::ReosMapToolMoveHydraulicNetworkElement( ReosHydraulicNetwork *network, ReosMap *map )
  : ReosMapTool( map, map )
  , ReosMapToolHydraulicElement( network )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolMoveHydraulicNetworkNode_p( canvas );

  connect( d, &ReosMapToolMoveHydraulicNetworkNode_p::itemMoved, this, [this]( const QString & elemId, const QPointF & position )
  {
    ReosHydrographNode *node = qobject_cast<ReosHydrographNode *>( mNetwork->getElement( elemId ) );
    if ( node )
      node->setPosition( position );
  } );
}

ReosMapToolMoveHydraulicNetworkElement::~ReosMapToolMoveHydraulicNetworkElement()
{
  if ( d )
    d->deleteLater();
}

ReosMapTool_p *ReosMapToolMoveHydraulicNetworkElement::tool_p() const
{
  return d;
}


ReosMapToolNewStructure2D::ReosMapToolNewStructure2D( ReosHydraulicNetwork *network, ReosMap *map )
  : ReosMapToolDrawPolygon( map, map ), ReosMapToolHydraulicElement( network )
{
  setStrokeWidth( 2 );
  setColor( ReosStyleRegistery::instance()->blueReos() );
  setSecondaryStrokeColor( Qt::white );
  setLineStyle( Qt::DotLine );
  setFillColor( ReosStyleRegistery::instance()->blueReos( 100 ) );
  setCursor( Qt::CrossCursor );

  enableSnapping( true );

  setAllowSelfIntersect( false );

  connect( this, &ReosMapToolDrawPolygon::drawn, this, &ReosMapToolNewStructure2D::onDomainDrawn );
}

void ReosMapToolNewStructure2D::onDomainDrawn( const QPolygonF &polygon )
{
  mNetwork->addElement( new ReosHydraulicStructure2D( polygon, map()->mapCrs(), mNetwork->context() ) );
}

ReosMapToolHydraulicElement::ReosMapToolHydraulicElement( ReosHydraulicNetwork *network ):
  mNetwork( network )
{}
