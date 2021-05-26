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
#include "reosmaptool_p.h"


ReosMapToolDrawHydraulicNetworkLink::ReosMapToolDrawHydraulicNetworkLink( ReosMap *map ): ReosMapTool( map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolDrawHydraulicNetworkLink_p( canvas );

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
