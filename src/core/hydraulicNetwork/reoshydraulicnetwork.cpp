/***************************************************************************
  reoshydraulicnetwork.cpp - ReosHydraulicNetwork

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
#include "reoshydraulicnetwork.h"
#include "reoshydraulicnode.h"
#include "reoshydrauliclink.h"
#include "reoshydrographtransfer.h"
#include <QUuid>

ReosHydraulicNetworkElement::ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent ):
  ReosDataObject( parent )
  , mNetWork( parent )
{
  mUid = QUuid::createUuid().toString();
}

ReosHydraulicNetworkElement::~ReosHydraulicNetworkElement()
{
}

QString ReosHydraulicNetworkElement::id() const
{
  return type() + QString( ':' ) + mUid;
}

void ReosHydraulicNetworkElement::destroy()
{
  if ( !mNetWork.isNull() )
    mNetWork->mElements.remove( id() );
  deleteLater();
}

void ReosHydraulicNetworkElement::positionChanged()
{
  if ( mNetWork )
    mNetWork->elemPositionChangedPrivate( this );
}


ReosHydraulicNetwork::ReosHydraulicNetwork( ReosModule *parent ): ReosModule( parent )
{
  ReosHydrographRoutingMethodFactories::instantiate( this );
}

QList<ReosHydraulicNetworkElement *> ReosHydraulicNetwork::getElements( const QString &type ) const
{
  QList<ReosHydraulicNetworkElement *> elems;
  for ( ReosHydraulicNetworkElement *elem : mElements )
    if ( elem->type() == type )
      elems.append( elem );

  return elems;
}

ReosHydraulicNetworkElement *ReosHydraulicNetwork::getElement( const QString &elemId ) const
{
  if ( mElements.contains( elemId ) )
    return mElements.value( elemId );
  else
    return nullptr;
}

void ReosHydraulicNetwork::addElement( ReosHydraulicNetworkElement *elem )
{
  mElements.insert( elem->id(), elem );
  emit elementAdded( elem );
}

void ReosHydraulicNetwork::removeElement( ReosHydraulicNetworkElement *elem )
{
  emit elementRemoved( elem );
  ReosHydraulicNode *node = qobject_cast<ReosHydraulicNode *>( elem );
  if ( node )
    for ( ReosHydraulicLink *link : node->links() )
      removeElement( link );
  elem->destroy();
}

void ReosHydraulicNetwork::elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem )
{
  emit elementPositionHasChanged( elem );
}
