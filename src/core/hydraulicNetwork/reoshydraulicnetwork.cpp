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

ReosHydraulicNetworkElement::ReosHydraulicNetworkElement( ReosHydraulicNetwork *parent ):
  QObject( parent )
  , mNetWork( parent )
{

}

ReosHydraulicNetworkElement::~ReosHydraulicNetworkElement()
{
  if ( !mNetWork.isNull() )
    mNetWork->mElements.removeOne( this );
}

void ReosHydraulicNetworkElement::destroy()
{
  if ( !mNetWork.isNull() )
    mNetWork->mElements.removeOne( this );
  deleteLater();
}

void ReosHydraulicNetworkElement::positionChanged()
{
  if ( mNetWork )
    mNetWork->elemPositionChangedPrivate( this );
}


QList<ReosHydraulicNetworkElement *> ReosHydraulicNetwork::getElements( const QString &type ) const
{
  QList<ReosHydraulicNetworkElement *> elems;
  for ( ReosHydraulicNetworkElement *elem : mElements )
    if ( elem->type() == type )
      elems.append( elem );

  return elems;
}

void ReosHydraulicNetwork::addElement( ReosHydraulicNetworkElement *elem )
{
  mElements.append( elem );
  emit elementAdded( elem );
}

void ReosHydraulicNetwork::elemPositionChangedPrivate( ReosHydraulicNetworkElement *elem )
{
  emit elementPostionHasChanged( elem );
}
