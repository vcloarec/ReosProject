/***************************************************************************
  reoshydraulicnode.cpp - ReosHydraulicNode

 ---------------------
 begin                : 19.5.2021
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
#include "reoshydraulicnode.h"
#include "reoshydrauliclink.h"

ReosHydraulicNode::ReosHydraulicNode( ReosHydraulicNetwork *parent )
  : ReosHydraulicNetworkElement( parent )
{}

ReosHydraulicNode::ReosHydraulicNode( const ReosEncodedElement &encodedElement, ReosHydraulicNetwork *parent )
  : ReosHydraulicNetworkElement( encodedElement, parent )
{}

void ReosHydraulicNode::attachBySide1( ReosHydraulicLink *link )
{
  link->attachOnSide1( this );
  mLinksBySide1.append( link );
  setObsolete();
}

void ReosHydraulicNode::detachFromSide1( ReosHydraulicLink *link )
{
  mLinksBySide1.removeOne( link );
  setObsolete();
}

void ReosHydraulicNode::attachBySide2( ReosHydraulicLink *link )
{
  link->attachOnSide2( this );
  mLinksBySide2.append( link );
  setObsolete();
}

void ReosHydraulicNode::detachFromSide2( ReosHydraulicLink *link )
{
  mLinksBySide2.removeOne( link );
  setObsolete();
}


QList<ReosHydraulicLink *> ReosHydraulicNode::links() const
{
  QList<ReosHydraulicLink *> ret;

  ret = linksBySide1();
  ret.append( linksBySide2() );

  return ret;

}

QList<ReosHydraulicLink *> ReosHydraulicNode::linksBySide1() const
{
  QList<ReosHydraulicLink *> ret;

  for ( const QPointer<ReosHydraulicLink> &l : mLinksBySide1 )
    if ( !l.isNull() )
      ret.append( l );

  return ret;
}

QList<ReosHydraulicLink *> ReosHydraulicNode::linksBySide2() const
{
  QList<ReosHydraulicLink *> ret;


  for ( const QPointer<ReosHydraulicLink> &l : mLinksBySide2 )
    if ( !l.isNull() )
      ret.append( l );

  return ret;
}

bool ReosHydraulicNode::canAcceptLink( const QString &, int ) {return false;}

ReosHydraulicNode::~ReosHydraulicNode() = default;

