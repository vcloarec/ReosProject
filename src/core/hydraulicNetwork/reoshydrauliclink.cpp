/***************************************************************************
  reoshydrauliclink.cpp - ReosHydraulicLink

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
#include "reoshydrauliclink.h"

ReosHydraulicLink::ReosHydraulicLink( QObject *parent ): QObject( parent )
{

}

void ReosHydraulicLink::attachOnSide1( ReosHydraulicNode *node )
{
  if ( !mNode_1.isNull() )
    mNode_1->mLinksBySide1.removeOne( this );

  mNode_1 = node;
  node->mLinksBySide1.append( this );
}

void ReosHydraulicLink::attachOnSide2( ReosHydraulicNode *node )
{
  if ( !mNode_2.isNull() )
    mNode_2->mLinksBySide2.removeOne( this );

  mNode_2 = node;
  node->mLinksBySide2.append( this );
}
