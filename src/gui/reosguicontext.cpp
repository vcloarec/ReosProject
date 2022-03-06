/***************************************************************************
  reosguicontext.cpp - ReosGuiContext

 ---------------------
 begin                : 28.12.2021
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
#include "reosguicontext.h"
#include "reosmapitem.h"
#include "reosmap.h"

ReosGuiContext::ReosGuiContext( QWidget *parent ):
  mParent( parent )
{}

ReosGuiContext::ReosGuiContext( const ReosGuiContext &other, QWidget *parent )
{
  *this = other;
  mParent = parent;
}

ReosGuiContext::ReosGuiContext( const ReosGuiContext &other )
{
  *this = other;
}

ReosMap *ReosGuiContext::map() const
{
  return mMap;
}

void ReosGuiContext::setMap( ReosMap *map )
{
  mMap = map;
}

QWidget *ReosGuiContext::parent() const
{
  return mParent;
}

ReosMapItem *ReosGuiContext::mapItems( const QString &description ) const
{
  for ( ReosMapItem *mi : std::as_const( mMapItems ) )
  {
    if ( mi->description().contains( description ) )
      return mi;
  }

  return nullptr;
}

void ReosGuiContext::addMapItems( ReosMapItem *mapItems )
{
  mMapItems.push( mapItems );
}

void ReosGuiContext::addAction( QAction *action )
{
  mActions.append( action );
}

QList<QAction *> ReosGuiContext::actions() const
{
  return mActions;
}
