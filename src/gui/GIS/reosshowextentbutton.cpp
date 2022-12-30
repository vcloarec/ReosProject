/***************************************************************************
  reosshowextentbutton.cpp - ReosShowExtentButton

 ---------------------
 begin                : 29.12.2022
 copyright            : (C) 2022 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosshowextentbutton.h"

#include "reosmapitem.h"
#include "reosgisengine.h"
#include "reosmap.h"

ReosShowExtentButton::ReosShowExtentButton( QWidget *parent )
  : QToolButton( parent )
{
  setIcon( QIcon( QStringLiteral( ":/images/extentOnMap.svg" ) ) );
  connect( this, &QToolButton::pressed, this, &ReosShowExtentButton::onPressed );
  connect( this, &QToolButton::released, this, &ReosShowExtentButton::onReleased );
}

void ReosShowExtentButton::onPressed()
{
  mMapExtent.reset( new ReosMapPolygon( mMap, ReosGisEngine::transformToCoordinates( mExtent.crs(), mExtent.toPolygon(), mMap->mapCrs() ) ) );
  mMapExtent->setColor( Qt::red );
  mMapExtent->setExternalColor( Qt::white );
  mMapExtent->setStyle( Qt::DashLine );
  mMapExtent->setWidth( 3 );
  mMapExtent->setExternalWidth( 5 );
  mMap->setExtent( mExtent );
}

void ReosShowExtentButton::onReleased()
{
  mMapExtent.reset();
}

void ReosShowExtentButton::setMap( ReosMap *map )
{
  mMap = map;
}

void ReosShowExtentButton::setExtent( const ReosMapExtent &newExtent )
{
  mExtent = newExtent;
}
