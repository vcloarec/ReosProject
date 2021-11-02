/***************************************************************************
  reoshubeauwidget.cpp - ReosHubEauWidget

 ---------------------
 begin                : 1.11.2021
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
#include "reoshubeauwidget.h"
#include "ui_reoshubeauwidget.h"

#include "reoshubeauserver.h"
#include "reosmap.h"
#include "reosgisengine.h"
#include "reosmapitem.h"

ReosHubEauWidget::ReosHubEauWidget( ReosMap *map, QWidget *parent )
  :  ReosActionWidget( parent )
  ,  ui( new Ui::ReosHubEauWidget )
  , mMap( map )

{
  ui->setupUi( this );
  setWindowFlag( Qt::Dialog );

  mServer = new ReosHubEauAccess( this );

  connect( mMap, &ReosMap::extentChanged, this, &ReosHubEauWidget::onMapExtentChanged );
  connect( this, &ReosActionWidget::opened, this, &ReosHubEauWidget::onMapExtentChanged );
  connect( this, &ReosActionWidget::closed, this, &ReosHubEauWidget::onClosed );
  connect( mServer, &ReosHubEauAccess::stationsUpdated, this, &ReosHubEauWidget::onStationUpdated );
}

ReosHubEauWidget::~ReosHubEauWidget()
{
  delete ui;
}

void ReosHubEauWidget::onMapExtentChanged()
{
  if ( !isVisible() )
    return;
  ReosMapExtent extent = mMap->extent();
  ReosMapExtent hubEauExtent = mMap->engine()->transformFromProjectExtent( extent, ReosGisEngine::wktEPSGCrs( 4326 ) );
  mServer->setExtent( hubEauExtent );
}

void ReosHubEauWidget::onStationUpdated()
{
  mStationsMarker.clear();

  const QList<ReosHubEauStation> &stations = mServer->stations();

  for ( const ReosHubEauStation &station : stations )
  {
    const QPointF pt = mMap->engine()->transformToProjectCoordinates( ReosGisEngine::wktEPSGCrs( 4326 ), QPointF( station.longitude, station.latitude ) );
    mStationsMarker.emplace_back( std::make_unique<ReosMapMarker>( mMap, pt ) );
    mStationsMarker.back()->setColor( Qt::green );
    mStationsMarker.back()->setWidth( 15 );
    mStationsMarker.back()->setExternalColor( Qt::black );
    mStationsMarker.back()->setExternalWidth( 3 );
  }

}

void ReosHubEauWidget::onClosed()
{
  mStationsMarker.clear();
}
