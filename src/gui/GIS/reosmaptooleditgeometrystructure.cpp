/***************************************************************************
  reosmaptooleditgeometrystructure.cpp - ReosMapToolEditGeometryStructure

 ---------------------
 begin                : 12.1.2022
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
#include "reosmaptooleditgeometrystructure.h"
#include "reosmaptooleditpolylinestructure_p.h"
#include "reosmaptooleditpolygonstructure_p.h"

#include "reospolylinesstructure.h"

ReosMapToolEditPolylineStructure::ReosMapToolEditPolylineStructure( ReosPolylinesStructure *structure, QObject *parent, ReosMap *map )
  : ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolylineStructure_p( canvas );
  d->setStructure( structure );
  setCursor( Qt::CrossCursor );

  std::unique_ptr<ReosEditPolylineStructureMenuPopulator> menuPopulator =
    std::make_unique<ReosEditPolylineStructureMenuPopulator>( d );

  setContextMenuPopulator( menuPopulator.release() );

  setUp();
}

ReosMapToolEditPolylineStructure::~ReosMapToolEditPolylineStructure()
{
  if ( !d.isNull() )
    d->deleteLater();
}


QActionGroup *ReosMapToolEditPolylineStructure::mainActions() const
{
  return d->mainActions();
}

ReosMapTool_p *ReosMapToolEditPolylineStructure::tool_p() const
{
  return d;
}

ReosMapToolEditPolygonStructure::ReosMapToolEditPolygonStructure( ReosPolygonStructure *structure, QObject *parent, ReosMap *map )
  : ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygonStructure_p( canvas );
  d->setStructure( structure );
  setCursor( Qt::CrossCursor );

}

ReosMapToolEditPolygonStructure::~ReosMapToolEditPolygonStructure()
{
  if ( !d.isNull() )
    d->deleteLater();
}

QActionGroup *ReosMapToolEditPolygonStructure::mainActions() const
{
  return d->mainActions();
}

ReosMapTool_p *ReosMapToolEditPolygonStructure::tool_p() const
{
  return d;
}


