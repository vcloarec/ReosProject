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
#include "reosmaptooleditgeometrystructure_p.h"

ReosMapToolEditGeometryStructure::ReosMapToolEditGeometryStructure( ReosPolylinesStructure *structure, QObject *parent, ReosMap *map )
  : ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolylineStructure_p( canvas );
  d->setStructure( structure );
  setCursor( Qt::CrossCursor );

  std::unique_ptr<ReosEditGeometryStructureMenuPopulator> menuPopulator =
    std::make_unique<ReosEditGeometryStructureMenuPopulator>( d );

  setContextMenuPopulator( menuPopulator.release() );

  setUp();
}

ReosMapToolEditGeometryStructure::~ReosMapToolEditGeometryStructure()
{
  if ( !d.isNull() )
    d->deleteLater();
}

void ReosMapToolEditGeometryStructure::setStructure( ReosPolylinesStructure *structure )
{
  d->setStructure( structure );
}

ReosMapTool_p *ReosMapToolEditGeometryStructure::tool_p() const
{
  return d;
}
