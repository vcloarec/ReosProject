/***************************************************************************
  reosmaptoolpolygonwatershed.cpp - ReosMapToolEditPolygonWatershed

 ---------------------
 begin                : 1.9.2026
 copyright            : (C) 2026 by Vincent Cloarec
 email                : vcloarec at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
#include "reosmaptoolpolygonwatershed.h"

#include <QColor>

#include "reospolygonwatershed.h"
#include "reosmaptoolpolygonwatershed_p.h"

ReosMapToolEditPolygonWatershed::ReosMapToolEditPolygonWatershed( ReosMap *map )
  : ReosMapTool( map, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditPolygonWatershed_p( canvas );
  setCursor( Qt::CrossCursor );
}

void ReosMapToolEditPolygonWatershed::setPolygonWatersed( ReosPolygonWatershed *polygonWatershed )
{
  d->setPolygonWatershed( polygonWatershed );
}

void ReosMapToolEditPolygonWatershed::setCurrentWatershedId( const QString &watershedId )
{
  d->setCurrentWatershed( watershedId );
}

ReosMapTool_p *ReosMapToolEditPolygonWatershed::tool_p() const
{
  return d;
}

ReosMapToolSelectPolygonWatershed::ReosMapToolSelectPolygonWatershed( ReosMap *map )
  : ReosMapTool( map, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolSelectPolygonWatershed_p( canvas );
  setCursor( Qt::ArrowCursor );

  connect( d, &ReosMapToolSelectPolygonWatershed_p::watershedFound, this, &ReosMapToolSelectPolygonWatershed::watershedFound );
}

void ReosMapToolSelectPolygonWatershed::setPolygonWatersed( ReosPolygonWatershed *polygonWatershed )
{
  d->setPolygonWatershed( polygonWatershed );
}

ReosMapTool_p *ReosMapToolSelectPolygonWatershed::tool_p() const
{
  return d;
}