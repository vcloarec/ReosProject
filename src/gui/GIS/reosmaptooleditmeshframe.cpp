/***************************************************************************
  reosmaptooleditmeshframe.cpp - ReosMapToolEditMeshFrame

 ---------------------
 begin                : 9.3.2022
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
#include "reosmaptooleditmeshframe.h"
#include "reosmaptooleditmeshframe_p.h"

ReosMapToolEditMeshFrame::ReosMapToolEditMeshFrame( ReosMesh *mesh, QObject *parent, ReosMap *map )
  : ReosMapTool( parent, map )
{
  QgsMapCanvas *canvas = qobject_cast<QgsMapCanvas *>( map->mapCanvas() );
  d = new ReosMapToolEditMeshFrame_p( mesh, canvas );
  setCursor( Qt::CrossCursor );
}

ReosMapToolEditMeshFrame::~ReosMapToolEditMeshFrame()
{
  if ( !d.isNull() )
    d->deleteLater();
}

QActionGroup *ReosMapToolEditMeshFrame::mainActions() const
{
  return d->mainActions();
}

ReosMapTool_p *ReosMapToolEditMeshFrame::tool_p() const
{
  return d;
}
