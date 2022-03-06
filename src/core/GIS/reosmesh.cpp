/***************************************************************************
  reosmesh.cpp - ReosMesh

 ---------------------
 begin                : 13.1.2022
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
#include "reosmesh.h"
#include "reosmesh_p.h"


ReosMesh *ReosMesh::createMemoryMesh( const QString &crs )
{
  return new ReosMesh_p( crs );
}

ReosMesh *ReosMesh::createMemoryMesh( const ReosEncodedElement &element, const QString &dataPath )
{
  return new ReosMesh_p( element, dataPath );
}


ReosMesh::ReosMesh( QObject *parent ): ReosDataObject( parent )
{}
