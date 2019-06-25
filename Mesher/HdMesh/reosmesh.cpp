/***************************************************************************
                      reosmesh.cpp
                     --------------------------------------
Date                 : 01-04-2019
Copyright            : (C) 2019 by Vincent Cloarec
email          : vcloarec at gmail dot com   /  projetreos at gmail dot com
 ***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

#include "reosmesh.h"


Vertex::~Vertex() {}


HdMesh::~HdMesh() {}

MeshIO::~MeshIO() {}

Segment::Segment(VertexPointer v1, VertexPointer v2):mVertex1(v1),mVertex2(v2)
{}

VertexPointer Segment::first() const {return mVertex1;}

VertexPointer Segment::second() const {return mVertex2;}


