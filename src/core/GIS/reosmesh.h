/***************************************************************************
  reosmesh.h - ReosMesh

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
#ifndef REOSMESH_H
#define REOSMESH_H

#include <QPointF>
#include <QVector>

#include "reosdataobject.h"

class QGraphicsView;
class QPainter;
class ReosMeshGenerator;
class  ReosMeshFrameData;

class ReosRenderedObject
{
  public:
    virtual ~ReosRenderedObject() {}

    virtual void render( QGraphicsView *canvas, QPainter *painter ) = 0; ///TODO look to see if painter can be deduced from canvas in QGIS
};

class ReosMesh: public ReosRenderedObject, public ReosDataObject
{
  public:

    //! Creates a new void mesh in memory
    static ReosMesh *createMemoryMesh();

    //! Returns whether the mesh is valid
    virtual bool isValid() const = 0;

    //! Returns vertex count
    virtual int vertexCount() const = 0;

    //! Returns face count
    virtual int faceCount() const = 0;

    //! Clears ans generates a new mesh with \a data
    virtual void generateMesh( const ReosMeshFrameData &data ) = 0;

    virtual void addVertex( const QPointF pt, double z, double tolerance ) = 0;

    virtual QString crs() const = 0;

  protected:
    ReosMesh( QObject *parent = nullptr );

};

#endif // REOSMESH_H
