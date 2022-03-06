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
#include <QPointer>

#include "reosdataobject.h"

class QGraphicsView;
class QPainter;
class ReosMeshGenerator;
class ReosMeshFrameData;
class ReosDigitalElevationModel;
class ReosMap;
class ReosTopographyCollection;


class ReosRenderedObject: public ReosDataObject
{
    Q_OBJECT
  public:
    ReosRenderedObject( QObject *parent ) : ReosDataObject( parent ) {}
    virtual void render( QGraphicsView *canvas, QPainter *painter ) = 0; ///TODO look to see if painter can be deduced from canvas in QGIS

    virtual void updateRendering( QGraphicsView *canvas ) = 0;

  signals:
    void renderingFinished();


};

class ReosMesh: public ReosRenderedObject
{
    Q_OBJECT
  public:

    //! Creates a new void mesh in memory
    static ReosMesh *createMemoryMesh( const QString &crs = QString() );

    static ReosMesh *createMemoryMesh( const ReosEncodedElement &element, const QString &dataPath );

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

    virtual QObject *data() const = 0;

    /**
     * Activate vertices elevation as a dataset group with \a name, returns a unique id of this dataset group.
     */
    virtual QString enableVertexElevationDataset( const QString &name ) = 0;

    //! Activates the dataset with \a id
    virtual bool activateDataset( const QString &id ) = 0;

    //! Returns an index correspondng to the dataset group with \a id
    virtual int datasetGroupIndex( const QString &id ) const = 0;

    virtual void applyTopographyOnVertices( ReosTopographyCollection *topographyCollection ) = 0;

    virtual ReosEncodedElement encode( const QString &dataPath ) const = 0;

  signals:
    void repaintRequested();

  protected:
    ReosMesh( QObject *parent = nullptr );

};

#endif // REOSMESH_H
