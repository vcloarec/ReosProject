/***************************************************************************
  reostriangularirregularnetwork.h - ReosTriangularIrregularNetwork

 ---------------------
 begin                : 12.4.2021
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
#ifndef REOSTRIANGULARIRREGULARNETWORK_H
#define REOSTRIANGULARIRREGULARNETWORK_H

#include <memory>
#include <QPointF>
#include <QObject>

#include "reosmapextent.h"
#include "reosprocess.h"

class ReosGiSEngine;
class QgsProcessingFeatureSource;
class QgsCoordinateTransform;

//! Class that represent a Triangulat Irregular Network (TIN)
class ReosTriangularIrregularNetwork : public QObject
{
    Q_OBJECT
  public:
    //! Vertex structure used to pass the vertex data
    struct Vertex
    {
      double x;
      double y;
      double z;
    };

    //! Constructor
    ReosTriangularIrregularNetwork( QObject *parent = nullptr ): QObject( parent ) {}

    //! Add a vertex to the TIN, return true if th epoint is effectivly added
    virtual bool addVertex( const Vertex &pt ) = 0;

    //! Add a constraint line to the TIN
    virtual void addConstraintLine( const QVector<Vertex> &vertices ) = 0;

    //! Remove a vertex in the TIN, return true if the vertex is effectivly removed
    virtual bool removeVertex( int vertexIndex ) = 0;

    //! Returns the vertices count
    virtual int vertexCount() const = 0;

    //! Returns the faces count
    virtual int triangleCount() const = 0;

    //! Returns the plan positon of the vertex with \a index
    virtual QPointF vertexXY( int index ) const = 0;

    //! Returns the minimum elevation of the TIN
    virtual double minimumElevation() const = 0;

    //! Returns the maximum elevation of the TIN
    virtual double maximumElevation() const = 0;

    //! Returns the extents of the TIN
    virtual ReosMapExtent extent() const = 0;

    //! Returns whether the TIN is automatically updated id new elements are added
    bool autoUpdate() const;

    //! Sets whether the TIN is automatically updated id new elements are added
    void setAutoUpdate( bool autoUpdate );

  signals:
    //! emitted when tue TIN is updated
    void updated();

  private:
    bool mAutoUpdate = true;
};

//! Class that represents a process used to add vector layer data to a TIN
class ReosAddVectorLayersToTinProcess : public ReosProcess
{
  public:
    //! Constructor with the \a tin to update
    ReosAddVectorLayersToTinProcess( ReosTriangularIrregularNetwork *tin );
    ~ReosAddVectorLayersToTinProcess();

    //! Sets the vector layer data see (see QGIS API documentation : QgsProcessingParameterTinInputLayers)
    void setLayerData( const QVariant &layerData );

    void start() override;

  private:
    ReosTriangularIrregularNetwork *mTin = nullptr;
    std::vector<std::unique_ptr<QgsProcessingFeatureSource>> mVertexFeatureSources;
    std::vector<std::unique_ptr<QgsProcessingFeatureSource>> mConstraintFeatureSources;
    std::vector<std::unique_ptr<QgsCoordinateTransform>> mVertexTransform;
    std::vector<std::unique_ptr<QgsCoordinateTransform>> mConstraintTransform;
    QVector<int> mVertexAttributes;
    QVector<int> mConstraintAttributes;

};


#endif // REOSTRIANGULARIRREGULARNETWORK_H
