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
#include <QSet>

#include "reosrenderedobject.h"

class ReosMeshGenerator;
class ReosMeshFrameData;
class ReosDigitalElevationModel;
class ReosMap;
class ReosTopographyCollection;
class ReosParameterDouble;
class ReosParameterInteger;
class ReosParameterSlope;
class ReosParameterArea;
class ReosHydraulicSimulationResults;


class ReosMeshQualityChecker : public ReosProcess
{
  public:
    virtual ~ReosMeshQualityChecker() {}
    struct QualityMeshResults
    {
      QString error;
      int errorVertex = -1;
      int errorFace = -1;
      QList<QPolygonF> minimumAngle;
      QList<QPolygonF> maximumAngle;;
      QList<QPointF> connectionCount;
      QList<QPointF> connectionCountBoundary;
      QList<QLineF> maximumSlope;
      QList<QPolygonF> minimumArea;
      QList<QPolygonF> maximumArea;
      QList<QPolygonF> maximumAreaChange;
    };

    virtual QualityMeshResults result() const = 0;

  protected:
    mutable QualityMeshResults mResult;
};


class ReosMesh: public ReosRenderedObject
{
    Q_OBJECT
  public:

    struct QualityMeshParameters
    {
      ReosParameterDouble *minimumAngle = nullptr;
      ReosParameterDouble *maximumAngle = nullptr;
      ReosParameterInteger *connectionCount = nullptr;
      ReosParameterInteger *connectionCountBoundary = nullptr;
      ReosParameterSlope *maximumSlope = nullptr;
      ReosParameterArea *minimumArea = nullptr;
      ReosParameterArea *maximumArea = nullptr;
      ReosParameterDouble *maximumAreaChange = nullptr;

      ReosEncodedElement encode() const;
      void decode( const ReosEncodedElement &element, QObject *parent );
    };

    enum QualityMeshCheck
    {
      MinimumAngle = 1 << 0,
      MaximumAngle = 1 << 1,
      ConnectionCount = 1 << 2,
      ConnectionCountBoundary = 1 << 3,
      MaximumSlope = 1 << 4,
      MinimumArea = 1 << 5,
      MaximumArea = 1 << 6,
      MaximumAreaChange = 1 << 7
    };
    Q_ENUM( QualityMeshCheck )
    Q_DECLARE_FLAGS( QualityMeshChecks, QualityMeshCheck )
    Q_FLAG( QualityMeshChecks )

    //! Creates a new void mesh in memory
    static ReosMesh *createMeshFrame( const QString &crs = QString() );

    static ReosMesh *createMeshFrameFromFile( const QString &dataPath );

    //! Returns whether the mesh is valid
    virtual bool isValid() const = 0;

    //! Returns vertex count
    virtual int vertexCount() const = 0;

    //! Returns face count
    virtual int faceCount() const = 0;

    //! Clears ans generates a new mesh with \a data
    virtual void generateMesh( const ReosMeshFrameData &data ) = 0;

    virtual QString crs() const = 0;

    virtual QVector<int> face( int faceIndex ) const = 0 ;

    virtual QPointF vertexPosition( int vertexIndex, const QString &destinationCrs = QString() ) = 0;

    virtual QObject *data() const = 0;

    /**
     * Activate vertices elevation as a dataset group with \a name, returns a unique id of this dataset group.
     */
    virtual QString enableVertexElevationDataset( const QString &name ) = 0;

    //! Returns the vertices elevation dataset id
    virtual QString verticesElevationDatasetId() const = 0;

    //! Returns all the dataset ids contained in the mesh
    virtual QStringList datasetIds() const = 0;

    //! Returns the name of the dataset with \a id
    virtual QString datasetName( const QString &id ) const = 0;

    //! Activates the dataset with \a id
    virtual bool activateDataset( const QString &id ) = 0;

    //! Returns an index correspondng to the dataset group with \a id
    virtual int datasetGroupIndex( const QString &id ) const = 0;

    virtual void applyTopographyOnVertices( ReosTopographyCollection *topographyCollection ) = 0;

    //! Returns the value of dataset \a datasetId at position \a pos in map coordinates
    virtual double datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const = 0;

    virtual void save( const QString &dataPath ) const = 0;

    virtual void stopFrameEditing( bool commit ) = 0;

    virtual ReosEncodedElement meshSymbology() const = 0;
    virtual void setMeshSymbology( const ReosEncodedElement &symbology ) = 0;

    virtual ReosEncodedElement datasetScalarGroupSymbology( const QString &id ) const = 0;

    virtual void setDatasetScalarGroupSymbology( const ReosEncodedElement &encodedElement, const QString &id ) = 0;

    //! Returns a process that check the quality of the mesh, caller take ownership
    virtual ReosMeshQualityChecker *getQualityChecker( QualityMeshChecks qualitiChecks, const QString &destinatonCrs ) const = 0;

    virtual void setSimulationResults( ReosHydraulicSimulationResults *result ) = 0;

    QualityMeshParameters qualityMeshParameters() const;
    void setQualityMeshParameter( const ReosEncodedElement &element );

    bool vertexIsOnBoundary( int vertexIndex ) const;
    bool vertexIsOnHoleBorder( int vertexIndex ) const;

  protected:
    ReosMesh( QObject *parent = nullptr );

    QualityMeshParameters mQualityMeshParameters;
    QSet<int> mBoundaryVerticesSet;
    QSet<int> mHolesVerticesVerticesSet;
};

#endif // REOSMESH_H
