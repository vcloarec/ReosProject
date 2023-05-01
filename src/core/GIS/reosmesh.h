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

#define SIP_NO_FILE

#include <QPointF>
#include <QVector>
#include <QPointer>
#include <QSet>

#include "reoscore.h"
#include "reosrenderedobject.h"
#include "reosencodedelement.h"
#include "reosmeshdatasetsource.h"

class ReosMeshGenerator;
struct ReosMeshFrameData;
class ReosDigitalElevationModel;
class ReosTopographyCollection;
class ReosParameterDouble;
class ReosParameterInteger;
class ReosParameterSlope;
class ReosParameterArea;
class ReosHydraulicSimulationResults;
class ReosSpatialPosition;
class ReosGisEngine;
class ReosMeshDatasetSource;
class ReosMesh;
class ReosColorShaderSettings;


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



class ReosMeshPointValue_p
{
  public:
    ReosMeshPointValue_p( const QPointF &point );
    virtual ~ReosMeshPointValue_p();

  private:
    std::atomic_int ref;
    QPointF mPoint;
    virtual double interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const = 0;
    virtual double interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const = 0;
    virtual double interpolateTerrainElevation( ReosMesh *mesh ) const = 0;

    friend class ReosMeshPointValue;
};

class REOSCORE_EXPORT ReosMeshPointValue
{
  public:
    ReosMeshPointValue();
    explicit ReosMeshPointValue( ReosMeshPointValue_p *poinValue );

    ~ReosMeshPointValue();

    ReosMeshPointValue( const ReosMeshPointValue &other );
    ReosMeshPointValue( ReosMeshPointValue &&other );

    ReosMeshPointValue &operator=( const ReosMeshPointValue &other );
    ReosMeshPointValue &operator=( ReosMeshPointValue &&other );

    QPointF position() const
    {
      if ( d )
        return d->mPoint;

      return QPointF();
    }

    double value( ReosMeshDatasetSource *source, int groupIndex, int index ) const;
    double terrainElevation( ReosMesh *mesh ) const;

  private:
    ReosMeshPointValue_p *d = nullptr;
};

class ReosMeshPointValueOnVertex : public ReosMeshPointValue_p
{
  public:
    ReosMeshPointValueOnVertex( int vertexIndex, int faceindex, const QPointF &point );

    int vertex() const;

  protected:
    double interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const override;
    double interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const override;
    double interpolateTerrainElevation( ReosMesh *mesh ) const override;

  private:
    int mVertexIndex = -1;
    int mFaceIndex = -1;

};

class ReosMeshPointValueOnEdge: public ReosMeshPointValue_p
{
  public:
    ReosMeshPointValueOnEdge( int vertexIndex1, int vertexIndex2, int face1, int face2, double posInEdge, const QPointF &point );

    int vertex1() const;
    int vertex2() const;

  protected:
    double interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const override;
    double interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const override;
    double interpolateTerrainElevation( ReosMesh *mesh ) const override;

    double interpolateValueOnEdge( double value1, double value2 ) const;

  private:
    int mVertex1 = -1;
    int mVertex2 = -1;
    int mFace1 = -1;
    int mFace2 = -1;
    double mPosInEdge = 0;

};

class ReosMeshPointValueOnFace: public ReosMeshPointValue_p
{
  public:
    ReosMeshPointValueOnFace( int vertexIndex1, int vertexIndex2, int vertexIndex3,
                              int face,
                              double lam1, double lam2, double lam3,
                              const QPointF &point );


  protected:
    double interpolateValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const override;
    double interpolateVectorValue( const QVector<double> &values, ReosMeshDatasetSource::Location location ) const override;
    double interpolateTerrainElevation( ReosMesh *mesh ) const override;

  private:
    int mVertex1 = -1;
    int mVertex2 = -1;
    int mVertex3 = -1;
    int mFace = -1;
    double mLam1 = 0;
    double mLam2 = 0;
    double mLam3 = 0;

    double interpolateValueOnFace( double v1, double v2, double v3 ) const;
};

/**
 * Class dedicaded to embed any specific mesh data derived from ReosMeshData::Data.
 * The derived class must return a pointer to this specific data and caller
 * of ReosMeshData::data() must know the type to cast it.
 */
class REOSCORE_EXPORT ReosMeshData
{
  public:
    class Data
    {
      public:
        virtual const void *data() const = 0;
        virtual ~Data() {}
    };

    ReosMeshData() = default;
    explicit ReosMeshData( Data *data );

    const void *data() const;

  private:
    std::shared_ptr<Data> mData;
};

class REOSCORE_EXPORT ReosMesh: public ReosRenderedObject
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

    struct WireFrameSettings
    {
      bool enabled = false;
      QColor color;
      double width = 0.2;
    };

    //! Creates a new void mesh in memory
    static ReosMesh *createMeshFrame( const QString &crs = QString(), QObject *parent = nullptr );

    static ReosMesh *createMeshFrameFromFile( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message );

    virtual void resetMeshFrameFromeFile( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message ) = 0;

    //! Returns whether the mesh is valid
    virtual bool isValid() const = 0;

    //! Returns vertex count
    virtual int vertexCount() const = 0;

    //! Returns face count
    virtual int faceCount() const = 0;

    //! Returns whether the mesh frame is modified
    virtual bool isFrameModified() const = 0;

    //! Clears ans generates a new mesh with \a data
    virtual void generateMesh( const ReosMeshFrameData &data ) = 0;

    virtual QString crs() const = 0;

    virtual QVector<QVector<int>> faces() const = 0;

    virtual QVector<int> face( int faceIndex ) const = 0 ;

    virtual QPointF vertexPosition( int vertexIndex, const QString &destinationCrs = QString() ) const = 0;

    virtual double vertexElevation( int vertexIndex ) const = 0 ;

    virtual QObject *data() const = 0;

    //! Returns a new instance of ReosMeshData that contains data related to the mesh frame
    virtual ReosMeshData meshDataFrame() const = 0;

    //! Returns the faces that intersect the \a polyline in map coordinates
    virtual QList<ReosMeshPointValue> drapePolyline( const QPolygonF &polyline, double tolerance ) const = 0;

    /**
     * Activate vertices elevation as a dataset group with \a name, returns a unique id of this dataset group.
     */
    virtual QString enableVertexElevationDataset( const QString &name ) = 0;

    //! Returns the vertices elevation dataset id
    virtual QString verticesElevationDatasetId() const = 0;

    //! Returns all the dataset ids contained in the mesh
    virtual QStringList datasetIds() const = 0;

    //! Returns all the vector dataset ids contained in the mesh
    virtual QStringList vectorDatasetIds() const = 0;

    //! Returns the name of the dataset with \a id
    virtual QString datasetName( const QString &id ) const = 0;

    //! Activates the dataset with \a id
    virtual bool activateDataset( const QString &id, bool update = true ) = 0;

    //! Activates the vector dataset with \a id
    virtual bool activateVectorDataset( const QString &id, bool update = true ) = 0;

    //! Returns the current scalar dataset Id
    virtual QString currentdScalarDatasetId() const = 0;

    //! Returns the current vector dataset Id
    virtual QString currentdVectorDatasetId() const = 0;

    //! Returns whether the mesh has a dataset group with \a id
    virtual bool hasDatasetGroupIndex( const QString &id ) const = 0;

    //! Returns a process that apply the topography collection on the mesh, caller take ownership
    virtual ReosProcess *applyTopographyOnVertices( ReosTopographyCollection *topographyCollection ) = 0;

    //! Apply the dem on the mesh
    virtual void applyDemOnVertices( ReosDigitalElevationModel *dem, const QString &destnationCrs ) = 0;

    //! Returns the value of dataset \a datasetId at position \a pos in map coordinates
    virtual double datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const = 0;

    //! Returns by reference the min max for the dataset group corresponding to \a id
    virtual void datasetGroupMinimumMaximum( const QString &datasetId, double &min, double &max ) const = 0;

    //! Save the mesh frame on UGRID file with path \a dataPath
    virtual void save( const QString &dataPath ) = 0;

    virtual void stopFrameEditing( bool commit, bool continueEditing = false ) = 0;

    virtual ReosColorShaderSettings *scalarColorShaderSettings() const = 0;

    virtual ReosColorShaderSettings *vectorColorShaderSettings() const = 0;

    virtual ReosColorShaderSettings *terrainColorShaderSettings() const = 0;

    virtual ReosEncodedElement datasetScalarGroupSymbology( const QString &id ) const = 0;

    //! Needed fo the vector settings widget
    virtual ReosEncodedElement datasetVectorGroupSymbology( const QString &id ) const = 0;

    virtual void setDatasetVectorGroupSymbology( const ReosEncodedElement &encodedElement, const QString &id ) = 0;

    virtual void activateDynamicTraces( bool activated ) = 0;

    virtual bool isDynamicTracesActive() const = 0;

    virtual void activateWireFrame( bool activate ) = 0;
    virtual bool isWireFrameActive() const = 0;

    //! Return the mesh frame symbology with encoded format
    virtual ReosEncodedElement wireFrameSymbology() const = 0;

    virtual WireFrameSettings wireFrameSettings() const = 0 ;

    virtual void setWireFrameSettings( const WireFrameSettings &wireFrameSettings, bool update = true ) = 0;

    virtual void update3DRenderer() = 0;

    //! Returns a process that check the quality of the mesh, caller take ownership
    virtual ReosMeshQualityChecker *getQualityChecker( QualityMeshChecks qualitiChecks, const QString &destinatonCrs ) const = 0;

    virtual void setSimulationResults( ReosHydraulicSimulationResults *result, const QString &destinationCrs ) = 0;

    virtual double interpolateDatasetValueOnPoint( const ReosMeshDatasetSource *datasetSource, const ReosSpatialPosition &position, int sourceGroupindex, int datasetIndex ) const = 0;

    virtual bool rasterizeDatasetValue( const QString &fileName, int sourceGroupindex, int datasetIndex, const QString destinationCrs, double resolution ) const = 0;

    QualityMeshParameters qualityMeshParameters() const;
    void setQualityMeshParameter( const ReosEncodedElement &element );

    void setBoundariesVertices( const QVector<QVector<int> > &vertices );
    void setHolesVertices( const QVector<QVector<QVector<int> > > &vertices );

    bool vertexIsOnBoundary( int vertexIndex ) const;
    bool vertexIsOnHoleBorder( int vertexIndex ) const;

    virtual QString verticalDataset3DId() const = 0;
    virtual void setVerticalDataset3DId( const QString &verticalDataset3DId, bool update = true ) = 0;

    double verticaleSCale() const;
    void setVerticaleSCale( double verticaleSCale );

    QMap<QString, QByteArray> datasetScalarSymbologies() const;
    void setDatasetScalarSymbologies( const QMap<QString, QByteArray> &datasetScalarSymbologies );

    QMap<QString, QByteArray> datasetVectorSymbologies() const;
    void setDatasetVectorSymbologies( const QMap<QString, QByteArray> &datasetVectorSymbologies );

    virtual QString exportAsMesh( const QString &fileName, ReosModule::Message &message ) const = 0;
    virtual ReosModule::Message exportSimulationResults( ReosHydraulicSimulationResults *result, const QString &fileName ) const = 0;

  signals:
    void terrainSymbologyChanged();

  protected:
    ReosMesh( QObject *parent = nullptr );

    QualityMeshParameters mQualityMeshParameters;
    QSet<int> mBoundaryVerticesSet;
    QSet<int> mHolesVerticesSet;
    QMap<QString, QByteArray> mDatasetScalarSymbologies;
    QMap<QString, QByteArray> mDatasetVectorSymbologies;

    double mVerticaleSCale = 1;
};

#endif // REOSMESH_H
