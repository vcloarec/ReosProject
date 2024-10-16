/***************************************************************************
  reosmesh_p.h - ReosMesh_p

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
#ifndef REOSMESH_P_H
#define REOSMESH_P_H

#define SIP_NO_FILE

#include <qgsmeshlayer.h>
#include <qgsrendercontext.h>
#include <qgsmesheditor.h>
#include <qgsmeshdataset.h>

#include "reosmesh.h"
#include "reoshydraulicsimulationresults.h"
#include "reosrenderersettings_p.h"

class ReosMeshDataProvider_p;
struct ReosMeshFrameData;
class QgsMeshDatasetGroup;
class ReosDigitalElevationModel;

class QGraphicsView;
class QgsMapLayerRenderer;

class ReosMeshScalarColorShaderSettings_p;
class ReosMeshVectorColorShaderSettings_p;
class ReosMeshTerrainColorShaderSettings_p;

class ReosMeshRendererCache_p;

class ReosRendererMeshMapTimeStamp_p: public ReosRendererObjectMapTimeStamp
{
  public:
    ReosRendererMeshMapTimeStamp_p( const QgsMeshDatasetIndex &scalarIndex, const QgsMeshDatasetIndex &vectorIndex, quint64 tracesAges );
    bool equal( ReosRendererObjectMapTimeStamp *other ) override;

  private:
    QgsMeshDatasetIndex mScalarIndex;
    QgsMeshDatasetIndex mVectorIndex;
    quint64 mTracesAge;
};

class ReosMeshData_ : public ReosMeshData::Data
{
  public:
    ReosMeshData_( const QgsMesh &mesh )
      : mMesh( mesh )
    {
    }

    ~ReosMeshData_();
    const void *data() const
    {
      return static_cast<const void *>( &mMesh );
    }

  private:
    QgsMesh mMesh;
};


/**
 * Implementation of a mesh in Reos environment.
 * This class contains a QgsMeshLayer that can be independant from the QgsProject.
 * The data provider of this QGIS layer is a derived class of QgsMeshDataProvider that allow creation of mesh frame in memory
 * and custom behaviors, especially mesh generation and editing.
 */
class ReosMeshFrame_p : public ReosMesh
{
    Q_OBJECT
  public:
    ReosMeshFrame_p( const QString &crs, QObject *parent );
    ReosMeshFrame_p( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message );
    ~ReosMeshFrame_p();

    void resetMeshFrameFromeFile( const QString &dataPath, const QString &destinationCrs, ReosModule::Message &message ) override;
    bool isValid() const override;
    int vertexCount() const override;
    QPointF vertexPosition( int vertexIndex, const QString &destinationCrs = QString() ) const override;
    double vertexElevation( int vertexIndex ) const override;
    QVector<QVector<int>> faces() const override;
    QVector<int> face( int faceIndex ) const override;
    int faceCount() const override;
    QList<ReosMeshPointValue> drapePolyline( const QPolygonF &polyline, double tolerance ) const override;
    QString enableVertexElevationDataset( const QString &name ) override;
    bool isFrameModified() const override;
    void generateMesh( const ReosMeshFrameData &data ) override;
    QString crs() const override;
    QObject *data() const override;
    ReosMeshData meshDataFrame() const override;
    ReosMapExtent extent() const override;
    ReosProcess *applyTopographyOnVertices( ReosTopographyCollection *topographyCollection ) override;
    void applyDemOnVertices( ReosDigitalElevationModel *dem, const QString &destinationCrs ) override;
    void applyConstantZValue( double zValue, const QString &destinationCrs ) override;
    double datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const override;
    void datasetGroupMinimumMaximum( const QString &datasetId, double &min, double &max ) const override;
    void save( const QString &dataPath ) override;
    void stopFrameEditing( bool commit, bool continueEditing = false ) override;
    ReosColorShaderSettings *scalarColorShaderSettings() const override;
    ReosColorShaderSettings *vectorColorShaderSettings() const override;
    ReosColorShaderSettings *terrainColorShaderSettings() const override;
    ReosEncodedElement datasetScalarGroupSymbology( const QString &id ) const override;
    ReosEncodedElement datasetVectorGroupSymbology( const QString &id ) const override;
    void activateDynamicTraces( bool activated ) override;
    bool isDynamicTracesActive() const override;
    ReosEncodedElement wireFrameSymbology() const override;
    void setDatasetVectorGroupSymbology( const ReosEncodedElement &encodedElement, const QString &id ) override;
    void activateWireFrame( bool activate ) override;
    bool isWireFrameActive() const override;

    ReosRendererObjectMapTimeStamp *createMapTimeStamp( ReosRendererSettings *settings ) const override;
    ReosObjectRenderer *createRenderer( ReosRendererSettings *settings ) override;
    void updateInternalCache( ReosObjectRenderer *renderer ) override;

    ReosMeshQualityChecker *getQualityChecker( QualityMeshChecks qualitiChecks, const QString &destinatonCrs ) const override;
    void setSimulationResults( ReosHydraulicSimulationResults *result, const QString &destinationCrs ) override;
    QString verticesElevationDatasetId() const override;
    bool activateDataset( const QString &id, bool update = true ) override;
    bool activateVectorDataset( const QString &id, bool update = true ) override;
    QStringList datasetIds() const override;
    QStringList vectorDatasetIds() const override;
    QString datasetName( const QString &id ) const override;
    bool hasDatasetGroupIndex( const QString &id ) const override;
    QString verticalDataset3DId() const override;
    void setVerticalDataset3DId( const QString &verticalDataset3DId, bool update = true ) override;
    QString currentdScalarDatasetId() const override;
    QString currentdVectorDatasetId() const override;
    void update3DRenderer() override;
    WireFrameSettings wireFrameSettings() const override ;
    void setWireFrameSettings( const WireFrameSettings &wireFrameSettings, bool update ) override;
    double interpolateDatasetValueOnPoint( const ReosMeshDatasetSource *datasetSource, const ReosSpatialPosition &position, int sourceGroupindex, int datasetIndex ) const override;
    bool rasterizeDatasetValue( const QString &fileName, const QString &groupId, int datasetIndex, QString detinationCrs, double resolution ) const override;
    QString exportAsMesh( const QString &fileName, ReosModule::Message &message ) const override;
    ReosModule::Message exportSimulationResults( ReosHydraulicSimulationResults *result, const QString &fileName, const ReosTimeWindow &timeWindow = ReosTimeWindow() ) const override;
    QList<ReosColorShaderSettings *> colorShaderSettings() const override;

    ReosMeshRendererCache_p *rendererCache() const;

  private:
    std::unique_ptr<QgsMeshLayer> mMeshLayer;
    ReosMeshDataProvider_p *mMeshDataProvider = nullptr;
    ReosMeshDataProvider_p *meshProvider() const;
    QMap<QString, int> mDatasetGroupsIndex;
    QgsMeshDatasetGroup *mZVerticesDatasetGroup = nullptr;
    QString mVerticesElevationDatasetName;
    const QString mVerticesElevationDatasetId = "vertices-elevation";
    QString mCurrentScalarDatasetId;
    QString mCurrentActiveVectorDatasetId;
    bool mTraceIsActive = false;
    QString mVerticalDataset3DId;
    WireFrameSettings mWireFrameSettings;

    std::unique_ptr<ReosMeshScalarColorShaderSettings_p> mScalarShaderSettings;
    std::unique_ptr<ReosMeshTerrainColorShaderSettings_p> mTerrainShaderSettings;
    std::unique_ptr<ReosMeshVectorColorShaderSettings_p> mVectorShaderSettings;

    std::unique_ptr<ReosMeshRendererCache_p> mRendererCache;

    void init();
    void addDatasetGroup( QgsMeshDatasetGroup *group, const QString &id = QString() );
    void firstUpdateOfTerrainScalarSetting();
    void restoreVertexElevationDataset();
    void updateWireFrameSettings( bool updateRenderer );
    int datasetGroupIndex( const QString &id ) const;
    ReosEncodedElement restoreScalarSymbologyOnMeshDatasetGroup( const QString &id );
    void applySymbologyOnScalarDataSet( const QString &id, QgsMeshRendererScalarSettings settings );
    ReosEncodedElement restoreVectorSymbologyOnMeshDatasetGroup( const QString &id );
    void applySymbologyOnVectorDataSet( const QString &id, QgsMeshRendererVectorSettings settings );

    QPointF tolayerCoordinates( const ReosSpatialPosition &position ) const;

    //! Returns the scalar symbology from the layer, if not exist return the mesh layer default symbology
    ReosEncodedElement datasetGroupScalarSymbologyfromLayer( const QString &datasetId ) const;
    ReosEncodedElement datasetGroupVectorSymbologyfromLayer( const QString &datasetId ) const;

    void renderingNeedUpdate();

    friend class ReosMeshScalarColorShaderSettings_p;
    friend class ReosMeshVectorColorShaderSettings_p;
    friend class ReosMeshTerrainColorShaderSettings_p;
};

class ReosMeshColorShaderSettings_p : public ReosColorShaderSettings_p
{
    Q_OBJECT
  public:
    ReosMeshColorShaderSettings_p( ReosMeshFrame_p *mesh );

    ReosMeshColorShaderSettings_p *clone() const override {return nullptr;}
    double classificationMinimum() const override;
    void setClassificationMinimum( double newClassificationMinimum ) override;
    double classificationMaximum() const override;
    void setClassificationMaximum( double newClassificationMaximum ) override;
    double opacity() const override;
    void setOpacity( double opacity ) override;

  protected:
    QPointer<ReosMeshFrame_p> mMesh;
    double mMinClassifiction;
    double mMaxClassification;
    double mOpacity;
    bool mIsValid = false;
};

class ReosMeshScalarColorShaderSettings_p : public ReosMeshColorShaderSettings_p
{
  public:
    ReosMeshScalarColorShaderSettings_p( ReosMeshFrame_p *mesh );
    void setCurrentSymbology( const ReosEncodedElement &symbology );

    bool isValid() const override;
    bool getDirectSourceMinMax( double &min, double &max ) const override;
    void onSettingsUpdated() override;

    QString title() const override;
};

class ReosMeshVectorColorShaderSettings_p : public ReosMeshColorShaderSettings_p
{
  public:
    ReosMeshVectorColorShaderSettings_p( ReosMeshFrame_p *mesh );
    void setCurrentSymbology( const ReosEncodedElement &symbology );

    bool isValid() const override;
    bool getDirectSourceMinMax( double &min, double &max ) const override;
    void onSettingsUpdated() override;

};

class ReosMeshTerrainColorShaderSettings_p : public ReosMeshColorShaderSettings_p
{
    Q_OBJECT
  public:
    ReosMeshTerrainColorShaderSettings_p( ReosMeshFrame_p *mesh );
    void setCurrentSymbology( const ReosEncodedElement &symbology );

    bool isValid() const override;
    bool getDirectSourceMinMax( double &min, double &max ) const override;
    void onSettingsUpdated() override;

    QString title() const override;

  signals:

    void meshTerrainSettingsChanged();
};

class ReosMeshQualityChecker_p : public ReosMeshQualityChecker
{
    Q_OBJECT
  public:

    ReosMeshQualityChecker_p( const QgsMesh &mesh,
                              ReosMesh::QualityMeshParameters params,
                              const QgsDistanceArea &distanceArea,
                              ReosMesh::QualityMeshChecks checks,
                              const QgsCoordinateTransform &transform );

    void start() override;
    QualityMeshResults result() const override;

  private:
    QgsMesh mMesh;
    double mMinimumAngle = 0;
    double mMaximumAngle = 0;
    int mConnectionCount = 0;
    int mConnectionCountBoundary = 0;
    double mMaximumSlope = 0;
    double mMinimumArea = 0; //in m2
    double mMaximumArea = 0; //in m2
    double mMaximumAreaChange = 0;
    QgsDistanceArea mDistanceArea;
    ReosMesh::QualityMeshChecks mChecks;
    QgsMeshEditingError mError;
    QgsCoordinateTransform mTransform;
};


class ReosResultDatasetGroup : public QgsMeshDatasetGroup
{
  public:
    ReosResultDatasetGroup( ReosMeshDatasetSource *simResult, int index, bool faceSupportActiveFlag = true, const ReosTimeWindow &timeWindow = ReosTimeWindow() );

    void initialize();
    QgsMeshDatasetMetadata datasetMetadata( int datasetIndex ) const;
    int datasetCount() const;

    QgsMeshDataset *dataset( int index ) const;
    QgsMeshDatasetGroup::Type type() const;
    QDomElement writeXml( QDomDocument &doc, const QgsReadWriteContext &context ) const {return QDomElement();}

  private:
    ReosMeshDatasetSource *mSimulationResult = nullptr;
    int mGroupIndex = -1;
    bool mFacesSupportActiveFlag = true;
    std::vector<std::unique_ptr<QgsMeshDataset>> mDatasets;
    ReosTimeWindow mTimeWindow;
};

class ReosResultDataset : public QgsMeshDataset
{
  public:
    ReosResultDataset( ReosMeshDatasetSource *simResult, int groupIndex, int index, bool faceSupportActiveFlag = true );

    QgsMeshDatasetValue datasetValue( int valueIndex ) const;
    QgsMeshDataBlock datasetValues( bool isScalar, int valueIndex, int count ) const;
    QgsMeshDataBlock areFacesActive( int faceIndex, int count ) const;
    bool isActive( int faceIndex ) const;
    QgsMeshDatasetMetadata metadata() const;
    int valuesCount() const;

  private:
    ReosMeshDatasetSource *mSimulationResult = nullptr;
    int mGroupIndex = -1;
    bool mfaceSupportActiveFlag = false;
    int mDatasetIndex = -1;

};

#endif // REOSMESH_P_H
