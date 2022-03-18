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

#include <qgsmeshlayer.h>
#include <qgsrendercontext.h>
#include <qgsmesheditor.h>

#include "reosmesh.h"

class ReosMeshDataProvider_p;
class ReosMeshFrameData;
class QgsMeshDatasetGroup;
class ReosDigitalElevationModel;

class QGraphicsView;
class QgsMapLayerRenderer;

/**
 * Implementation of a mesh in Reos environment.
 * This class contains a QgsMeshLayer that can be independant from the QgsProject.
 * The data provider of this QGIS layer is a derived class of QgsMeshDataProvider that allow creation of mesh frame in memory
 * and custom behaviors, especially mesh generation and editing.
 */
class ReosMeshFrame_p : public ReosMesh
{
  public:
    ReosMeshFrame_p( const QString &crs, QObject *parent );
    ReosMeshFrame_p( const QString &dataPath );
    bool isValid() const override;
    int vertexCount() const override;
    int faceCount() const override;
    QString enableVertexElevationDataset( const QString &name ) override;
    bool activateDataset( const QString &id ) override;
    void generateMesh( const ReosMeshFrameData &data ) override;
    QString crs() const override;
    QObject *data() const override;
    int datasetGroupIndex( const QString &id ) const override;
    void applyTopographyOnVertices( ReosTopographyCollection *topographyCollection ) override;
    double datasetScalarValueAt( const QString &datasetId, const QPointF &pos ) const override;

    void save( const QString &dataPath ) const override;
    void stopFrameEditing( bool commit ) override;

    ReosEncodedElement meshSymbology() const override;
    void setMeshSymbology( const ReosEncodedElement &symbology ) override;
    ReosEncodedElement datasetGroupSymbology( const QString &id ) const override;

    ReosObjectRenderer *createRenderer( QGraphicsView *view ) override;

    ReosMeshQualityChecker *getQualityChecker( QualityMeshChecks qualitiChecks, const QString &destinatonCrs ) const override;

    QString verticesElevationDatasetId() const override;

  private:

    std::unique_ptr<QgsMeshLayer> mMeshLayer;
    ReosMeshDataProvider_p *meshProvider() const;
    QMap<QString, int> mDatasetGroupsIndex;
    QgsMeshDatasetGroup *mZVerticesDatasetGroup = nullptr;
    QString mVerticesElevationDatasetName;
    QString mVerticesElevationDatasetId;


    void init();
    void activateVertexZValueDatasetGroup();
    QString addDatasetGroup( QgsMeshDatasetGroup *group, const QString &id = QString() );
    void firstUpdateOfTerrainScalarSetting();
    void restoreVertexElevationDataset();

    std::map <QGraphicsView *, std::unique_ptr<QgsMapLayerRenderer>> mRenders;
};

class ReosMeshRenderer_p : public ReosObjectRenderer
{
  public:
    ReosMeshRenderer_p( QGraphicsView *canvas, QgsMeshLayer *layer );
    void render() const;

  protected:
    void stopRendering();

  private:
    std::unique_ptr<QgsMapLayerRenderer> mLayerRender;
    std::unique_ptr<QPainter> mPainter;
    QgsRenderContext mRenderContext;

};


class ReosMeshQualityChecker_p : public ReosMeshQualityChecker
{
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

#endif // REOSMESH_P_H
