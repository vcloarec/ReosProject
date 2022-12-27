/***************************************************************************
  reosmeshdataprovider_p.h - ReosMeshDataProvider_p

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
#ifndef REOSMESHDATAPROVIDER_P_H
#define REOSMESHDATAPROVIDER_P_H

#include <qgsmeshdataprovider.h>
#include <qgsmeshdataset.h>
#include <qgsprovidersublayerdetails.h>

#include "reosmesh.h"
#include "reosmeshgenerator.h"
#include "reosmeshdatasetsource.h"

class ReosMeshGenerator;
struct ReosMeshFrameData;
class ReosDigitalElevationModel;
class ReosTopographyCollection;

class ReosMeshDataProvider_p: public QgsMeshDataProvider
{
    Q_OBJECT
  public:

    ReosMeshDataProvider_p();

    void setFilePath( const QString &filePath );

    void generateMesh( const ReosMeshFrameData &data );

    void applyDemOnVertices( ReosDigitalElevationModel *dem );

    void applyTopographyOnVertices( ReosTopographyCollection *topographyCollection, ReosProcess *process = nullptr );

    //! Overrides the crs, used when the mesh provider is created from scratch
    void overrideCrs( const QgsCoordinateReferenceSystem &crs );

    void loadMeshFrame( const QString &filePath, ReosModule::Message &message );

    bool saveMeshFrameToFile( const QgsMesh &mesh );

    void setDatasetSource( ReosMeshDatasetSource *datasetSource );

    //! Dataset handling
  public:
    bool addDataset( const QString & ) override {return false;}
    QStringList extraDatasets() const override {return QStringList();}
    int datasetGroupCount() const override;
    int datasetCount( int groupIndex ) const override;
    QgsMeshDatasetGroupMetadata datasetGroupMetadata( int groupIndex ) const override;
    QgsMeshDatasetMetadata datasetMetadata( QgsMeshDatasetIndex index ) const override;
    QgsMeshDatasetValue datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const override;
    QgsMeshDataBlock datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const override;
    QgsMesh3dDataBlock dataset3dValues( QgsMeshDatasetIndex index, int faceIndex, int count ) const override;

    bool isFaceActive( QgsMeshDatasetIndex, int ) const override; //not implemented
    QgsMeshDataBlock areFacesActive( QgsMeshDatasetIndex index, int faceIndex, int count ) const override;

    bool persistDatasetGroup( const QString &outputFilePath,
                              const QString &outputDriver,
                              const QgsMeshDatasetGroupMetadata &meta,
                              const QVector<QgsMeshDataBlock> &datasetValues,
                              const QVector<QgsMeshDataBlock> &datasetActive,
                              const QVector<double> &times ) override; //not implemented


    bool persistDatasetGroup( const QString &outputFilePath,
                              const QString &outputDriver,
                              QgsMeshDatasetSourceInterface *source,
                              int datasetGroupIndex ) override; // not implemented

    int vertexCount() const override;
    int faceCount() const override;
    int edgeCount() const override; //not implemented
    void populateMesh( QgsMesh *mesh ) const override;

    bool saveMeshFrame( const QgsMesh &mesh ) override;

    QgsCoordinateReferenceSystem crs() const override {return mCrs;}
    QgsRectangle extent() const override;
    bool isValid() const override  {return true;}
    QString name() const override  {return "ReosMeshMemory";}
    QString description() const override {return "reos mesh";}

    void close() override {}
    virtual QgsMeshDriverMetadata driverMetadata()  const override;

//***********************

  private:
    QgsMesh mMesh;
    QgsCoordinateReferenceSystem mCrs;
    QString mFilePath;
    QgsRectangle mExtent;
    ReosMeshDatasetSource *mDatasetSource = nullptr;

    static QgsMesh convertFrameFromReos( const ReosMeshFrameData &reosMesh );
};


class ReosMeshProviderMetaData: public QgsProviderMetadata
{
  public:
    ReosMeshProviderMetaData();

    ReosMeshDataProvider_p *createProvider( const QString &, const QgsDataProvider::ProviderOptions &, QgsDataProvider::ReadFlags ) override;

    ProviderCapabilities providerCapabilities() const override {return FileBasedUris;}
    QgsProviderMetadata::ProviderMetadataCapabilities capabilities() const override {return QgsProviderMetadata::LayerTypesForUri;}
};

#endif // REOSMESHDATAPROVIDER_P_H
