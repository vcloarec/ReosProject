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
class ReosMeshFrameData;
class ReosDigitalElevationModel;
class ReosTopographyCollection;

class ReosMeshDataProvider_p: public QgsMeshDataProvider
{
    Q_OBJECT
  public:
    ReosMeshDataProvider_p(): QgsMeshDataProvider( "mesh", QgsDataProvider::ProviderOptions() ) {}

    void setFilePath( const QString &filePath );
    void setMDALDriver( const QString &driverName );

    void generateMesh( const ReosMeshFrameData &data );

    void applyDemOnVertices( ReosDigitalElevationModel *dem );

    void applyTopographyOnVertices( ReosTopographyCollection *topographyCollection );

    //! Overrides the crs, used when the mesh provider is created from scratch
    void overrideCrs( const QgsCoordinateReferenceSystem &crs );

    void loadMeshFrame( const QString &filePath, const QString &driverName );

    bool saveMeshFrameToFile( const QgsMesh &mesh );

    void setDatasetSource( ReosMeshDatasetSource *datasetSource );

  public:
    bool addDataset( const QString &uri ) override {return false;}
    QStringList extraDatasets() const override {return QStringList();}
    int datasetGroupCount() const override;
    int datasetCount( int groupIndex ) const override;
    QgsMeshDatasetGroupMetadata datasetGroupMetadata( int groupIndex ) const override;
    QgsMeshDatasetMetadata datasetMetadata( QgsMeshDatasetIndex index ) const override;
    QgsMeshDatasetValue datasetValue( QgsMeshDatasetIndex index, int valueIndex ) const override;
    QgsMeshDataBlock datasetValues( QgsMeshDatasetIndex index, int valueIndex, int count ) const override;
    QgsMesh3dDataBlock dataset3dValues( QgsMeshDatasetIndex index, int faceIndex, int count ) const override;

    bool isFaceActive( QgsMeshDatasetIndex index, int faceIndex ) const {return true;}
    QgsMeshDataBlock areFacesActive( QgsMeshDatasetIndex index, int faceIndex, int count ) const;
    bool persistDatasetGroup( const QString &outputFilePath,
                              const QString &outputDriver,
                              const QgsMeshDatasetGroupMetadata &meta,
                              const QVector<QgsMeshDataBlock> &datasetValues,
                              const QVector<QgsMeshDataBlock> &datasetActive,
                              const QVector<double> &times ) {return false;}
    bool persistDatasetGroup( const QString &outputFilePath,
                              const QString &outputDriver,
                              QgsMeshDatasetSourceInterface *source,
                              int datasetGroupIndex )
    {return false;}

    int vertexCount() const;
    int faceCount() const;
    int edgeCount() const {return 0;}
    void populateMesh( QgsMesh *mesh ) const;

    bool saveMeshFrame( const QgsMesh &mesh ) override;

    QgsCoordinateReferenceSystem crs() const {return mCrs;}
    QgsRectangle extent() const;
    bool isValid() const {return true;}
    QString name() const {return "ReosMeshMemory";}
    QString description() const {return "reos mesh";}

    void close() {}
    virtual QgsMeshDriverMetadata driverMetadata()  const;

//***********************

  private:
    QgsMesh mMesh;
    QgsCoordinateReferenceSystem mCrs;
    QString mFilePath;
    QString mMDALDriverName;
    QgsRectangle mExtent;
    ReosMeshDatasetSource *mDatasetSource = nullptr;

    static QgsMesh convertFrameFromReos( const ReosMeshFrameData &reosMesh );

};


class ReosMeshProviderMetaData: public QgsProviderMetadata
{
  public:
    ReosMeshProviderMetaData() : QgsProviderMetadata( QStringLiteral( "ReosMesh" ), QStringLiteral( "reos mesh" ) ) {}
    QString filters( FilterType type ) override {return QString();}
    QList<QgsMeshDriverMetadata> meshDriversMetadata() override {return QList<QgsMeshDriverMetadata>();}
    ReosMeshDataProvider_p *createProvider( const QString &uri, const QgsDataProvider::ProviderOptions &options, QgsDataProvider::ReadFlags flags = QgsDataProvider::ReadFlags() ) override
    {
      return new ReosMeshDataProvider_p();
    }

    bool createMeshData( const QgsMesh &mesh,
                         const QString &fileName,
                         const QString &driverName,
                         const QgsCoordinateReferenceSystem &crs ) const override {return false;}

    bool createMeshData( const QgsMesh &mesh,
                         const QString &uri,
                         const QgsCoordinateReferenceSystem &crs ) const override  {return false;}

    QVariantMap decodeUri( const QString &uri ) const override {return QVariantMap();}
    QString encodeUri( const QVariantMap &parts ) const override {return QString();}
    ProviderCapabilities providerCapabilities() const override {return FileBasedUris;}
    QgsProviderMetadata::ProviderMetadataCapabilities capabilities() const override {return QgsProviderMetadata::LayerTypesForUri;}
    QList< QgsProviderSublayerDetails > querySublayers( const QString &uri, Qgis::SublayerQueryFlags flags = Qgis::SublayerQueryFlags(), QgsFeedback *feedback = nullptr ) const override {return QList< QgsProviderSublayerDetails >() ;}
};

#endif // REOSMESHDATAPROVIDER_P_H
